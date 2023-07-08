cmake_minimum_required(VERSION 3.25)
project(nhtml VERSION 0.1.0 LANGUAGES CXX)

## ============================================================================
##  Global CMake Variables.
## ============================================================================
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})
set(CMAKE_CXX_STANDARD_REQUIRED ON)

## ============================================================================
##  Global compiler options.
## ============================================================================
## Turn on diagnostics colours.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-fdiagnostics-color=always)
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-fcolor-diagnostics)
endif()

## ============================================================================
##  Compiler options.
## ============================================================================
add_library(_nhtml_options INTERFACE)

## Flags for Clang and GCC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(_nhtml_options INTERFACE
        ## Warnings.
        -Wall -Wextra     # Enable ‘all’ warnings.
        -Wundef           # Invalid #undef or undefined macro in #if.
        -Wshadow          # Shadowing.
        -Wcast-align      # Casting that changes alignment.
        -Wconversion      # Implicit conversions.
        -Wsign-conversion # Implicit sign conversions.
        -Wlogical-op      # Duplicate or unintended logical operators.
        -Wformat=2        # Stricter format checking.

        ## Disabled warnings.
        -Wno-unused-function
        -Wno-unused-local-typedefs
        -Wno-unused-parameter

        ## NULL Errors.
        -Werror=null-dereference
        -Werror=nonnull # Passing NULL to nonnull parameter.

        ## Memory Errors.
        -Werror=address              # Suspicious use of addresses.
        -Werror=init-self            # Initialization of a variable with itself.
        -Werror=invalid-memory-model # For atomics.
        -Werror=maybe-uninitialized
        -Werror=uninitialized
        -Werror=return-local-addr

        ## Return type.
        -Werror=return-type
        -Werror=missing-noreturn

        ## C/C++.
        -Werror=implicit-fallthrough
        -Werror=missing-include-dirs # User-specified include dir does not exist.
        -Werror=pointer-arith        # Disallow void* and function pointer arithmetic.
        -Werror=string-compare       # Nonsensical string comparisons.
        -Werror=switch               # Missing switch cases.
        # -Werror=switch-enum          # Switch on enum (even if there is a default case).
        -Werror=write-strings        # Strings in C should be const char*.

        ## C++.
        -Werror=missing-field-initializers
        -Werror=missing-requires
        -Werror=non-virtual-dtor
        -Werror=pessimizing-move
    )
endif()

## Additional flags for Clang.
if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(_nhtml_options INTERFACE -Werror=dangling )
endif()

## Flags for MSVC.
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_compile_options(_nhtml_options INTERFACE
        /W4 # Enable ‘all’ warnings.

        # Allow unnamed structs/unions.
        /wd4201
    )
endif()

## On Windows, don’t suggest the _s nonsense functions.
if (WIN32)
    target_compile_definitions(func PRIVATE
        _CRT_SECURE_NO_WARNINGS
        _CRT_SECURE_NO_WARNINGS_GLOBALS
        _CRT_NONSTDC_NO_WARNINGS
    )
endif()

## Debug/Release flags.
target_compile_options(_nhtml_options INTERFACE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic>
    $<$<CONFIG:RELEASE>:-O3 -march=native>
)
target_link_options(_nhtml_options INTERFACE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic>
    $<$<CONFIG:RELEASE>:-O3 -march=native>
)
target_compile_definitions(_nhtml_options INTERFACE
    $<$<CONFIG:DEBUG>:_GLIBCXX_DEBUG=1>
)

## ============================================================================
##  Submodules and include dirs.
## ============================================================================
## Link against libfmt.
target_link_libraries(_nhtml_options INTERFACE fmt)

## ‘src’ should be an include directory.
target_include_directories(_nhtml_options INTERFACE src)

## ‘include’ too, if it exists.
if (EXISTS ${PROJECT_SOURCE_DIR}/include)
    target_include_directories(_nhtml_options INTERFACE include)
endif()

## As well as everything in ‘libs’.
if (EXISTS ${PROJECT_SOURCE_DIR}/libs)
    file(GLOB libs ${PROJECT_SOURCE_DIR}/libs/*)

    ## Add the include directory to the include path, or the root
    ## directory if there is no include directory.
    foreach(lib ${libs})
        if (IS_DIRECTORY ${lib}/include)
            target_include_directories(_nhtml_options INTERFACE ${lib}/include)
        else()
            target_include_directories(_nhtml_options INTERFACE ${lib})
        endif()
    endforeach()

    ## Also add all of them as subdirectories if they have a CMakeLists.txt.
    foreach (lib ${libs})
        if (EXISTS ${lib}/CMakeLists.txt)
            add_subdirectory(${lib})
        endif()
    endforeach()
endif()

if (DEFINED CLOPTS_INCLUDE_DIR)
    target_include_directories(_nhtml_options INTERFACE "${CLOPTS_INCLUDE_DIR}")
endif()

## ============================================================================
##  V8.
## ============================================================================
if (NOT NHTML_DISABLE_EVAL)
    target_link_libraries(_nhtml_options INTERFACE v8)
else()
    add_definitions(-DNHTML_DISABLE_EVAL)
endif()

## ============================================================================
##  Executables and libraries.
## ============================================================================
## Everything in ‘src’ is part of the project.
file(GLOB_RECURSE library_sources lib/*.cc include/*.hh)

## As well as everything in ‘include’, if it exists.
if (EXISTS ${PROJECT_SOURCE_DIR}/include)
    file(GLOB_RECURSE includes include/*.hh)
    list(APPEND sources ${includes})
endif()

## Add the library and the executable.
add_library(nhtml STATIC ${library_sources})

if (NHTML_BUILD_NHTMLC)
    add_executable(nhtmlc src/nhtmlc.cc)
endif()

if (UNIX AND NHTML_BUILD_TESTS)
    add_executable(run-tests src/test_runner.cc)
endif()

## Apply our options.
target_include_directories(nhtml PUBLIC "${PROJECT_SOURCE_DIR}/include")
target_link_libraries(nhtml PRIVATE _nhtml_options)

if (NHTML_BUILD_NHTMLC)
    target_link_libraries(nhtmlc PRIVATE _nhtml_options nhtml)
endif()

if (UNIX AND NHTML_BUILD_TESTS)
    target_compile_definitions(run-tests PRIVATE "__NHTML_DIR__=${PROJECT_SOURCE_DIR}")
    target_link_libraries(run-tests PRIVATE _nhtml_options nhtml)
endif()
