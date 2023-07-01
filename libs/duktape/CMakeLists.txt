cmake_minimum_required(VERSION 3.25)
project(duktape VERSION 0.1.0 LANGUAGES CXX C)

add_library(duktape STATIC duktape.c)
target_include_directories(duktape SYSTEM PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}")
target_compile_options(duktape PRIVATE -fno-exceptions -fno-rtti)

target_compile_options(duktape PRIVATE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic>
    $<$<CONFIG:RELEASE>:-O3 -march=native>
)

target_link_options(duktape PRIVATE
    $<$<CONFIG:DEBUG>:-O0 -g3 -ggdb3 -rdynamic>
    $<$<CONFIG:RELEASE>:-O3 -march=native>
)

target_compile_definitions(duktape PRIVATE
    $<$<CONFIG:DEBUG>:_GLIBCXX_DEBUG=1>
)
