# NHTML

NHTML stands for ‘**N**ot **HTML**’, and is a templating language that compiles to HTML. It is intended to both make writing
and parsing HTML documents easier.

This project consists of two main parts: the NHTML library and `nhtmlc`, a simple command-line utility that uses the library
to compile NHTML files and projects to HTML.

## Building
This project uses CMake, and [fmtlib](https://github.com/fmtlib/fmt). To build it, make sure you have all required dependencies
and run the following commands:
```bash
mkdir -p out
cmake -B out
cmake --build out
```
## NHTML Library
To use the library, include `<nhtml/nhtml.hh>`. The `nhtml::document` class represents a single NHTML document. You can either
create your own document and add elements to it (as well as to those elements), which you can create by calling `nhtml::element::make()`,
or you can parse an NHTML file or string using `nhtml::parse_file()`/`nhtml::parse()`.

The document can be written to a file or string by calling `document.write(FILE*, options)`/`document.string(options)`. The `options`
are an object of type `nhtml::document::write_options` and can be used to configure the output. Alternatively, to use the default
options, simply pass `{}`.

To link against the library, simply link against `libnhtml.a` or against the `nhtml` target if you’re including this
project via CMake.

## NHTML Compiler
The compiler is not built by default. To use it, you also need [libclopts](https://github.com/Sirraide/clopts). Either put the
`clopts.hh` header somewhere in your include path or set `-DCLOPTS_INCLUDE_DIR=/path/to/clopts/include` when running CMake. You
also have to define `-DNHTML_BUILD_NHTMLC=ON` when configuring the build.

For more information on how to use the compiler, run `nhtmlc --help`.

## NHTML Syntax
Forthcoming. For more information, see `grammar.bnf` as well as the
tests in `tests/`.

## Tests
This project also includes a test runner, but for simplicity’s sake, it is only supported and thus only built on Linux,
and only if `-DNHTML_BUILD_TESTS=ON` is passed to CMake.