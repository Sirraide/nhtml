#include <fmt/format.h>

/// Duktape fatal error handler.
extern "C" [[gnu::used, noreturn]] void fatal_handler(void*, const char* msg) {
    fmt::print(stderr, "[NHTML] Fatal: {}\n", msg);
    std::abort();
}