/// Test runner for NHTML. Copied from a different project. Disclaimer: hardly any thought
/// was put into the design of this test runner save for the multithreading aspect of it, for
/// which reason it is probably one of messiest C++ programs out there. This is not good code
/// and it should not be taken as a reference for anything.

#ifndef __linux__
#    error Sorry, this program only runs on linux.
#endif

#include <algorithm>
#include <csignal>
#include <fcntl.h>
#include <filesystem>
#include <fmt/format.h>
#include <fstream>
#include <mutex>
#include <nhtml/internal/file.hh>
#include <nhtml/nhtml.hh>
#include <ranges>
#include <sys/wait.h>
#include <thread>
#include <valarray>
#include <vector>
#include <clopts.hh>

#define STR_(x) #x
#define STR(x) STR_(x)

#define CAT_(x, y) x##y
#define CAT(x, y) CAT_(x, y)

namespace fs = std::filesystem;
namespace rgs = std::ranges;

#define defer auto CAT($$defer_instance_, __COUNTER__) = $$defer{} % [&]()

template <typename callable>
struct $$defer_type {
    callable cb;
    explicit $$defer_type(callable&& _cb)
        : cb(std::forward<callable>(_cb)) {}
    ~$$defer_type() { cb(); }
};

struct $$defer {
    template <typename callable>
    $$defer_type<callable> operator%(callable&& cb) {
        return $$defer_type<callable>{std::forward<callable>(cb)};
    }
};

template <typename... arguments>
void print(FILE* file, fmt::format_string<arguments...> fmt, arguments&&... args) {
    flockfile(file);
    fmt::print(file, fmt, std::forward<arguments>(args)...);
    funlockfile(file);
}

template <typename... arguments>
void print(fmt::format_string<arguments...> fmt, arguments&&... args) {
    ::print(stdout, fmt, std::forward<arguments>(args)...);
}

template <typename... arguments>
[[noreturn]] void die(fmt::format_string<arguments...> fmt, arguments&&... args) {
    ::print(stderr, fmt, std::forward<arguments>(args)...);
    print(stderr, "\n");
    std::_Exit(1);
}

struct errored : std::runtime_error {
    using std::runtime_error::runtime_error;
};

struct timeout : std::runtime_error {
    using std::runtime_error::runtime_error;
};

struct failed : std::runtime_error {
    template <typename... arguments>
    failed(fmt::format_string<arguments...> fmt, arguments&&... args)
        : std::runtime_error{fmt::format(fmt, std::forward<arguments>(args)...)} {}
};

struct invalid : std::runtime_error {
    template <typename... arguments>
    invalid(fmt::format_string<arguments...> fmt, arguments&&... args)
        : std::runtime_error{fmt::format(fmt, std::forward<arguments>(args)...)} {}
};

struct pipes {
    int fd[2];
    pipes() {
        if (pipe2(fd, O_CLOEXEC) < 0) throw invalid{"Could not create pipe: {}", std::strerror(errno)};

        /// Make reads non-blocking.
        int flags = fcntl(fd[0], F_GETFL, 0);
        if (flags < 0) throw invalid{"Could not get pipe flags: {}", std::strerror(errno)};
        if (fcntl(fd[0], F_SETFL, flags | O_NONBLOCK) < 0) throw invalid{"Could not set pipe flags: {}", std::strerror(errno)};
    }

    ~pipes() {
        close(fd[0]);
        close(fd[1]);
    }
};

std::size_t thread_count;
std::valarray<size_t> tests_run, tests_errored, tests_failed, tests_invalid, tests_skipped, tests_passed;
const fs::path test_dir = fs::path{STR(__NHTML_DIR__)} / "tests";

using exit_status = int;
using exit_code = int;

std::pair<exit_status, std::string> run_command(auto&& executable, const std::vector<std::string_view>& args, size_t thread_id) {
    const char* exe_name;
    if constexpr (requires { executable.data(); }) exe_name = std::forward<decltype(executable)>(executable).data();
    else if constexpr (requires { executable.c_str(); }) exe_name = std::forward<decltype(executable)>(executable).c_str();
    else exe_name = std::forward<decltype(executable)>(executable);

    std::vector<const char*> argv;
    argv.reserve(args.size() + 2);
    argv.push_back(exe_name);
    for (auto& arg : args) argv.push_back(arg.data());
    argv.push_back(nullptr);

    pipes p;
    auto pid = fork();
    if (pid < 0) throw invalid{"Could not fork: {}", std::strerror(errno)};

    /// Child process.
    if (pid == 0) {
        dup2(p.fd[1], STDOUT_FILENO);
        dup2(p.fd[1], STDERR_FILENO);
        close(p.fd[0]);
        close(p.fd[1]);
        execvp(exe_name, (char* const*) argv.data());
        std::_Exit(127);
    }

    /// Read the output and wait for the child to exit.
    std::string output{};
    char buf[1024]{};
    ssize_t n{};
    size_t sleep_count{};
    int status{};
    bool exited = false;
    for (;;) {
        /// Read from the pipe.
        errno = 0;
        n = read(p.fd[0], buf, sizeof(buf));
        if (n < 0) {
            if (errno == EINTR) continue;
            if (exited) break;

            /// If we couldn’t, check if the child has exited.
            if (errno == EAGAIN
#if EAGAIN != EWOULDBLOCK
                or errno == EWOULDBLOCK
#endif
            ) {
                errno = 0;
                auto res = waitpid(pid, &status, WNOHANG);
                if (res < 0) {
                    if (errno == EINTR) continue;
                    throw invalid{"Could not wait for child: {}", std::strerror(errno)};
                }

                /// The child is still running; sleep for a bit.
                if (res == 0) {
                    if (sleep_count++ < 100) {
                        std::this_thread::sleep_for(std::chrono::milliseconds(10));
                        continue;
                    }

                    /// Otherwise, kill the child.
                    kill(pid, SIGKILL);
                    throw timeout{""};
                }

                /// The child has exited.
                if (WIFEXITED(status) or WIFSIGNALED(status)) {
                    exited = true;

                    /// Try reading one more time.
                    continue;
                }
            }

            /// Otherwise, there was some other error.
            throw invalid{"Could not read from pipe: {}", std::strerror(errno)};
        }

        /// Done reading.
        if (n == 0) break;

        /// Append the output.
        output.append(buf, size_t(n));
    }

    /// Wait for the child to exit.
    if (not exited) {
        do {
            if (waitpid(pid, &status, 0) < 0) {
                if (errno == EINTR) continue;
                throw invalid{"Could not wait for child: {}", std::strerror(errno)};
            }
        } while (not WIFEXITED(status) and not WIFSIGNALED(status));
    }

    return {status, std::move(output)};
}

struct [[nodiscard]] result {
    exit_code code;
    std::string error;
    std::string output;
};

template <typename error_type>
result run_with_timeout(auto&& command, const std::vector<std::string_view>& args, size_t thread_id) {
    try {
        auto&& [status, output] = run_command(std::forward<decltype(command)>(command), args, thread_id);
        if (not WIFEXITED(status)) return {status, fmt::format("Child process terminated by signal {}\n", WTERMSIG(status)), std::move(output)};
        return {WEXITSTATUS(status), "", std::move(output)};
    } catch (const timeout&) {}
    throw error_type{"Child process timed out"};
}

constexpr int signame2int(std::string_view name) {
    if (name == "SIGABRT") return SIGABRT;
    if (name == "SIGINT") return SIGINT;
    if (name == "SIGTERM") return SIGTERM;
    if (name == "SIGKILL") return SIGKILL;
    if (name == "SIGTRAP") return SIGTRAP;
    if (name == "SIGILL") return SIGILL;
    if (name == "SIGFPE") return SIGFPE;
    if (name == "SIGSEGV") return SIGSEGV;
    if (name == "SIGBUS") return SIGBUS;
    if (name == "SIGPIPE") return SIGPIPE;
    throw invalid{"Invalid signal name: {}", name};
}

constexpr std::string_view int2signame(int sig) {
    switch (sig) {
        case SIGABRT: return "SIGABRT";
        case SIGINT: return "SIGINT";
        case SIGTERM: return "SIGTERM";
        case SIGKILL: return "SIGKILL";
        case SIGTRAP: return "SIGTRAP";
        case SIGILL: return "SIGILL";
        case SIGFPE: return "SIGFPE";
        case SIGSEGV: return "SIGSEGV";
        case SIGBUS: return "SIGBUS";
        case SIGPIPE: return "SIGPIPE";
        default: return "???";
    }
}

template <int colour, int escape_colour>
std::string escape(std::string str) {
    for (size_t i = 0; i < str.size(); i++) {
        switch (str[i]) {
            case '\a': {
                static const auto s = fmt::format("\033[{}m\\a\033[{}m", escape_colour, colour);
                str.replace(i, 1, s);
                i += 1;
            } break;
            case '\b': {
                static const auto s = fmt::format("\033[{}m\\b\033[{}m", escape_colour, colour);
                str.replace(i, 1, s);
                i += 1;
            } break;
            case '\f': {
                static const auto s = fmt::format("\033[{}m\\f\033[{}m", escape_colour, colour);
                str.replace(i, 1, s);
                i += 1;
            } break;
            case '\r': {
                static const auto s = fmt::format("\033[{}m\\r\033[{}m", escape_colour, colour);
                str.replace(i, 1, s);
                i += 1;
            } break;
            case '\t': {
                static const auto s = fmt::format("\033[{}m\\t\033[{}m", escape_colour, colour);
                str.replace(i, 1, s);
                i += 1;
            } break;
            case '\v': {
                static const auto s = fmt::format("\033[{}m\\v\033[{}m", escape_colour, colour);
                str.replace(i, 1, s);
                i += 1;
            } break;
            case '\0': {
                static const auto s = fmt::format("\033[{}m\\0\033[{}m", escape_colour, colour);
                str.replace(i, 1, s);
                i += 1;
            } break;
            default: break;
        }
    }
    return str;
}

#define CATCH()                                                                    \
    catch (const failed& e) {                                                      \
        tests_failed[id] += 2;                                                     \
        print("\033[31m[Failure]\033[33m {}: \033[31m{}\n\n", basename, e.what()); \
    }                                                                              \
    catch (const errored& e) {                                                     \
        tests_errored[id]++;                                                       \
        print("\033[31m[Errored]\033[33m {}: \033[m\n{}\n", basename, e.what());   \
    }                                                                              \
    catch (const std::exception& e) {                                              \
        tests_invalid[id]++;                                                       \
        print("\033[34m[Invalid]\033[33m {}: \033[34m{}\n\n", basename, e.what()); \
    }                                                                              \
    catch (...) {                                                                  \
        tests_invalid[id]++;                                                       \
        print("\033[34m[Invalid]\033[33m {}: Unknown error\n\n", basename);        \
    }

void run_test(const fs::path& path, size_t id) {
    auto basename = relative(path, test_dir).string();
    try {
        tests_run[id]++;

        /// Get the expected output.
        auto expected_path = test_dir / "expected" / relative(path, test_dir);
        expected_path.replace_extension(".html");
        auto expected = nhtml::detail::file::map(expected_path);
        if (not expected) throw invalid("{}", std::move(expected.error()));

        /// Get the content of the first line of the test, which must be a comment.
        std::string line;
        std::ifstream heresy{path};
        std::getline(heresy, line);
        bool should_error = nhtml::detail::tolower(line).contains("error");

        /// Compile the file.
        auto doc = nhtml::parse_file(path);
        if (not doc) throw errored(doc.error());

        /// Stringify the document.
        auto actual = nhtml::detail::trim(doc->string({}));
        auto expected_text = nhtml::detail::trim(std::move(expected->contents).string());
        if ((actual == expected_text) == should_error) throw failed(
            "\033[33mExpected:\n\033[32m{}\033[33m\nActual:\n\033[31m{}\033[33m",
            escape<32, 36>(expected_text),
            escape<31, 35>(actual)
        );

        /// Test passed.
        tests_passed[id]++;
        print("\033[32m[Passed]\033[33m {}\n", basename);
    }
    CATCH()
}

using namespace command_line_options;
using options = clopts< // clang-format off
    option<"--threads", "Number of threads to use", nhtml::i64>,
    help<>
>; // clang-format on

int main(int argc, char **argv) {
    options::parse(argc, argv);

    /// Make sure the test dir exists..
    if (not fs::exists(test_dir)) die("Test directory does not exist: '{}'", test_dir.string());

    /// Get all the test files.
    std::vector<fs::path> tests;
    for (const auto& entry : fs::recursive_directory_iterator{test_dir}) {
        /// Skip anything that isn’t a dir or a file.
        if (not entry.is_regular_file()) continue;

        /// Skip non-nhtml files.
        if (entry.path().extension() != ".nhtml") continue;

        /// Add the test.
        tests.emplace_back(entry.path());
    }

    /// Determine the number of threads to use.
    thread_count = (nhtml::usz) options::get_or<"--threads">(std::min<size_t>(std::thread::hardware_concurrency(), tests.size()));
    thread_count = std::max<nhtml::usz>(thread_count, 1);

    /// Initialise the data.
    tests_run.resize(thread_count + 1);
    tests_errored.resize(thread_count + 1);
    tests_failed.resize(thread_count + 1);
    tests_invalid.resize(thread_count + 1);
    tests_skipped.resize(thread_count + 1);
    tests_passed.resize(thread_count + 1);

    /// Start the threads.
    std::vector<std::jthread> threads;
    for (size_t i = 0; i < thread_count; i++) threads.emplace_back([&, i] {
        auto per_thread = tests.size() / thread_count;
        for (size_t j = i * per_thread; j < std::min(tests.size(), (i + 1) * per_thread); j++) run_test(tests[j], i);
    });

    /// Run remaining tests on the main thread.
    for (size_t i = thread_count * (tests.size() / thread_count); i < tests.size(); i++) run_test(tests[i], thread_count);

    /// Wait for all the threads to finish.
    rgs::for_each(threads, &std::jthread::join);

    /// Print summary.
    print("\033[33mTotal:    {}\n", tests_run.sum());
    print("\033[33mSkipped:  {}\n", tests_skipped.sum());
    print("\033[33mPassed:   {}{}\n", tests_passed.sum() ? "\033[32m" : "\033[31m", tests_passed.sum());
    print("\033[33mFailed:   {}{}\n", tests_failed.sum() ? "\033[31m" : "\033[32m", tests_failed.sum());
    print("\033[33mErrored:  {}{}\n", tests_errored.sum() ? "\033[31m" : "\033[32m", tests_errored.sum());
    print("\033[33mInvalid:  {}{}\033[m\n", tests_invalid.sum() ? "\033[31m" : "\033[32m", tests_invalid.sum());
}
