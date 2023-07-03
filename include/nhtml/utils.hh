#ifndef NHTML_UTILS_HH
#define NHTML_UTILS_HH

#include <algorithm>
#include <chrono>
#include <debug/map>
#include <expected>
#include <filesystem>
#include <fmt/format.h>
#include <ranges>
#include <string>
#include <variant>
#include <vector>

#ifndef __has_builtin
#    define __has_builtin(x) 0
#endif

#define NHTML_FWD(X) std::forward<decltype(X)>(X)

#define NHTML_CHECK(...)                                                           \
    do {                                                                           \
        auto _nhtml_res = __VA_ARGS__;                                             \
        if (not _nhtml_res) return std::unexpected(std::move(_nhtml_res.error())); \
    } while (false)

#define NHTML_STR_(X) #X
#define NHTML_STR(X) NHTML_STR_(X)

#define NHTML_CAT_(X, Y) X##Y
#define NHTML_CAT(X, Y) NHTML_CAT_(X, Y)

#define NHTML_REPEAT(n) NHTML_REPEAT_IMPL(n, NHTML_CAT(_nhtml_repeat_, __COUNTER__), NHTML_CAT(_nhtml_until_, __COUNTER__))
#define NHTML_REPEAT_IMPL(n, count, until) for (size_t count = 0, until = n; count < until; count++)

#define NHTML_FILENAME() ::nhtml::detail::format_filename(__FILE__)
#define NHTML_UNREACHABLE() ::nhtml::detail::assert_fail("Unreachable", __LINE__, NHTML_FILENAME())
#define NHTML_ASSERT(X, ...)                                                                                           \
    do {                                                                                                               \
        if (not x) ::nhtml::detail::assert_fail(NHTML_STR(X), __LINE__, NHTML_FILENAME(), __VA_OPT__(, ) __VA_ARGS__); \
    } while (0)

namespace nhtml {
using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using usz = size_t;
using uptr = uintptr_t;

using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using isz = ptrdiff_t;
using iptr = intptr_t;

namespace chrono = std::chrono;
namespace rgs = std::ranges;
namespace fs = std::filesystem;
using namespace std::literals;

namespace detail {
/// Wrapper around is_same_v.
template <typename T, typename U>
concept is = std::is_same_v<std::remove_cvref_t<T>, std::remove_cvref_t<U>>;

/// Overload set.
template <typename... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};

template <typename... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

template <typename... arguments>
[[noreturn]] void assert_fail(std::string_view cond, int line, std::string_view file, fmt::format_string<arguments...> fmt = "", arguments&&... args) {
    fmt::print(stderr, "{}:{}: Assertion failed '{}'", file, line, cond);
    if constexpr (sizeof...(arguments)) fmt::print(stderr, ": ");
    fmt::print(stderr, fmt, std::forward<arguments>(args)...);
    fmt::print(stderr, "\n");
    std::exit(1);
}

std::string format_filename(std::string_view file);

/// Reference to a string. May be owned or not.
class string_ref : public std::variant<std::string, std::string_view> {
    using base = std::variant<std::string, std::string_view>;
public:
    string_ref()
        : base(""sv) {}

    /// Construct a string_ref from a string.
    string_ref(std::string str)
        : base(std::move(str)) {}

    /// Construct a string_ref from a string_view.
    string_ref(std::string_view str)
        : base(str) {}

    /// Construct a string_ref from a const char*.
    string_ref(const char* str)
        : base(std::string_view{str}) {}

    /// Construct a string_ref from a const char* and a size.
    string_ref(const char* str, usz size)
        : base(std::string_view{str, size}) {}

    /// Construct a string_ref from a string literal.
    template <usz N>
    string_ref(const char (&str)[N])
        : base(std::string_view{str, N - 1}) {}

    /// Get the string data as a string_view.
    [[nodiscard]] explicit operator std::string_view() const { return sv(); }

    /// Get the string data.
    [[nodiscard]] const char* data() const {
        return std::visit([](auto&& arg) { return arg.data(); }, *this);
    }

    /// Get the string size.
    [[nodiscard]] usz size() const {
        return std::visit([](auto&& arg) { return arg.size(); }, *this);
    }

    /// Get the string data as a string_view.
    [[nodiscard]] std::string_view sv() const {
        return std::visit([](auto&& arg) { return std::string_view{arg}; }, *this);
    }

    /// Get the string data as a string.
    [[nodiscard]] std::string string() const& {
        return std::visit([](auto&& arg) { return std::string{arg}; }, *this);
    }

    /// Get the string data as a string.
    [[nodiscard]] std::string string() && {
        /// Move and not forward is intentional.
        return std::visit([](auto&& arg) { return std::string{std::move(arg)}; }, std::move(*this));
    }

    /// Comparison operators.
    bool operator==(const auto& other) const { return std::string_view(*this) == std::string_view(other); }
};

/// Convert to lowercase.
inline std::string tolower(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), [](char c) { return std::tolower(c); });
    return str;
}

/// Result type.
template <typename value_type = void>
struct [[nodiscard]] res : std::expected<value_type, std::string> {
    using base = std::expected<value_type, std::string>;
    using base::base;
};

/// This causes problems, so we overwrite it.
template <>
struct [[nodiscard]] res<bool> : std::expected<bool, std::string> {
    using base = std::expected<bool, std::string>;
    using base::base;

    /// This is semantically ambiguous.
    operator bool() = delete;

    /// Check the value.
    [[nodiscard]] bool is_true() const { return base::operator bool() and base::value(); }

    /// Check if there is an error.
    [[nodiscard]] bool is_error() const { return not base::operator bool(); }
};

/// Case-insensitive string map.
template <typename value_type>
class icase_map {
    using map_type = std::map<std::string, value_type>;
    map_type map{};

public:
    /// Insert or get a value.
    ///
    /// Note: We accept a string by value since we need to transform
    /// it to lowercase anyway to look up the value.
    [[nodiscard]] auto operator[](std::string key) -> value_type& { return map[tolower(std::move(key))]; }

    /// Insert a value only if it doesn’t already exist.
    /// \return true if the value was inserted, false if it already existed.
    [[nodiscard]] bool try_emplace(std::string key, value_type value) {
        auto [it, inserted] = map.try_emplace(tolower(std::move(key)), std::move(value));
        return inserted;
    }

    /// Find a value.
    [[nodiscard]] auto find(std::string key) const { return map.find(tolower(std::move(key))); }

    void clear() { map.clear(); }

    auto begin() const { return map.begin(); }
    auto end() const { return map.end(); }

    usz size() const { return map.size(); }
};

/// Replace all occurrences of `from` with `to` in `str`.
[[gnu::always_inline]] constexpr void replace_all(
    std::string& str,
    std::string_view from,
    std::string_view to
) {
    if (from.empty()) return;
    for (usz i = 0; i = str.find(from, i), i != std::string::npos; i += to.length())
        str.replace(i, from.length(), to);
}

/// Determine the width of a number.
constexpr usz number_width(usz number, usz base = 10) {
    return number == 0 ? 1 : usz(std::log(number) / std::log(base) + 1);
}

/// Escape a a string.
std::string escape(std::string str);

/// Trim a string.
auto trim(is<std::string> auto&& s) -> decltype(s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) { return !std::isspace(ch); }));
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) { return !std::isspace(ch); }).base(), s.end());
    return std::forward<decltype(s)>(s);
}

} // namespace detail

} // namespace nhtml
#endif // NHTML_UTILS_HH
