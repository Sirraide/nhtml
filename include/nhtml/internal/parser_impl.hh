#ifndef NHTML_PARSER_IMPL_HH
#define NHTML_PARSER_IMPL_HH

#include <deque>
#include <fmt/color.h>
#include <fmt/format.h>
#include <nhtml/internal/file.hh>
#include <nhtml/internal/utils.hh>
#include <nhtml/internal/source_location.hh>
#include <nhtml/internal/eval.hh>
#include <nhtml/utils.hh>

namespace nhtml::detail {
/// ===========================================================================
///  Tokens
/// ===========================================================================
enum struct tk {
    invalid,
    eof,
    name,
    number,
    string,
    class_name, /// Oh the irony...
    id,

    lbrace,
    rbrace,
    lbrack,
    rbrack,
    lparen,
    rparen,
    percent,

    eq,
    comma,
};

/// A token.
struct token {
    /// The type of the token.
    tk type = tk::invalid;

    /// Token text.
    std::string text;

    /// Number.
    isz integer;

    /// Source location.
    loc location;
};

/// Stringify a token type.
constexpr auto tk_to_str(tk t) -> std::string_view {
    switch (t) {
        case tk::invalid: return "invalid token";
        case tk::eof: return "end of file";
        case tk::name: return "name";
        case tk::class_name: return ".class";
        case tk::id: return "#id";
        case tk::number: return "number";
        case tk::string: return "string";
        case tk::lbrace: return "{";
        case tk::rbrace: return "}";
        case tk::lbrack: return "[";
        case tk::rbrack: return "]";
        case tk::lparen: return "(";
        case tk::rparen: return ")";
        case tk::percent: return "%";
        case tk::eq: return "=";
        case tk::comma: return ",";
    }
    return "<unknown>";
}

/// ===========================================================================
///  Diagnostics.
/// ===========================================================================
/// Diagnostic severity.
enum struct diag_kind {
    error,
    warning,
    note,
    none,
};

/// Get the colour of a diagnostic.
constexpr auto diag_colour(diag_kind kind) {
    switch (kind) {
        case diag_kind::error: return fmt::fg(fmt::terminal_color::red) | fmt::emphasis::bold;
        case diag_kind::warning: return fmt::fg(fmt::terminal_color::yellow) | fmt::emphasis::bold;
        case diag_kind::note: return fmt::fg(fmt::terminal_color::green) | fmt::emphasis::bold;
        default: return fmt::text_style{};
    }
}

/// Get the name of a diagnostic.
constexpr std::string_view diag_name(diag_kind kind) {
    switch (kind) {
        case diag_kind::error: return "Error";
        case diag_kind::warning: return "Warning";
        case diag_kind::note: return "Note";
        default: return "Diagnostic";
    }
}

/// Check if a character is allowed at the start of an identifier.
constexpr bool isstart(char c) {
    return isalpha(c) or c == '_' or c == '$';
}

/// Check if a character is allowed in an identifier.
constexpr bool iscontinue(char c) {
    return isstart(c) or isdigit(c) or c == '-' or c == '!' or c == '?';
}

constexpr bool is_binary(char c) { return c == '0' or c == '1'; }
constexpr bool is_decimal(char c) { return c >= '0' and c <= '9'; }
constexpr bool is_octal(char c) { return c >= '0' and c <= '7'; }
constexpr bool is_hex(char c) { return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F'); }

struct parser {
    using err = std::unexpected<std::string>;

    /// The current token.
    token tok{};

    /// The last character lexed.
    char lastc = ' ';

    /// Lookahead tokens.
    std::vector<token> lookahead_tokens;

    /// Owned files.
    std::deque<file> files;

    /// File stack.
    std::vector<file*> file_stack;

    /// Parsed document.
    document doc;

#ifndef NHTML_DISABLE_EVAL
    /// Script evaluator.
    eval_ctx eval;

    /// Scripts to evaluate.
    std::vector<std::pair<std::string, loc>> scripts;
#endif

    /// Copying/Moving is disallowed.
    parser(const parser&) = delete;
    parser(parser&&) = delete;
    parser& operator=(const parser&) = delete;
    parser& operator=(parser&&) = delete;

    parser();
    ~parser();

    /// =======================================================================
    ///  Lexer Operations
    /// =======================================================================
    /// Issue a diagnostic.
    template <typename... arguments>
    [[nodiscard]] auto diag(
        diag_kind kind,
        loc where,
        fmt::format_string<arguments...> fmt,
        arguments&&... args
    ) -> std::unexpected<std::string> {
        using fmt::fg;
        using enum fmt::emphasis;
        using enum fmt::terminal_color;

        /// If this diagnostic is suppressed, do nothing.
        if (kind == diag_kind::none) return err(""s);

        /// Output string.
        std::string out;

        /// If the location is invalid, either because the specified file does not
        /// exists, its position is out of bounds or 0, or its length is 0, then we
        /// skip printing the location.
        if (not seekable(where)) {
            /// Even if the location is invalid, print the file name if we can.
            if (where.file < files.size()) {
                const auto& file = files[where.file];
                out += fmt::format(bold, "{}: ", file.name.native());
            }

            /// Print the message.
            out += fmt::format(diag_colour(kind), "{}: ", diag_name(kind));
            out += fmt::format(fmt, std::forward<arguments>(args)...);
            out += fmt::format("\n");
            return err(out);
        }

        /// If the location is valid, get the line, line number, and column number.
        const auto [line, col, line_start, line_end] = seek(where);

        /// Split the line into everything before the range, the range itself,
        /// and everything after.
        std::string before(line_start, col);
        std::string range(line_start + col, where.len);
        std::string after(line_start + col + where.len, line_end);

        /// Replace tabs with spaces. We need to do this *after* splitting
        /// because this invalidates the offsets.
        replace_all(before, "\t", "    ");
        replace_all(range, "\t", "    ");
        replace_all(after, "\t", "    ");

        /// Print the file name, line number, and column number.
        const auto& file = files[where.file];
        out += fmt::format(bold, "{}:{}:{}: ", file.name.native(), line, col);

        /// Print the diagnostic name and message.
        out += fmt::format(diag_colour(kind), "{}: ", diag_name(kind));
        out += fmt::format(fmt, std::forward<arguments>(args)...);
        out += fmt::format("\n");

        /// Print the line up to the start of the location, the range in the right
        /// colour, and the rest of the line.
        out += fmt::format(" {} | {}", line, before);
        out += fmt::format(diag_colour(kind), "{}", range);
        out += fmt::format("{}\n", after);

        /// Determine the number of digits in the line number.
        const auto digits = number_width(line);

        /// Underline the range. For that, we first pad the line based on the number
        /// of digits in the line number and append more spaces to line us up with
        /// the range.
        NHTML_REPEAT(digits + before.size() + sizeof("  | ") - 1)
        out += fmt::format(" ");

        /// Finally, underline the range.
        NHTML_REPEAT(range.size())
        out += fmt::format(diag_colour(kind), "~");
        out += fmt::format("\n");
        return err(out);
    }

    auto curr() const -> const char* { return file_stack.back()->curr; }
    auto end() const -> const char* { return file_stack.back()->end; }

    [[nodiscard]] auto lastc_loc() const -> loc;
    [[nodiscard]] auto look_ahead(usz number_of_tokens) -> res<token*>;
    [[nodiscard]] auto next() -> res<void>;
    void next_char();

    [[nodiscard]] auto read_until_chars(
        std::same_as<char> auto... c
    ) -> res<std::string>
    requires (sizeof...(c) >= 1);

    void skip_line();
    void skip_whitespace();

    [[nodiscard]] auto lex_number() -> res<void>;
    [[nodiscard]] auto lex_string(char delim) -> res<void>;
    [[nodiscard]] bool seekable(loc l) const;

    /// Seek to a source location. The location must be valid.
    [[nodiscard]] auto seek(loc l) const -> loc_info;

    /// =======================================================================
    ///  Parser Operations
    /// =======================================================================
    /// Parser primitives.
    bool at(std::same_as<tk> auto&&... t);

    /// Add a file.
    auto add_file(file&& f) -> res<void>;

    auto parse(file&& f) -> res<document>;
    auto parse_element() -> res<element::ptr>;

    auto parse_named_element(
        std::string name,
        loc l,
        element::class_list classes = {},
        std::string id = "",
        element::attribute_list attributes = {},
        element::inline_style style = {}
    ) -> res<element::ptr>;

    auto parse_text_elem() -> res<element::ptr>;
    auto parse_attribute_list(element::attribute_list& attrs) -> res<void>;

    /// Select against a CSS selector.
    auto query_selector_impl(std::string_view selector) -> element*;

    template <tk open = tk::lbrack, char open_char = '[', char close = ']', bool line_comments = false>
    auto parse_nested_language_data(std::string& style) -> res<void>;
};
} // namespace nhtml::detail

#endif // NHTML_PARSER_IMPL_HH
