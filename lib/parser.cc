#include <deque>
#include <fmt/color.h>
#include <nhtml/parser.hh>
#include <unordered_set>
#include <utility>

namespace nhtml::detail {
namespace {
struct parser;
using el = element::ptr;
using err = std::unexpected<std::string>;

#define check NHTML_CHECK

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
auto tk_to_str(tk t) -> std::string_view {
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
static constexpr auto diag_colour(diag_kind kind) {
    switch (kind) {
        case diag_kind::error: return fmt::fg(fmt::terminal_color::red) | fmt::emphasis::bold;
        case diag_kind::warning: return fmt::fg(fmt::terminal_color::yellow) | fmt::emphasis::bold;
        case diag_kind::note: return fmt::fg(fmt::terminal_color::green) | fmt::emphasis::bold;
        default: return fmt::text_style{};
    }
}

/// Get the name of a diagnostic.
static constexpr std::string_view diag_name(diag_kind kind) {
    switch (kind) {
        case diag_kind::error: return "Error";
        case diag_kind::warning: return "Warning";
        case diag_kind::note: return "Note";
        default: return "Diagnostic";
    }
}

/// ===========================================================================
///  Parser
/// ===========================================================================
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

    /// Copying/Moving is disallowed.
    parser() = default;
    parser(const parser&) = delete;
    parser(parser&&) = delete;
    parser& operator=(const parser&) = delete;
    parser& operator=(parser&&) = delete;

    /// =======================================================================
    ///  Lexer Operations
    /// =======================================================================
    /// Issue a diagnostic.
    template <typename... arguments>
    auto diag(diag_kind kind, loc where, fmt::format_string<arguments...> fmt, arguments&&... args) -> std::unexpected<std::string> {
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
        NHTML_REPEAT (digits + before.size() + sizeof("  | ") - 1) out += fmt::format(" ");

        /// Finally, underline the range.
        NHTML_REPEAT (range.size()) out += fmt::format(diag_colour(kind), "~");
        out += fmt::format("\n");
        return err(out);
    }

    auto curr() const -> const char* { return file_stack.back()->curr; }
    auto end() const -> const char* { return file_stack.back()->end; }

    auto lastc_loc() const -> loc {
        loc l;
        l.pos = static_cast<u32>(curr() - file_stack.back()->contents.data() - 1);
        l.len = 1;
        l.file = static_cast<u16>(file_stack.back()->file_index);
        return l;
    }

    auto look_ahead(usz number_of_tokens) -> res<token*> {
        /// If we don't have enough tokens, lex them.
        if (lookahead_tokens.size() <= number_of_tokens) {
            auto current_token = std::move(tok);
            while (lookahead_tokens.size() < number_of_tokens) {
                check(next());
                lookahead_tokens.push_back(std::move(tok));
                tok = {};
            }
            tok = std::move(current_token);
        }

        /// Return the token.
        return &lookahead_tokens[number_of_tokens - 1];
    }

    auto next() -> res<void> {
        /// Pop lookahead tokens.
        if (not lookahead_tokens.empty()) {
            tok = std::move(lookahead_tokens.back());
            lookahead_tokens.pop_back();
            return {};
        }

        /// Skip whitespace.
        skip_whitespace();

        /// Keep returning EOF if we're at EOF.
        if (lastc == 0) {
            tok.type = tk::eof;
            return {};
        }

        /// Reset the token. We set the token type to 'invalid' here so that,
        /// if we encounter an error, we can just issue a diagnostic and return
        /// without setting the token type. The parser will then stop because
        /// it encounters an invalid token.
        tok.type = tk::invalid;
        tok.location.pos = static_cast<u32>(curr() - file_stack.back()->contents.data() - 1);

        /// Reads a name.
        auto read_name = [this] {
            /// Characters that delimit a name.
            static constexpr auto name_delims = "()[]{}.,#= \t\r\n\f\v"sv;

            tok.type = tk::name;
            tok.text.clear();
            while (not name_delims.contains(lastc) and lastc != 0) {
                tok.text += lastc;
                next_char();
            }
        };

        /// Lex the token.
        switch (lastc) {
            case '(':
                next_char();
                tok.type = tk::lparen;
                break;

            case '[':
                next_char();
                tok.type = tk::lbrack;
                break;

            case ']':
                next_char();
                tok.type = tk::rbrack;
                break;

            case ')':
                next_char();
                tok.type = tk::rparen;
                break;

            case '{':
                next_char();
                tok.type = tk::lbrace;
                break;

            case '}':
                next_char();
                tok.type = tk::rbrace;
                break;

            /// Maybe comment.
            case '/':
                /// Lookahead to see if this is a comment.
                if (curr() < end() and *curr() == '/') {
                    skip_line();
                    return next();
                }

                /// Not a comment. Handle in default case.
                goto default_case;

            /// Class.
            case '.':
                next_char();
                read_name();
                tok.type = tk::class_name;
                break;

            /// ID.
            case '#':
                next_char();
                read_name();
                tok.type = tk::id;
                break;

            /// Equals sign.
            case '=':
                next_char();
                tok.type = tk::eq;
                break;

            /// Comma.
            case ',':
                next_char();
                tok.type = tk::comma;
                break;

            default:
            default_case:
                read_name();
                break;
        }

        /// Set the end of the token.
        tok.location.len = static_cast<u16>(curr() - file_stack.back()->contents.data() - tok.location.pos - 1);
        if (curr() == end() and not lastc) tok.location.len++;
        return {};
    }

    void next_char() {
        if (curr() == end()) {
            lastc = 0;
            return;
        }

        lastc = *file_stack.back()->curr++;

        /// Collapse CRLF and LFCR to a single newline,
        /// but keep CRCR and LFLF as two newlines.
        if (lastc == '\r' || lastc == '\n') {
            /// Two newlines in a row.
            if (curr() != end() && (*curr() == '\r' || *curr() == '\n')) {
                bool same = lastc == *curr();
                lastc = '\n';

                /// CRCR or LFLF.
                if (same) return;

                /// CRLF or LFCR.
                file_stack.back()->curr++;
            }

            /// Either CR or LF followed by something else.
            lastc = '\n';
        }
    }

    auto read_until_chars(std::same_as<char> auto... c) -> res<std::string>
    requires (sizeof...(c) >= 1)
    {
        std::string s;
        while (((lastc != c) and ...) and lastc != 0) {
            s += lastc;
            next_char();
        }

        /// Check for EOF.
        if (lastc == 0) {
            if constexpr (sizeof...(c) == 1) return diag(diag_kind::error, loc{}, "Unexpected end of file while looking for '{}'", c...);
            else {
                std::string chars;
                ((chars += fmt::format("'{}', ", c)), ...);
                chars.pop_back(), chars.pop_back();
                return diag(diag_kind::error, loc{}, "Unexpected end of file while looking for one of {}", std::move(chars));
            }
        }

        /// Get the next token.
        check(next());

        /// Return lexed text.
        return s;
    }

    void skip_line() {
        while (lastc != '\n' && lastc != 0) next_char();
    }

    void skip_whitespace() {
        while (std::isspace(lastc)) next_char();
    }

    auto lex_number() -> res<void> {
        /// Helper function that actually parses a number.
        auto lex_number_impl = [this](bool pred(char), usz conv(char), usz base) -> res<void> {
            /// Need at least one digit.
            if (not pred(lastc)) return diag(diag_kind::error, lastc_loc() << 1 <<= 1, "Invalid integer literal");

            /// Parse the literal.
            usz value{};
            do {
                usz old_value = value;
                value *= base;

                /// Check for overflow.
                if (value < old_value) {
                overflow:
                    /// Consume the remaining digits so we can highlight the entire thing in the error.
                    while (pred(lastc)) next_char();
                    return diag(diag_kind::error, loc{tok.location.pos, lastc_loc()} >>= -1, "Integer literal overflow");
                }

                /// Add the next digit.
                old_value = value;
                value += conv(lastc);
                if (value < old_value) goto overflow;

                /// Yeet it.
                next_char();
            } while (pred(lastc));

            /// The next character must not be a start character.
            if (isstart(lastc))
                return diag(diag_kind::error, loc{tok.location.pos, lastc_loc()}, "Invalid character in integer literal: '{}'", lastc);

            /// We have a valid integer literal!
            tok.type = tk::number;
            tok.integer = isz(value);
            return {};
        };

        /// If the first character is a 0, then this might be a non-decimal constant.
        if (lastc == 0) {
            next_char();

            /// Hexadecimal literal.
            if (lastc == 'x' or lastc == 'X') {
                next_char();
                static const auto xctoi = [](char c) -> usz {
                    switch (c) {
                        case '0' ... '9': return static_cast<usz>(c - '0');
                        case 'a' ... 'f': return static_cast<usz>(c - 'a');
                        case 'A' ... 'F': return static_cast<usz>(c - 'A');
                        default: NHTML_UNREACHABLE();
                    }
                };
                return lex_number_impl(is_hex, xctoi, 16);
            }

            /// Octal literal.
            if (lastc == 'o' or lastc == 'O') {
                next_char();
                return lex_number_impl(
                    is_octal,
                    [](char c) { return static_cast<usz>(c - '0'); },
                    8
                );
            }

            /// Binary literal.
            if (lastc == 'b' or lastc == 'B') {
                next_char();
                return lex_number_impl(
                    is_binary,
                    [](char c) { return static_cast<usz>(c - '0'); },
                    2
                );
            }

            /// Multiple leading 0’s are not permitted.
            if (std::isdigit(lastc))
                return diag(diag_kind::error, lastc_loc() << 1, "Leading 0 in integer literal. (Hint: Use 0o/0O for octal literals)");

            /// Integer literal must be a literal 0.
            if (isstart(lastc))
                return diag(diag_kind::error, lastc_loc() <<= 1, "Invalid character in integer literal: '{}'", lastc);

            /// Integer literal is 0.
            tok.type = tk::number;
            tok.integer = 0;
            return {};
        }

        /// If the first character is not 0, then we have a decimal literal.
        return lex_number_impl(
            is_decimal,
            [](char c) { return static_cast<usz>(c - '0'); },
            10
        );
    }

    auto lex_string(char delim) -> res<void> {
        /// Yeet the delimiter.
        tok.text.clear();
        next_char();

        /// Lex the string. If it’s a raw string, we don’t need to
        /// do any escaping.
        if (delim == '\'') {
            while (lastc != delim && lastc != 0) {
                tok.text += lastc;
                next_char();
            }
        }

        /// Otherwise, we also need to replace escape sequences.
        else if (delim == '"') {
            while (lastc != delim && lastc != 0) {
                if (lastc == '\\') {
                    next_char();
                    switch (lastc) {
                        case 'a': tok.text += '\a'; break;
                        case 'b': tok.text += '\b'; break;
                        case 'f': tok.text += '\f'; break;
                        case 'n': tok.text += '\n'; break;
                        case 'r': tok.text += '\r'; break;
                        case 't': tok.text += '\t'; break;
                        case 'v': tok.text += '\v'; break;
                        case '\\': tok.text += '\\'; break;
                        case '\'': tok.text += '\''; break;
                        case '"': tok.text += '"'; break;
                        case '0': tok.text += '\0'; break;
                        default:
                            return diag(diag_kind::error, {tok.location.pos, lastc_loc()}, "Invalid escape sequence");
                    }
                } else {
                    tok.text += lastc;
                }
                next_char();
            }
        }

        /// Other string delimiters are invalid.
        else { return diag(diag_kind::error, lastc_loc() << 1, "Invalid delimiter: {}", delim); }

        /// Make sure we actually have a delimiter.
        if (lastc != delim) return diag(diag_kind::error, lastc_loc() << 1, "Unterminated string literal");
        next_char();

        /// This is a valid string.
        tok.type = tk::string;
        return {};
    }

    [[nodiscard]] bool seekable(loc l) const {
        if (l.file >= files.size()) return false;
        const auto& f = files[l.file];
        return usz(l.pos) + l.len <= f.contents.size() and l.len;
    }

    /// Seek to a source location. The location must be valid.
    [[nodiscard]] auto seek(loc l) const -> loc_info {
        loc_info info{};

        /// Get the file that the location is in.
        const auto& f = files[l.file];

        /// Seek back to the start of the line.
        const char* const data = f.contents.data();
        info.line_start = data + l.pos;
        while (info.line_start > data and *info.line_start != '\n') info.line_start--;
        if (*info.line_start == '\n') info.line_start++;

        /// Seek forward to the end of the line.
        const char* const line_end = data + f.contents.size();
        info.line_end = data + l.pos + l.len;
        while (info.line_end < line_end and *info.line_end != '\n') info.line_end++;

        /// Determine the line and column number.
        info.line = 1;
        for (const char* d = data; d < data + l.pos; d++) {
            if (*d == '\n') {
                info.line++;
                info.col = 0;
            } else {
                info.col++;
            }
        }

        /// Done!
        return info;
    }

    /// =======================================================================
    ///  Parser Operations
    /// =======================================================================
#define advance()                                                \
    do {                                                         \
        auto r = next();                                         \
        if (not r) return std::unexpected{std::move(r.error())}; \
    } while (false)

    /// Parser primitives.
    bool at(std::same_as<tk> auto&&... t) { return ((tok.type == t) or ...); }

    /// Add a file.
    auto add_file(file&& f) -> res<void> {
        if (files.size() > std::numeric_limits<u16>::max()) return mkerr("Sorry, but we can't handle more than 65536 files.");

        files.push_back(std::move(f));
        file_stack.push_back(&files.back());
        auto newf = file_stack.back();

        newf->file_index = u16(files.size() - 1);
        newf->curr = newf->contents.data();
        newf->end = newf->contents.data() + newf->contents.size();

        return {};
    }

    /// Parse the input.
    /// <document> ::= { <element> }
    auto parse(file&& f) -> res<document> {
        /// Add the file.
        check(add_file(std::move(f)));

        /// Read the first token.
        next_char();
        check(next());

        /// Parse the document.
        while (tok.type != tk::eof) {
            /// Parse an element.
            auto e = parse_element();
            if (not e) return err{e.error()};
            doc.elements.push_back(std::move(*e));
        }

        /// Return the document.
        return std::move(doc);
    }

    /// Parse an element.
    /// <element>  ::= <element-named> | <element-text> | <element-implicit-div>
    auto parse_element() -> res<el> {
        auto l = tok.location;
        switch (tok.type) {
            /// Named element.
            case tk::name: {
                auto name = tolower(tok.text);
                advance();
                return parse_named_element(std::move(name), l);
            }

            /// Class.
            case tk::class_name: {
                auto cl = tok.text;
                advance();
                return parse_named_element("div"s, l, {std::move(cl)});
            }

            /// ID.
            case tk::id: {
                auto id = tok.text;
                advance();
                return parse_named_element("div"s, l, {}, std::move(id));
            }

            /// Attribute list.
            case tk::lbrack: {
                element::attribute_list attrs;
                check(parse_attribute_list(attrs));
                return parse_named_element("div"s, l, {}, "", std::move(attrs));
            }

            default: return diag(diag_kind::error, tok.location, "Expected element, got {}", tk_to_str(tok.type));
        }
    }

    /// Parse a named element.
    ///
    /// <element-named> ::= NAME <element-named-rest>
    /// <element-implicit-div> ::= ( <attr-list> | CLASS | ID) <element-named-rest>
    /// <element-named-rest> ::= { <attr-list> | CLASS | ID } [ <content> ]
    /// <element-text>  ::= [ TEXT ] <text-body>
    /// <content>  ::= "{" { <element> } "}" | <text-body>
    auto parse_named_element(
        std::string name,
        loc l,
        element::class_list classes = {},
        std::string id = "",
        element::attribute_list attributes = {}
    ) -> res<el> {
        /// Text element.
        if (name == "text") return parse_text_elem();

        /// Parse the classes.
        while (at(tk::class_name, tk::id, tk::lbrack)) {
            switch (tok.type) {
                case tk::class_name:
                    classes.insert(trim(tolower(tok.text)));
                    advance();
                    break;

                case tk::id:
                    if (not id.empty()) return diag(diag_kind::error, tok.location, "Element already has an ID");
                    id = trim(tok.text);
                    advance();
                    break;

                case tk::lbrack:
                    check(parse_attribute_list(attributes));
                    break;

                default: std::unreachable();
            }
        }

        /// Create an element, attach classes, etc.
        auto make = [&](auto&&... args) -> res<el> {
            auto e = element::make(NHTML_FWD(args)...);
            e->classes = std::move(classes);
            e->id = std::move(id);
            e->attributes = std::move(attributes);

            /// If this is a link, then the name is actually ‘a’, and we
            /// add an implicit href attribute.
            if (e->tag_name.starts_with('/')) {
                if (not e->attributes.try_emplace("href", e->tag_name))
                    return diag(diag_kind::error, l, "Cannot specify href attribute for link");
                e->tag_name = "a";
            }

            return e;
        };

        /// Element contains other elements.
        if (at(tk::lbrace)) {
            /// Yeet "{".
            advance();

            /// Parse the elements.
            std::vector<el> elements;
            while (not at(tk::rbrace)) {
                auto e = parse_element();
                if (not e) return err{e.error()};
                elements.push_back(std::move(*e));
            }

            /// Yeet "}".
            if (not at(tk::rbrace)) return diag(diag_kind::error, tok.location, "Expected '}}', got {}", tk_to_str(tok.type));
            advance();

            /// Return the element.
            return make(std::move(name), std::move(elements));
        }

        /// Element contains only text.
        if (at(tk::lparen)) {
            /// Parse the text.
            auto text = parse_text_elem();
            if (not text) return err{std::move(text.error())};
            return make(std::move(name), std::move(*text));
        }

        /// Element is empty.
        return make(std::move(name));
    }

    /// Parse element text body.
    /// <text-body>  ::= "(" TOKENS ")"
    auto parse_text_elem() -> res<el> {
        /// Must be at '('.
        if (not at(tk::lparen)) return diag(diag_kind::error, tok.location, "Expected '(', got {}", tk_to_str(tok.type));

        /// Get the text.
        auto text = read_until_chars(')');

        /// Must be at ')'.
        if (not at(tk::rparen)) return diag(diag_kind::error, tok.location, "Expected ')', got {}", tk_to_str(tok.type));
        advance();

        /// Create the text element.
        auto e = element::make();
        e->tag_name = "text";
        e->content = std::move(*text);
        return e;
    }

    /// Parse a list of attributes.
    /// <attr-list> ::= "[" { <attr> [ "," ] } "]"
    /// <attr> ::= NAME "=" VALUE
    auto parse_attribute_list(element::attribute_list& attrs) -> res<void> {
        /// Must be at '['.
        if (not at(tk::lbrack)) return diag(diag_kind::error, tok.location, "Expected '[', got {}", tk_to_str(tok.type));
        advance();

        /// Parse the attributes.
        while (not at(tk::rbrack)) {
            /// Must be at a name.
            if (not at(tk::name)) return diag(diag_kind::error, tok.location, "Expected attribute name, got {}", tk_to_str(tok.type));
            auto name = tok.text;
            auto l = tok.location;
            advance();

            /// An attribute may, but need not, have a value
            std::string value;
            if (at(tk::eq)) {
                /// Read the value. An attribute value is everything up to the next comma or closing bracket.
                /// This means that we need to lex manually, for which reason we don’t advance() past the `=`,
                /// since the parser is currently at the first character after the `=`.
                auto s = read_until_chars(',', ']');
                if (not s) return err{std::move(s.error())};
                value = std::move(*s);
            }

            /// Add the attribute.
            if (not attrs.try_emplace(std::move(name), std::move(value)))
                return diag(diag_kind::error, l, "Duplicate '{}' attribute", name);

            /// Must be at a comma or ']' (end of list).
            if (not at(tk::rbrack, tk::comma)) return diag(diag_kind::error, tok.location, "Expected ']' or ',', got {}", tk_to_str(tok.type));
            if (at(tk::comma)) advance();
        }

        /// Must be at ']'.
        if (not at(tk::rbrack)) return diag(diag_kind::error, tok.location, "Expected ']', got {}", tk_to_str(tok.type));
        advance();

        /// Done.
        return {};
    }
};

/// Parse a file.
auto parse(file&& f) -> std::expected<document, std::string> {
    parser p;
    return p.parse(std::move(f));
}

} // namespace
} // namespace nhtml::detail

/// ===========================================================================
///  API
/// ===========================================================================
auto nhtml::parse(detail::string_ref data, fs::path filename) -> std::expected<document, std::string> {
    /// Create the file.
    detail::file f;
    f.contents = std::move(data);
    f.parent_directory = detail::file::get_parent_directory(filename);
    f.name = std::move(filename);

    /// Parse it.
    return detail::parse(std::move(f));
}

auto nhtml::parse_file(fs::path filename) -> std::expected<document, std::string> {
    auto f = detail::file::map(std::move(filename));
    if (not f) return std::unexpected{f.error()};
    return detail::parse(std::move(f.value()));
}
