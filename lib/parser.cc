#include <deque>
#include <duktape.h>
#include <fmt/color.h>
#include <functional>
#include <nhtml/internal/parser_impl.hh>
#include <nhtml/parser.hh>
#include <unordered_map>
#include <utility>

using namespace nhtml::detail;

#define check                       NHTML_CHECK
#define DUKTAPE_FLAGS               (DUK_COMPILE_EVAL | DUK_COMPILE_NOSOURCE | DUK_COMPILE_NOFILENAME | DUK_COMPILE_SAFE)
#define NHTML_PARSER_INSTANCE_KEY   DUK_GLOBAL_SYMBOL("nhtml.parser.instance")
#define NHTML_ELEMENT_HANDLE_KEY    DUK_GLOBAL_SYMBOL("nhtml.element.handle")
#define NHTML_ELEMENT_PROTOTYPE_KEY DUK_GLOBAL_SYMBOL("nhtml.element.prototype")

/// ===========================================================================
///  Parser
/// ===========================================================================
parser::parser()
    : eval(*this) {
}

parser::~parser() {
}

auto nhtml::detail::parser::lastc_loc() const -> loc {
    loc l;
    l.pos = static_cast<u32>(curr() - file_stack.back()->contents.data() - 1);
    l.len = 1;
    l.file = static_cast<u16>(file_stack.back()->file_index);
    return l;
}

auto nhtml::detail::parser::look_ahead(usz number_of_tokens) -> res<token*> {
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

auto nhtml::detail::parser::next() -> res<void> {
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
        static constexpr auto name_delims = "()[]{}.,%#= \t\r\n\f\v"sv;

        tok.type = tk::name;
        tok.text.clear();
        while (not name_delims.contains(lastc) and lastc != 0) {
            tok.text += lastc;
            next_char();
        }

        /// Check for invalid names. This generally means we’ve added a
        /// character to the delimiters without handling it in the lexer
        /// below.
        if (tok.text.empty()) tok.type = tk::invalid;
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

        /// Inline style.
        case '%':
            next_char();
            tok.type = tk::percent;
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

void nhtml::detail::parser::next_char() {
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

void nhtml::detail::parser::skip_line() {
    while (lastc != '\n' && lastc != 0) next_char();
}

void nhtml::detail::parser::skip_whitespace() {
    while (std::isspace(lastc)) next_char();
}

auto nhtml::detail::parser::lex_number() -> res<void> {
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

auto nhtml::detail::parser::lex_string(char delim) -> res<void> {
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

auto nhtml::detail::parser::read_until_chars(std::same_as<char> auto... c) -> res<std::string>
requires (sizeof...(c) >= 1) {
    std::string s;
    while (((lastc != c) and ...) and lastc != 0) {
        s += lastc;
        next_char();
    }

    /// Check for EOF.
    if (lastc == 0) {
        if constexpr (sizeof...(c) == 1) {
            return diag(
                diag_kind::error,
                loc{},
                "Unexpected end of file while looking for '{}'",
                c...
            );
        } else {
            std::string chars;
            ((chars += fmt::format("'{}', ", c)), ...);
            chars.pop_back(), chars.pop_back();
            return diag(
                diag_kind::error,
                loc{},
                "Unexpected end of file while looking for one of {}",
                std::move(chars)
            );
        }
    }

    /// Get the next token.
    NHTML_CHECK(next());

    /// Return lexed text.
    return s;
}

bool nhtml::detail::parser::seekable(loc l) const {
    if (l.file >= files.size()) return false;
    const auto& f = files[l.file];
    return usz(l.pos) + l.len <= f.contents.size() and l.len;
}

/// Seek to a source location. The location must be valid.
auto nhtml::detail::parser::seek(loc l) const -> loc_info {
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

bool nhtml::detail::parser::at(std::same_as<tk> auto&&... t) { return ((tok.type == t) or ...); }

/// Add a file.
auto nhtml::detail::parser::add_file(file&& f) -> res<void> {
    if (files.size() > std::numeric_limits<u16>::max())
        return mkerr("Sorry, but we can't handle more than {} files.", std::numeric_limits<u16>::max());

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
auto nhtml::detail::parser::parse(file&& f) -> res<document> {
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
        if (e.value() and e.value().get()) doc.elements.push_back(std::move(*e));
    }

#ifndef NHTML_DISABLE_EVAL
    for (auto& [script, location] : scripts)
        if (auto res = eval(script, location); not res)
            return err{res.error()};
#endif

    /// Return the document.
    return std::move(doc);
}

/// Parse an element.
/// <element> ::= <element-named> | <element-text> | <element-implicit-div> | <style-tag>
/// <style-tag> ::= "style" <css-data>
auto nhtml::detail::parser::parse_element() -> res<element::ptr> {
    auto l = tok.location;
    switch (tok.type) {
        /// Named element.
        case tk::name: {
            auto name = tolower(tok.text);
            advance();

            /// Style tags need special handling.
            if (name == "style") {
                std::string style;
                check(parse_nested_language_data<tk::lbrace, '{', '}'>(style));

                /// Style tags never contain other tags or anything other than CSS.
                auto e = element::make(std::move(name));
                e->content = std::move(style);
                return e;
            }

#ifndef NHTML_DISABLE_EVAL
            /// So do eval tags.
            if (name == "eval" or name == "eval!") {
                std::string code{'{'};
                check(parse_nested_language_data<tk::lbrace, '{', '}', true>(code));
                code += '}';

                if (name == "eval!") {
                    auto res = eval(code, l);
                    if (not res) return err{res.error()};
                } else {
                    scripts.emplace_back(std::move(code), l);
                }

                return {};
            }
#endif

            /// Regular element.
            return parse_named_element(std::move(name), l);
        }

        /// Class.
        case tk::class_name: {
            auto cl = tok.text;
            advance();
            return parse_named_element("div", l, {std::move(cl)});
        }

        /// ID.
        case tk::id: {
            auto id = tok.text;
            advance();
            return parse_named_element("div", l, {}, std::move(id));
        }

        /// Attribute list.
        case tk::lbrack: {
            element::attribute_list attrs;
            check(parse_attribute_list(attrs));
            return parse_named_element("div", l, {}, "", std::move(attrs));
        }

        /// Inline style.
        case tk::percent: {
            element::inline_style style;
            advance();
            check(parse_nested_language_data(style));
            return parse_named_element("div", l, {}, "", {}, std::move(style));
        }

        default: return diag(diag_kind::error, tok.location, "Expected element, got {}", tk_to_str(tok.type));
    }
}

/// Parse a named element.
///
/// <element-named> ::= NAME <element-named-rest>
/// <element-implicit-div> ::= ( <element-data> ) <element-named-rest>
/// <element-named-rest> ::= { <element-data> } [ <content> ]
/// <element-data> ::= <attr-list> | <inline-style> | CLASS | ID
/// <element-text>  ::= [ TEXT ] <text-body>
/// <inline-style> ::= "%" <css-data>
/// <content>  ::= "{" { <element> } "}" | <text-body>
auto nhtml::detail::parser::parse_named_element(
    std::string name,
    loc l,
    element::class_list classes,
    std::string id,
    element::attribute_list attributes,
    element::inline_style style
) -> res<element::ptr> {
    /// Text element.
    if (name == "text") return parse_text_elem();

    /// Parse the classes.
    while (at(tk::class_name, tk::id, tk::lbrack, tk::percent)) {
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

            case tk::percent:
                advance();
                check(parse_nested_language_data(style));
                break;

            default: std::unreachable();
        }
    }

    /// Create an element, attach classes, etc.
    auto make = [&](auto&&... args) -> res<element::ptr> {
        auto e = element::make(NHTML_FWD(args)...);
        e->classes = std::move(classes);
        e->id = std::move(id);
        e->attributes = std::move(attributes);
        if (not style.empty()) {
            auto& st = e->attributes["style"];
            if (not st.empty()) st += "; ";
            st += style;
        }

        /// If this is a link, then the name is actually ‘a’, and we
        /// add an implicit href attribute.
        if (e->tag_name.starts_with('/')) {
            if (not e->attributes.try_emplace("href", e->tag_name))
                return diag(diag_kind::error, l, "Cannot specify href attribute for link");
            e->tag_name = "a";

            /// If the element is empty, we set the text to the value of the link.
            if (std::holds_alternative<std::monostate>(e->content)) e->content = e->attributes["href"];
        }

        return e;
    };

    /// Element contains other elements.
    if (at(tk::lbrace)) {
        /// Yeet "{".
        advance();

        /// Parse the elements.
        std::vector<element::ptr> elements;
        while (not at(tk::rbrace)) {
            auto e = parse_element();
            if (not e) return err{e.error()};
            if (e.value() and e.value().get()) elements.push_back(std::move(*e));
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
auto nhtml::detail::parser::parse_text_elem() -> res<element::ptr> {
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
auto nhtml::detail::parser::parse_attribute_list(element::attribute_list& attrs) -> res<void> {
    /// Must be at '['.
    if (not at(tk::lbrack)) return diag(diag_kind::error, tok.location, "Expected '[', got {}", tk_to_str(tok.type));
    advance();

    /// Parse the attributes.
    while (not at(tk::rbrack)) {
        /// Must be at a name.
        if (not at(tk::name)) return diag(diag_kind::error, tok.location, "Expected attribute name, got {}", tk_to_str(tok.type));
        auto name = tok.text;
        auto l = tok.location;

        /// Attribute name may not be 'id' or 'class'.
        if (name == "id" or name == "class")
            return diag(diag_kind::error, l, "Cannot specify '{}' attribute manually. Use the '.' or '#' syntax instead.", name);

        /// An attribute may, but need not, have a value
        advance();
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
        if (not attrs.try_emplace(name, std::move(trim(std::move(value)))))
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

/// Parse inline CSS.
/// <css-data> ::= DELIMITER CSS DELIMITER
/// <eval-tag> ::= "eval" "{" TEXT "}"
template <tk open, char open_char, char close, bool line_comments>
auto nhtml::detail::parser::parse_nested_language_data(std::string& style) -> res<void> {
    auto loc = tok.location;
    if (not at(open)) return diag(diag_kind::error, loc, "Expected '{}' after (inline) style", tk_to_str(open));

    /// Very crude CSS string, brackets, and comment ‘parser’.
    u64 open_brackets = 1;
    bool in_comment = false;
    enum struct string_kind {
        none,
        single_quoted,
        double_quoted,
    } str = string_kind::none;
    while (lastc) {
        /// Comments are skipped.
        if (in_comment) {
            if (lastc == '*' and (next_char(), lastc) == '/') in_comment = false;
            next_char();
            continue;
        }

        /// Brackets and comments are ignored in strings.
        if (str != string_kind::none) {
            if (
                (str == string_kind::single_quoted and lastc == '\'') or
                (str == string_kind::double_quoted and lastc == '\"')
            ) str = string_kind::none;

            style += lastc;
            next_char();
            continue;
        }

        switch (lastc) {
            case open_char:
                open_brackets++;
                goto append;

            case close:
                if (not --open_brackets) goto done_parsing_css;
                goto append;

            case '/':
                next_char();
                if (lastc == '*') {
                    in_comment = true;
                    next_char();
                    continue;
                }

                if constexpr (line_comments) {
                    if (lastc == '/') {
                        while (lastc and lastc != '\n') next_char();
                        continue;
                    }
                }

                style += '/';
                goto append;

            case '\'':
                str = string_kind::single_quoted;
                goto append;

            case '\"':
                str = string_kind::double_quoted;
                goto append;

            default:
            append:
                style.push_back(lastc);
                next_char();
        }
    }

done_parsing_css:
    if (lastc != close) return diag(diag_kind::error, loc, "Unterminated (inline) style starting here");
    next_char();
    advance();
    return {};
}

namespace {
bool selector_matches(std::string_view sel, nhtml::element* e) {
    bool tag_matched = false;
    while (not sel.empty()) {
        /// Extract part of a selector.
        const auto selector_part = [&](std::string_view seps) -> std::string_view {
            auto end = sel.find_first_of(seps);
            if (end == std::string_view::npos) end = sel.size();
            return sel.substr(0, end);
        };

        /// Match the selector.
        switch (sel[0]) {
            /// Match id.
            case '#': {
                sel.remove_prefix(1);
                auto part = selector_part(".[");
                if (part != e->id) return false;
                sel.remove_prefix(part.size());
                break;
            }

            /// Match class.
            case '.': {
                sel.remove_prefix(1);
                auto part = selector_part("#[");
                if (nhtml::rgs::find(e->classes, part) == e->classes.end()) return false;
                sel.remove_prefix(part.size());
                break;
            }

            /// Match attribute.
            case '[': {
                sel.remove_prefix(1);
                auto name = selector_part("=]");
                const auto it = e->attributes.find(std::string{name});
                if (it == e->attributes.end()) return false;

                /// Ignore everything up to ']'
                auto end = selector_part("]");
                sel.remove_prefix(end.size());
                if (sel[0] == ']') sel.remove_prefix(1);
                break;
            }

            /// Match tag. This is only allowed at the beginning of the selector.
            default: {
                if (tag_matched) return false;
                tag_matched = true;
                auto tag_name = selector_part(".#[");
                if (tag_name != e->tag_name) return false;
                sel.remove_prefix(tag_name.size());
                break;
            }
        }
    }

    /// If the selector is empty, we have a match.
    return true;
};
} // namespace

auto nhtml::detail::parser::query_selector(std::string_view selector) -> element* {
    for (auto& e : doc.elements)
        if (auto res = query_selector_impl(selector, e.get()))
            return res;

    return nullptr;
}

auto nhtml::detail::parser::query_selector_impl(std::string_view selector, element* root) -> element* {
    if (selector_matches(selector, root)) return root;
    if (std::holds_alternative<element::vector>(root->content))
        for (auto& c : std::get<element::vector>(root->content))
            if (auto res = query_selector_impl(selector, c.get()))
                return res;

    return nullptr;
}

/// ===========================================================================
///  API
/// ===========================================================================
namespace {
/// Parse a file.
auto parse(file&& f) -> std::expected<nhtml::document, std::string> {
    parser p;
    return p.parse(std::move(f));
}
}

auto nhtml::parse(detail::string_ref data, fs::path filename) -> std::expected<document, std::string> {
    /// Create the file.
    detail::file f;
    f.contents = std::move(data);
    f.parent_directory = detail::file::get_parent_directory(filename);
    f.name = std::move(filename);

    /// Parse it.
    return ::parse(std::move(f));
}

auto nhtml::parse_file(fs::path filename) -> std::expected<document, std::string> {
    auto f = detail::file::map(std::move(filename));
    if (not f) return std::unexpected{f.error()};
    return ::parse(std::move(f.value()));
}
