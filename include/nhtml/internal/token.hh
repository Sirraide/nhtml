#ifndef NHTML_INTERNAL_TOKEN_HH
#define NHTML_INTERNAL_TOKEN_HH

#include <nhtml/internal/source_location.hh>
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
    right_angle,
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
        case tk::right_angle: return ">";
    }
    return "<unknown>";
}
} // namespace nhtml::detail

#endif // NHTML_INTERNAL_TOKEN_HH
