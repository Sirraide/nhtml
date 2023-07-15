#ifndef NHTML_PARSER_HH
#define NHTML_PARSER_HH

#include <expected>
#include <nhtml/core.hh>
#include <nhtml/utils.hh>

namespace nhtml {
/// Options passed to the parser.
struct parse_options {
    /// Directories to search for included files.
    std::span<std::string_view> include_directories{};

#ifndef NHTML_DISABLE_EVAL
    /// JS code to execute before parsing.
    std::string js_prelude;
#endif
};

/// Parse an NHTML string.
///
/// \param data The data to parse.
/// \param filename The filename to use for diagnostics.
/// \param options Options to pass to the parser.
/// \return The parsed document, or an error message.
auto parse(
    detail::string_ref data,
    fs::path filename = "<input>",
    parse_options options = {}
) -> detail::res<document>;

/// Parse an NHTML file.
///
/// \param filename The file to parse.
/// \param options Options to pass to the parser.
/// \return The parsed document, or an error message.
auto parse_file(fs::path filename, parse_options options = {}) -> detail::res<document>;
} // namespace nhtml

#endif // NHTML_PARSER_HH
