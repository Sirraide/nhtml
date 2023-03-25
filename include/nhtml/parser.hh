#ifndef NHTML_PARSER_HH
#define NHTML_PARSER_HH

#include <expected>
#include <nhtml/core.hh>
#include <nhtml/utils.hh>

namespace nhtml {
/// Parse an NHTML string.
///
/// \param data The data to parse.
/// \param filename The filename to use for diagnostics.
/// \param directory The parent directory of the file. relative to which to resolve include and
///                  link paths. Defaults the parent directory of the filename if it is a valid
///                  path and the current working directory otherwise.
/// \return The parsed document, or an error message.
auto parse(detail::string_ref data, fs::path filename = "<input>") -> std::expected<document, std::string>;

/// Parse an NHTML file.
///
/// \param filename The file to parse.
/// \param directory The parent directory of the file. relative to which to resolve include and
///                  link paths. Defaults the parent directory of the filename if it is a valid
///                  path and the current working directory otherwise.
auto parse_file(fs::path filename) -> std::expected<document, std::string>;
} // namespace nhtml

#endif // NHTML_PARSER_HH
