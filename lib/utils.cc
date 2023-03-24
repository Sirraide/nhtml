#include <nhtml/utils.hh>

std::string nhtml::detail::format_filename(std::string_view file) {
    if (auto slash = file.find_last_of("/\\"); slash != std::string_view::npos)
        file.remove_prefix(slash + 1);
    return std::string(file);
}

std::string nhtml::detail::escape(std::string str) {
    replace_all(str, "\\", "\\\\");
    replace_all(str, "\"", "\\\"");
    replace_all(str, "\a", "\\a");
    replace_all(str, "\b", "\\b");
    replace_all(str, "\f", "\\f");
    replace_all(str, "\n", "\\n");
    replace_all(str, "\r", "\\r");
    replace_all(str, "\t", "\\t");
    replace_all(str, "\v", "\\v");
    return str;
}
