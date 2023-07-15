#include <clopts.hh>
#include <nhtml/parser.hh>

using namespace command_line_options;
using nhtml::i64;
using nhtml::u64;
using options = clopts< // clang-format off
    positional<"path", "The file or directory to compile">,
    flag<"-c", "Compile a single file and exit">,
    option<"-o", "The output file or directory">,
    multiple<option<"-I", "Add a directory to the include path">>,
    option<"--f-columns", "Maximum column number for text (soft cap, default: 140)", i64>,
    option<"--f-indent-width", "Indent width (default: 4); ignored if --f-use-tabs is also specified", i64>,
    option<"--f-quoting-style", "Quoting style for attributes ('single' or 'double'; default: single)">, /// TODO: Libclopts: Add enum support.
    flag<"--f-no-indent", "Disable indenting">,
    flag<"--f-use-tabs", "Indent using tabs instead of spaces">,
    flag<"--f-self-close-xml-tags", "Use self-closing XML tags in e.g. SVGs">,
    help<>
>; // clang-format on

template <detail::static_string s>
decltype(options::get<s>()) o() {
    return options::get<s>();
}

template <detail::static_string s>
auto o(auto default_value) -> decltype(options::get_or<s>(default_value)) {
    return options::get_or<s>(default_value);
}

template <typename... arguments>
[[noreturn]] void die(fmt::format_string<arguments...> fmt, arguments&&... args) {
    fmt::print(stderr, "nhtmlc: ");
    fmt::print(stderr, fmt, std::forward<arguments>(args)...);
    fmt::print(stderr, "\n");
    exit(1);
}

auto get_options() -> nhtml::document::write_opts {
    using wo = nhtml::document::write_opts;

    static const auto get_quoting_style = [] -> nhtml::quoting_style {
        if (auto sty = o<"--f-quoting-style">()) {
            if (*sty == "single") return nhtml::quoting_style::single_quotes;
            if (*sty == "double") return nhtml::quoting_style::double_quotes;
            die("invalid quoting style '{}'", *sty);
        }

        return nhtml::quoting_style::single_quotes;
    };

    return {
        .indent_width = u64(o<"--f-indent-width">(wo::default_indent_width)),
        .text_columns = u64(o<"--f-columns">(wo::default_text_columns)),
        .attribute_quoting_style = get_quoting_style(),
        .no_indent = o<"--f-no-indent">() or wo::default_no_indent,
        .self_close_xml_tags = o<"--f-self-close-xml-tags">() or wo::default_self_close_xml_tags,
        .use_tabs = o<"--f-use-tabs">() or wo::default_use_tabs,
    };
}

int main(int argc, char** argv) {
    options::parse(argc, argv);

    /// Check include paths.
    auto& includes = *options::get<"-I">();
    std::erase_if(includes, [](const std::string& s) {
        std::error_code ec;
        if (auto res = std::filesystem::exists(s, ec); not res or ec) {
            fmt::print(stderr, "Include directory '{}' does not exist. Ignoring...", s);
            return true;
        }
        return false;
    });

    /// Compiling a single file.
    if (options::get<"-c">()) {
        auto fname = options::get<"path">();
        auto doc = nhtml::parse_file(
            *fname,
            {
                .include_directories = std::move(includes),
            }
        );

        if (not doc) {
            fmt::print(stderr, "nhtmlc: {}\n", doc.error());
            return 1;
        }

        /// Open the output file.
        auto ofile = options::get<"-o">();
        FILE* f = ofile ? fopen(ofile->c_str(), "w") : stdout;
        if (not f) {
            fmt::print(stderr, "nhtmlc: could not open output file '{}'\n", ofile ? *ofile : "stdout");
            return 1;
        }

        /// Write the document to the output file.
        auto document = std::move(*doc);
        document.write(f, get_options());
        if (f != stdout) fclose(f);
        return 0;
    }

    /// Currently not implemented.
    fmt::print(stderr, "nhtmlc: directory compilation is not implemented yet. Use -c to compile a single file.\n");
    return 1;
}
