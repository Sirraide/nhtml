#include <nhtml/parser.hh>
#include <clopts.hh>

using namespace command_line_options;
using nhtml::i64;
using nhtml::u64;
using options = clopts<// clang-format off
    positional<"path", "The file or directory to compile">,
    flag<"-c", "Compile a single file and exit">,
    option<"-o", "The output file or directory">,
    option<"--f-columns", "Maximum column number (soft cap, default: 140)", i64>,
    option<"--f-indent-width", "Indent width (default: 4); ignore if --f-use-tabs is also specified", i64>,
    flag<"--f-no-indent", "Disable indenting (default: false)">,
    flag<"--f-use-tabs", "Indent using tabs (default: false)">,
    help
>;// clang-format on

template <static_string s>
decltype(options::get<s>()) o() {
    return options::get<s>();
}

int main(int argc, char** argv) {
    options::parse(argc, argv);

    /// Compiling a single file.
    if (options::get<"-c">()) {
        auto fname = options::get<"path">();
        auto doc = nhtml::parse_file(*fname);
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
        using wo = nhtml::document::write_opts;
        document.write(f, {
            .indent_width = u64(o<"--f-indent-width">() ? *o<"--f-indent-width">() : i64(wo::default_indent_width)),
            .text_columns = u64(o<"--f-columns">() ? *o<"--f-columns">() : i64(wo::default_text_columns)),
            .no_indent = o<"--f-no-indent">() ? *o<"--f-no-indent">() : wo::default_no_indent,
            .use_tabs = o<"--f-use-tabs">() ? *o<"--f-use-tabs">() : wo::default_use_tabs,
        });
        if (f != stdout) fclose(f);
        return 0;
    }

    /// Currently not implemented.
    fmt::print(stderr, "nhtmlc: directory compilation is not implemented yet\n");
    return 1;
}
