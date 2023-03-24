#include <nhtml/parser.hh>
#include <clopts.hh>

using namespace command_line_options;
using options = clopts<// clang-format off
    positional<"path", "The file or directory to compile">,
    flag<"-c", "Compile a single file and exit">,
    option<"-o", "The output file or directory">,
    help
>;// clang-format on

int main(int argc, char** argv) {
    options::parse(argc, argv);

    /// Compiling a single file.
    if (options::get<"-c">()) {
        auto fname = options::get<"path">();
        auto doc = nhtml::parse_file(*fname);
        if (!doc) {
            fmt::print(stderr, "nhtmlc: {}\n", doc.error());
            return 1;
        }

        /// Open the output file.
        auto ofile = options::get<"-o">();
        FILE* f = ofile ? fopen(ofile->c_str(), "w") : stdout;
        if (!f) {
            fmt::print(stderr, "nhtmlc: could not open output file '{}'\n", ofile ? *ofile : "stdout");
            return 1;
        }

        /// Write the document to the output file.
        auto document = std::move(*doc);
        document.write(f);
        if (f != stdout) fclose(f);
        return 0;
    }

    /// Currently not implemented.
    fmt::print(stderr, "nhtmlc: directory compilation is not implemented yet\n");
    return 1;
}
