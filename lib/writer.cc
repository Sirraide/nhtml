#include <nhtml/core.hh>
#include <nhtml/internal/utils.hh>

namespace nhtml::detail {
/// ===========================================================================
///  Document
/// ===========================================================================
template <typename output_type_t>
struct html_writer {
    using output_type = std::conditional_t<std::is_pointer_v<output_type_t>, output_type_t, output_type_t&>;
    const output_type out;
    usz lines = 0;
    document::write_opts opts;
    bool in_xml = false;

    explicit html_writer(output_type _out, document::write_opts _opts)
        : out(_out)
        , opts(_opts) {
        if (opts.text_columns == 0) opts.text_columns = 1000000000;
    }

    /// Write a formatted string to a file or string.
    template <typename... arguments>
    void write(fmt::format_string<arguments...> fmt, arguments&&... args) {
        if constexpr (is<output_type, std::string>) out += fmt::format(fmt, std::forward<arguments>(args)...);
        else fmt::print(out, fmt, std::forward<arguments>(args)...);
    }

    /// Write indentation.
    void indent(usz indent_level) {
        if (not opts.no_indent) {
            if (opts.use_tabs) write("\t");
            else write("{:>{}}", "", indent_level * opts.indent_width);
        }
    }

    /// Write a line break.
    void nl() {
        write("\n");
        lines++;
    }

    /// Write text to a file or string.
    ///
    /// This does several things:
    ///     - Ensures text is indented correctly.
    ///     - Wraps the text at a maximum line length.
    ///     - Escapes special characters.
    ///     - Collapses whitespace.
    ///
    /// \param text The text to write.
    /// \param indent_level The indentation level.
    /// \param line_length The current line length.
    /// \return Whether we’ve emitted a newline.
    void write_text(std::string_view text, usz indent_level, usz line_length, bool wrap = true) {
        /// For line wrapping.
        const usz max_line_length =
            wrap
                ? std::max<usz>(opts.text_columns - indent_level * 4, 40)
                : std::numeric_limits<usz>::max();
        bool have_line_break = false;
        bool need_indent = line_length == 0;

        /// Write and update the line length.
        auto eject_text = [&](std::string_view t) {
            if (need_indent) {
                indent(indent_level);
                need_indent = false;
            }

            write("{}", t);
            line_length += t.size();
        };

        /// Write the current segment to the output.
        auto it = text.begin();
        auto eject = [&] {
            /// The line (segment) to write.
            auto chars = std::min<usz>(usz(it - text.begin()), text.size());
            auto line = std::string_view{&*text.begin(), chars};

            /// If the segment would make the line too long, break it on the
            /// first space—starting from the back—that makes the line short
            /// enough to fit. First, if we haven’t emitted a newline after
            /// the opening tag yet, write one.
            if (line_length + line.size() > max_line_length and not have_line_break) {
                nl();
                have_line_break = true;
                line_length = 0;
                need_indent = true;
            }

            /// Then, break the line as often as necessary.
            auto segment_to_write = line;
            while (line_length + segment_to_write.size() > max_line_length) {
                /// Find the last space. Break if there is none.
                auto space = segment_to_write.find_last_of(" \r\n\t\v\f");
                if (space == std::string_view::npos) break;

                /// If the segment is short enough, write it and start a new one.
                if (line_length + space <= max_line_length) {
                    /// Write the segment.
                    eject_text(line.substr(0, space));
                    nl();
                    line_length = 0;
                    need_indent = true;
                    segment_to_write = line = line.substr(space + 1);
                    continue;
                }

                /// The line is still too long. Try again with a shorter line.
                segment_to_write = line.substr(0, space);
            }

            /// Write the rest of the line.
            eject_text(segment_to_write);
            text.remove_prefix(chars);
        };

        /// Skip leading whitespace.
        while (it != text.end() and std::isspace(*it)) {
            ++it;
            text.remove_prefix(1);
        }

        /// Number of whitespace characters in the current segment.
        usz ws_count = 0;
        for (; it != text.end(); ++it) {
            switch (*it) {
                /// Whitespace.
                case ' ':
                case '\t':
                case '\r':
                case '\n':
                case '\v':
                case '\f':
                    /// More than one whitespace character in a row. Write the
                    /// current segment to the output and start a new one.
                    if (++ws_count == 2) eject();
                    continue; /// Skip ws_count reset.

                /// Characters that should be escaped.
                case '<':
                case '>':
                case '&':
                case '"':
                case '\'':
                    eject();
                    switch (*it) {
                        case '<': eject_text("&lt;"); break;
                        case '>': eject_text("&gt;"); break;
                        case '&': eject_text("&amp;"); break;
                        case '"': eject_text("&quot;"); break;
                        case '\'': eject_text("&apos;"); break;
                    }
                    break;

                /// All other characters.
                default: break;
            }

            /// Reset ws count.
            ws_count = 0;
        }

        /// Write the last segment.
        while (ws_count) {
            text.remove_suffix(1);
            ws_count--;
        }
        eject();

        /// Last line break.
        if (have_line_break) nl();
    }

    /// Write an element to a file or string.
    void write(const element& el, usz i) {
        /// Text. Not a tag.
        if (el.tag_name == "text") {
            write_text(std::get<std::string>(el.content), i, 0);
            return;
        }

        /// Style tag.
        if (el.tag_name == "style") {
            indent(i);
            if (std::holds_alternative<std::string>(el.content)) {
                write("<style>\n");
                indent(i + 1);
                write("{}\n", trim(auto{std::get<std::string>(el.content)}));
                indent(i);
                write("</style>\n");
            } else {
                write("<style></style>");
            }
            return;
        }

        /// Raw HTML.
        if (el.tag_name == "__html__") {
            write("{}\n", trim(auto{std::get<std::string>(el.content)}), i, 0, false);
            return;
        }

        /// Check for XML elements.
        tempset in_xml = in_xml or el.tag_name == "svg";

        /// TODO: Special handling for <p>, <pre>, <code>, <script>, <style>.
        indent(i);
        write("<{}", el.tag_name);

        /// Escape single quotes.
        static const auto escape_quotes = [](std::string s, quoting_style sty) {
            if (sty == quoting_style::single_quotes) replace_all(s, "'", "&#39;"); /// Note: ASCII 39 = "'".
            else if (sty == quoting_style::double_quotes) replace_all(s, "\"", "&quot;");
            else NHTML_UNREACHABLE();
            return s;
        };

        /// Quote to use.
        const auto quote = opts.attribute_quoting_style == quoting_style::single_quotes ? "'" : "\"";

        /// Write classes.
        if (not el.classes.empty()) {
            write(" class={}", quote);
            for (auto it = el.classes.begin(); it != el.classes.end(); ++it) {
                if (it != el.classes.begin()) write(" ");
                write("{}", escape_quotes(*it, opts.attribute_quoting_style));
            }
            write("{}", quote);
        }

        /// Write ID.
        if (not el.id.empty()) write(" id={}{}{}", quote, escape_quotes(el.id, opts.attribute_quoting_style), quote);

        /// Write attributes.
        for (const auto& [name, value] : el.attributes) {
            /// Emit the attribute.
            write(" {}={}{}{}", name, quote, escape_quotes(value, opts.attribute_quoting_style), quote);
        }

        /// Empty tag in XML: Use self-closing syntax if requested.
        if (std::holds_alternative<std::monostate>(el.content) and in_xml and opts.self_close_xml_tags) {
            write("/>");
            nl();
            return;
        }

        /// Close opening tag. Note the line in case we need to insert a line break later.
        write(">");
        auto line = lines;

        /// Write content.
        // clang-format off
        std::visit(overloaded {
            [](std::monostate) {},
            [&](const std::string& str) {
                write_text(str, i + 1, el.tag_name.size() + 2);
            },
            [&](const element::vector& els) {
                /// No elements to write.
                if (els.empty()) return;

                /// If the only element is text, write as text instead.
                if (els.size() == 1 and els[0]->tag_name == "text") {
                    write_text(std::get<std::string>(els[0]->content), i + 1, el.tag_name.size() + 2);
                    return;
                }

                /// Otherwise, write the elements.
                nl();
                for (const auto& e : els) write(*e, i + 1);
            },
        }, el.content); // clang-format on

        if (line != lines) indent(i);
        write("</{}>", el.tag_name);
        nl();
    }

    /// Write a document to a file or string.
    void write(const document& doc) {
        for (const auto& el : doc.elements) write(*el, 0);
    }
};

template <typename t>
html_writer(t, document::write_opts) -> html_writer<t>;

} // namespace nhtml::detail

/// ===========================================================================
///  Document
/// ===========================================================================
void nhtml::document::write(FILE* output_file, write_opts opts) const {
    detail::html_writer writer{output_file, opts};
    writer.write(*this);
}

auto nhtml::document::string(write_opts opts) const -> std::string {
    std::string out;
    detail::html_writer writer{out, opts};
    writer.write(*this);
    return out;
}
