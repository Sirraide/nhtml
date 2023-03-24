#include <nhtml/core.hh>
namespace nhtml::detail {
/// ===========================================================================
///  Document
/// ===========================================================================
template <typename output_type_t>
struct html_writer {
    using output_type = std::conditional_t<std::is_pointer_v<output_type_t>, output_type_t, output_type_t&>;
    const output_type out;

    explicit html_writer(output_type _out)
        : out(_out) {}

    /// Write a formatted string to a file or string.
    template <typename... arguments>
    void write(fmt::format_string<arguments...> fmt, arguments&&... args) {
        if constexpr (is<output_type, std::string>) out += fmt::format(fmt, std::forward<arguments>(args)...);
        else fmt::print(out, fmt, std::forward<arguments>(args)...);
    }

    /// Write indentation.
    void indent(usz indent_level) {
        write("{:>{}}", "", indent_level * 4);
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
    bool write_text(std::string_view text, usz indent_level, usz line_length) {
        /// For line wrapping.
        const usz max_line_length = std::max<usz>(140 - indent_level * 4, 40);
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
            auto chars = usz(it - text.begin());
            auto line = std::string_view{&*text.begin(), chars};

            /// If the segment would make the line too long, break it on the
            /// first space—starting from the back—that makes the line short
            /// enough to fit. First, if we haven’t emitted a newline after
            /// the opening tag yet, write one.
            if (line_length + line.size() > max_line_length and not have_line_break) {
                write("\n");
                have_line_break = true;
                line_length = 0;
                indent(indent_level);
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
                    write("\n");
                    line_length = 0;
                    segment_to_write = line = line.substr(space + 1);
                    if (not line.empty()) indent(indent_level);
                    continue;
                }

                /// The line is still too long. Try again with a shorter line.
                segment_to_write = line.substr(0, space);
            }

            /// Write the rest of the line.
            eject_text(segment_to_write);
            text.remove_prefix(chars);
        };

        /// Number of whitespace characters in the current segment. Set to 1
        /// initially so we skip leading whitespace.
        usz ws_count = 1;
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
        while (ws_count--) text.remove_suffix(1);
        eject();
        return have_line_break;
    }

    /// Write an element to a file or string.
    void write(const element& el, usz i) {
        /// Text. Not a tag.
        if (el.tag_name == "text") {
            write_text(std::get<std::string>(el.content), i, 0);
            return;
        }

        /// TODO: Special handling for <p>, <pre>, <code>, <script>, <style>.
        indent(i);
        write("<{}", el.tag_name);

        /// Write attributes.
        if (not el.class_list.empty()) {
            write(" class=\"");
            for (auto it = el.class_list.begin(); it != el.class_list.end(); ++it) {
                if (it != el.class_list.begin()) write(" ");
                write("{}", *it);
            }
            write("\"");
        }

        /// Write ID.
        if (not el.id.empty()) write(" id=\"{}\"", el.id);

        write(">"); // clang-format off

        /// Write content.
        std::visit(overloaded {
            [&](const std::string& str) {
                if (write_text(str, i, el.tag_name.size() + 2))
                    write("\n");
            },
            [&](const element::vector& els) {
                /// No elements to write.
                if (els.empty()) return;

                /// If the only element is text, write as text instead.
                if (els.size() == 1 and els[0]->tag_name == "text") {
                    write_text(std::get<std::string>(els[0]->content), i, el.tag_name.size() + 2);
                    return;
                }

                /// Otherwise, write the elements.
                write("\n");
                for (const auto& e : els) write(*e, i + 1);
                indent(i);
            },
        }, el.content); // clang-format on
        write("</{}>\n", el.tag_name);
    }

    /// Write a document to a file or string.
    void write(const document& doc) {
        for (const auto& el : doc.elements) write(*el, 0);
    }
};

template <typename t>
html_writer(t) -> html_writer<t>;

} // namespace nhtml::detail

/// ===========================================================================
///  Document
/// ===========================================================================
void nhtml::document::write(FILE* output_file) const {
    detail::html_writer writer{output_file};
    writer.write(*this);
}

auto nhtml::document::string() const -> std::string {
    std::string out;
    detail::html_writer writer{out};
    writer.write(*this);
    return out;
}
