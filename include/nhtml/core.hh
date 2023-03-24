#ifndef NHTML_CORE_HH
#define NHTML_CORE_HH

#include <nhtml/utils.hh>
#include <optional>
#include <set>
#include <unordered_set>
#include <vector>

namespace nhtml {
/// An NHTML element.
struct element {
    using ptr = std::unique_ptr<element>;
    using vector = std::vector<element::ptr>;

private:
    explicit element() {}
    explicit element(std::string _tag_name)
        : tag_name(std::move(_tag_name)) {}
    explicit element(std::string _tag_name, element::ptr&& el)
        : tag_name(std::move(_tag_name))
        , content(vector{}) {
        std::get<vector>(content).push_back(std::move(el));
    }
    explicit element(std::string _tag_name, std::vector<element::ptr>&& els)
        : tag_name(std::move(_tag_name))
        , content(std::move(els)) {}

public:
    /// Create a new element.
    template <typename... arguments>
    static ptr make(arguments&&... args) {
        /// Note: We can’t use std::make_unique here because the constructor is private.
        return ptr{new element{std::forward<arguments>(args)...}};
    }

    /// The element’s name.
    std::string tag_name;

    /// The element’s classes. Ordered for deterministic output.
    std::set<std::string> class_list;

    /// The element’s ID, if any.
    std::string id;

    /// The element’s content.
    std::variant<vector, std::string> content;
};

/// An NHTML document.
struct document {
    /// Stringification options.
    struct write_opts {
        static constexpr u64 default_indent_width = 4;
        static constexpr u64 default_text_columns = 140;
        static constexpr bool default_no_indent = false;
        static constexpr bool default_use_tabs = false;

        /// Number of spaces per indentation level. Ignored for tabs.
        u64 indent_width = default_indent_width;

        /// Maximum column number.
        u64 text_columns = default_text_columns;

        /// Whether to disable indenting.
        bool no_indent = default_no_indent;

        /// Whether to use tabs instead of spaces.
        bool use_tabs = default_use_tabs;
    };

    /// The document’s child elements.
    std::vector<element::ptr> elements;

    /// Create a new document.
    explicit document() = default;

    /// Write the document to a file.
    ///
    /// \param output_file The file to write to.
    /// \param opts Stringification options. Pass `{}` for defaults.
    void write(FILE* output_file, write_opts opts) const;

    /// Write the document to a string.
    ///
    /// \param opts Stringification options. Pass `{}` for defaults.
    /// \return The stringified document.
    [[nodiscard]] auto string(write_opts opts) const -> std::string;
};
} // namespace nhtml

#endif // NHTML_CORE_HH
