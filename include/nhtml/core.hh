#ifndef NHTML_CORE_HH
#define NHTML_CORE_HH

#include <map>
#include <nhtml/utils.hh>
#include <optional>
#include <set>
#include <unordered_set>
#include <vector>
#include <atomic>

namespace nhtml {
namespace detail {
class parser;
}

/// Quoting style for attributes.
enum struct quoting_style {
    single_quotes,
    double_quotes,
};

/// An NHTML element.
struct element {
    class ptr {
        element* data;

    public:
        ptr() : data(nullptr) {}
        ptr(std::nullptr_t) : data(nullptr) {}
        ptr(element* _data) : data(_data) { data->refcount++; }

        ptr(const ptr& other) : data(other.data) { data->refcount++; }
        ptr(ptr&& other) : data(other.data) { other.data = nullptr; }

        ptr &operator=(const ptr& other) {
            if (data) data->refcount--;
            data = other.data;
            data->refcount++;
            return *this;
        }

        ptr &operator=(ptr&& other) {
            if (data) data->refcount--;
            data = other.data;
            other.data = nullptr;
            return *this;
        }

        ~ptr() {
            if (data and --data->refcount == 0) delete data;
        }

        auto get() const -> element* { return data; }

        explicit operator bool() const { return data; }
        auto operator->() const -> element* { return data; }
        auto operator*() const -> element& { return *data; }
        bool operator==(const ptr& other) const { return data == other.data; }
    };

    using vector = std::vector<ptr>;
    using class_list = std::set<std::string>;
    using attribute_list = detail::icase_map<std::string>;
    using inline_style = std::string;

    /// The element’s name.
    std::string tag_name;

    /// The element’s classes. Ordered for deterministic output.
    class_list classes;

    /// The element’s attributes. Ordered for deterministic output.
    attribute_list attributes;

    /// The element’s ID, if any.
    std::string id;

    /// The element’s content.
    std::variant<std::monostate, std::string, vector> content = std::monostate{};

private:
    friend class detail::parser;

    /// Reference count.
    std::atomic<usz> refcount = 0;

    /// Used by query_selector_all().
    bool selected = false;

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
        , content(std::move(els)) {
        if (std::get<vector>(content).empty()) content = std::monostate{};
    }

public:
    /// Create a new element.
    template <typename... arguments>
    static ptr make(arguments&&... args) {
        /// Note: We can’t use std::make_unique here because the constructor is private.
        return ptr{new element{std::forward<arguments>(args)...}};
    }
};

/// An NHTML document.
struct document {
    /// Stringification options.
    struct write_opts {
        static constexpr u64 default_indent_width = 4;
        static constexpr u64 default_text_columns = 140;
        static constexpr bool default_no_indent = false;
        static constexpr bool default_use_tabs = false;
        static constexpr bool default_self_close_xml_tags = false;
        static constexpr quoting_style default_quoting_style = quoting_style::single_quotes;

        /// Number of spaces per indentation level. Ignored for tabs.
        u64 indent_width = default_indent_width;

        /// Maximum column number.
        u64 text_columns = default_text_columns;

        /// Whether to quote attribute values using single or double quotes.
        quoting_style attribute_quoting_style = default_quoting_style;

        /// Whether to disable indenting.
        bool no_indent = default_no_indent;

        /// Whether to self-close XML-style tags.
        bool self_close_xml_tags = default_self_close_xml_tags;

        /// Whether to use tabs instead of spaces.
        bool use_tabs = default_use_tabs;
    };

    /// The document’s child elements.
    std::vector<element::ptr> elements;

    /// Create a new document.
    explicit document() = default;
    document(const document&) = delete;
    document(document&&) = default;
    document& operator=(const document&) = delete;
    document& operator=(document&&) = default;

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
