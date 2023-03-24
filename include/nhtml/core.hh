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
    /// The document’s child elements.
    std::vector<element::ptr> elements;

    /// Create a new document.
    explicit document() = default;

    /// Write the document to a file.
    void write(FILE* output_file) const;

    /// Write the document to a string.
    auto string() const -> std::string;
};
} // namespace nhtml

#endif // NHTML_CORE_HH
