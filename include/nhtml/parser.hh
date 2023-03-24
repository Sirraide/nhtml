#ifndef NHTML_PARSER_HH
#define NHTML_PARSER_HH

#include <expected>
#include <nhtml/core.hh>
#include <nhtml/utils.hh>

#ifdef __linux__
#    include <fcntl.h>
#    include <sys/mman.h>
#    include <sys/stat.h>
#else
#    include <fstream>
#endif

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

/// Implementation details.
namespace detail {
struct parser;

/// ===========================================================================
///  Source locations.
/// ===========================================================================
/// Source location.
struct loc {
    u32 pos{};
    u16 file{};
    u16 len{};

    /// Construct a source location from a position, file, and length.
    constexpr loc(u32 p = 0, u16 f = 0, u16 l = 0)
        : pos(p)
        , file(f)
        , len(l) {}

    /// Construct a source location between two other locations.
    constexpr loc(loc start, loc end) {
        /// Different files is not gonna work...
        if (start.file != end.file) {
            pos = 0;
            len = 0;
            return;
        }

        /// Start may be after end.
        if (start.pos > end.pos) std::swap(start, end);

        /// Set the position and length.
        pos = start.pos;
        len = static_cast<u16>(end.pos - start.pos + end.len);

        /// Set the file.
        file = start.file;
    }

    /// Shift a source location to the left.
    constexpr loc operator<<(isz amount) const {
        loc l = *this;
        l.pos = static_cast<u32>(pos - amount);
        return l;
    }

    /// Shift a source location to the right.
    constexpr loc operator>>(isz amount) const {
        loc l = *this;
        l.pos = static_cast<u32>(pos + amount);
        return l;
    }

    /// Extend a source location to the left.
    constexpr loc operator<<=(isz amount) const {
        loc l = *this << amount;
        l.len = static_cast<u16>(l.len + amount);
        return l;
    }

    /// Extend a source location to the right.
    constexpr loc operator>>=(isz amount) const {
        loc l = *this;
        l.len = static_cast<u16>(l.len + amount);
        return l;
    }
};

/// A decoded source location.
struct loc_info {
    usz line;
    usz col;
    const char* line_start;
    const char* line_end;
};

/// Helper to make an error.
template <typename... arguments>
auto mkerr(fmt::format_string<arguments...> fmt, arguments&&... args) -> std::unexpected<std::string> {
    return std::unexpected{fmt::format(fmt, std::forward<arguments>(args)...)};
}

/// A source file.
struct file {
    string_ref contents;
    fs::path name;
    fs::path parent_directory;

    static auto map(fs::path filename) -> std::expected<file, std::string> {
        file f;

        /// Open.
#ifdef __linux__
        int fd = ::open(filename.c_str(), O_RDONLY);
        if (fd < 0) [[unlikely]]
            return mkerr("Could not open file: {}: {}", filename.native(), std::strerror(errno));

        /// Get size.
        struct stat s {};
        if (::fstat(fd, &s)) [[unlikely]]
            return mkerr("Could not stat file: {}: {}", filename.native(), std::strerror(errno));
        auto sz = size_t(s.st_size);

        /// If the size is not zero, map the file. Otherwise, set the contents to the empty string.
        if (sz != 0) [[likely]] {
            /// Map.
            auto* mem = (char*) ::mmap(nullptr, sz, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
            if (mem == MAP_FAILED) [[unlikely]]
                return mkerr("Could not mmap file: {}: {}", filename.native(), std::strerror(errno));
            ::close(fd);

            /// Copy to string.
            f.contents = std::string{mem, sz};

            /// Unmap.
            if (::munmap(mem, sz)) [[unlikely]]
                return mkerr("Could not munmap file: {}: {}", filename.native(), std::strerror(errno));
        } else [[unlikely]] {
            f.contents = "";
        }

#else
        std::ifstream file(filename);
        if (not file) return mkerr("Could not open file: {}", filename.native());
        f.contents = std::string{std::istreambuf_iterator<char>(file), {}};
#endif
        /// Set the parent path name.
        f.parent_directory = get_parent_directory(filename);
        f.name = std::move(filename);
        return f;
    }

    static auto get_parent_directory(const fs::path& filename) -> fs::path {
        /// Determine the parent directory of the file.
        /// If the path has a parent, use that.
        std::error_code ec;
        if (not filename.has_parent_path()) {
            auto par = filename.parent_path();
            auto par_canon = fs::canonical(par, ec);
            if (not ec and fs::exists(par_canon, ec) and fs::is_directory(par_canon, ec) and not ec) return par_canon;
        }

        /// Otherwise, use the current working directory.
        auto path = fs::current_path(ec);
        if (ec) return "";
        return path;
    }
};

} // namespace detail

} // namespace nhtml

#endif // NHTML_PARSER_HH
