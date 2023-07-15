#ifndef NHTML_INTERNAL_FILE_HH
#define NHTML_INTERNAL_FILE_HH

#include <nhtml/core.hh>
#include <nhtml/internal/token.hh>

#ifdef __linux__
#    include <fcntl.h>
#    include <sys/mman.h>
#    include <sys/stat.h>
#else
#    include <fstream>
#endif

/// Implementation details.
namespace nhtml::detail {
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

    /// Index of the file in the parser's file list.
    u16 file_index = 0;

    /// Current position in the file.
    const char* curr{};
    const char* end{};

    static auto map(fs::path filename) -> res<file> {
        file f;

        /// Open.
#ifdef __linux__
        int fd = ::open(filename.c_str(), O_RDONLY);
        if (fd < 0) [[unlikely]]
            return mkerr("Could not open file '{}': {}", filename.string(), std::strerror(errno));

        /// Get size.
        struct stat s {};
        if (::fstat(fd, &s)) [[unlikely]]
            return mkerr("Could not stat file '{}': {}", filename.string(), std::strerror(errno));
        auto sz = size_t(s.st_size);

        /// If the size is not zero, map the file. Otherwise, set the contents to the empty string.
        if (sz != 0) [[likely]] {
            /// Map.
            auto* mem = (char*) ::mmap(nullptr, sz, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
            if (mem == MAP_FAILED) [[unlikely]]
                return mkerr("Could not mmap file '{}': {}", filename.string(), std::strerror(errno));
            ::close(fd);

            /// Copy to string.
            f.contents = std::string{mem, sz};

            /// Unmap.
            if (::munmap(mem, sz)) [[unlikely]]
                return mkerr("Could not munmap file '{}': {}", filename.string(), std::strerror(errno));
        } else [[unlikely]] {
            f.contents = "";
        }

#else
        std::ifstream file(filename);
        if (not file) return mkerr("Could not open file '{}'", filename.string());
        f.contents = std::string{std::istreambuf_iterator<char>(file), {}};
#endif
        /// Set the parent path name.
        f.parent_directory = get_parent_directory(filename);
        f.name = std::move(filename);
        return f;
    }

    static auto get_parent_directory(const fs::path& raw_filename) -> fs::path {
        std::error_code ec;
        auto filename = fs::absolute(raw_filename, ec);
        if (ec) return "";

        /// Determine the parent directory of the file.
        /// If the path has a parent, use that.
        if (filename.has_parent_path()) {
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

} // namespace nhtml::detail

#endif // NHTML_INTERNAL_FILE_HH
