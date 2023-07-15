#ifndef NHTML_SOURCE_LOCATION_HH
#define NHTML_SOURCE_LOCATION_HH

#include <nhtml/utils.hh>

namespace nhtml::detail {
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

    /// Check if this source location is valid.
    constexpr explicit operator bool() const { return len != 0; }
};

/// A decoded source location.
struct loc_info {
    usz line;
    usz col;
    const char* line_start;
    const char* line_end;
};
}

#endif // NHTML_SOURCE_LOCATION_HH
