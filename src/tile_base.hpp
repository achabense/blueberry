#pragma once

#include "rule.hpp"

namespace aniso {
    struct vecT {
        int x, y;

        int xy() const { return x * y; }

        friend bool operator==(const vecT&, const vecT&) = default;
        friend vecT operator+(const vecT& a, const vecT& b) { return {.x = a.x + b.x, .y = a.y + b.y}; }
        friend vecT operator-(const vecT& a, const vecT& b) { return {.x = a.x - b.x, .y = a.y - b.y}; }
        friend vecT operator/(const vecT& a, int b) { return {.x = a.x / b, .y = a.y / b}; }
        friend vecT operator*(const vecT& a, int b) { return {.x = a.x * b, .y = a.y * b}; }
        friend vecT operator/(const vecT& a, double b) = delete;
        friend vecT operator*(const vecT& a, double b) = delete;

        [[nodiscard]] vecT plus(int dx, int dy) const { return {.x = x + dx, .y = y + dy}; }
        [[nodiscard]] vecT minus(int dx, int dy) const { return {.x = x - dx, .y = y - dy}; }
        bool both_gteq(const vecT& b) const { return x >= b.x && y >= b.y; } // >=
        bool both_lteq(const vecT& b) const { return x <= b.x && y <= b.y; } // <=
        bool both_lt(const vecT& b) const { return x < b.x && y < b.y; }     // <
        bool both_gt(const vecT& b) const { return x > b.x && y > b.y; }     // >
    };

    inline vecT clamp(const vecT& p, const vecT& min, const vecT& max) { // []
        assert(min.both_lteq(max));
        return {.x = std::clamp(p.x, min.x, max.x), .y = std::clamp(p.y, min.y, max.y)};
    }

    struct rangeT {
        vecT begin, end; // [)

        vecT size() const {
            assert(begin.both_lteq(end));
            return end - begin;
        }

        bool empty() const {
            assert(begin.both_lteq(end));
            return begin.x == end.x || begin.y == end.y;
        }
    };

    inline rangeT common(const rangeT& a, const rangeT& b) {
        const vecT begin = clamp(a.begin, b.begin, b.end);
        const vecT end = clamp(a.end, b.begin, b.end);
        if (begin.both_lt(end)) {
            return {begin, end};
        } else {
            return {};
        }
    }

    namespace _misc {
        // Should be non-empty.
        template <class T>
        struct tile_ref_ {
            T* data; // Non-owning; points at [0][0].
            vecT size;
            int stride; // Number of elements (instead of bytes).

            tile_ref_(T* data, vecT size, int stride) : data{data}, size{size}, stride{stride} {
                assert(data && size.x > 0 && size.y > 0 && size.x <= stride);
            }

            tile_ref_(T* data, vecT size) : data{data}, size{size}, stride{size.x} {
                assert(data && size.x > 0 && size.y > 0);
            }

            operator tile_ref_<const T>() const
                requires(!std::is_const_v<T>)
            {
                return std::bit_cast<tile_ref_<const T>>(*this);
            }

            bool contains(vecT pos) const { return pos.x >= 0 && pos.y >= 0 && pos.x < size.x && pos.y < size.y; }
            bool contains(int x, int y) const { return x >= 0 && y >= 0 && x < size.x && y < size.y; }
            bool contains(const rangeT& range) const {
                return range.begin.both_gteq({0, 0}) && range.end.both_lteq(size);
            }

            T* line(int y) const {
                assert(y >= 0 && y < size.y);
                return data + stride * y;
            }
            T& at(vecT pos) const {
                assert(contains(pos));
                return *(data + stride * pos.y + pos.x);
            }
            T& at(int x, int y) const {
                assert(contains(x, y));
                return *(data + stride * y + x);
            }

            [[nodiscard]] tile_ref_ clip(const rangeT& range) const {
                assert(!range.empty() && contains(range));
                return {&at(range.begin), range.size(), stride};
            }
            [[nodiscard]] tile_ref_ clip_corner(vecT corner) const {
                assert(corner.both_gt({0, 0}) && corner.both_lteq(size));
                return {data, corner, stride};
            }

            void for_each_line(const auto& fn) const {
                T* data = this->data;
                for (int y = 0; y < size.y; ++y, data += stride) {
                    if constexpr (requires { fn((int)y, std::span{data, data + size.x}); }) {
                        fn((int)y, std::span{data, data + size.x});
                    } else {
                        static_assert(requires { fn(std::span{data, data + size.x}); });
                        fn(std::span{data, data + size.x});
                    }
                }
            }

            // TODO: (wontfix?) there are several uses of `const std::invocable<...> auto& fn` in the project.
            // In these cases, the concept applies to `T` instead of `const T`, so it is requiring non-const-invocable
            // instead. (Also, should these `fn` really be passed as `const auto&`?)
            void for_all_data(const auto& fn) const {
                static_assert(requires { fn(std::span{data, data + size.x * size.y}); });
                if (size.x == stride) {
                    fn(std::span{data, data + size.xy()});
                } else {
                    this->for_each_line(fn);
                }
            }

            template <class U>
            void for_all_data_vs(const tile_ref_<U>& b, const auto& fn) const {
                static_assert(requires { fn(data, b.data, 1); });
                assert(size == b.size);

                if (size.x == stride && b.size.x == b.stride) {
                    fn(data, b.data, size.xy());
                    return;
                }

                T* this_data = data;
                U* that_data = b.data;
                for (int y = 0; y < size.y; ++y, this_data += stride, that_data += b.stride) {
                    fn(this_data, that_data, size.x);
                }
            }
        };
    } // namespace _misc

    using tile_ref = _misc::tile_ref_<cellT>;
    using tile_const_ref = _misc::tile_ref_<const cellT>;

    inline bool equal(const tile_const_ref a, const tile_const_ref b) {
        if (a.size != b.size) {
            return false;
        }
        if (a.size.x == a.stride && b.size.x == b.stride) {
            return std::equal(a.data, a.data + a.size.xy(), b.data);
        }

        const cellT *a_data = a.data, *b_data = b.data;
        for (int y = 0; y < a.size.y; ++y, a_data += a.stride, b_data += b.stride) {
            if (!std::equal(a_data, a_data + a.size.x, b_data)) {
                return false;
            }
        }
        return true;
    }
} // namespace aniso
