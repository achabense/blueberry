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
        [[nodiscard]] vecT div_and_trunc(double b) const { return {.x = int(x / b), .y = int(y / b)}; }
        [[nodiscard]] vecT mul_and_trunc(double b) const { return {.x = int(x * b), .y = int(y * b)}; }

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

        friend bool operator==(const rangeT&, const rangeT&) = default;

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

            int area() const { return size.xy(); }

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

            tile_ref_ top(int y) const { return clip({{0, 0}, {size.x, y}}); }
            tile_ref_ left(int x) const { return clip({{0, 0}, {x, size.y}}); }
            tile_ref_ bottom(int y) const { return clip({{0, size.y - y}, size}); }
            tile_ref_ right(int x) const { return clip({{size.x - x, 0}, size}); }

            void for_each_line(const auto& fn) const {
                T* p = data;
                for (int y = 0; y < size.y; ++y, p += stride) {
                    if constexpr (requires { fn((int)y, std::span{p, p + size.x}); }) {
                        fn((int)y, std::span{p, p + size.x});
                    } else { // `fn` knows size.x.
                        static_assert(requires { fn((int)y, (T*)p); });
                        fn((int)y, (T*)p);
                    }
                }
            }

            template <class F>
                requires requires(const F& fn) { fn(std::span{data, data + size.xy()}); }
            void for_all_data(const F& fn) const {
                if (size.x == stride) {
                    fn(std::span{data, data + size.xy()});
                } else {
                    T* p = data;
                    for (int y = 0; y < size.y; ++y, p += stride) {
                        fn(std::span{p, p + size.x});
                    }
                }
            }
        };

        template <class A, class B, class F>
            requires requires(A* a, B* b, const F& fn) { fn(a, b, 1); }
        inline void for_all_data(const tile_ref_<A> a, const tile_ref_<B> b, const F& fn) {
            assert(a.size == b.size);
            const vecT size = a.size;

            if (size.x == a.stride && size.x == b.stride) {
                fn(a.data, b.data, size.xy());
            } else {
                A* a_data = a.data;
                B* b_data = b.data;
                for (int y = 0; y < size.y; ++y, a_data += a.stride, b_data += b.stride) {
                    fn(a_data, b_data, size.x);
                }
            }
        }

        template <class A, class B, class C, class F>
            requires requires(A* a, B* b, C* c, const F& fn) { fn(a, b, c, 1); }
        inline void for_all_data(const tile_ref_<A> a, const tile_ref_<B> b, const tile_ref_<C> c, const F& fn) {
            assert(a.size == b.size && a.size == c.size);
            const vecT size = a.size;

            if (size.x == a.stride && size.x == b.stride && size.x == c.stride) {
                fn(a.data, b.data, c.data, size.xy());
            } else {
                A* a_data = a.data;
                B* b_data = b.data;
                C* c_data = c.data;
                for (int y = 0; y < size.y; ++y, a_data += a.stride, b_data += b.stride, c_data += c.stride) {
                    fn(a_data, b_data, c_data, size.x);
                }
            }
        }
    } // namespace _misc

    using _misc::for_all_data;
    using tile_ref = _misc::tile_ref_<cellT>;
    using tile_const_ref = _misc::tile_ref_<const cellT>;

    static_assert(std::is_trivially_copyable_v<tile_ref>);
    static_assert(std::is_trivially_copyable_v<tile_const_ref>);

    inline bool equal(const cellT* a, const cellT* b, int len) { return std::equal(a, a + len, b); }

    inline bool equal(const tile_const_ref a, const tile_const_ref b) {
        if (a.size != b.size) {
            return false;
        }
        const vecT size = a.size;
        if (size.x == a.stride && size.x == b.stride) {
            return equal(a.data, b.data, size.xy());
        }

        const cellT *a_data = a.data, *b_data = b.data;
        for (int y = 0; y < size.y; ++y, a_data += a.stride, b_data += b.stride) {
            if (!equal(a_data, b_data, size.x)) {
                return false;
            }
        }
        return true;
    }
} // namespace aniso
