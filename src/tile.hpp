#pragma once

#include <charconv>
#include <format>

#include "tile_base.hpp"

namespace aniso {
    inline vecT divmul_floor(const vecT a, const vecT b) { return {.x = (a.x / b.x) * b.x, .y = (a.y / b.y) * b.y}; }
    inline vecT divmul_ceil(const vecT a, const vecT b) {
        const auto divmul_ceil = [](int a, int b) -> int { return ((a + b - 1) / b) * b; };
        return {.x = divmul_ceil(a.x, b.x), .y = divmul_ceil(a.y, b.y)};
    }

    inline vecT min(const vecT a, const vecT b) { return {.x = std::min(a.x, b.x), .y = std::min(a.y, b.y)}; }
    inline vecT max(const vecT a, const vecT b) { return {.x = std::max(a.x, b.x), .y = std::max(a.y, b.y)}; }

    namespace _misc {
        class wrapped_int {
            int i; // [0, r).
            const int r;

        public:
            wrapped_int(int i, int r) : i(i), r(r) { assert(i >= 0 && i < r); }

            int operator++(int) {
                int j = i++;
                if (i == r) {
                    i = 0;
                }
                return j;
            }
        };
    } // namespace _misc

    inline int count(const tile_const_ref tile) {
        int c = 0;
        tile.for_all_data([&c](std::span<const cellT> line) {
            for (const cellT v : line) {
                c += v;
            }
        });
        return c;
    }

    // (Both are not periodic.)
    inline int count_diff(const tile_const_ref a, const tile_const_ref b) {
        int c = 0;
        a.for_all_data_vs(b, [&c](const cellT* a, const cellT* b, int len) {
            for (int i = 0; i < len; ++i) {
                c += a[i] != b[i];
            }
        });
        return c;
    }

    // (`dest` and `source` should not overlap.)
    inline void copy(const tile_ref dest, const tile_const_ref source) {
        dest.for_all_data_vs(source, [](cellT* d, const cellT* s, int len) { //
            std::copy_n(s, len, d);
        });
    }

    // (`dest` should not overlap with `source` or `repeat`.)
    // (`repeat.at(0, 0)` is bound to `source.at(0, 0)`.)
    inline void copy_diff(const tile_ref dest, const tile_const_ref source,
                          const tile_const_ref repeat /*background*/) {
        if (repeat.size == vecT{1, 1}) {
            // Different from `blit` here.
            dest.for_all_data_vs(source, [background = *repeat.data](cellT* d, const cellT* s, int len) {
                for (int i = 0; i < len; ++i) {
                    if (s[i] != background) {
                        d[i] = s[i];
                    }
                }
            });
            return;
        }

        assert(dest.size == source.size);
        _misc::wrapped_int dy(0, repeat.size.y);
        dest.for_each_line([&](const int y, std::span<cellT> line) {
            const cellT* const rep = repeat.line(dy++);
            const cellT* s = source.line(y);
            _misc::wrapped_int dx(0, repeat.size.x);
            for (cellT& cell : line) {
                const cellT v = *s++;
                if (v != rep[dx++]) {
                    cell = v;
                }
            }
        });
    }

    // (`dest` and `source` should not overlap.)
    enum class blitE { Copy, Or, And, Xor };
    template <blitE mode>
    inline void blit(const tile_ref dest, const tile_const_ref source) {
        if constexpr (mode == blitE::Copy) {
            copy(dest, source);
        } else {
            dest.for_all_data_vs(source, [](cellT* d, const cellT* s, int len) {
                for (int i = 0; i < len; ++i) {
                    if constexpr (mode == blitE::Or) {
                        d[i] = d[i] | s[i];
                    } else if constexpr (mode == blitE::And) {
                        d[i] = d[i] & s[i];
                    } else {
                        static_assert(mode == blitE::Xor);
                        d[i] = d[i] ^ s[i];
                    }
                }
            });
        }
    }

    inline void blit(const tile_ref dest, const tile_const_ref source, blitE mode) {
        switch (mode) {
            case blitE::Copy: blit<blitE::Copy>(dest, source); return;
            case blitE::Or: blit<blitE::Or>(dest, source); return;
            case blitE::And: blit<blitE::And>(dest, source); return;
            // case blitE::Xor: blit<blitE::Xor>(dest, source); return;
            // (Not used in the program.)
            default: assert(false);
        }
    }

    // The result will be completely dependent on the state of `rand` and `density`.
    // (`bernoulli_distribution` cannot guarantee this.)
    inline void random_fill(const tile_ref tile, std::mt19937& rand, double density) {
        const uint32_t c = std::mt19937::max() * std::clamp(density, 0.0, 1.0);
        tile.for_all_data([&](std::span<cellT> line) { //
            std::ranges::generate(line, [&] { return cellT(rand() < c ? 1 : 0); });
        });
    }

    inline void random_flip(const tile_ref tile, std::mt19937& rand, double density) {
        const uint32_t c = std::mt19937::max() * std::clamp(density, 0.0, 1.0);
        tile.for_all_data([&](std::span<cellT> line) {
            for (cellT& cell : line) {
                cell = cell ^ (rand() < c);
            }
        });
    }

    inline void fill(const tile_ref tile, const cellT v) {
        tile.for_all_data([v](std::span<cellT> line) { //
            std::ranges::fill(line, v);
        });
    }

    // (`tile` and `repeat` should not overlap.)
    // (`repeat.at(0, 0)` is bound to `tile.at(0, 0)`.)
    inline void fill(const tile_ref tile, const tile_const_ref repeat) {
        if (repeat.size == vecT{1, 1}) {
            fill(tile, *repeat.data);
            return;
        }

        _misc::wrapped_int dy(0, repeat.size.y);
        tile.for_each_line([&](const int y, std::span<cellT> line) {
            const cellT* const rep = repeat.line(dy++);
            _misc::wrapped_int dx(0, repeat.size.x);
            for (cellT& cell : line) {
                cell = rep[dx++];
            }
        });
    }

    inline void fill_outside(const tile_ref tile, const rangeT& range, const cellT v) {
        assert(tile.contains(range));
        tile.for_each_line([&](const int y, std::span<cellT> line) {
            if (y < range.begin.y || y >= range.end.y) {
                std::ranges::fill(line, v);
            } else {
                std::ranges::fill(line.first(range.begin.x), v);
                std::ranges::fill(line.subspan(range.end.x), v);
            }
        });
    }

    // (`repeat.at(0, 0)` is bound to `tile.at(0, 0)` instead of `range.begin`.)
    inline void fill_outside(const tile_ref tile, const rangeT& range, const tile_const_ref repeat) {
        if (repeat.size == vecT{1, 1}) {
            fill_outside(tile, range, *repeat.data);
            return;
        }

        assert(tile.contains(range));
        const auto fill = [](std::span<cellT> line, const cellT* rep, _misc::wrapped_int dx) {
            for (cellT& cell : line) {
                cell = rep[dx++];
            }
        };

        _misc::wrapped_int dy(0, repeat.size.y);
        tile.for_each_line([&](const int y, std::span<cellT> line) {
            const cellT* const rep = repeat.line(dy++);
            if (y < range.begin.y || y >= range.end.y) {
                fill(line, rep, {0, repeat.size.x});
            } else {
                fill(line.first(range.begin.x), rep, {0, repeat.size.x});
                fill(line.subspan(range.end.x), rep, {range.end.x % repeat.size.x, repeat.size.x});
            }
        });
    }

    inline rangeT bounding_box(const tile_const_ref tile, const cellT background) {
        int min_x = INT_MAX, max_x = INT_MIN;
        int min_y = INT_MAX, max_y = INT_MIN;
        tile.for_each_line([&](const int y, std::span<const cellT> line) {
            for (int x = 0; const cellT cell : line) {
                if (cell != background) {
                    min_x = std::min(min_x, x);
                    max_x = std::max(max_x, x);
                    min_y = std::min(min_y, y);
                    max_y = std::max(max_y, y);
                }
                ++x;
            }
        });
        if (max_x != INT_MIN) {
            return {.begin{.x = min_x, .y = min_y}, .end{.x = max_x + 1, .y = max_y + 1}};
        } else {
            return {};
        }
    }

    // (`repeat.at(0, 0)` is bound to `tile.at(0, 0)`.)
    inline rangeT bounding_box(const tile_const_ref tile, const tile_const_ref repeat /*background*/) {
        if (repeat.size == vecT{1, 1}) {
            return bounding_box(tile, *repeat.data);
        }

        int min_x = INT_MAX, max_x = INT_MIN;
        int min_y = INT_MAX, max_y = INT_MIN;
        _misc::wrapped_int dy(0, repeat.size.y);
        tile.for_each_line([&](const int y, std::span<const cellT> line) {
            const cellT* const rep = repeat.line(dy++);
            _misc::wrapped_int dx(0, repeat.size.x);
            for (int x = 0; const cellT cell : line) {
                if (cell != rep[dx++]) {
                    min_x = std::min(min_x, x);
                    max_x = std::max(max_x, x);
                    min_y = std::min(min_y, y);
                    max_y = std::max(max_y, y);
                }
                ++x;
            }
        });
        if (max_x != INT_MIN) {
            return {.begin{.x = min_x, .y = min_y}, .end{.x = max_x + 1, .y = max_y + 1}};
        } else {
            return {};
        }
    }

    // `tile.size` -> non-periodic, or the period is too large
    inline vecT spatial_period(const tile_const_ref tile) {
        auto has_period_x = [tile](const int p_x) -> bool {
            for (int y = 0; y < tile.size.y; ++y) {
                for (int x = p_x; x < tile.size.x; ++x) {
                    if (tile.at(x, y) != tile.at(x - p_x, y)) {
                        return false;
                    }
                }
            }
            return true;
        };

        auto has_period_y = [tile](const int p_y) -> bool {
            for (int y = p_y; y < tile.size.y; ++y) {
                for (int x = 0; x < tile.size.x; ++x) {
                    if (tile.at(x, y) != tile.at(x, y - p_y)) {
                        return false;
                    }
                }
            }
            return true;
        };

        vecT p_size = tile.size;
        for (int x = 1; x < std::min(tile.size.x, 60); ++x) {
            if (has_period_x(x)) {
                p_size.x = x;
                break;
            }
        }
        for (int y = 1; y < std::min(tile.size.y, 60); ++y) {
            if (has_period_y(y)) {
                p_size.y = y;
                break;
            }
        }
        return p_size;
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        // TODO: many tests in this header can be simplified if using `tileT`.
        inline const testT test_periodic_functions = [] {
            const vecT padding_a{.x = int(testT::rand() % 5), .y = int(testT::rand() % 5)};
            const vecT padding_b{.x = int(testT::rand() % 5), .y = int(testT::rand() % 5)};
            const vecT inner_size{10, 10};
            const vecT size = padding_a + inner_size + padding_b;
            const auto tile_data = std::make_unique_for_overwrite<cellT[]>(size.xy());
            const tile_ref tile{tile_data.get(), size};
            const rangeT inner_range{padding_a, padding_a + inner_size};
            fill(tile.clip(inner_range), {0});
            const cellT period[4]{1, 0, 0, 1}; // Checkerboard.
            fill_outside(tile, inner_range, {period, {2, 2}});
            const rangeT test_range = bounding_box(tile, {period, {2, 2}});
            assert(!test_range.empty());
            assert(test_range.begin == inner_range.begin && test_range.end == inner_range.end);
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    // (`dest` and `source` should not overlap.)
    // Map (0, 0) to (wrap(to.x), wrap(to.y)).
    inline void rotate_copy_00_to(const tile_ref dest, const tile_const_ref source, vecT to) {
        assert(dest.size == source.size);
        const vecT size = dest.size;

        const auto wrap = [](int v, int r) { return ((v % r) + r) % r; };
        to = {.x = wrap(to.x, size.x), .y = wrap(to.y, size.y)};
        assert(to.both_gteq({0, 0}) && to.both_lt(size));

        if (to.x == 0 && to.y == 0) {
            copy(dest, source);
            return;
        }

#ifndef NDEBUG
        const cellT test = source.at(0, 0);
#endif // !NDEBUG
        source.for_each_line([&](int y, std::span<const cellT> line) {
            cellT* const dest_ = dest.line((y + to.y) % size.y);
            std::copy_n(line.data(), size.x - to.x, dest_ + to.x);
            std::copy_n(line.data() + size.x - to.x, to.x, dest_);
        });
        assert(test == dest.at(to));
    }

    namespace _misc {
        //  https://conwaylife.com/wiki/Run_Length_Encoded
        inline void to_RLE(std::string& str, const tile_const_ref tile) {
            class putT {
                std::string& str;
                size_t last_nl;
                int n = 0;
                char ch = 'b'; // 'b', 'o', '$'.
            public:
                putT(std::string& str) : str(str), last_nl(str.size()) { assert(str.empty() || str.back() == '\n'); }
                void flush() {
                    if (n != 0) {
                        // (58 is an arbitrary value that satisfies the line-length limit.)
                        if (str.size() > last_nl + 58) {
                            str += '\n';
                            last_nl = str.size();
                        }

                        if (n != 1) {
                            str += std::to_string(n);
                        }
                        str += ch;
                        n = 0;
                    }
                }
                void append(int n2, char ch2) {
                    assert(ch2 == 'b' || ch2 == 'o' || ch2 == '$');
                    if (ch == ch2) {
                        n += n2;
                    } else {
                        flush();
                        n = n2;
                        ch = ch2;
                    }
                }
            };

            putT put{str};
            tile.for_each_line([&put, h = tile.size.y](const int y, std::span<const cellT> line) {
                if (y != 0) {
                    put.append(1, '$');
                }
                const cellT *pos = line.data(), *const end = line.data() + line.size();
                while (pos != end) {
                    const cellT cell = *pos++;
                    int n = 1;
                    while (pos != end && *pos == cell) {
                        ++n;
                        ++pos;
                    }
                    const bool omit = y != 0 && y != h - 1 && pos == end && cell == 0;
                    if (!omit) {
                        put.append(n, cell ? 'o' : 'b');
                    }
                }
            });
            put.flush();
        }
    } // namespace _misc

    inline std::string to_RLE_str(const tile_const_ref tile, const ruleT* rule = nullptr) {
        std::string str = rule ? std::format("x = {}, y = {}, rule = {}\n", tile.size.x, tile.size.y, to_MAP_str(*rule))
                               : std::format("x = {}, y = {}\n", tile.size.x, tile.size.y);
        _misc::to_RLE(str, tile);
        str += '!';
        return str;
    }

    // It's unfortunate that the project didn't consistently use intX_t from the beginning...
    static_assert(INT_MAX < LLONG_MAX);

    [[nodiscard]] inline std::string_view strip_RLE_header(std::string_view text,
                                                           std::optional<ruleT>* rule = nullptr) {
        const auto line_size = [](std::string_view str) {
            const auto find_nl = str.find('\n');
            return find_nl == str.npos ? str.size() : find_nl + 1;
        };

        // TODO: whether to allow empty lines in between?
        while (text.starts_with('#')) {
            text.remove_prefix(line_size(text));
        }
        if (text.starts_with('x')) {
            const auto size = line_size(text);
            if (rule) {
                // (Not interested in whether the header has correct format.)
                const auto extr = extract_MAP_str(text.substr(0, size));
                if (extr.has_rule()) {
                    rule->emplace(extr.get_rule());
                }
            }
            text.remove_prefix(size);
        }
        return text;
    }

    // (The size is calculated from the contents instead of header.)
    inline void from_RLE_str(std::string_view text, const auto& prepare) {
        static_assert(requires {
            { prepare((long long)(0), (long long)(0)) } -> std::same_as<std::optional<tile_ref>>;
        });

        text = strip_RLE_header(text);

        struct takerT {
            const char *str, *end;
            takerT(std::string_view text) : str(text.data()), end(str + text.size()) {}

            std::pair<int, char> take() {
                while (str != end && (*str == '\n' || *str == '\r' || *str == ' ')) {
                    ++str;
                }
                if (str == end) {
                    return {1, '!'};
                }

                int n = 1;
                if (*str >= '1' && *str <= '9') {
                    const auto [ptr, ec] = std::from_chars(str, end, n);
                    if (ec != std::errc{} || ptr == end) {
                        return {1, '!'};
                    }
                    str = ptr;
                }
                assert(str != end);
                switch (*str++) {
                    case 'b': return {n, 'b'};
                    case 'o': return {n, 'o'};
                    case '$': return {n, '$'};
                    default: return {1, '!'};
                }
            }
        };

        const auto [width, height] = [text] {
            long long x = 0, y = 0, xmax = 0;
            for (takerT taker(text);;) {
                const auto [n, c] = taker.take();
                assert(n >= 0);
                if (c == '!') {
                    break;
                } else if (c == 'b' || c == 'o') {
                    x += n;
                    xmax = std::max(xmax, x);
                } else {
                    assert(c == '$');
                    y += n;
                    x = 0;
                }
            }
            return std::pair{xmax, x == 0 ? y : y + 1};
        }();

        if (std::optional<tile_ref> tile_opt = prepare(width, height)) {
            assert(width != 0 && height != 0);
            const tile_ref tile = *tile_opt;
            assert(tile.size.x == width && tile.size.y == height);
            fill(tile, {0}); // `tile` data might be dirty.

            int x = 0, y = 0;
            for (takerT taker(text);;) {
                const auto [n, c] = taker.take();
                if (c == '!') {
                    break;
                } else if (c == 'b' || c == 'o') {
                    std::fill_n(tile.line(y) + x, n, cellT(c == 'o' ? 1 : 0));
                    x += n;
                } else {
                    y += n;
                    x = 0;
                }
            }
        }
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_RLE_str = [] {
            const vecT sizes[]{{.x = 1, .y = 1}, {.x = 10, .y = 1}, {.x = 1, .y = 10}, {.x = 32, .y = 60}};
            for (const vecT size : sizes) {
                const auto a_data = std::make_unique_for_overwrite<cellT[]>(size.xy());
                const auto b_data = std::make_unique_for_overwrite<cellT[]>(size.xy());
                const tile_ref a{a_data.get(), size};
                const tile_ref b{b_data.get(), size};
                random_fill(a, testT::rand, 0.5);
                from_RLE_str(to_RLE_str(a, nullptr), [&](long long w, long long h) {
                    assert(w == size.x && h == size.y);
                    return std::optional{b};
                });
                assert(std::equal(a_data.get(), a_data.get() + size.xy(), b_data.get()));
            }
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    inline constexpr int calc_border_size(const vecT size) { return 2 * (size.x + size.y) + 4; }

    namespace _misc {
        template <bool is_const>
        struct border_ref_ {
            using ptrT = std::conditional_t<is_const, const cellT*, cellT*>;

            vecT size; // For this size.
            ptrT data; // [x][2]*(y+2)[x]

            operator border_ref_<true>() const
                requires(!is_const)
            {
                return std::bit_cast<border_ref_<true>>(*this);
            }

            ptrT up_line() const { return data; }
            ptrT down_line() const { return data + size.x + 2 * size.y + 4; }

            void set_lr(int y, cellT l, cellT r) const
                requires(!is_const)
            {
                assert(y >= -1 && y <= size.y);
                cellT* dest = data + size.x + 2 * (y + 1);
                dest[0] = l, dest[1] = r;
            }

            std::pair<cellT, cellT> get_lr(int y) const {
                assert(y >= -1 && y <= size.y);
                const cellT* dest = data + size.x + 2 * (y + 1);
                return {dest[0], dest[1]};
            }

            void collect_from(const tile_const_ref q, const tile_const_ref w, const tile_const_ref e, //
                              const tile_const_ref a, /*        s          */ const tile_const_ref d, //
                              const tile_const_ref z, const tile_const_ref x, const tile_const_ref c) const
                requires(!is_const)
            {
                std::copy_n(w.line(w.size.y - 1), size.x, up_line());
                std::copy_n(x.line(0), size.x, down_line());
                set_lr(-1, q.at(q.size.x - 1, q.size.y - 1), e.at(0, e.size.y - 1));
                set_lr(size.y, z.at(z.size.x - 1, 0), c.at(0, 0));
                for (int y = 0; y < size.y; ++y) {
                    set_lr(y, a.at(a.size.x - 1, y), d.at(0, y));
                }
            }
        };
    } // namespace _misc

    using border_ref = _misc::border_ref_<false /* !is_const */>;
    using border_const_ref = _misc::border_ref_<true /* is_const */>;

    // `dest` and `source` may either refer to the same area, or completely non-overlapping.
    inline void apply_rule(const rule_like auto& rule, const tile_ref dest, const tile_const_ref source,
                           const border_const_ref source_border) {
        // (Relying on current coding scheme.)
        static_assert(codeT::bpos_q == 8 && codeT::bpos_w == 7 && codeT::bpos_e == 6 && //
                      codeT::bpos_a == 5 && codeT::bpos_s == 4 && codeT::bpos_d == 3 && //
                      codeT::bpos_z == 2 && codeT::bpos_x == 1 && codeT::bpos_c == 0);
        // ~ "arbitrary-signed-int << bits" is made well-defined in C++20.
        static_assert((INT_MIN << 1) == 0);

        assert(source.size == dest.size);
        assert(source.size == source_border.size);
        const vecT size = source.size;

        const auto vec_p6_data = std::make_unique_for_overwrite<uint8_t[]>(size.x);
        uint8_t* const vec_p6 = vec_p6_data.get(); // 0b..qweasd
        {
            const cellT *const up = source_border.up_line(), *cn = source.line(0);
            const auto [up_l, up_r] = source_border.get_lr(-1);
            const auto [cn_l, cn_r] = source_border.get_lr(0);
            int p3_up = (up_l << 1) | up[0]; // 0b...qwe (up[x-1][x][x+1])
            int p3_cn = (cn_l << 1) | cn[0]; // 0b...asd (cn[x-1][x][x+1])
            for (int x = 0; x < size.x - 1; ++x) {
                p3_up = (p3_up << 1) | up[x + 1];
                p3_cn = (p3_cn << 1) | cn[x + 1];
                vec_p6[x] = (p3_up << 3) | (p3_cn & 0b111);
            }
            p3_up = (p3_up << 1) | up_r;
            p3_cn = (p3_cn << 1) | cn_r;
            vec_p6[size.x - 1] = (p3_up << 3) | (p3_cn & 0b111);
        }

        for (int y = 0; y < size.y; ++y) {
            cellT* const dest_ = dest.line(y);
            const cellT* const dw = y == size.y - 1 ? source_border.down_line() : source.line(y + 1);
            const auto [dw_l, dw_r] = source_border.get_lr(y + 1);
            int p3_dw = (dw_l << 1) | dw[0]; // 0b...zxc (dw[x-1][x][x+1])
            for (int x = 0; x < size.x - 1; ++x) {
                p3_dw = (p3_dw << 1) | dw[x + 1];
                const int code_raw = (vec_p6[x] << 3) | (p3_dw & 0b111);
                dest_[x] = rule(codeT(code_raw & 0b111'111'111));
                vec_p6[x] = code_raw;
            }
            p3_dw = (p3_dw << 1) | dw_r;
            const int code_raw = (vec_p6[size.x - 1] << 3) | (p3_dw & 0b111);
            dest_[size.x - 1] = rule(codeT(code_raw & 0b111'111'111));
            vec_p6[size.x - 1] = code_raw;
        }
    }

    inline void apply_rule_torus(const rule_like auto& rule, const tile_ref dest, const tile_const_ref source) {
        assert(source.size == dest.size);
        const vecT size = source.size;

        const auto border_data = std::make_unique_for_overwrite<cellT[]>(calc_border_size(size));
        const border_ref border{.size = size, .data = border_data.get()};
        border.collect_from(source, source, source, source, source, source, source, source);
        apply_rule(rule, dest, source, border);
    }

    inline void apply_rule_torus(const rule_like auto& rule, const tile_ref tile) { //
        apply_rule_torus(rule, tile, tile);
    }

    inline void fake_apply(const tile_const_ref tile, lockT& lock) {
        if (tile.size.x <= 2 || tile.size.y <= 2) {
            return;
        }

        for (int y = 1; y < tile.size.y - 1; ++y) {
            const cellT* const up = tile.line(y - 1);
            const cellT* const cn = tile.line(y);
            const cellT* const dw = tile.line(y + 1);
            for (int x = 1; x < tile.size.x - 1; ++x) {
                lock[encode({
                    up[x - 1], up[x], up[x + 1], //
                    cn[x - 1], cn[x], cn[x + 1], //
                    dw[x - 1], dw[x], dw[x + 1], //
                })] = true;
            }
        }
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_apply_1 = [] {
            const ruleT rule = make_rule([](codeT) { return cellT(testT::rand() & 1); });

            for_each_code([&](codeT code) {
                const auto make_ref = [](cellT& c) { return tile_ref{&c, {1, 1}}; };
                auto [q, w, e, a, s, d, z, x, c] = decode(code);

                cellT dest{}, border_data[calc_border_size({1, 1})]{};
                const border_ref border{.size = {1, 1}, .data = border_data};
                border.collect_from(make_ref(q), make_ref(w), make_ref(e), make_ref(a), make_ref(d), make_ref(z),
                                    make_ref(x), make_ref(c));

                apply_rule(rule, make_ref(dest), make_ref(s), border);
                assert(dest == rule[code]);
            });
        };

        inline const testT test_apply_2 = [] {
            const ruleT copy_q = make_rule([](codeT c) { return c.get(codeT::bpos_q); });

            std::array<cellT, 120> data[2]{};
            const tile_ref tile{&data[0][0], {.x = 10, .y = 12}};
            const tile_ref compare{&data[1][0], {.x = 10, .y = 12}};
            random_fill(tile, testT::rand, 0.5);

            for (int i = 0; i < 12; ++i) {
                rotate_copy_00_to(compare, tile, {1, 1});
                apply_rule_torus(copy_q, tile, tile);
                assert(data[0] == data[1]);
            }
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    class tileT {
        vecT m_size;
        cellT* m_data; // [x]*y

    public:
        bool empty() const {
            assert((m_size.x == 0 && m_size.y == 0 && !m_data) || (m_size.x > 0 && m_size.y > 0 && m_data));
            return m_size.x == 0;
        }

        tileT() : m_size{}, m_data{} {}

        void swap(tileT& other) noexcept {
            std::swap(m_size, other.m_size);
            std::swap(m_data, other.m_data);
        }
        tileT(tileT&& other) noexcept : m_size{}, m_data{} { swap(other); }
        tileT& operator=(tileT&& other) noexcept {
            swap(other);
            return *this;
        }

        explicit tileT(const vecT size) : m_size{size}, m_data{} {
            assert((size.x == 0 && size.y == 0) || (size.x > 0 && size.y > 0));
            if (m_size.x > 0) {
                m_data = new cellT[m_size.xy()]{};
            }
        }

        ~tileT() { delete[] m_data; }

        explicit tileT(const tile_const_ref tile) : m_size{tile.size}, m_data{} {
            assert(tile.size.x > 0 && tile.size.y > 0);
            m_data = new cellT[m_size.xy()];
            copy(data(), tile);
        }

        tileT(const tileT& other) : m_size{other.m_size}, m_data{} {
            if (!other.empty()) {
                m_data = new cellT[m_size.xy()];
                std::copy_n(other.m_data, m_size.xy(), m_data);
            }
        }
        tileT& operator=(const tileT&) = delete; // -> `= tileT(other)`

        void resize(const vecT size) {
            if (m_size != size) {
                tileT(size).swap(*this);
            }
        }

        vecT size() const { return m_size; }

        tile_ref data() {
            assert(!empty());
            return {m_data, m_size}; // continuous
        }
        tile_const_ref data() const {
            assert(!empty());
            return {m_data, m_size}; // continuous
        }

        void run_torus(const rule_like auto& rule) { //
            apply_rule_torus(rule, data());
        }

        friend bool operator==(const tileT& a, const tileT& b) {
            if (a.m_size != b.m_size) {
                return false;
            } else if (a.empty()) {
                assert(b.empty());
                return true;
            } else {
                return std::equal(a.m_data, a.m_data + a.m_size.xy(), b.m_data);
            }
        }
    };

} // namespace aniso
