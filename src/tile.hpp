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

    // (Only for describing precondition.)
    inline bool non_overlapping(const tile_const_ref, const tile_const_ref) { return true; }
    inline bool non_overlapping_or_same_area(const tile_const_ref, const tile_const_ref) { return true; }

    inline int count(const tile_const_ref tile) {
        int c = 0;
        tile.for_all_data([&c](std::span<const cellT> line) {
            for (const cellT v : line) {
                c += v;
            }
        });
        return c;
    }

    // (Typically small enough.)
    struct backgroundT : tile_const_ref {};

    inline bool is_pure_0(const backgroundT background) { //
        return *background.data == cellT{0} && (background.size == vecT{1, 1} || !count(background));
    }

    inline bool is_pure(const backgroundT background) {
        if (background.size == vecT{1, 1}) {
            return true;
        }
        const int c = count(background);
        return c == 0 || c == background.size.xy();
    }

    inline void copy(const tile_ref dest, const tile_const_ref source) {
        assert(non_overlapping(dest, source));
        for_all_data(dest, source, [](cellT* d, const cellT* s, int len) { //
            std::copy_n(s, len, d);
        });
    }

    inline void copy_common(const tile_ref dest, const tile_const_ref source) {
        const vecT common = min(dest.size, source.size);
        copy(dest.clip_corner(common), source.clip_corner(common));
    }

    inline void self_repeat(const tile_ref tile, const vecT period) {
        assert(period.both_gteq({1, 1}));
        if (period.x < tile.size.x) {
            for (int y = 0; y < std::min(tile.size.y, period.y); ++y) {
                cellT* const ln = tile.line(y);
                for (int i = period.x; i < tile.size.x; ++i) {
                    ln[i] = ln[i - period.x];
                }
            }
        }
        for (int y = period.y; y < tile.size.y; ++y) {
            std::copy_n(tile.line(y - period.y), tile.size.x, tile.line(y));
        }
    }

    enum class blitE { Copy, Or, And, Xor };
    template <blitE mode>
    inline void blit(const tile_ref dest, const tile_const_ref source) {
        assert(non_overlapping(dest, source));
        if constexpr (mode == blitE::Copy) {
            copy(dest, source);
        } else {
            for_all_data(dest, source, [](cellT* d, const cellT* s, int len) {
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

    inline void blit(const tile_ref dest, const tile_const_ref source, const blitE mode) {
        switch (mode) {
            case blitE::Copy: blit<blitE::Copy>(dest, source); return;
            case blitE::Or: blit<blitE::Or>(dest, source); return;
            case blitE::And: blit<blitE::And>(dest, source); return;
            // case blitE::Xor: blit<blitE::Xor>(dest, source); return;
            // (Not used in the program.)
            default: assert(false);
        }
    }

    inline void flip(const tile_ref tile) {
        tile.for_all_data([&](std::span<cellT> line) {
            for (cellT& cell : line) {
                cell = !cell;
            }
        });
    }

    inline void fill(const tile_ref tile, const cellT v) {
        tile.for_all_data([v](std::span<cellT> line) { //
            std::ranges::fill(line, v);
        });
    }

    // (`background.at(0, 0)` is bound to `tile.at(0, 0)`.)
    inline void fill(const tile_ref tile, const backgroundT background) {
        assert(non_overlapping(tile, background));
        if (is_pure(background)) {
            fill(tile, *background.data);
        } else {
            copy_common(tile, background);
            self_repeat(tile, background.size);
        }
    }

    // The result will be completely dependent on the state of `rand` and `density`.
    // (`bernoulli_distribution` cannot guarantee this.)
    inline void random_fill(const tile_ref tile, std::mt19937& rand, const double density) {
        constexpr uint32_t max = std::mt19937::max();
        const uint32_t c = max * std::clamp(density, 0.0, 1.0);
        if (c == 0 || c == max) {
            fill(tile, cellT(c == 0 ? 0 : 1));
        } else {
            tile.for_all_data([&](std::span<cellT> line) { //
                std::ranges::generate(line, [&] { return cellT(rand() < c ? 1 : 0); });
            });
        }
    }

    inline void random_flip(const tile_ref tile, std::mt19937& rand, const double density) {
        constexpr uint32_t max = std::mt19937::max();
        const uint32_t c = max * std::clamp(density, 0.0, 1.0);
        if (c == 0) { // Noop
        } else if (c == max) {
            flip(tile);
        } else {
            tile.for_all_data([&](std::span<cellT> line) {
                for (cellT& cell : line) {
                    cell = cell ^ (rand() < c);
                }
            });
        }
    }

    namespace _misc {
        inline void for_each_line(const backgroundT background, const vecT size, const auto& fn) {
            const vecT matching_size{.x = size.x, .y = std::min(background.size.y, size.y)};
            const auto matching_bg = std::make_unique_for_overwrite<cellT[]>(matching_size.xy());
            fill({matching_bg.get(), matching_size}, background);
            const cellT *const begin = matching_bg.get(), *const end = begin + matching_size.xy();

            const cellT* p = begin;
            for (int y = 0; y < size.y; ++y, p += size.x) {
                if (p == end) {
                    p = begin;
                }
                if constexpr (requires { fn((int)y, std::span(p, p + size.x)); }) {
                    fn((int)y, std::span(p, p + size.x));
                } else { // `fn` knows size.x.
                    static_assert(requires { fn((int)y, (const cellT*)p); });
                    fn((int)y, (const cellT*)p);
                }
            }
        }
    } // namespace _misc

    // (`background.at(0, 0)` is bound to `tile.at(0, 0)`.)
    inline int count_diff(const tile_const_ref tile, const backgroundT background) {
        if (is_pure(background)) {
            const int c = count(tile);
            return *background.data == cellT{0} ? c : tile.size.xy() - c;
        }

        int c = 0;
        _misc::for_each_line(background, tile.size, [&](const int y, const cellT* const bg) {
            const cellT* const line = tile.line(y);
            for (int x = 0; x < tile.size.x; ++x) {
                c += bg[x] != line[x];
            }
        });
        return c;
    }

    // (`background.at(0, 0)` is bound to `source.at(0, 0)`.)
    inline void copy_diff(const tile_ref dest, const tile_const_ref source, const backgroundT background,
                          const bool optimize = true) {
        assert(non_overlapping(dest, source) && non_overlapping(dest, background));
        assert(dest.size == source.size);

        if (optimize && is_pure(background)) {
            if (*background.data == cellT{0}) {
                blit<blitE::Or>(dest, source);
            } else {
                blit<blitE::And>(dest, source);
            }
            return;
        }

        _misc::for_each_line(background, source.size, [&](const int y, const cellT* const bg) {
            const cellT* const s = source.line(y);
            cellT* const d = dest.line(y);
            for (int x = 0; x < source.size.x; ++x) {
                if (bg[x] != s[x]) {
                    d[x] = s[x];
                }
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

    // (`background.at(0, 0)` is bound to `tile.at(0, 0)` instead of `range.begin`.)
    inline void fill_outside(const tile_ref tile, const rangeT& range, const backgroundT background) {
        if (is_pure(background)) {
            fill_outside(tile, range, *background.data);
            return;
        }

        assert(tile.contains(range));
        assert(non_overlapping(tile, background));

        _misc::for_each_line(background, tile.size, [&](const int y, std::span<const cellT> bg) {
            cellT* const line = tile.line(y);
            if (y < range.begin.y || y >= range.end.y) {
                std::ranges::copy(bg, line);
            } else {
                std::ranges::copy(bg.first(range.begin.x), line);
                std::ranges::copy(bg.subspan(range.end.x), line + range.end.x);
            }
        });
    }

    inline rangeT bounding_box(const tile_const_ref tile, const cellT background) {
        int min_x = INT_MAX, max_x = INT_MIN;
        int min_y = INT_MAX, max_y = INT_MIN;
        tile.for_each_line([&](const int y, const cellT* const line) {
            for (int x = 0; x < tile.size.x; ++x) {
                if (line[x] != background) {
                    min_x = std::min(min_x, x);
                    max_x = std::max(max_x, x);
                    min_y = std::min(min_y, y);
                    max_y = std::max(max_y, y);
                }
            }
        });
        if (max_x != INT_MIN) {
            return {.begin{.x = min_x, .y = min_y}, .end{.x = max_x + 1, .y = max_y + 1}};
        } else {
            return {};
        }
    }

    // (`background.at(0, 0)` is bound to `tile.at(0, 0)`.)
    inline rangeT bounding_box(const tile_const_ref tile, const backgroundT background) {
        if (is_pure(background)) {
            return bounding_box(tile, *background.data);
        }

        int min_x = INT_MAX, max_x = INT_MIN;
        int min_y = INT_MAX, max_y = INT_MIN;
        _misc::for_each_line(background, tile.size, [&](const int y, const cellT* const bg) {
            const cellT* const line = tile.line(y);
            for (int x = 0; x < tile.size.x; ++x) {
                if (line[x] != bg[x]) {
                    min_x = std::min(min_x, x);
                    max_x = std::max(max_x, x);
                    min_y = std::min(min_y, y);
                    max_y = std::max(max_y, y);
                }
            }
        });
        if (max_x != INT_MIN) {
            return {.begin{.x = min_x, .y = min_y}, .end{.x = max_x + 1, .y = max_y + 1}};
        } else {
            return {};
        }
    }

    // Treat corner as background.
    inline rangeT bounding_box(const tile_const_ref tile, const vecT period) { //
        return bounding_box(tile, backgroundT{tile.clip_corner(min(period, tile.size))});
    }

    namespace _misc {
        inline bool has_period_x(const tile_const_ref tile, const int p) {
            assert(0 < p && p <= tile.size.x);
            for (int y = 0; y < tile.size.y; ++y) {
                const cellT* ln = tile.line(y);
                if (!equal(ln, ln + p, tile.size.x - p)) { // [i] == [i+p]
                    return false;
                }
            }
            return true;
        }

        inline bool has_period_y(const tile_const_ref tile, const int p) {
            assert(0 < p && p <= tile.size.y);
            for (int y = p; y < tile.size.y; ++y) {
                if (!equal(tile.line(y), tile.line(y - p), tile.size.x)) {
                    return false;
                }
            }
            return true;
        }
    } // namespace _misc

    inline bool has_enclosing_period(const tile_const_ref tile, const vecT period) {
        if (!tile.size.both_gteq(period * 2)) {
            return false;
        }
        return _misc::has_period_x(tile.top(period.y), period.x) &&    //
               _misc::has_period_x(tile.bottom(period.y), period.x) && //
               _misc::has_period_y(tile.left(period.x), period.y) &&   //
               _misc::has_period_y(tile.right(period.x), period.y);
    }

    inline std::optional<vecT> spatial_period_full_area(const tile_const_ref tile, const vecT max_period) {
        const vecT limit = min(tile.size / 2, max_period);
        int period_x = 0;
        for (int x = 1; x <= limit.x; ++x) {
            if (_misc::has_period_x(tile, x)) {
                period_x = x;
                break;
            }
        }
        if (period_x) {
            for (int y = 1; y <= limit.y; ++y) {
                if (_misc::has_period_y(tile, y)) {
                    return vecT{.x = period_x, .y = y};
                }
            }
        }
        return std::nullopt;
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        // (Only for tests.)
        template <int x, int y>
        struct fixed_tile {
            static constexpr vecT size{.x = x, .y = y};
            cellT data[x * y]{};

            friend bool operator==(const fixed_tile&, const fixed_tile&) = default;

            /*implicit*/ operator tile_ref() { return {data, size}; }
            /*implicit*/ operator tile_const_ref() const { return {data, size}; }
        };

        inline const testT test_periodic_functions = [] {
            const vecT padding_a{.x = int(testT::rand() % 5), .y = int(testT::rand() % 5)};
            const vecT padding_b{.x = int(testT::rand() % 5), .y = int(testT::rand() % 5)};
            const vecT inner_size{10, 10};
            const vecT size = padding_a + inner_size + padding_b;
            const auto tile_data = std::make_unique_for_overwrite<cellT[]>(size.xy());
            const tile_ref tile{tile_data.get(), size};
            const rangeT inner_range{padding_a, padding_a + inner_size};
            fill(tile.clip(inner_range), {0});
            const fixed_tile<2, 2> period{{{1}, {0}, {0}, {1}}}; // Checkerboard.
            fill_outside(tile, inner_range, backgroundT{period});
            const rangeT test_range = bounding_box(tile, backgroundT{period});
            assert(!test_range.empty());
            assert(test_range.begin == inner_range.begin && test_range.end == inner_range.end);
        };

        inline const testT test_periodic_functions_2 = [] {
            fixed_tile<7, 7> tile{};
            const fixed_tile<2, 2> period{{{0}, {0}, {0}, {1}}}; // 0 0
            fill(tile, backgroundT{period});                     // 0 1
            assert((spatial_period_full_area(tile, tile.size) == vecT{2, 2}));
            // Note that for this case the expected period (2,2) != smallest-enclosing-period (1,1)...
            assert(has_enclosing_period(tile, {2, 2}) && has_enclosing_period(tile, {1, 1}));
        };

        inline const testT test_periodic_functions_3 = [] {
            fixed_tile<12, 10> dest{}, source{};
            random_fill(dest, testT::rand, 0.5);
            random_fill(source, testT::rand, 0.5);

            const fixed_tile<2, 2> all_0{{{0}, {0}, {0}, {0}}};
            const fixed_tile<2, 2> all_1{{{1}, {1}, {1}, {1}}};
            for (const auto& bg : {all_0, all_1}) {
                assert(is_pure(backgroundT{bg}));
                auto test_opt = dest, test_xopt = dest;

                copy_diff(test_opt, source, backgroundT{bg}, true);
                copy_diff(test_xopt, source, backgroundT{bg}, false);
                assert(test_opt == test_xopt);
            }
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    // Rotate (0, 0) to (wrap(to.x), wrap(to.y)).
    inline void rotate_copy_00_to(const tile_ref dest, const tile_const_ref source, const vecT to_) {
        assert(non_overlapping(dest, source));
        assert(dest.size == source.size);
        const vecT size = dest.size;

        const auto wrap = [](int v, int r) { return ((v % r) + r) % r; };
        const vecT to = {.x = wrap(to_.x, size.x), .y = wrap(to_.y, size.y)};
        assert(to.both_gteq({0, 0}) && to.both_lt(size));

        if (to.x == 0 && to.y == 0) {
            copy(dest, source);
            return;
        }

        assert_val(const cellT test = source.at(0, 0));
        source.for_each_line([&](const int y, const cellT* const line) {
            cellT* const dest_ = dest.line((y + to.y) % size.y);
            std::copy_n(line, size.x - to.x, dest_ + to.x);
            std::copy_n(line + size.x - to.x, to.x, dest_);
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
            tile.for_each_line([&put, size = tile.size](const int y, const cellT* const line) {
                if (y != 0) {
                    put.append(1, '$');
                }
                const cellT *pos = line, *const end = line + size.x;
                while (pos != end) {
                    const cellT cell = *pos++;
                    int n = 1;
                    while (pos != end && *pos == cell) {
                        ++n;
                        ++pos;
                    }
                    const bool omit = y != 0 && y != size.y - 1 && pos == end && cell == 0;
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
            const auto find_nl = str.find('\n'); // Able to deal with "\r\n".
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

    struct prepareT {
        long long x, y;

        bool empty() const {
            assert((x == 0 && y == 0) || (x > 0 && y > 0));
            return x <= 0 || y <= 0;
        }
    };

    // (The size is calculated from the contents instead of header.)
    inline void from_RLE_str(std::string_view text, const func_ref<std::optional<tile_ref>(prepareT)> prepare) {
        text = strip_RLE_header(text);

        struct takerT {
            const char *str, *end;
            takerT(std::string_view text) : str(text.data()), end(str + text.size()) {}

            // '?' ~ parsing error.
            std::pair<int, char> take() {
                while (str != end && (*str == '\n' || *str == '\r' || *str == ' ')) {
                    ++str;
                }
                if (str == end) {
                    return {1, '?'};
                } else if (*str == '!') {
                    return {1, '!'};
                }

                int n = 1;
                if (*str >= '1' && *str <= '9') {
                    const auto [ptr, ec] = std::from_chars(str, end, n);
                    if (ec != std::errc{} || ptr == end) {
                        return {1, '?'};
                    }
                    assert(n > 0);
                    str = ptr;
                }
                assert(str != end);
                switch (*str++) {
                    case 'b': return {n, 'b'};
                    case 'o': return {n, 'o'};
                    case '$': return {n, '$'};
                    default: return {1, '?'};
                }
            }
        };

        const prepareT p_size = [text] {
            long long x = 0, y = 0, xmax = 0;
            for (takerT taker(text);;) {
                const auto [n, c] = taker.take();
                if (c == '!') {
                    return prepareT{xmax, x == 0 ? y : y + 1};
                } else if (n <= 0 || c == '?') {
                    return prepareT{0, 0};
                } else if (c == 'b' || c == 'o') {
                    x += n;
                    xmax = std::max(xmax, x);
                } else {
                    assert(c == '$');
                    y += n;
                    x = 0;
                }
            }
        }();

        if (const std::optional<tile_ref> tile_opt = prepare(p_size)) {
            assert(!p_size.empty());
            const tile_ref tile = *tile_opt;
            assert(p_size.x == tile.size.x && p_size.y == tile.size.y);
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
                from_RLE_str(to_RLE_str(a, nullptr), [&](const prepareT p_size) {
                    assert(p_size.x == size.x && p_size.y == size.y);
                    return std::optional{b};
                });
                assert(equal(a_data.get(), b_data.get(), size.xy()));
            }
        };

        inline const testT test_RLE_str_2 = [] {
            for (const char* str : {"", "o", "b", "book", "0b!", "-1o!", "!", "b2!", "b1!"}) {
                from_RLE_str(str, [](const prepareT p_size) {
                    assert(p_size.empty());
                    return std::nullopt;
                });
            }
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    namespace _misc {
        class encoding_buf : no_copy {
            // (Relying on the current encoding scheme (0b-qwe'asd'zxc).)
            static_assert(codeT::bpos_q == 8 && codeT::bpos_w == 7 && codeT::bpos_e == 6 && //
                          codeT::bpos_a == 5 && codeT::bpos_s == 4 && codeT::bpos_d == 3 && //
                          codeT::bpos_z == 2 && codeT::bpos_x == 1 && codeT::bpos_c == 0);
            // ("arbitrary-signed-int << bits" is made well-defined in C++20.)
            static_assert((INT_MIN << 1) == 0);

            uint8_t* vec_p6; // 0b..up[x-1][x][x+1]'cn[x-1][x][x+1]
            int len;

        public:
            encoding_buf(const int l) : vec_p6(new uint8_t[l]{}), len(l) {}
            ~encoding_buf() { delete[] vec_p6; }

            // (The two functions can be impl-ed as a single template (class F = int); however, as tested the debug perf will be nasty.)
            void feed(const cellT l, const cellT r, const cellT* const line) {
                const int len_minus_1 = len - 1; // (For better debug-mode codegen.)
                uint8_t* const vec_p6_local = vec_p6;

                int p3 = (l << 1) | line[0];
                for (int x = 0; x < len_minus_1; ++x) {
                    p3 = (p3 << 1) | line[x + 1]; // 0b...[x-1][x][x+1]
                    const int code_raw = (vec_p6_local[x] << 3) | (p3 & 0b111);
                    vec_p6_local[x] = code_raw;
                }
                p3 = (p3 << 1) | r;
                const int code_raw = (vec_p6_local[len_minus_1] << 3) | (p3 & 0b111);
                vec_p6_local[len_minus_1] = code_raw;
            }

            void feed(const cellT l, const cellT r, const cellT* const line, cellT* const dest,
                      const rule_like auto& rule) {
                const int len_minus_1 = len - 1; // (For better debug-mode codegen.)
                uint8_t* const vec_p6_local = vec_p6;

                int p3 = (l << 1) | line[0];
                for (int x = 0; x < len_minus_1; ++x) {
                    p3 = (p3 << 1) | line[x + 1]; // 0b...[x-1][x][x+1]
                    const int code_raw = (vec_p6_local[x] << 3) | (p3 & 0b111);
                    vec_p6_local[x] = code_raw;
                    dest[x] = rule(codeT(code_raw & 0b111'111'111));
                }
                p3 = (p3 << 1) | r;
                const int code_raw = (vec_p6_local[len_minus_1] << 3) | (p3 & 0b111);
                vec_p6_local[len_minus_1] = code_raw;
                dest[len_minus_1] = rule(codeT(code_raw & 0b111'111'111));
            }
        };
    } // namespace _misc

    inline void apply_rule_torus(const rule_like auto& rule, const tile_ref tile) {
        const vecT size = tile.size;
        const auto first_line = std::make_unique_for_overwrite<cellT[]>(size.x);
        std::copy_n(tile.line(0), size.x, first_line.get());

        _misc::encoding_buf encoder(size.x);
        {
            const cellT* const up = tile.line(size.y - 1);
            encoder.feed(up[size.x - 1], up[0], up);
            const cellT* const cn = tile.line(0);
            encoder.feed(cn[size.x - 1], cn[0], cn);
        }
        for (int y = 0; y < size.y; ++y) {
            const cellT* const dw = y + 1 < size.y ? tile.line(y + 1) : first_line.get();
            encoder.feed(dw[size.x - 1], dw[0], dw, tile.line(y), rule);
        }
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_apply_torus = [] {
            const ruleT copy_q = make_rule([](codeT c) { return c.get(codeT::bpos_q); });

            fixed_tile<10, 12> tile{}, compare{};
            random_fill(tile, testT::rand, 0.5);

            for (int i = 0; i < 12; ++i) {
                rotate_copy_00_to(compare, tile, {1, 1});
                apply_rule_torus(copy_q, tile);
                assert(tile == compare);
            }
        };
    } // namespace _tests
#endif // ENABLE_TESTS

#if 0
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

    inline void apply_rule(const rule_like auto& rule, const tile_ref dest, const tile_const_ref source,
                           const border_const_ref source_border) {
        assert(non_overlapping_or_same_area(source, dest));
        assert(source.size == dest.size);
        assert(source.size == source_border.size);
        const vecT size = source.size;

        _misc::encoding_buf encoder(size.x);
        {
            const auto [up_l, up_r] = source_border.get_lr(-1);
            encoder.feed(up_l, up_r, source_border.up_line());
            const auto [cn_l, cn_r] = source_border.get_lr(0);
            encoder.feed(cn_l, cn_r, source.line(0));
        }
        for (int y = 0; y < size.y; ++y) {
            const cellT* const dw = y == size.y - 1 ? source_border.down_line() : source.line(y + 1);
            const auto [dw_l, dw_r] = source_border.get_lr(y + 1);
            encoder.feed(dw_l, dw_r, dw, dest.line(y), rule);
        }
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

            for (const codeT code : each_code) {
                const auto make_ref = [](cellT& c) { return tile_ref{&c, {1, 1}}; };
                auto [q, w, e, a, s, d, z, x, c] = decode(code);

                cellT dest{}, border_data[calc_border_size({1, 1})]{};
                const border_ref border{.size = {1, 1}, .data = border_data};
                border.collect_from(make_ref(q), make_ref(w), make_ref(e), make_ref(a), make_ref(d), make_ref(z),
                                    make_ref(x), make_ref(c));

                apply_rule(rule, make_ref(dest), make_ref(s), border);
                assert(dest == rule[code]);
            }
        };
    } // namespace _tests
#endif // ENABLE_TESTS
#endif

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

        // Not value-preserving.
        bool resize(const vecT size) {
            if (m_size != size) {
                tileT(size).swap(*this);
                return true;
            }
            return false;
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
                return equal(a.m_data, b.m_data, a.m_size.xy());
            }
        }
    };

    inline std::optional<int> torus_period(const ruleT& rule, const tile_const_ref init, const int max_period) {
        tileT torus(init);
        for (int g = 1; g <= max_period; ++g) {
            torus.run_torus(rule);
            if (equal(init, torus.data())) {
                return g;
            }
        }
        return std::nullopt;
    }

    // Always non-empty.
    // template <int capacity_ = 16>
    class tile_buf {
        static constexpr int capacity_ = 16;
        vecT m_size;
        cellT m_data[capacity_]; // data outside of m_size remain all-0.

    public:
        explicit tile_buf(const vecT size) : m_size{size}, m_data{} { //
            assert(size.both_gt({0, 0}) && size.xy() <= capacity_);
        }
        /*implicit*/ constexpr tile_buf(const cellT c) : m_size{1, 1}, m_data{} { m_data[0] = c; }
        /*implicit*/ tile_buf(const tile_const_ref tile) : tile_buf{tile.size} { copy(data(), tile); }

        // (`tile_ref -> tile_const_ref -> tile_buf` cannot work.)
        // https://stackoverflow.com/questions/17463324/nested-implicit-conversion-in-c
        /*implicit*/ tile_buf(const tile_ref tile) : tile_buf{tile.size} { copy(data(), tile); }

        tile_buf(const tile_buf&) = default;
        tile_buf& operator=(const tile_buf&) = default;
        friend bool operator==(const tile_buf&, const tile_buf&) = default;

        static constexpr int capacity() { return capacity_; }
        vecT size() const { return m_size; }
        tile_ref data() { return {m_data, m_size}; }
        tile_const_ref data() const { return {m_data, m_size}; }

        /*implicit*/ operator backgroundT() const { return {data()}; }

        // Value-preserving.
        void resize(const vecT size) {
            tile_buf resized{size};
            copy_common(resized.data(), data());
            *this = resized;
        }

        uint32_t to_u32() const {
            uint32_t v = 0;
            for (int i = 0, len = m_size.xy(); i < len; ++i) {
                v |= m_data[i] << i;
            }
            return v | (m_size.x << 16) | (m_size.y << 24);
        }
    };

    static_assert(std::is_trivially_copyable_v<tile_buf>);

    class tilesetT {
        std::vector<uint32_t> m_keys;
        bool contains(const uint32_t key) const { //
            return std::ranges::find(m_keys, key) != m_keys.end();
        }

    public:
        bool empty() const { return m_keys.empty(); }
        int size() const { return m_keys.size(); }

        bool contains(const tile_buf& tile) const { //
            return contains(tile.to_u32());
        }

        void insert(const tile_buf& tile) {
            if (const uint32_t key = tile.to_u32(); !contains(key)) {
                m_keys.push_back(key);
            }
        }

        void insert_rotation_group(const tile_buf& tile) {
            const vecT size = tile.size();
            cellT data[tile_buf::capacity() * 4]{};
            tile_ref doubled(data, size * 2);
            fill(doubled, tile);
            for (int y = 0; y < size.y; ++y) {
                for (int x = 0; x < size.x; ++x) {
                    const vecT pos{.x = x, .y = y};
                    insert(doubled.clip({pos, pos + size}));
                }
            }
        }
    };

} // namespace aniso
