#include "utils.hpp"

#ifdef YDEBUG
#include "rule.hpp"
#include "rule_algo.hpp"
#include "tile.hpp"

// Note: `extern` is unnecessary, but without it MSVC says "function can be made static" (false-positive).

namespace aniso::_tests {
    static std::mt19937& get_rand() {
        static std::mt19937 rand{(uint32_t)std::time(0)};
        return rand;
    }

    static ruleT get_rand_rule(std::mt19937& rand = get_rand()) { //
        return ruleT::create([&rand](auto) { return cellT(rand() & 1); });
    }

    static lockT get_rand_lock(std::mt19937& rand = get_rand()) { //
        return lockT::create([&rand](auto) { return bool(rand() & 1); });
    }

    // v "rule.hpp"

    extern void test_codeT() {
        for (const codeT code : each_code) {
            assert(encode(decode(code)) == code);
        }
        assert(for_each_code_all_of([](codeT code) { return encode(decode(code)) == code; }));
    }

    extern void test_MAP_str() {
        {
            const std::string_view str = "...";
            const auto extr = extract_MAP_str(str, true);
            assert(extr.prefix == "..." && extr.suffix == "");
            assert(!extr.has_rule() && !extr.has_lock());
        }

        {
            // https://golly.sourceforge.io/Help/Algorithms/QuickLife.html
            // > So, Conway's Life (B3/S23) encoded as a MAP rule is:
            // > rule = MAPARYXfhZofugWaH7oaIDogBZofuhogOiAaIDogIAAgAAWaH7oaIDogGiA6ICAAIAAaIDogIAAgACAAIAAAAAAAA
            const std::string_view gol_str =
                "MAPARYXfhZofugWaH7oaIDogBZofuhogOiAaIDogIAAgAAWaH7oaIDogGiA6ICAAIAAaIDogIAAgACAAIAAAAAAAA";
            assert(to_MAP_str(game_of_life()) == gol_str);

            const auto extr = extract_MAP_str(gol_str, true);
            assert(extr.prefix == "" && extr.suffix == "");
            assert(extr.get_rule() == game_of_life());
            assert(!extr.has_lock());
        }

        {
            const ruleT rule = get_rand_rule();
            const lockT lock = get_rand_lock();

            const std::string rule_only = "(prefix)" + to_MAP_str(rule) + "(suffix)";
            const std::string with_lock = "(prefix)" + to_MAP_str(rule, &lock) + "(suffix)";

            const auto extr1 = extract_MAP_str(rule_only, true);
            const auto extr2 = extract_MAP_str(with_lock, true);

            assert(extr1.prefix == "(prefix)" && extr2.prefix == "(prefix)");
            assert(extr1.suffix == "(suffix)" && extr2.suffix == "(suffix)");
            assert(extr1.get_rule() == rule);
            assert(extr2.get_rule() == rule);
            assert(!extr1.has_lock());
            assert(extr2.get_lock() == lock);

            assert(extract_all_rules("").empty());
            assert(extract_all_rules(rule_only).size() == 1);
        }
    }

    extern void test_compressT() {
        const ruleT a = get_rand_rule();
        const compressT b = a;
        assert(b == compressT(a));
        assert(b.decompress() == a);
    }

    // v "rule_algo.hpp"

    extern void test_partition_defl() { //
        assert(partitionT{} == partitionT{equivT{}});
    }

    extern void test_mappers() {
        for (const codeT code : each_code) {
            assert(mp_identity(code) == code);

            for (const mapperT* m : {&mp_refl_asd, &mp_refl_wsx, &mp_refl_qsc, &mp_refl_esz, &mp_C2}) {
                assert((*m)((*m)(code)) == code);
            }
            assert(mp_C4(mp_C4(code)) == mp_C2(code));

            assert(mp_reverse(mp_reverse(code)) == code);

            const codeT hex = mp_hex_ignore(code);

            for (const mapperT* m : {&mp_hex_refl_asd, &mp_hex_refl_qsc, &mp_hex_refl_wsx, &mp_hex_refl_aq,
                                     &mp_hex_refl_qw, &mp_hex_refl_wd, &mp_hex_C2}) {
                assert((*m)((*m)(hex)) == hex);
            }
            assert(mp_hex_C6(mp_hex_C6(mp_hex_C6(hex))) == mp_hex_C2(hex));
            assert(mp_hex_C3(mp_hex_C3(mp_hex_C3(hex))) == hex);

            assert(mp_hex_C6(mp_hex_C6(hex)) == mp_hex_C3(hex)); // As both are defined clockwise.
            // Otherwise will need to test:
            // assert(mp_hex_C6(mp_hex_C6(hex)) == mp_hex_C3(mp_hex_C3(hex)));
        }
    }

    extern void test_subset_intersection() {
        const subsetT sc = make_subset({mp_ignore_s}, rule_all_zero()) & make_subset({mp_reverse}, rule_identity());

        // (`maskT` used to refer to the discoverd rule.)
        // 2024/1/20 2AM
        // There is NO problem in the algorithm.
        // It's just that, in this situation the maskT has a strong bias, so that it's too easy to generate
        // rules in a certain direction...
        using enum codeT::bposE;
        assert(sc.contains(ruleT::create([](codeT c) { return c.get(bpos_q); })));
        assert(sc.contains(ruleT::create([](codeT c) { return c.get(bpos_w); })));
        assert(sc.contains(ruleT::create([](codeT c) { return c.get(bpos_e); })));
        assert(sc.contains(ruleT::create([](codeT c) { return c.get(bpos_a); })));
        assert(!sc.contains(rule_identity()));
        assert(sc.contains(ruleT::create([](codeT c) { return c.get(bpos_d); })));
        assert(sc.contains(ruleT::create([](codeT c) { return c.get(bpos_z); })));
        assert(sc.contains(ruleT::create([](codeT c) { return c.get(bpos_x); })));
        assert(sc.contains(ruleT::create([](codeT c) { return c.get(bpos_c); })));
    };

    extern void test_trans_reverse() {
        // TODO: (& symmetric rules) enhance to also test effect on patterns?
        const ruleT rule = get_rand_rule();
        assert(rule == trans_reverse(trans_reverse(rule)));
    };

    extern void test_both_compiles() {
        if /*constexpr*/ (0) {
            const subsetT a{};
            const subsetT_v2 b{};
            (void)distance(a, {}, {});
            (void)distance(b, {}, {});
            (void)approximate(a, {});
            (void)approximate(b, {});
            (void)transform(a, {}, nullptr, [](bool*, bool*) {});
            (void)transform(b, {}, nullptr, [](bool*, bool*) {});
        }
    }

    // v "tile.hpp"

    template <int x, int y>
    struct fixed_tile {
        static constexpr vecT size{.x = x, .y = y};
        cellT data[x * y]{};

        friend bool operator==(const fixed_tile&, const fixed_tile&) = default;

        /*implicit*/ operator tile_ref() { return {data, size}; }
        /*implicit*/ operator tile_const_ref() const { return {data, size}; }
    };

    extern void test_periodic_functions() {
        auto& rand = get_rand();

        {
            const vecT padding_a{.x = int(rand() % 5), .y = int(rand() % 5)};
            const vecT padding_b{.x = int(rand() % 5), .y = int(rand() % 5)};
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
        }

        {
            fixed_tile<7, 7> tile{};
            const fixed_tile<2, 2> period{{{0}, {0}, {0}, {1}}}; // 0 0
            fill(tile, backgroundT{period});                     // 0 1
            assert((spatial_period_full_area(tile, tile.size) == vecT{2, 2}));
            // Note that for this case the expected period (2,2) != smallest-enclosing-period (1,1)...
            assert(has_enclosing_period(tile, {2, 2}) && has_enclosing_period(tile, {1, 1}));
        }

        {
            fixed_tile<12, 10> dest{}, source{};
            random_fill(dest, rand, 0.5);
            random_fill(source, rand, 0.5);

            const fixed_tile<2, 2> all_0{{{0}, {0}, {0}, {0}}};
            const fixed_tile<2, 2> all_1{{{1}, {1}, {1}, {1}}};
            for (const auto& bg : {all_0, all_1}) {
                assert(is_pure(backgroundT{bg}));
                auto test_opt = dest, test_xopt = dest;

                copy_diff(test_opt, source, backgroundT{bg}, true);
                copy_diff(test_xopt, source, backgroundT{bg}, false);
                assert(test_opt == test_xopt);
            }
        }
    }

    extern void test_RLE_str() {
        const vecT sizes[]{{.x = 1, .y = 1}, {.x = 10, .y = 1}, {.x = 1, .y = 10}, {.x = 32, .y = 60}};
        for (const vecT size : sizes) {
            const auto a_data = std::make_unique_for_overwrite<cellT[]>(size.xy());
            const auto b_data = std::make_unique_for_overwrite<cellT[]>(size.xy());
            const tile_ref a{a_data.get(), size};
            const tile_ref b{b_data.get(), size};
            random_fill(a, get_rand(), 0.5);
            from_RLE_str(to_RLE_str(a, nullptr), [&](const prepareT p_size) {
                assert(p_size.x == size.x && p_size.y == size.y);
                return std::optional{b};
            });
            assert(equal(a_data.get(), b_data.get(), size.xy()));
        }

        for (const char* str : {"", "o", "b", "book", "0b!", "-1o!", "!", "b2!", "b1!", "10$!"}) {
            assert(!is_RLE_str(str));
        }
    }

    extern void test_apply_torus() {
        const ruleT copy_q = ruleT::create([](codeT c) { return c.get(codeT::bpos_q); });

        fixed_tile<10, 12> tile{}, compare{};
        random_fill(tile, get_rand(), 0.5);

        for (int i = 0; i < 12; ++i) {
            rotate_copy_00_to(compare, tile, {1, 1});
            apply_rule_torus(tile, copy_q);
            assert(tile == compare);
        }
    };
} // namespace aniso::_tests
#endif // YDEBUG
