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
            assert(to_MAP_str(game_of_life) == gol_str);

            const auto extr = extract_MAP_str(gol_str, true);
            assert(extr.prefix == "" && extr.suffix == "");
            assert(extr.get_rule() == game_of_life);
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
        const subsetT sc = make_subset({mp_ignore_s}, rule_all_zero) & make_subset({mp_reverse}, rule_identity);

        // (`maskT` used to refer to the discoverd rule.)
        // 2024/1/20 2AM
        // There is NO problem in the algorithm.
        // It's just that, in this situation the maskT has a strong bias, so that it's too easy to generate
        // rules in a certain direction...
        using enum codeT::bposE;
        assert(sc.contains(create_rule_copy_from(bpos_q)));
        assert(sc.contains(create_rule_copy_from(bpos_w)));
        assert(sc.contains(create_rule_copy_from(bpos_e)));
        assert(sc.contains(create_rule_copy_from(bpos_a)));
        assert(!sc.contains(rule_identity));
        assert(sc.contains(create_rule_copy_from(bpos_d)));
        assert(sc.contains(create_rule_copy_from(bpos_z)));
        assert(sc.contains(create_rule_copy_from(bpos_x)));
        assert(sc.contains(create_rule_copy_from(bpos_c)));
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

    extern void test_periodic_functions() {
        auto& rand = get_rand();
        struct bg_data {
            cellT data[4]{}; // size = {2, 2}
            operator backgroundT() const { return {{data, {2, 2}}}; }
        };

        {
            const vecT padding_a{.x = int(rand() % 5), .y = int(rand() % 5)};
            const vecT padding_b{.x = int(rand() % 5), .y = int(rand() % 5)};
            const vecT inner_size{10, 10};
            const vecT size = padding_a + inner_size + padding_b;
            tileT tile(size);
            const rangeT inner_range{padding_a, padding_a + inner_size};
            // fill(tile.data(inner_range), {0});
            const bg_data bg{{{1}, {0}, {0}, {1}}}; // Checkerboard.
            fill_outside(tile.data(), inner_range, bg);
            const rangeT test_range = bounding_box(tile.data(), bg);
            assert(!test_range.empty() && test_range == inner_range);
        }

        {
            tileT tile({7, 7});
            const bg_data bg{{{0}, {0}, {0}, {1}}}; // 0 0
            fill(tile.data(), bg);                  // 0 1
            assert((spatial_period_full_area(tile.data(), tile.size()) == vecT{2, 2}));
            // Note that for this case the expected period (2,2) != smallest-enclosing-period (1,1)...
            assert(has_enclosing_period(tile.data(), {2, 2}) && has_enclosing_period(tile.data(), {1, 1}));
        }

        {
            tileT dest({12, 10}), source({12, 10});
            random_fill(dest.data(), rand, 0.5);
            random_fill(source.data(), rand, 0.5);

            const bg_data all_0{{{0}, {0}, {0}, {0}}};
            const bg_data all_1{{{1}, {1}, {1}, {1}}};
            for (const auto& bg : {all_0, all_1}) {
                assert(is_pure(bg));
                auto test_opt = dest, test_xopt = dest;

                copy_diff(test_opt.data(), source.data(), bg, true);
                copy_diff(test_xopt.data(), source.data(), bg, false);
                assert(test_opt == test_xopt);
            }
        }
    }

    extern void test_RLE_str() {
        const vecT sizes[]{{.x = 1, .y = 1}, {.x = 10, .y = 1}, {.x = 1, .y = 10}, {.x = 32, .y = 60}};
        for (const vecT size : sizes) {
            tileT a(size), b(size);
            random_fill(a.data(), get_rand(), 0.5);
            from_RLE_str(to_RLE_str(a.data(), nullptr), [&](const prepareT p_size) {
                assert(p_size.x == size.x && p_size.y == size.y);
                return b.data();
            });
            assert(a == b);
        }

        for (const char* str : {"", "o", "b", "book", "0b!", "-1o!", "!", "b2!", "b1!", "10$!"}) {
            assert(!is_RLE_str(str));
        }
    }

    extern void test_apply_torus() {
        const ruleT copy_q = create_rule_copy_from(codeT::bpos_q);

        tileT tile({10, 12}), compare({10, 12});
        random_fill(tile.data(), get_rand(), 0.5);

        for (int i = 0; i < 12; ++i) {
            rotate_copy_00_to(compare.data(), tile.data(), {1, 1});
            tile.run_torus(copy_q);
            assert(tile == compare);
        }
    };
} // namespace aniso::_tests
#endif // YDEBUG
