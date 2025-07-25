#include <deque>

#include "rule_algo.hpp"

#include "common.hpp"

namespace aniso {
    // !!TODO: (v0.9.9) when partialT is supported, add strobing-set (01 10) and the other 3 non-strobing cases (01 11, 00 10, 11 00).
    static const struct : no_copy {
        subsetT ignore_q = make_subset({mp_ignore_q});
        subsetT ignore_w = make_subset({mp_ignore_w});
        subsetT ignore_e = make_subset({mp_ignore_e});
        subsetT ignore_a = make_subset({mp_ignore_a});
        subsetT ignore_s_z = make_subset({mp_ignore_s}, rule_all_zero);
        subsetT ignore_s_i = make_subset({mp_ignore_s}, rule_identity);
        subsetT ignore_d = make_subset({mp_ignore_d});
        subsetT ignore_z = make_subset({mp_ignore_z});
        subsetT ignore_x = make_subset({mp_ignore_x});
        subsetT ignore_c = make_subset({mp_ignore_c});

        subsetT ignore_hex = make_subset({mp_hex_ignore});
        subsetT ignore_jvn = make_subset({mp_jvn_ignore});
        subsetT ignore_wadx = make_subset({mp_ignore_wadx});

        // rule[{0}] == rule[{511}].
        subsetT single_stable_state{.rule = rule_all_zero, .p = [] {
                                        equivT eq{};
                                        eq.add_eq({0}, {511});
                                        return eq;
                                    }()};
        subsetT self_complementary = make_subset({mp_reverse}, rule_identity);

        subsetT native_isotropic = make_subset({mp_refl_wsx, mp_refl_qsc});
        subsetT native_refl_wsx = make_subset({mp_refl_wsx});
        subsetT native_refl_asd = make_subset({mp_refl_asd});
        subsetT native_refl_qsc = make_subset({mp_refl_qsc});
        subsetT native_refl_esz = make_subset({mp_refl_esz});
        subsetT native_C2 = make_subset({mp_C2});
        subsetT native_C4 = make_subset({mp_C4});

        subsetT native_tot_exc_s = make_subset({mp_C8, mp_tot_exc_s});
        subsetT native_tot_inc_s = make_subset({mp_C8, mp_tot_inc_s});

        subsetT hex_isotropic = make_subset({mp_hex_refl_asd, mp_hex_refl_aq});
        subsetT hex_refl_asd = make_subset({mp_hex_refl_asd});
        subsetT hex_refl_qsc = make_subset({mp_hex_refl_qsc});
        subsetT hex_refl_wsx = make_subset({mp_hex_refl_wsx});
        subsetT hex_refl_aq = make_subset({mp_hex_refl_aq});
        subsetT hex_refl_qw = make_subset({mp_hex_refl_qw});
        subsetT hex_refl_wd = make_subset({mp_hex_refl_wd});
        subsetT hex_C2 = make_subset({mp_hex_C2});
        subsetT hex_C3 = make_subset({mp_hex_C3});
        subsetT hex_C6 = make_subset({mp_hex_C6});

        subsetT hex_tot_exc_s = make_subset({mp_hex_C6, mp_hex_tot_exc_s});
        subsetT hex_tot_inc_s = make_subset({mp_hex_C6, mp_hex_tot_inc_s});

        subsetT jvn_tot_exc_s = make_subset({mp_jvn_ignore, mp_C4, mp_jvn_tot_exc_s});
        subsetT jvn_tot_inc_s = make_subset({mp_jvn_ignore, mp_C4, mp_jvn_tot_inc_s});

        void test() const {
            assert(ignore_e.includes(ignore_hex));
            assert(ignore_z.includes(ignore_hex));
            assert(ignore_q.includes(ignore_jvn));
            assert(ignore_e.includes(ignore_jvn));
            assert(ignore_z.includes(ignore_jvn));
            assert(ignore_c.includes(ignore_jvn));
            assert(ignore_w.includes(ignore_wadx));
            assert(ignore_a.includes(ignore_wadx));
            assert(ignore_d.includes(ignore_wadx));
            assert(ignore_x.includes(ignore_wadx));

            assert(native_C2.includes(native_C4));
            for (const subsetT* set :
                 {&native_refl_wsx, &native_refl_asd, &native_refl_qsc, &native_refl_esz, &native_C2, &native_C4}) {
                assert(set->includes(native_isotropic));
            }
            assert(native_isotropic.includes(native_tot_exc_s));
            assert(native_tot_exc_s.includes(native_tot_inc_s));

            assert(hex_C2.includes(hex_C6));
            assert(hex_C3.includes(hex_C6));
            for (const subsetT* set : {&hex_refl_asd, &hex_refl_qsc, &hex_refl_wsx, &hex_refl_aq, &hex_refl_qw,
                                       &hex_refl_wd, &hex_C2, &hex_C3, &hex_C6}) {
                assert(ignore_hex.includes(*set));
                assert(set->includes(hex_isotropic));
            }
            assert(hex_isotropic.includes(hex_tot_exc_s));
            assert(hex_tot_exc_s.includes(hex_tot_inc_s));

            assert(native_isotropic->k() == 102);
            assert(hex_isotropic->k() == 26);
            assert((native_isotropic & ignore_jvn)->k() == 12);
            assert((native_isotropic & ignore_wadx)->k() == 12);

            assert(native_tot_exc_s->k() == 9 * 2); // 0...8
            assert(native_tot_inc_s->k() == 10);    // 0...9
            assert(hex_tot_exc_s->k() == 7 * 2);    // 0...6
            assert(hex_tot_inc_s->k() == 8);        // 0...7
            assert(jvn_tot_exc_s->k() == 5 * 2);    // 0...4
            assert(jvn_tot_inc_s->k() == 6);        // 0...5

            const ruleT gol = game_of_life();
            assert(native_isotropic.contains(gol));
            assert(native_tot_exc_s.contains(gol));
            assert(!native_tot_inc_s.contains(gol));
        }
    } subsets;

#ifdef ENABLE_TESTS
    namespace _tests {
        static const testT test_subsets = [] { subsets.test(); };
    } // namespace _tests
#endif // ENABLE_TESTS

} // namespace aniso

// `aniso::trans_reverse` cannot be directly declared and called in other TUs, as "the definition of
// an inline function must be reachable in the translation unit where it is accessed".
// aniso::ruleT rule_algo::trans_reverse(const aniso::ruleT& rule) { //
//     return aniso::trans_reverse(rule);
// }

bool rule_algo::is_hexagonal_rule(const aniso::ruleT& rule) { //
    return aniso::subsets.ignore_hex.contains(rule);
}

static int fit_count(int avail, int size, int spacing) { //
    return std::max(1, (avail + spacing) / (size + spacing));
}

// `subsetT` (and `mapperT` pair) are highly customizable. However, for sanity there is no plan to
// support user-defined subsets in the gui part.
class subset_selector : no_copy {
    aniso::subsetT m_current = aniso::subsetT::universal();

    struct termT {
        const char* const title;
        const aniso::subsetT* const set;
        const char* const desc;

        bool selected = false;
        bool including = false; // set.includes(m_current).
        bool disabled = false;  // m_current & set -> empty.
    };

    using terms_data = std::vector<termT>;
    terms_data m_terms{};

    struct terms_ref {
        int pos, size;

        auto get(terms_data& terms) const { //
            return std::span<termT>(terms.data() + pos, size);
        }
    };
    terms_ref terms_ignore{};
    terms_ref terms_misc{};
    terms_ref terms_native{};
    terms_ref terms_totalistic{};
    terms_ref terms_hex{};

    void update_current() {
        m_current = aniso::subsetT::universal();

        for (const termT& t : m_terms) {
            assert_implies(t.disabled, !t.selected);
            if (t.selected) {
                m_current = m_current & *t.set;
            }
        }

        for (termT& t : m_terms) {
            t.including = t.set->includes(m_current);
            t.disabled = !aniso::has_common(*t.set, m_current);

            assert_implies(t.selected, t.including);
            assert_implies(t.disabled, !t.selected);
        }
    }

public:
    // `sel` should be either nullptr or address of one of members in `aniso::subsets`.
    void select_single(const aniso::subsetT* const sel) {
        assert_implies(sel, std::ranges::find(m_terms, sel, &termT::set) != m_terms.end());

        for (termT& t : m_terms) {
            t.disabled = false; // Will be updated by `update_current`.
            t.selected = t.set == sel;
        }
        update_current();
    }

    explicit subset_selector(const aniso::subsetT* init_sel = nullptr) {
        using aniso::subsets;
        constexpr int reserve_cap = 50;
        m_terms.reserve(reserve_cap);

        struct terms_scope : no_copy {
            terms_data& data;
            terms_ref& ref;
            terms_scope(terms_data& d, terms_ref& r) : data(d), ref(r) { ref.pos = data.size(); }
            ~terms_scope() { ref.size = data.size() - ref.pos; }
        };
        {
            terms_scope scope(m_terms, terms_ignore);
            m_terms.emplace_back(
                "q", &subsets.ignore_q,
                "Rules whose values are independent of 'q'. That is, for any two cases where only 'q' differs, the rule will map center cell to the same value.\n\n"
                "    |0 w e|       |1 w e|\n"
                "rule|a s d| = rule|a s d|\n"
                "    |z x c|       |z x c|\n\n"
                "Therefore, these rules will behave as if the neighborhood does not include 'q'. The same applies to 'w/e/a/d/z/x/c', except for 's'.\n\n"
                "('q/w/e/a/s/d/z/x/c' are named after keys in the 'qwerty' keyboard.)");
            m_terms.emplace_back("w", &subsets.ignore_w, "Independent of 'w'. (See 'q' for details.)");
            m_terms.emplace_back("e", &subsets.ignore_e, "Independent of 'e'. (See 'q' for details.)");
            m_terms.emplace_back("a", &subsets.ignore_a, "Independent of 'a'. (See 'q' for details.)");
            m_terms.emplace_back(
                "s", &subsets.ignore_s_z,
                "For any two cases where only 's' (the center cell itself) differs, the rule will map center cell to the same value.\n\n"
                "    |q w e|       |q w e|\n"
                "rule|a 0 d| = rule|a 1 d|\n"
                "    |z x c|       |z x c|\n\n"
                "So when the surrounding cells are the same, there must be: either s:0->1, s:1->1 or s:0->0, s:1->0.\n\n"
                "(It's not obvious what's special about this set; though this is defined in the same way as other independence sets, it's not suitable to treat this as \"independent of 's'\".)");
            m_terms.emplace_back("d", &subsets.ignore_d, "Independent of 'd'. (See 'q' for details.)");
            m_terms.emplace_back("z", &subsets.ignore_z, "Independent of 'z'. (See 'q' for details.)");
            m_terms.emplace_back("x", &subsets.ignore_x, "Independent of 'x'. (See 'q' for details.)");
            m_terms.emplace_back("c", &subsets.ignore_c, "Independent of 'c'. (See 'q' for details.)");
        }
        {
            terms_scope scope(m_terms, terms_misc);
            if (0) {
                m_terms.emplace_back(
                    "s(*)", &subsets.ignore_s_i,
                    "Similar to 's' - for any two cases where only 's' differs, the rule will map the center cell to values so that the resulting \"flip-ness\" will be the same. That is:\n\n"
                    "     |q w e|             |q w e|\n"
                    "(rule|a 0 d| = 0) = (rule|a 1 d| = 1)\n"
                    "     |z x c|             |z x c|\n\n"
                    "So when the surrounding cells are the same, there must be: either s:0->0, s:1->1 (no flip in either case) or s:0->1, s:1->0 (flip in both cases).\n\n"
                    "(This is provided for completeness; it's not obvious what's special about this set.)");
            }
            m_terms.emplace_back(
                "Hex", &subsets.ignore_hex,
                "Rules whose values are independent of 'e' and 'z'. As a result, they can emulate range-1 hexagonal neighborhood - for any rule in this set, there exists an actual rule in the hexagonal tiling with the same behavior.\n\n"
                "To be exact, for any pattern, if evolved under such a rule, the dynamics will correspond to a projected version evolved under an actual rule in the hexagonal space. See the last line for illustration.\n\n"
                "(For windows displaying such rules, you can hover and press '6' to see the projected view.)");
            m_terms.emplace_back(
                "Jvn", &subsets.ignore_jvn,
                "Rules whose values are independent of 'q', 'e', 'z' and 'c'. As a result, they can emulate range-1 von Neumann neighborhood directly.\n\n"
                "(This can work naturally with native-symmetry sets.)");
            m_terms.emplace_back("Wadx", &subsets.ignore_wadx,
                                 "Rules whose values are independent of 'w', 'a', 'd' and 'x'.\n\n"
                                 "(This can work naturally with native-symmetry sets.)");
            m_terms.emplace_back(
                "Compl", &subsets.self_complementary,
                "Self-complementary rules. That is, their 0/1 reversal duals are just themselves - for any pattern, [applying such a rule -> flipping all values] has the same effect as [flipping all values -> applying the same rule].\n\n"
                "To achieve this, for any case and its complement, the rule will map center cell to values so that the resulting \"flip-ness\" are the same.\n\n"
                "     |q w e|             |!q!w!e|\n"
                "(rule|a s d| = s) = (rule|!a!s!d| = !s)\n"
                "     |z x c|             |!z!x!c|\n\n"
                "(You can get the 0/1 reversal dual for any rule in the 'Misc' window.)");

            // TODO: define into `subsets`.
            // Bg-xor invariance.
            // (Self-complementary is actually all-1 xor invar.)
            {
                // Checkerboard.
                // (Does {mp_xor_chk_a/b (alone), rule_identity} make any sense?)
                using namespace aniso;
                constexpr mapperT mp_xor_ckbd_a("!qw!e"
                                                "a!sd"
                                                "!zx!c");
                constexpr mapperT mp_xor_ckbd_b("q!we"
                                                "!as!d"
                                                "z!xc");
                static const subsetT ckbd = make_subset({mp_xor_ckbd_a, mp_xor_ckbd_b}, rule_identity);
                m_terms.emplace_back(
                    "Ckbd", &ckbd,
                    "For any pattern, [applying such a rule -> xor with checkerboard bg] (in arbitrary alignment) has the same effect as [xor with checkerboard bg -> applying the same rule].");
            }

            m_terms.emplace_back("Uniq", &subsets.single_stable_state,
                                 "Rules that map all-0 and all-1 cases to the same value.\n\n"
                                 "    |0 0 0|       |1 1 1|\n"
                                 "rule|0 0 0| = rule|1 1 1|\n"
                                 "    |0 0 0|       |1 1 1|");

            if constexpr (0) {
                // Stripe; not quite interesting...
                // ({a+b/c+d, rule_identity} can represent invar in one direction.)
                using namespace aniso;
                constexpr mapperT mp_xor_stp_a("!q!w!e"
                                               "asd"
                                               "!z!x!c");
                constexpr mapperT mp_xor_stp_b("qwe"
                                               "!a!s!d"
                                               "zxc");
                constexpr mapperT mp_xor_stp_c("!qw!e"
                                               "!as!d"
                                               "!zx!c");
                constexpr mapperT mp_xor_stp_d("q!we"
                                               "a!sd"
                                               "z!xc");
                static const subsetT stp =
                    make_subset({mp_xor_stp_a, mp_xor_stp_b, mp_xor_stp_c, mp_xor_stp_d}, rule_identity);
                m_terms.emplace_back("Stp", &stp, "(Debug mode) Stripe-xor invariance.");
            }
        }
        {
            terms_scope scope(m_terms, terms_native);
            m_terms.emplace_back("Iso", &subsets.native_isotropic,
                                 "Isotropic MAP rules, i.e. rules that preserve all symmetries.\n\n"
                                 "(This is equal to the intersection of the following sets in this line.)");
            m_terms.emplace_back(
                "|", &subsets.native_refl_wsx,
                "Rules that preserve reflection symmetry, taking '|' as the axis.\n\n"
                "For any pattern and its leftside-right mirror image, if evolved under such a rule they will remain in mirror.\n\n"
                "For any leftside-right symmetric pattern, if evolved under such a rule it will remain leftside-right symmetric.");
            m_terms.emplace_back("-", &subsets.native_refl_asd, "Similar to '|'; the axis is '-'.");
            m_terms.emplace_back("\\", &subsets.native_refl_qsc, "Similar to '|'; the axis is '\\'.");
            m_terms.emplace_back("/", &subsets.native_refl_esz, "Similar to '|'; the axis is '/'.");
            m_terms.emplace_back("C2", &subsets.native_C2, "Rules that preserve 2-fold rotational symmetry.");
            m_terms.emplace_back("C4", &subsets.native_C4,
                                 "4-fold rotational symmetry. This is a strict subset of 'C2'.");
        }
        {
            terms_scope scope(m_terms, terms_totalistic);
            m_terms.emplace_back(
                "Tot", &subsets.native_tot_exc_s,
                "Outer-totalistic MAP rules, i.e. rules whose values are only dependent on 's' and the sum of other cells. This is a strict subset of isotropic rules ('Iso').\n\n"
                "(This is also known as life-like rules, and is where the B/S notation applies.)");
            m_terms.emplace_back(
                "Tot(+s)", &subsets.native_tot_inc_s,
                "Inner-totalistic MAP rules, i.e. rules whose values are only dependent on the sum of all cells (including 's'). This is a strict subset of outer-totalistic rules ('Tot').");
            m_terms.emplace_back("Hex", &subsets.hex_tot_exc_s, "Outer-totalistic hexagonal rules.");
            m_terms.emplace_back("Hex(+s)", &subsets.hex_tot_inc_s, "Inner-totalistic hexagonal rules.");
            m_terms.emplace_back("Jvn", &subsets.jvn_tot_exc_s, "Outer-totalistic von Neumann rules.");
            m_terms.emplace_back("Jvn(+s)", &subsets.jvn_tot_inc_s, "Inner-totalistic von Neumann rules.");
        }
        {
            // q w -    q w
            // a s d ~ a s d
            // - x c    x c
            terms_scope scope(m_terms, terms_hex);
            m_terms.emplace_back(
                "Iso", &subsets.hex_isotropic,
                "Rules that emulate isotropic hexagonal rules.\n\n"
                "All sets in this line are strict subsets of 'Hex'; for windows displaying such rules, you can hover and press '6' to better view the symmetries in the hexagonal space.\n\n"
                "(Some of these sets behave like subsets of native-symmetry sets; however, they are not conceptually related.)");
            m_terms.emplace_back(
                "a-d", &subsets.hex_refl_asd,
                "Rules that emulate reflection symmetry in the hexagonal tiling, taking the axis from 'a' to 'd' (a-to-d).");
            m_terms.emplace_back("q-c", &subsets.hex_refl_qsc, "Similar to 'a-d'; the axis is q-to-c.");
            m_terms.emplace_back("w-x", &subsets.hex_refl_wsx, "Similar to 'a-d'; the axis is w-to-x.");
            m_terms.emplace_back("a|q", &subsets.hex_refl_aq, "Similar to 'a-d'; the axis is vertical to a-to-q.");
            m_terms.emplace_back("q|w", &subsets.hex_refl_qw, "Similar to 'a-d'; the axis is vertical to q-to-w.");
            m_terms.emplace_back("w|d", &subsets.hex_refl_wd, "Similar to 'a-d'; the axis is vertical to w-to-d.");
            m_terms.emplace_back("C2", &subsets.hex_C2,
                                 "Rules that emulate 2-fold rotational symmetry in the hexagonal tiling.");
            m_terms.emplace_back("C3", &subsets.hex_C3, "3-fold rotational symmetry.");
            m_terms.emplace_back("C6", &subsets.hex_C6,
                                 "6-fold rotational symmetry. This is a strict subset of both 'C2' and 'C3'.");
        }
        assert(m_terms.size() <= reserve_cap);
        assert(std::ranges::all_of(m_terms, [](const termT& t) { //
            return t.title && t.set && t.desc && !t.selected && !t.including && !t.disabled;
        }));

        select_single(init_sel);
    }

    uint64_t rep() const {
        assert(m_terms.size() <= 64);

        uint64_t val = 0;
        for (int i = 0; const termT& t : m_terms) {
            val |= uint64_t(t.selected || t.including) << i;
            ++i;
        }
        return val;
    }

private:
    enum centerE { Selected, Including, Disabled, None };

    // (Follows `ImGui::Dummy` or `ImGui::InvisibleButton`.)
    static void put_term(bool contains_rule, centerE center, char title /* '\0' ~ no title */) {
        const ImU32 cent_col = center == Selected    ? IM_COL32(65, 150, 255, 255) // Roughly _ButtonHovered
                               : center == Including ? IM_COL32(25, 60, 100, 255)  // Roughly _Button
                                                     : IM_COL32_BLACK_TRANS;
        const ImU32 ring_col = contains_rule ? IM_COL32(0, 255, 0, 255)  // Light green
                                             : IM_COL32(0, 100, 0, 255); // Dull green
        ImU32 title_col = IM_COL32_WHITE;
        if (center == Disabled) {
            title_col = IM_COL32_GREY(150, 255);
            if (!title) {
                title = '-';
            }
        }

        imgui_ItemRectFilled(IM_COL32_BLACK);
        if (title && (center == None || center == Disabled)) {
            imgui_ItemStr(title_col, {&title, 1});
        } else {
            imgui_ItemRectFilled(cent_col, ImVec2(4, 4));
        }
        imgui_ItemRect(ring_col);
    }

public:
    // TODO: improve...
    static void about() {
        const auto explain = [sqr_size = square_size()](bool contains_rule, centerE center, std::string_view desc) {
            ImGui::Dummy(sqr_size);
            put_term(contains_rule, center, '\0');
            if constexpr (debug_mode_log_aware) {
                if (GImGui->LogEnabled) {
                    imgui_LogRenderedText(ImGui::GetItemRectMin(), "..");
                }
            }
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            ImGui::AlignTextToFramePadding(); // `Dummy` does not align automatically.
            imgui_Str(": ");
            ImGui::SameLine(0, 0);
            imgui_Str(desc);
        };

        imgui_Str(
            "[S] refers to the intersection of selected sets. A rule belongs to [S] iff it belongs to every selected set.\n\n"
            "1. [S] is guaranteed to be non-empty. If only one set is selected, [S] will be identical to it; if no sets are selected, [S] will be the entire MAP set.\n"
            "2. The program has access to all rules in [S].\n\n"
            "Click a set to toggle selection; hold right button and click to select only that set.");

        explain(false, None, "Not selected.");
        explain(false, Selected, "Selected.");
        explain(false, Including,
                "Not selected, but [S] already belongs to this set. (So [S] behaves as if this is selected too.)");
        explain(
            false, Disabled,
            "Not selectable, as its intersection with [S] will be empty. (This doesn't affect single-set selection.)");
        // explain(true, None, "The rule belongs to this set.");
        // explain(false, None, "The rule does not belong to this set.");
    }

    const aniso::subsetT& get() const { return m_current; }

    void clear() {
        for (termT& t : m_terms) {
            t.selected = false;
        }
        update_current();
    }

    void match(const aniso::ruleT& rule) {
        for (termT& t : m_terms) {
            t.disabled = false; // Will be updated by `update_current`.
            t.selected = t.set->contains(rule);
        }
        update_current();
    }

    struct select_mode {
        const aniso::ruleT* rule = nullptr;
        bool select = false;
        bool tooltip = false;

        bool valid() const { return rule || select; }
    };

    void select(const select_mode mode) {
        assert(mode.valid());
        if (ImGui::BeginTable("Checklists", 2,
                              ImGuiTableFlags_BordersInner | ImGuiTableFlags_SizingFixedFit |
                                  ImGuiTableFlags_NoKeepColumnsVisible)) {
            int id = 0;
            const auto check = [&, r_down = ImGui::IsMouseDown(ImGuiMouseButton_Right)](termT& term,
                                                                                        const bool show_title) {
                const bool contains_rule = mode.rule && term.set->contains(*mode.rule);
                const char title = show_title ? term.title[0] : '\0';
                if (!mode.select) {
                    ImGui::Dummy(square_size());
                    put_term(contains_rule, None, title);
                    return;
                }

                assert_implies(term.disabled, !term.selected);
                const bool selectable = r_down || !term.disabled;

                ImGui::PushID(id++);
                ImGui::BeginDisabled(!selectable);
                if (ImGui::InvisibleButton("##Invisible", square_size())) {
                    if (r_down) {
                        select_single(term.set);
                    } else if (!term.disabled) {
                        term.selected = !term.selected;
                        update_current();
                    }
                }
                ImGui::EndDisabled();
                ImGui::PopID();
                put_term(contains_rule,
                         term.selected    ? Selected
                         : term.including ? Including
                         : term.disabled  ? Disabled
                                          : None,
                         title);
                if (selectable && ImGui::IsItemHovered() && imgui_IsItemOrNoneActive()) {
                    imgui_ItemRectFilled(ImGui::IsItemActive() ? IM_COL32_GREY(255, 55) : IM_COL32_GREY(255, 45));
                }
                if (mode.tooltip) {
                    imgui_ItemTooltip([&] {
                        ImGui::Text("Groups:%d", term.set->p.k());
                        ImGui::Separator();
                        imgui_Str(term.desc);
                    });
                }
            };

            const auto checklist = [&](const std::span<termT> terms) {
                for (bool first = true; termT & t : terms) {
                    if (!std::exchange(first, false)) {
                        ImGui::SameLine();
                    }
                    ImGui::BeginGroup();
                    const float title_w = imgui_CalcTextSize(t.title).x;
                    const float button_w = ImGui::GetFrameHeight();
                    if (title_w < button_w) {
                        imgui_AddCursorPosX((button_w - title_w) / 2);
                    }
                    imgui_Str(t.title);
                    if (button_w < title_w) {
                        imgui_AddCursorPosX((title_w - button_w) / 2);
                    }
                    check(t, false);
                    ImGui::EndGroup();
                }
            };

            const auto put_row = [](const char* l_str, const auto& r_logic) {
                ImGui::TableNextRow();
                ImGui::TableNextColumn();
                imgui_Str(l_str);

                ImGui::TableNextColumn();
                r_logic();
            };

            put_row("Neighborhood\n& misc", [&] {
                ImGui::BeginGroup();
                ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(2, 2));
                const std::span<termT> ignore = terms_ignore.get(m_terms);
                for (int l = 0; l < 3; ++l) {
                    check(ignore[l * 3 + 0], true);
                    ImGui::SameLine();
                    check(ignore[l * 3 + 1], true);
                    ImGui::SameLine();
                    check(ignore[l * 3 + 2], true);
                }
                ImGui::PopStyleVar();
                ImGui::EndGroup();

                ImGui::SameLine();
                checklist(terms_misc.get(m_terms));
            });

            put_row("Native\nsymmetry", [&] { checklist(terms_native.get(m_terms)); });
            put_row("Totalistic", [&] { checklist(terms_totalistic.get(m_terms)); });
            put_row("q w -    q w\n"
                    "a s d ~ a s d\n"
                    "- x c    x c",
                    [&] { checklist(terms_hex.get(m_terms)); });

            ImGui::EndTable();
        }
    }
};

// TODO: show whether belongs to the working set && dist to the observer?
// TODO: share with `edit_rule` (`subset_selector`).
void previewer::_show_belongs(const aniso::ruleT& rule) {
    static subset_selector dummy{};
    dummy.select({.rule = &rule, .select = false, .tooltip = false});
}

static const char* check_contains(const aniso::ruleT& rule, const aniso::subsetT& working_set) {
    return working_set.contains(rule) ? nullptr
                                      : "The rule does not belong to [S].\n\n"
                                        "(To get a similar rule in [S], try using 'Approx' in the 'Misc' window.)";
}

// (Should appear after regular tooltip.)
static const aniso::ruleT* get_deliv(const pass_rule::passT& pass, const aniso::subsetT& working_set) {
    if (!pass.rule) {
        return nullptr;
    } else if (const char* msg = check_contains(*pass.rule, working_set)) {
        if (pass.hov_for_tooltip() && imgui_BeginTooltipFirstOnly()) {
            ImGui::PushTextWrapPos(wrap_len());
            imgui_Str(msg);
            ImGui::PopTextWrapPos();
            ImGui::EndTooltip();
        }
        return nullptr;
    } else {
        return pass.get_deliv();
    }
}

// !!TODO: these tooltips read still problematic...
class rule_selector : no_copy {
    enum tagE { Zero, Identity, Other, None };
    tagE m_tag = Zero;

    aniso::ruleT m_other = aniso::game_of_life(); // (!= 'Zero' or 'Identity'.)

    struct termT {
        tagE tag;
        const char* label;
        const char* desc;
    };
    static constexpr termT terms[3]{
        {Zero, "Zero",
         "The all-0 rule, i.e. the rule that maps cell to 0 in all cases.\n\n"
         "1. For any rule in any case, being same as this rule means the rule maps cell to 0, and being different means the rule maps cell to 1.\n"
         "2. The distance to this rule equals the number of groups that map cells to 1."},

        {Identity, "Identity",
         "The rule that preserves cell's value in all cases.\n\n"
         "For any rule in any case, being same as this rule means the cell will stay unchanged (0->0 or 1->1), and being different means the cell will \"flip\" (0->1 or 1->0)."},

        // !!TODO: unfinished...
        {Other, "Other",
         "Another rule that's neither 'Zero' nor 'Identity'. This is initially the Game of Life rule.\n\n"
         "[S] will be updated when you drag a rule to it, or when [R] changes and no longer contains [S] (in this case, [S] will be updated to an unspecified rule in [S]).\n"
         "If the rule equals 'Zero' or 'Identity' ... will select directly without updating 'Other'; otherwise, will update and select 'Other'...\n"
         "When you select with the radio buttons, 'Other' will not be changed...\n"
         "... (... 'Compl & Total(+s)'.)"},
    };

    const aniso::ruleT& get_rule(tagE tag) const {
        return tag == Zero       ? aniso::rule_all_zero //
               : tag == Identity ? aniso::rule_identity
                                 : m_other;
    }

    void set(const aniso::ruleT& rule) {
        if (rule == aniso::rule_all_zero) {
            m_tag = Zero;
        } else if (rule == aniso::rule_identity) {
            m_tag = Identity;
        } else {
            m_tag = Other;
            m_other = rule;
        }
    }

public:
    // !!TODO: unfinished...
    static void about() {
        imgui_Str(
            "[S] divides all cases into different groups, and any two rules in [S] must have either all-same or all-different values in each group.\n\n"
            "Due to this structure:\n"
            "1. The \"distance\" between any two rules in [S] can be defined as the number of groups where they have different values.\n"
            "2. Any rule in [S] can serve as an \"observer\" to measure relative distance and compare (same or different) with other rules.\n\n"
            "[R] refers to an arbitrary rule in [S] to serve this role. It doesn't affect rule generation.\n\n"
            "1. Being same or different than 'Zero' / 'Identity' has natural interpretations (actual value / flipness).\n"
            "2. 'Other' represents an arbitrary rule that's neither 'Zero' nor 'Identity'...");
    }

    const aniso::ruleT& get() const { return get_rule(m_tag); }

    void sync(const aniso::subsetT& working_set) {
        if (!working_set.contains(get())) {
            set(working_set.rule);
        }
    }

    void select(const aniso::subsetT& working_set) {
        assert(working_set.contains(get()));
        const auto peek_dist = [&](const aniso::ruleT& rule) { // (Should appear after regular tooltip.)
            if (const aniso::ruleT* peek = pass_rule::peek()) {
                if (imgui_IsItemHoveredForTooltip(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem) &&
                    working_set.contains(*peek) && imgui_BeginTooltipFirstOnly()) {
                    const int dist = aniso::distance(working_set, rule, *peek);
                    ImGui::Text("Dist:%d%s", dist, dist == 0 ? " (same rule)" : "");
                    ImGui::EndTooltip();
                    highlight_item();
                }
            }
        };

        tagE highlight = None;
        ImGui::AlignTextToFramePadding();
        imgui_StrWithID("[R]");
        guide_mode::item_tooltip("Drag a rule here to replace.");
        // pass_rule::source(get());
        if (const auto* deliv = get_deliv(pass_rule::dest(), working_set)) {
            set(*deliv);
            messenger::dot();
            highlight = m_tag;
        }
        peek_dist(get());

        ImGui::SameLine(0, 0);
        imgui_Str(" ~");
        for (const auto& term : terms) {
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());

            const aniso::ruleT& rule = get_rule(term.tag);
            const bool belongs = working_set.contains(rule);
            if (!belongs) { // In disabled-style, but won't affect dragging.
                const float disabled_alpha = ImGui::GetStyle().DisabledAlpha;
                const ImU32 disabled_col = ImGui::GetColorU32(ImGuiCol_FrameBg, disabled_alpha);
                for (const auto col : {ImGuiCol_FrameBg, ImGuiCol_FrameBgActive, ImGuiCol_FrameBgHovered}) {
                    ImGui::PushStyleColor(col, disabled_col);
                }
                ImGui::PushStyleColor(ImGuiCol_Text, ImGui::GetColorU32(ImGuiCol_Text, disabled_alpha));
            }
            if (ImGui::RadioButton(term.label, m_tag == term.tag) && belongs) {
                m_tag = term.tag;
            }
            if (highlight == term.tag) {
                highlight_item();
            }
            if (!belongs) {
                ImGui::PopStyleColor(4);
            }

            if (!pass_rule::source(rule)) {
                imgui_ItemTooltip([&] {
                    if (!belongs) {
                        imgui_Str("This rule does not belong to [S].");
                        ImGui::Separator();
                    }
                    imgui_Str(term.desc);
                    previewer::preview(-1, previewer::default_settings, rule);
                });

                if (belongs) {
                    peek_dist(rule);
                }
            }
        }

        // assert(working_set.contains(get()));
    }
};

// 0/1-rev, approx and buffers...
static open_state misc_window(const ImVec2& init_pos, const aniso::subsetT& working_set) {
    bool open = true;
    ImGui::SetNextWindowPos(init_pos, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);

    static previewer::configT config{previewer::default_settings};
    const int group_spacing_x = ImGui::GetStyle().ItemSpacing.x + 3;
    const ImVec2 min_size = [&] {
        const auto& style = ImGui::GetStyle();
        const int min_size_x = config.width() * 2 + group_spacing_x + style.ScrollbarSize;
        const int min_size_y =
            ImGui::GetFrameHeight() + style.ItemSpacing.y * 3 + ImGui::GetTextLineHeight() * 2 + config.height();
        return ImVec2(min_size_x, min_size_y) + style.WindowPadding * 2;
    }();

    ImGui::SetNextWindowSizeConstraints(min_size, {min_size.x + 120, 500});
    ImGui::SetNextWindowSize(min_size, ImGuiCond_FirstUseEver);
    if (auto window = imgui_Window("Misc utils", &open, ImGuiWindowFlags_NoSavedSettings)) {
        static std::optional<aniso::ruleT> rule_01_rev = aniso::trans_reverse(aniso::game_of_life());
        static std::optional<aniso::ruleT> rule_approx;
        static std::optional<aniso::ruleT> rule_temp[6];

        const auto clear_button = [](int& id, std::optional<aniso::ruleT>& rule) {
            ImGui::PushID(id++);
            if (double_click_button_small("Clear") && messenger::dot()) {
                rule.reset();
            }
            ImGui::PopID();
        };
        const auto show_rule = [](int& id, std::optional<aniso::ruleT>& rule) {
            // https://stackoverflow.com/questions/73817020/why-is-there-no-built-in-way-to-get-a-pointer-from-an-stdoptional
            previewer::preview_or_dummy(id++, config, rule ? &*rule : nullptr);
        };

        const bool to_top = ImGui::SmallButton("Top") && messenger::dot();
        ImGui::SameLine();
        config.set("Settings", true /*small*/);

        if (to_top) {
            ImGui::SetNextWindowScroll({0, 0});
        }

        ImGui::Separator();

        // TODO: ?`imgui_FillAvailRect(IM_COL32_GREY(24, 255));`
        ImGui::PushStyleColor(ImGuiCol_ChildBg, IM_COL32_GREY(24, 255));
        if (auto child = imgui_ChildWindow("Page")) {
            int id = 0;

            // ImGui::Separator();
            ImGui::BeginGroup();
            clear_button(id, rule_01_rev);
            ImGui::SameLine();
            imgui_Str("0/1 reversal"); // TODO: add record?
            {
                ImGui::SameLine();
                ImGui::BeginDisabled(!rule_01_rev.has_value());
                if (ImGui::SmallButton("0/1##Rev") && rule_01_rev.has_value()) {
                    rule_01_rev = aniso::trans_reverse(*rule_01_rev);
                    // messenger::dot(); // TODO: whether to show dot? (Will have no effect if the rule is self-compl...)
                }
                ImGui::EndDisabled();
            }
            ImGui::SameLine();
            imgui_StrTooltip(
                "(?)",
                "Drag a rule here to get the 0/1 reversal dual for it. That is, for any pattern, [applying the original rule -> flipping all values] has the same effect as [flipping all values -> applying the dual].\n\n"
                "(If the rule is self-complementary, this will result in the same rule.)");
            show_rule(id, rule_01_rev);
            if (const auto* deliv = pass_rule::dest().get_deliv()) {
                rule_01_rev = aniso::trans_reverse(*deliv);
                messenger::dot();
            }
            ImGui::EndGroup();

            ImGui::SameLine(0, group_spacing_x);
            const float cursor_pos = ImGui::GetCursorPosX();

            if (rule_approx && !working_set.contains(*rule_approx)) {
                rule_approx.reset();
            }
            ImGui::BeginGroup();
            clear_button(id, rule_approx);
            ImGui::SameLine();
            imgui_Str("Approx");
            ImGui::SameLine();
            imgui_StrTooltip("(?)", "Drag a rule here to get a similar rule in [S].\n\n"
                                    "(If the rule already belongs to [S], this will result in the same rule.)");
            show_rule(id, rule_approx);
            if (const auto* deliv = pass_rule::dest().get_deliv()) {
                rule_approx = aniso::approximate_v(working_set, *deliv);
                messenger::dot();
            }
            ImGui::EndGroup();

            for (int i = 0; auto& rule : rule_temp) {
                const int this_i = ++i;
                if (this_i & 1) {
                    ImGui::Spacing(); // ImGui::Separator();
                } else {
                    // (`SameLine(0, group_spacing_x)` doesn't align well when preview windows are too small.)
                    ImGui::SameLine(cursor_pos);
                }

                ImGui::BeginGroup();
                clear_button(id, rule);
                ImGui::SameLine();
                ImGui::Text("Temp %d", this_i);
                if (this_i == 1) {
                    ImGui::SameLine();
                    imgui_StrTooltip("(?)", "Drag a rule here for later use.");
                }
                show_rule(id, rule);
                if (const auto* deliv = pass_rule::dest().get_deliv()) {
                    rule = *deliv;
                    messenger::dot();
                }
                ImGui::EndGroup();
            }
        }
        ImGui::PopStyleColor();
    };
    return {open};
}

struct page_adapter {
    int page_size = 6, perline = 3;
    ImVec2 min_req_size = {};

    bool try_resize(const ImVec2 item_size) {
        if (!ImGui::IsWindowAppearing()) {
            const ImVec2 avail_size = ImGui::GetContentRegionAvail();
            const int spacing_x = ImGui::GetStyle().ItemSpacing.x + 3;
            const int spacing_y = ImGui::GetStyle().ItemSpacing.y;
            const int xc = fit_count(avail_size.x, item_size.x, spacing_x);
            const int yc = fit_count(avail_size.y, item_size.y, spacing_y * 2);
            perline = xc;
            return compare_update(page_size, xc * yc);
        }
        return false;
    }

    void display(const func_ref<void(int)> item) {
        imgui_FillAvailRect(IM_COL32_GREY(24, 255)); // Background

        // (The same value as in `edit_rule`.)
        const int spacing_x = ImGui::GetStyle().ItemSpacing.x + 3;
        for (int j = 0; j < page_size; ++j) {
            if (j % perline != 0) {
                ImGui::SameLine(0, spacing_x);
            } else if (j != 0) {
                ImGui::Spacing(); // ImGui::Separator();
            }

            item(j);

            if (j == 0) {
                // The enclosing window should be able to fully contain at least one item.
                min_req_size = imgui_CalcRequiredWindowSize();
            }
        }
    }

    static constexpr const char* resizing_policy =
        "Resize the window to change page size; double-click window's resize border to fit the page.";
};

class target_rule : no_copy {
    rule_with_rec m_rule{};
    rule_snapshot m_snapshot{};
    bool m_window = false;

    test_appearing m_appearing{};

public:
    operator const aniso::ruleT&() const { return m_rule.get(); }
    const aniso::ruleT& get() const { return m_rule.get(); }
    void set(const aniso::ruleT& rule) { m_rule.set(rule); }

    bool sync(const aniso::subsetT& working_set, const aniso::ruleT& defl) {
        if (!m_rule.assigned() || !working_set.contains(m_rule.get())) {
            // TODO: or require manually specifying a rule?
            assert(working_set.contains(defl));
            m_rule.set(defl);
            return true;
        }
        return false;
    }

    bool display(const char* label, const char* snapshot_title, const previewer::configT& settings,
                 const aniso::subsetT& working_set) {
        if (m_appearing.update()) {
            m_snapshot.clear();
            m_window = false;
        }

        bool updated = false;
        bool opened = false;
        if (!m_window) {
            imgui_StrWithID(label);

            if (!pass_rule::source(m_rule.get())) {
                // ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, ImVec2(0, 0));
                imgui_ItemTooltip([&] { previewer::preview(-1, settings, m_rule.get()); });
                // ImGui::PopStyleVar();

                rclick_popup::popup(ImGui::GetItemID(), [&] {
                    imgui_StrTooltip(
                        "(...)",
                        "[X]/[Y]/[Z] can be arbitrary rules in [S]. When [S] changes and no longer contains them (& initially), they will be reset to an unspecified rule in [S].\n\n" // !!TODO: update...
                        "Drag a rule to the label to replace.\n"
                        "Drag the label to send the rule elsewhere.\n"
                        "Use 'Show in window' to display the rule in a regular window (recommended for 'Random-access').");
                    ImGui::Separator();

                    if (ImGui::Selectable("Show in window")) {
                        opened = true;
                        m_window = true;
                    }

                    selectable_to_take_snapshot("Recent", m_rule.rec(), m_snapshot);
                });
            }
            if (const auto* deliv = get_deliv(pass_rule::dest(), working_set)) {
                m_rule.set(*deliv);
                messenger::dot();
                updated = true;
            }
        } else {
            imgui_StrDisabled(label);
        }

        if (m_window) {
            if (opened) {
                ImGui::SetNextWindowCollapsed(false, ImGuiCond_Always);
                if (ImGui::IsMousePosValid()) {
                    const float h = ImGui::GetFrameHeight();
                    ImGui::SetNextWindowPos(ImGui::GetMousePos() - ImVec2{h * 2, floor(h / 2)}, ImGuiCond_Always);
                }
            }
            if (auto window = imgui_Window(label, &m_window,
                                           ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
                item_to_take_snapshot(ImGui::SmallButton, "Recent", m_rule.rec(), m_snapshot);
                previewer::preview(0, settings, m_rule.get());
                if (const auto* deliv = get_deliv(pass_rule::dest(), working_set)) {
                    m_rule.set(*deliv);
                    messenger::dot();
                    updated = true;
                }
            }
        }

        if (m_snapshot) {
            // (The function is no longer stored; preserved for reference.)
            // https://stackoverflow.com/questions/21443023/capturing-a-reference-by-reference-in-a-c11-lambda
            const auto get = [&]() -> decltype(auto) { return m_rule.get(); };
            const auto set = [&](const aniso::ruleT& r) {
                if (const char* msg = check_contains(r, working_set)) {
                    messenger::set_msg(msg);
                } else {
                    m_rule.set(r);
                    updated = true;
                }
            };
            display_snapshot(snapshot_title, m_snapshot, m_rule.rec(), {{.get = get, .set = set}});
        }

        return updated;
    }
};

// TODO: define as classes...
// TODO: support separate context? e.g. random ~ totalistic & random-access ~ isotropic
static open_state traverse_window(const ImVec2& init_pos, const aniso::subsetT& working_set, const aniso::ruleT& defl) {
    bool open = true;
    ImGui::SetNextWindowPos(init_pos, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);

    static page_adapter adapter{};
    ImGui::SetNextWindowSizeConstraints(adapter.min_req_size, ImVec2(FLT_MAX, FLT_MAX));
    // !!TODO: better title...
    imgui_Window::next_window_titlebar_tooltip = page_adapter::resizing_policy;
    if (auto window =
            imgui_Window("Traverse [S]", &open, ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoScrollbar)) {
        static target_rule orderer{};
        static std::deque<aniso::ruleT> page{};
        static previewer::configT config{previewer::default_settings};
        {
            // TODO: unnecessarily expensive (can be replaced by selector.rep()).
            static aniso::subsetT cmp_set = working_set;
            if (compare_update(cmp_set, working_set)) {
                page.clear();
            }
            if (orderer.sync(working_set, defl)) {
                page.clear();
            }
        }

        enum roleE { First, Last };
        const auto reset_page = [&](const roleE role, const aniso::ruleT rule /*by value*/) {
            const auto fill_next = [&] {
                assert(!page.empty());
                while (page.size() < adapter.page_size) {
                    const aniso::ruleT rule = aniso::flatten::next(working_set, orderer, page.back());
                    if (rule == page.back()) {
                        return false; // Reaches the end of the sequence.
                    }
                    page.push_back(rule);
                }
                return true;
            };
            const auto fill_prev = [&] {
                assert(!page.empty());
                while (page.size() < adapter.page_size) {
                    const aniso::ruleT rule = aniso::flatten::prev(working_set, orderer, page.front());
                    if (rule == page.front()) {
                        return false;
                    }
                    page.push_front(rule);
                }
                return true;
            };

            page.clear();
            page.push_back(rule);
            if (role == First) {
                if (!fill_next()) {
                    fill_prev();
                }
            } else {
                if (!fill_prev()) {
                    fill_next();
                }
            }
        };

        static input_int input_dist{};
        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip(
            "(...)",
            "The seq is able to iterate through all rules in [S] in the following order: firstly [X], then all rules with distance = 1 to it, then 2, 3, ..., up to the largest distance (i.e. the number of groups in [S]).\n\n"
            "The seq will be cleared automatically if [S] or [X] changes.\n\n"
            "The \"dist\" in this window refers to distance to [X]. For example, 'Go to dist' will go to the first rule with specified distance to [X].\n\n"
            "1. If [S] is small enough (i.e. having only a few groups), you can easily traverse all rules, and it doesn't matter which rule serves as [X].\n"
            "(Examples include self-complementary totalistic rules ('Comp' & 'Tot'), inner-totalistic rules ('Tot(+s)'), isotropic von-Neumann rules ('All' & 'Von'), and so on.)\n"
            "2. If [S] is large, this still works (the page is generated on demand; rules outside of the page are never stored), but 'Random'/'Random-access' may be more suitable tools.");
        ImGui::SameLine();
        imgui_Str("Go to dist ~ ");
        ImGui::SameLine(0, 0);
        ImGui::SetNextItemWidth(imgui_CalcButtonSize("Max:0000").x);
        if (const auto dist = input_dist.input(5, "##Seek", std::format("Max:{}", working_set->k()).c_str())) {
            reset_page(First, aniso::flatten::first_d(working_set, orderer, *dist));
        }
        ImGui::SameLine();
        if (orderer.display("[X]", "Recent ([X])", config, working_set)) {
            page.clear();
        }

        switch (sequence::seq("<|", "Prev", "Next", "|>")) {
            case 0: reset_page(First, aniso::flatten::first(working_set, orderer)); break;
            case 1:
                if (page.empty()) {
                    reset_page(First, aniso::flatten::first(working_set, orderer));
                } else {
                    reset_page(Last, aniso::flatten::prev(working_set, orderer, page.front()));
                }
                break;
            case 2:
                if (page.empty()) {
                    reset_page(First, aniso::flatten::first(working_set, orderer));
                } else {
                    reset_page(First, aniso::flatten::next(working_set, orderer, page.back()));
                }
                break;
            case 3: reset_page(Last, aniso::flatten::last(working_set, orderer)); break;
        }

        ImGui::SameLine();
        if (page.empty()) {
            imgui_Str("Dist:N/A");
        } else {
            const int min_dist = aniso::distance(working_set, orderer, page.front());
            const int max_dist = aniso::distance(working_set, orderer, page.back());
            assert(min_dist <= max_dist);
            if (min_dist == max_dist) {
                ImGui::Text("Dist:%d", min_dist);
            } else {
                ImGui::Text("Dist:%d~%d", min_dist, max_dist);
            }
        }
        rclick_popup::popup(imgui_GetItemPosID(), [] {
            if (ImGui::Selectable("Clear") && messenger::dot()) {
                page.clear();
            }
        });

        ImGui::SameLine();
        config.set("Settings");

        ImGui::Separator();

        if (adapter.try_resize(config.size_imvec()) && !page.empty()) {
            reset_page(First, page.front());
        }
        adapter.display([&](const int j) {
            assert(j >= 0);
            previewer::preview_or_dummy(j, config, j < page.size() ? &page[j] : nullptr);
            if (j == 0) {
                if (page.empty()) {
                    guide_mode::item_tooltip("Drag a rule here to go to the place starting with it.");
                }
                if (const auto* deliv = get_deliv(pass_rule::dest(), working_set)) {
                    reset_page(First, *deliv);
                    messenger::dot();
                }
            }
        });
    }
    return {open};
}

static open_state random_rule_window(const ImVec2& init_pos, const aniso::subsetT& working_set,
                                     const aniso::ruleT& defl) {
    bool open = true;
    ImGui::SetNextWindowPos(init_pos, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);

    static page_adapter adapter{};
    ImGui::SetNextWindowSizeConstraints(adapter.min_req_size, ImVec2(FLT_MAX, FLT_MAX));
    imgui_Window::next_window_titlebar_tooltip = page_adapter::resizing_policy;
    if (auto window =
            imgui_Window("Random rules", &open, ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoScrollbar)) {
        static target_rule target{};
        static previewer::configT config{previewer::default_settings};
        target.sync(working_set, defl);

        static bool exact_mode = false;
        static double rate = 0.29;
        const int c_group = working_set->k();
        const int c_free = c_group;           // TODO: temporarily preserved.
        int free_dist = round(rate * c_free); // Intended distance.

        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip(
            "(...)",
            "The seq is able to generate random rules in [S] with specified distance around/exactly to [Y].\n\n"
            "When you are at the last page (or when the page is empty; 'At' ~ 'N/A'), '>>>' will generate new pages of rules; otherwise, '<</>>>' serves to iterate through generated rules. Note that nothing will happen immediately after you update [Y], as [Y] only affects how to generate new rules.\n\n"
            "1. In the default settings ([S] ~ isotropic set; [Y] ~ all-0 rule; distance ~ 'Around' 30), '>>>' can generate random isotropic rules with around 30 groups having '1'.\n"
            "2. To generate random rules close to a certain rule, you can update [Y] and set a low distance.");
        ImGui::SameLine();
        imgui_RadioButton("Around", &exact_mode, false);
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        imgui_RadioButton("Exactly", &exact_mode, true);
        ImGui::SameLine();
        ImGui::SetNextItemWidth(floor(item_width * 0.9));
        if (imgui_StepSliderInt::fn("##Dist", &free_dist, 0, c_free) && c_free != 0) {
            rate = double(free_dist) / c_free;
            assert(round(rate * c_free) == free_dist);
        }
        ImGui::SameLine();
        target.display("[Y]", "Recent ([Y])", config, working_set);

        static std::vector<aniso::compressT> rules{};
        static int page_no = 0;

        const auto calc_page = [&]() -> int { return (rules.size() + adapter.page_size - 1) / adapter.page_size; };
        const auto last_page = [&]() -> int { return rules.empty() ? 0 : calc_page() - 1; };
        assert(0 <= page_no && page_no <= last_page());

        const auto set_last_page = [&] { page_no = last_page(); };
        const auto set_next_page = [&] {
            if (page_no < last_page()) {
                ++page_no;
                return;
            }

            const int count = (rules.size() / adapter.page_size) * adapter.page_size + adapter.page_size - rules.size();
            assert(1 <= count && count <= adapter.page_size);
            static std::mt19937 rand = rand_source::create();
            rand_source::perturb(rand); // Additional entropy.
            for (int i = 0; i < count; ++i) {
                rules.push_back(exact_mode ? aniso::random_rule_c(working_set, target, rand, free_dist)
                                           : aniso::random_rule_p(working_set, target, rand, rate));
            }
            assert((rules.size() % adapter.page_size) == 0);
            page_no = (rules.size() / adapter.page_size) - 1; // == last_page().
        };

        switch (sequence::seq("<|", "<<", ">>>", "|>")) {
            case 0: page_no = 0; break;
            case 1: page_no = std::max(page_no - 1, 0); break;
            case 2: set_next_page(); break;
            case 3: set_last_page(); break;
        }

        ImGui::SameLine();
        if (!rules.empty()) {
            // TODO: will this be confusing when the page is resized?
            ImGui::Text("Pages:%d At:%d", calc_page(), page_no + 1);
        } else {
            ImGui::Text("Pages:%d At:N/A", calc_page());
        }
        rclick_popup::popup(imgui_GetItemPosID(), [] {
            if (ImGui::Selectable("Clear") && messenger::dot()) {
                rules.clear();
                rules.shrink_to_fit();
                page_no = 0;
            }
        });

        ImGui::SameLine();
        config.set("Settings");

        ImGui::Separator();

        // TODO: reconsider page-resized logic (seeking to the last page may still be confusing).
        if (adapter.try_resize(config.size_imvec())) {
            set_last_page();
        }
        adapter.display([&](const int j) {
            const int r = page_no * adapter.page_size + j;
            assert(r >= 0);
            previewer::preview_or_dummy(j, config, r < rules.size() ? &rules[r] : nullptr);
        });
    }
    return {open};
}

// TODO: support random-access in a separate window?
// static open_state random_access_window(const ImVec2& init_pos, const aniso::subsetT& working_set);

void edit_rule(frame_main_token) {
    static test_appearing appearing{};
    appearing.update();

    // Select working set ([S]).
    static subset_selector select_working{&aniso::subsets.native_isotropic};
    {
        static bool collapse = false;
        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip("(...)", subset_selector::about);
        ImGui::SameLine();
        imgui_Str("[S]");
        guide_mode::item_tooltip("Drag a rule here to select all sets containing the rule.");
        if (const auto pass = pass_rule::dest(); pass.rule) {
            if (collapse && pass.hov_for_tooltip() && imgui_BeginTooltipFirstOnly()) {
                select_working.select({.rule = pass.rule, .select = true, .tooltip = false});
                ImGui::EndTooltip();
            }
            if (pass.deliv) {
                select_working.match(*pass.rule);
                messenger::dot();
            }
        }
        // ImGui::SameLine(0, 0);
        // imgui_Str(" ~");

        ImGui::SameLine();
        ImGui::Checkbox("Collapse", &collapse);

        if (!collapse) {
            // (Superseded by ctrl mode.)
            // ImGui::SameLine();
            // if (ImGui::Button("Reset##Sets")) {
            //     select_working.select_single(&aniso::subsets.native_isotropic);
            // }
            ImGui::Separator();
            select_working.select({.rule = pass_rule::peek(), .select = true, .tooltip = true});
        } else {
            ImGui::SameLine();
            menu_like_popup::button("Select");
            menu_like_popup::popup([] { select_working.select({.select = true, .tooltip = true}); });
        }
    }
    const aniso::subsetT& working_set = select_working.get();

    ImGui::Separator();

    // Select observing rule ([R]).
    static rule_selector select_rule;
    {
        select_rule.sync(working_set);

        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip("(...)", rule_selector::about);
        ImGui::SameLine();
        select_rule.select(working_set);
    }
    const aniso::ruleT& observer = select_rule.get();

    ImGui::Separator();

    static bool show_random_access = false;
    // appearing.reset_if_appearing(show_random_access);
    static target_rule target{}; // Random-access
    static previewer::configT config{previewer::default_settings};
    {
        static bool show_misc = false;
        appearing.reset_if_appearing(show_misc);
        ImGui::Checkbox("Misc", &show_misc);
        guide_mode::item_tooltip("0/1 reversal dual, approximation, and temp rules.");
        if (show_misc) {
            const ImVec2 init_pos = ImGui::GetItemRectMax() + ImVec2(30, -100);
            misc_window(init_pos, working_set).reset_if_closed(show_misc);
        }
    }
    ImGui::SameLine();
    {
        static bool show_trav = false;
        appearing.reset_if_appearing(show_trav);
        ImGui::Checkbox("Traverse", &show_trav);
        guide_mode::item_tooltip("Iterate through all rules in [S].\n\n"
                                 "(This is mainly useful for small sets.)");
        if (show_trav) {
            const ImVec2 init_pos = ImGui::GetItemRectMax() + ImVec2(30, -100);
            traverse_window(init_pos, working_set, observer).reset_if_closed(show_trav);
        }
    }
    ImGui::SameLine();
    {
        static bool show_rand = false;
        appearing.reset_if_appearing(show_rand);
        ImGui::Checkbox("Random", &show_rand);
        guide_mode::item_tooltip("Get random rules in [S].");
        if (show_rand) {
            const ImVec2 init_pos = ImGui::GetItemRectMax() + ImVec2(30, -100);
            random_rule_window(init_pos, working_set, observer).reset_if_closed(show_rand);
        }
    }
    ImGui::SameLine();
    {
        ImGui::Checkbox("Random-access", &show_random_access);
        // !!TODO: unfinished...
        guide_mode::item_tooltip(
            "Enable random-access editing, i.e. flipping values of [Z] by groups. When this is turned on, the table will display the flipping result for each group, i.e. all rules with dist = 1 to [Z] (in [S]).\n\n"
            "By clicking a group button, the values of [Z] (in that group) will be flipped (equivalent to dragging the rule to [Z]). The operation effectively swaps [Z] and the rule (this will be obvious if you turn on the window for [Z]), and can be undone by clicking the same button again.\n\n"
            "1. Collapse the set table ('Collapse') to leave more room for preview windows.\n"
            "2. Open menu for [Z] for the record (to switch among recently tested rules).\n"
            "3. Use 'Misc' window's temp rules to ...");

        if (!show_random_access) {
            if (const auto* deliv = get_deliv(pass_rule::dest(), working_set)) {
                target.set(*deliv);
                show_random_access = true;
                messenger::dot();
                // (v will be skipped for this frame; that's ok.)
            }
        } else {
            target.sync(working_set, observer);

            ImGui::SameLine();
            target.display("[Z]", "Recent ([Z])", config, working_set);
            ImGui::SameLine();
            config.set("Settings");
        }
    }

    // TODO: there should finally be one single if-show_random_access scope...
    // const bool working_contains = show_random_access /*workaround*/ && working_set.contains(target);
    assert_implies(show_random_access, working_set.contains(target));

    enum class displayE { Observer, Target, Comp };
    static displayE disp_mode = displayE::Observer;

    ImGui::AlignTextToFramePadding();
    ImGui::Text("Groups:%d", working_set->k());
    {
        using enum displayE;
        if (!show_random_access) {
            disp_mode = Observer;
        } else if (disp_mode == Observer) {
            disp_mode = Target;
        }

        static constexpr std::pair<displayE, const char*> terms[]{
            {Observer, "[R]##disp"}, {Target, "[Z]##disp"}, {Comp, "Comp##disp"}};
        float spacing = imgui_ItemSpacingX() * 3;
        for (const auto& [mode, label] : terms) {
            ImGui::SameLine(0, std::exchange(spacing, imgui_ItemInnerSpacingX()));
            const bool selectable = !show_random_access ? mode == Observer : mode != Observer;
            ImGui::BeginDisabled(!selectable);
            imgui_RadioButton(label, &disp_mode, mode);
            ImGui::EndDisabled();
            if (!selectable) {
                imgui_ItemTooltip(show_random_access ? "!!TODO" : "...");
            }
        }
        ImGui::SameLine();
        imgui_StrTooltip("(?)", [] {
            imgui_StrPair("[R]:  ", "Display values of [R] (0/1).");
            imgui_StrPair("[Z]:  ", "Display values of [Z] (for 'Random-access').");
            imgui_StrPair("Comp: ", "Compare whether [Z] is same (O) or different (I) than [R].");
        });
    }
    if (show_random_access) {
        ImGui::SameLine(0, imgui_ItemSpacingX() * 3);
        ImGui::Text("Dist:%d", aniso::distance(working_set, observer, target));
        ImGui::SameLine();
        imgui_StrTooltip("(?)",
                         "Distance between [Z] and [R], i.e. the number of groups where they have different values.");
    }

    // TODO: whether to add here? whether to hide when it's not needed?
    // ImGui::SameLine();
    // if (ImGui::SmallButton("Top") && messenger::dot()) {
    //     ImGui::SetNextWindowScroll({0, 0});
    // }

    ImGui::Separator();

    // TODO: ?`imgui_FillAvailRect(IM_COL32_GREY(24, 255));`
    ImGui::PushStyleColor(ImGuiCol_ChildBg, IM_COL32_GREY(24, 255));
    if (auto child = imgui_ChildWindow("Groups")) {
        // set_scroll_by_up_down(preview_mode ? floor(config.height() * 0.5) : ImGui::GetFrameHeight());

        const bool comp_mode = disp_mode == displayE::Comp;
        const auto disp = [&] {
            aniso::codeT::map_to<bool> disp{};
            if (comp_mode) {
                assert(show_random_access);
                const aniso::ruleT &a = observer, &b = target.get();
                for (const auto code : aniso::each_code) {
                    disp[code] = a[code] != b[code];
                }
            } else {
                // (Avoiding `bit_cast`.)
                assert_implies(disp_mode == displayE::Target, show_random_access);
                const aniso::ruleT& r = disp_mode == displayE::Target ? target.get() : observer;
                for (const auto code : aniso::each_code) {
                    disp[code] = r[code];
                }
            }
            return disp;
        }();
        const char* const labels_disp[2]{comp_mode ? "-O" : "-0", comp_mode ? "-I" : "-1"};
        const char* const labels_disp_from_to[2]{comp_mode ? "-O -> I:" : "-0 -> 1:",
                                                 comp_mode ? "-I -> O:" : "-1 -> 0:"};

        // Precise vertical alignment:
        // https://github.com/ocornut/imgui/issues/2064
        const auto align_text = [](float height) {
            imgui_AddCursorPosY(std::max(0.0f, (height - ImGui::GetTextLineHeight()) / 2));
        };

        constexpr int button_zoom = init_compact_mode ? 6 : 7; // Also for image-zoom.
        constexpr ImVec2 button_padding = {2, 2};
        constexpr ImVec2 image_padding = {1, 1};
        const int spacing_x = ImGui::GetStyle().ItemSpacing.x + (show_random_access ? 3 : 5);
        const int group_size_x = [&]() -> int {
            if (show_random_access) {
                int size = button_padding.x * 2 + 3 * button_zoom; // button-size
                size += imgui_ItemInnerSpacingX() + imgui_CalcTextSize(labels_disp_from_to[0]).x;
                return std::max(size, config.width());
            } else {
                return image_padding.x * 2 + 3 * button_zoom + imgui_ItemInnerSpacingX() +
                       imgui_CalcTextSize(labels_disp[0]).x;
            }
        }();
        const int perline = fit_count(ImGui::GetContentRegionAvail().x, group_size_x, spacing_x);

        // std::vector<aniso::groupT> working_set_groups = working_set->group_vec();
        // if (!working_contains) {
        //     std::ranges::stable_partition(working_set_groups, [&](const aniso::groupT& group) {
        //         return !aniso::all_same_or_different(group, observer, target);
        //     });
        // }
        for (int n = 0; const aniso::groupT& group : working_set->groups()) {
            const int this_n = n++;
            if (this_n % perline != 0) {
                ImGui::SameLine(0, spacing_x);
            } else if (this_n != 0) {
                ImGui::Spacing(); // ImGui::Separator();
            }

            const aniso::codeT head = group[0];
            const auto group_details = [&] {
                // ImGui::PushTextWrapPos(-1); // No wrapping.
                const int group_size = group.size();
                ImGui::Text("Cases:%d", group_size);
                ImGui::Separator();
                constexpr int perline = 8;
                constexpr int max_to_show = perline * 6;
                for (int n = 0; const aniso::codeT code : group.first(std::min(group_size, max_to_show))) {
                    if (n++ % perline != 0) {
                        ImGui::SameLine();
                    }
                    code_image(code, button_zoom, ImVec4(1, 1, 1, 1), ImVec4(0.5, 0.5, 0.5, 1));
                    ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                    align_text(ImGui::GetItemRectSize().y);
                    imgui_Str(labels_disp[disp[code]]);
                }
                if (group_size > max_to_show) {
                    imgui_Str("...");
                }
                // ImGui::PopTextWrapPos();
            };

            if (show_random_access) {
                const auto get_adjacent_rule = [&] { return aniso::flip_values_v(group, target); };

                ImGui::BeginGroup();
                // _ButtonHovered: ImVec4(0.26f, 0.59f, 0.98f, 1.00f)
                // [0]:Button, [1]:Hover, [2]:Active
                static constexpr ImVec4 button_color[3]{
                    {0.26f, 0.59f, 0.98f, 0.70f}, {0.26f, 0.59f, 0.98f, 0.85f}, {0.26f, 0.59f, 0.98f, 1.00f}};
                // static constexpr ImVec4 button_col_impure[3]{
                //     {0.26f, 0.59f, 0.98f, 0.30f}, {0.26f, 0.59f, 0.98f, 0.40f}, {0.26f, 0.59f, 0.98f, 0.50f}};
                ImGui::PushStyleColor(ImGuiCol_Button, button_color[0]);
                ImGui::PushStyleColor(ImGuiCol_ButtonHovered, button_color[1]);
                ImGui::PushStyleColor(ImGuiCol_ButtonActive, button_color[2]);
                ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, button_padding);
                if (code_button(head, button_zoom)) {
                    target.set(get_adjacent_rule());
                }
                ImGui::PopStyleVar();
                ImGui::PopStyleColor(3);

                // !!TODO: (v0.9.9) sometimes noisy; should be able to turn off this tooltip.
                // imgui_ItemTooltip(group_details);
                if (ImGui::BeginItemTooltip()) {
                    group_details();
                    ImGui::EndTooltip();
                }

                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                align_text(ImGui::GetItemRectSize().y);
                imgui_Str(labels_disp_from_to[disp[head]]);

                previewer::preview(/*id ~*/ this_n, config, get_adjacent_rule /*()*/);
                ImGui::EndGroup();
            } else {
                code_image(head, button_zoom, ImVec4(1, 1, 1, 1), ImVec4(0.5, 0.5, 0.5, 1));
                // imgui_ItemTooltip(group_details);
                if (ImGui::BeginItemTooltip()) {
                    group_details();
                    ImGui::EndTooltip();
                }
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                align_text(ImGui::GetItemRectSize().y);
                imgui_Str(labels_disp[disp[head]]);
            }
        }
    }
    ImGui::PopStyleColor();
}

[[maybe_unused]] static void static_constraints(aniso::partialT& out) {
    enum stateE { Any_background, O, I, O_background, I_background };

    // (Follows `ImGui::Dummy` or `ImGui::InvisibleButton`.)
    static const auto put_col = [](stateE state, bool disabled = false) {
        static const ImU32 cols[5]{IM_COL32_GREY(80, 255),   //
                                   IM_COL32_BLACK,           //
                                   IM_COL32_WHITE,           //
                                   IM_COL32(80, 0, 80, 255), //
                                   IM_COL32(200, 0, 200, 255)};
        assert_implies(disabled, state == Any_background);
        imgui_ItemRectFilled(disabled ? IM_COL32_GREY(60, 255) : cols[state]);
        imgui_ItemRect(IM_COL32_GREY(160, 255));
    };

    constexpr int r = 10; // TODO: use separate values for w and h?
    static stateE board[r][r]{/* Any_background... */};
    static stateE state_lbutton = I;
    constexpr stateE state_rbutton = Any_background;
    const auto description = [] {
        const auto term = [](stateE s, const char* desc) {
            ImGui::Dummy(square_size());
            put_col(s);
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            ImGui::AlignTextToFramePadding(); // `Dummy` does not align automatically.
            imgui_Str(desc);
        };

        imgui_Str("Operations:\n"
                  "Left-click a cell to set the value.\n"
                  "Right-click to set back to any-background.\n"
                  "Scroll in the board to change the value for left-click.\n");
        term(O, ": Supposed to remain 0.");
        term(I, ": Supposed to remain 1.");
        term(O_background, ": Background 0.");
        term(I_background, ": Background 1.");
        term(Any_background, ": Any background.");
        imgui_Str("By 'Adopt', you will get a rule-lock pair that can satisfy the constraints represented by the "
                  "arrangements. (For example, a pattern of white cells surrounded by any-background cells will keep "
                  "stable whatever its surroundings are. You can right-click this '(...)' to get an example.)");
    };

    ImGui::AlignTextToFramePadding();
    imgui_StrTooltip("(...)", description);
    if (ImGui::IsItemClicked(ImGuiMouseButton_Right)) {
        for (auto& l : board) {
            for (auto& s : l) {
                s = Any_background;
            }
        }
        for (int y = 1; y <= 4; ++y) {
            for (int x = 1; x <= 4; ++x) {
                board[y][x] = ((x == 2 || x == 3) && (y == 2 || y == 3)) ? O : I;
            }
        }
    }
    ImGui::SameLine();
    if (ImGui::Button("Clear")) {
        for (auto& l : board) {
            for (auto& s : l) {
                s = Any_background;
            }
        }
    }
    ImGui::SameLine();
    const bool empty = [] {
        for (const auto& l : board) {
            for (const auto& s : l) {
                if (s != Any_background) {
                    return false;
                }
            }
        }
        return true;
    }();
    if (empty) {
        ImGui::BeginDisabled();
    }
    const bool ret = ImGui::Button("Adopt");
    if (empty) {
        ImGui::EndDisabled();
        imgui_ItemTooltip("Empty.");
    }

    // Display-only; the value of `state_lbutton` is controlled by mouse-scrolling.
    ImGui::BeginDisabled();
    for (const stateE s : {O, I, O_background, I_background}) {
        if (s != O) {
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        }
        // No need for unique ID here (as the return value is not used).
        ImGui::RadioButton("##Radio", s == state_lbutton);
        // TODO: show message?
        // imgui_ItemTooltip("Scroll in the board to change the value for left-click.");
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        ImGui::Dummy(square_size());
        put_col(s);
    }
    ImGui::EndDisabled();

    ImGui::BeginGroup();
    ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, {0, 0});
    for (int y = 0; y < r; ++y) {
        for (int x = 0; x < r; ++x) {
            if (x != 0) {
                ImGui::SameLine();
            }
            const bool editable = y >= 1 && y < r - 1 && x >= 1 && x < r - 1;
            stateE& state = board[y][x];

            // No need for unique ID here (as IsItemHovered + IsMouseDown doesn't rely on ID).
            ImGui::InvisibleButton("##Invisible", square_size(),
                                   ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
            if (editable && ImGui::IsItemHovered()) {
                if (ImGui::IsMouseDown(ImGuiMouseButton_Right)) {
                    state = state_rbutton;
                } else if (ImGui::IsMouseDown(ImGuiMouseButton_Left)) {
                    state = state_lbutton;
                }
            }
            put_col(state, !editable /*-> disabled*/);
        }
    }
    ImGui::PopStyleVar();
    ImGui::EndGroup();
    if (ImGui::IsItemHovered()) {
        if (imgui_MouseScrollingDown()) {
            state_lbutton = (stateE)std::min((int)I_background, state_lbutton + 1);
        } else if (imgui_MouseScrollingUp()) {
            state_lbutton = (stateE)std::max((int)O, state_lbutton - 1);
        }
    }

    if (ret) {
        aniso::partialT partial{};
        for (int y = 1; y < r - 1; ++y) {
            for (int x = 1; x < r - 1; ++x) {
                if (board[y][x] == O || board[y][x] == I) {
                    // For example:
                    //  O   O_b  I                  001       001
                    // [Any] O   O  will result in [0]00 and [1]00 being set.
                    //  I_b  I  I_b                 111       111
                    for (const auto code : aniso::each_code) {
                        const auto imbue = [](aniso::cellT& c, stateE state) {
                            if (state == O || state == O_background) {
                                c = {0};
                            } else if (state == I || state == I_background) {
                                c = {1};
                            }
                        };

                        aniso::situT situ = aniso::decode(code);

                        imbue(situ.q, board[y - 1][x - 1]);
                        imbue(situ.w, board[y - 1][x]);
                        imbue(situ.e, board[y - 1][x + 1]);
                        imbue(situ.a, board[y][x - 1]);
                        imbue(situ.s, board[y][x]);
                        imbue(situ.d, board[y][x + 1]);
                        imbue(situ.z, board[y + 1][x - 1]);
                        imbue(situ.x, board[y + 1][x]);
                        imbue(situ.c, board[y + 1][x + 1]);

                        partial.set(aniso::encode(situ), aniso::cellT(board[y][x] == O ? 0 : 1));
                    }
                }
            }
        }
        out = partial;
    }
}
