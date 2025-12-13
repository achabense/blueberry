#include "rule_algo.hpp"

#include "common.hpp"

// TODO: support toggling tooltips (subsets & rule tags & groups)?

namespace aniso {
    static const struct : no_copy {
        subsetT ignore_q = make_subset({mp_ignore_q});
        subsetT ignore_w = make_subset({mp_ignore_w});
        subsetT ignore_e = make_subset({mp_ignore_e});
        subsetT ignore_a = make_subset({mp_ignore_a});
        subsetT ignore_s_z = make_subset({mp_ignore_s}, rule_all_zero());
        subsetT ignore_s_i = make_subset({mp_ignore_s}, rule_identity());
        subsetT ignore_d = make_subset({mp_ignore_d});
        subsetT ignore_z = make_subset({mp_ignore_z});
        subsetT ignore_x = make_subset({mp_ignore_x});
        subsetT ignore_c = make_subset({mp_ignore_c});

        subsetT ignore_hex = make_subset({mp_hex_ignore});
        subsetT ignore_jvn = make_subset({mp_jvn_ignore});
        subsetT ignore_wadx = make_subset({mp_ignore_wadx});

        subsetT self_complementary = make_subset({mp_reverse}, rule_identity());

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

#ifdef YDEBUG
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

            assert(!self_complementary.contains(rule_all_zero()));

            assert(native_isotropic.contains(game_of_life()));
            assert(native_tot_exc_s.contains(game_of_life()));
            assert(!native_tot_inc_s.contains(game_of_life()));
        }
#endif // YDEBUG
    } subsets;

#ifdef YDEBUG
    namespace _tests {
        static const bool test_subsets = (subsets.test(), true);
    } // namespace _tests
#endif

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

class set_ptr {
    const aniso::subsetT* s = nullptr;
    std::unique_ptr<aniso::partialT> p = nullptr;

public:
    set_ptr(const aniso::subsetT* s) : s{s} { assert(s); }
    set_ptr(const aniso::partialT& p) : p{new auto(p)} {}
    const aniso::subsetT* get_s() const { return s; }
    const aniso::partialT* get_p() const { return p.get(); }

    bool is_universal() const { //
        return s ? s->is_universal() : p->is_universal();
    }
    bool contains(const aniso::ruleT& rule) const { //
        return s ? s->contains(rule) : p->contains(rule);
    }
    bool includes(const aniso::subsetT_v2& set) const { //
        return s ? aniso::includes(*s, set) : aniso::includes(*p, set);
    }
    bool has_common(const aniso::subsetT_v2& set) const { //
        return s ? aniso::has_common(*s, set) : aniso::has_common(*p, set);
    }
    void intersect(aniso::subsetT& s2, aniso::partialT& p2) const {
        // (Working as both &= return void. (Otherwise should be `void(i &= j)`.))
        s ? s2 &= * s : p2 &= *p;
    }
};

// `subsetT` (and `mapperT` pair) are highly customizable. However, for sanity there is no plan to
// support user-defined subsets in the gui part.
class subset_selector : no_copy {
    aniso::subsetT_v2 m_current{}; // Intersection of selected sets.
    bool set_changed = true;

    class termT : public set_ptr { // !is_universal().
        struct infoT {
            const char* title;
            const char* desc;
            bool includes = false;  // includes(m_current).
            bool has_common = true; // has_common(m_current).
        } info;

    public:
        bool selected = false;
        explicit termT(set_ptr set, const char* title, const char* desc, const aniso::subsetT_v2* current = nullptr)
            : set_ptr{std::move(set)}, info{.title = title, .desc = desc} {
            assert(title && desc && !is_universal());
            if (current) {
                (void)update_info(*current);
            }
        }

        const infoT* operator->() const { return &info; }
        bool update_info(const aniso::subsetT_v2& current) {
            const bool changed = compare_update(info.includes, includes(current));
            assert_implies(info.includes, has_common(current));
            info.has_common = info.includes /*perf*/ || has_common(current);

            assert_implies(selected, info.includes);
            assert_implies(!info.has_common, !selected);
            return changed;
        }
    };

    using terms_data = std::vector<termT>;
    // TODO: refactor if possible...
    // (Ideally should be two regular members, but `std::views::concat` doesn't exist in real world...)
    terms_data m_terms_ex[2]{};
    terms_data& m_terms_s = m_terms_ex[0]; // subsetT only.
    terms_data& m_terms_p = m_terms_ex[1]; // partialT only.

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
        aniso::subsetT s{};
        aniso::partialT p{};
        for (const termT& t : std::views::join(m_terms_ex)) {
            if (t.selected) {
                t.intersect(s, p);
            }
        }
        m_current = s & p;
        for (termT& t : std::views::join(m_terms_ex)) {
            if (t.update_info(m_current)) {
                set_changed = true;
            }
        }
    }

    void toggle(termT& term) {
        assert(term.selected || term->has_common);

        term.selected = !term.selected;
        update_current();
    }

    bool select_single(termT& term) {
        bool update = false;
        for (termT& t : std::views::join(m_terms_ex)) {
            if (compare_update(t.selected, &t == &term)) {
                update = true;
            }
        }
        if (update) {
            update_current();
        }
        return update;
    }

public:
    // (Always true for the first call.)
    bool set_changed_since_last_check() { return std::exchange(set_changed, false); }

    // !!TODO: support predefined p-sets (e.g. strobing-set (01 10) and (01 11, 00 10, 11 00))
    // `init_s` should be either nullptr or address of one of sets in `m_terms`.
    explicit subset_selector(const aniso::subsetT* init_s = nullptr) {
        using aniso::subsets;
        constexpr int reserve_cap = 50;
        m_terms_s.reserve(reserve_cap);
        assert(m_current.is_universal());

        struct terms_scope : no_copy {
            terms_data& data;
            terms_ref& ref;
            terms_scope(terms_data& d, terms_ref& r) : data(d), ref(r) { ref.pos = data.size(); }
            ~terms_scope() { ref.size = data.size() - ref.pos; }

            void append(const char* title, const aniso::subsetT* set, const char* desc) {
                data.push_back(termT(set, title, desc /*, nullptr*/));
            }
        };
        {
            // TODO: confusing for non-qwerty keyboard users...
            terms_scope scope(m_terms_s, terms_ignore);
            scope.append(
                "q", &subsets.ignore_q,
                "Rules whose values are independent of 'q'. That is, for any two cases where only 'q' differs, the rule will map center cell to the same value.\n\n"
                "    |0 w e|       |1 w e|\n"
                "rule|a s d| = rule|a s d|\n"
                "    |z x c|       |z x c|\n\n"
                "Therefore, these rules will behave as if the neighborhood does not include 'q'. The same applies to 'w/e/a/d/z/x/c', except for 's'.\n\n"
                "('q/w/e/a/s/d/z/x/c' are named after keys in the 'qwerty' keyboard.)");
            scope.append("w", &subsets.ignore_w, "Independent of 'w'. (See 'q' for details.)");
            scope.append("e", &subsets.ignore_e, "Independent of 'e'. (See 'q' for details.)");
            scope.append("a", &subsets.ignore_a, "Independent of 'a'. (See 'q' for details.)");
            scope.append(
                "s", &subsets.ignore_s_z,
                "For any two cases where only 's' (the center cell itself) differs, the rule will map center cell to the same value.\n\n"
                "    |q w e|       |q w e|\n"
                "rule|a 0 d| = rule|a 1 d|\n"
                "    |z x c|       |z x c|\n\n"
                "So when the surrounding cells are the same, there must be: either s:0->1, s:1->1 or s:0->0, s:1->0.\n\n"
                "(It's not obvious what's special about this set; though this is defined in the same way as other independence sets, it's not suitable to treat this as \"independent of 's'\".)");
            scope.append("d", &subsets.ignore_d, "Independent of 'd'. (See 'q' for details.)");
            scope.append("z", &subsets.ignore_z, "Independent of 'z'. (See 'q' for details.)");
            scope.append("x", &subsets.ignore_x, "Independent of 'x'. (See 'q' for details.)");
            scope.append("c", &subsets.ignore_c, "Independent of 'c'. (See 'q' for details.)");
        }
        {
            terms_scope scope(m_terms_s, terms_misc);
            if constexpr (0) {
                scope.append(
                    "s(*)", &subsets.ignore_s_i,
                    "Similar to 's' - for any two cases where only 's' differs, the rule will map the center cell to values so that the resulting \"flip-ness\" will be the same. That is:\n\n"
                    "     |q w e|             |q w e|\n"
                    "(rule|a 0 d| = 0) = (rule|a 1 d| = 1)\n"
                    "     |z x c|             |z x c|\n\n"
                    "So when the surrounding cells are the same, there must be: either s:0->0, s:1->1 (no flip in either case) or s:0->1, s:1->0 (flip in both cases).\n\n"
                    "(This is provided for completeness; it's not obvious what's special about this set.)");
            }
            scope.append(
                "Hex", &subsets.ignore_hex,
                "Rules whose values are independent of 'e' and 'z'. As a result, they can emulate range-1 hexagonal neighborhood - for any rule in this set, there exists an actual rule in the hexagonal tiling with the same behavior.\n\n"
                "To be exact, for any pattern, if evolved under such a rule, the dynamics will correspond to a projected version evolved under an actual rule in the hexagonal space. See the last line for illustration.\n\n"
                "(For windows displaying such rules, you can hover and press 6 to see the projected view.)");
            scope.append(
                "Jvn", &subsets.ignore_jvn,
                "Rules whose values are independent of 'q', 'e', 'z' and 'c'. As a result, they can emulate range-1 von Neumann neighborhood directly.\n\n"
                "(This can work naturally with native-symmetry sets.)");
            scope.append("Wadx", &subsets.ignore_wadx,
                         "Rules whose values are independent of 'w', 'a', 'd' and 'x'.\n\n"
                         "(This can work naturally with native-symmetry sets.)");
            scope.append(
                "0v1", &subsets.self_complementary,
                "Self-complementary rules. That is, their 0/1 reversal duals are equal to themselves - for any pattern, [applying such a rule -> flipping all values] has the same effect as [flipping all values -> applying the same rule].\n\n"
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
                static const subsetT ckbd = make_subset({mp_xor_ckbd_a, mp_xor_ckbd_b}, rule_identity());
                scope.append(
                    "Ckbd", &ckbd,
                    "For any pattern, [applying such a rule -> xor with checkerboard bg] (in arbitrary alignment) has the same effect as [xor with checkerboard bg -> applying the same rule].");
            }

            if constexpr (0) {
                using namespace aniso;
                // rule[{0}] == rule[{511}].
                static subsetT single_stable_state{.rule = rule_all_zero(), .p = [] {
                                                       equivT eq{};
                                                       eq.add_eq({0}, {511});
                                                       return eq;
                                                   }()};
                scope.append("00=11", &single_stable_state,
                             "Rules that map all-0 and all-1 cases to the same value.\n\n"
                             "    |0 0 0|       |1 1 1|\n"
                             "rule|0 0 0| = rule|1 1 1|\n"
                             "    |0 0 0|       |1 1 1|");

                // Stripe; not quite interesting...
                // ({a+b/c+d, rule_identity} can represent invar in one direction.)
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
                    make_subset({mp_xor_stp_a, mp_xor_stp_b, mp_xor_stp_c, mp_xor_stp_d}, rule_identity());
                scope.append("Stp", &stp, "(Debug mode) Stripe-xor invariance.");
            }
        }
        {
            terms_scope scope(m_terms_s, terms_native);
            scope.append("Iso", &subsets.native_isotropic,
                         "Isotropic MAP rules, i.e. rules that preserve all spatial symmetries.\n\n"
                         "(This is equal to the intersection of the following sets in this line.)");
            scope.append(
                "|", &subsets.native_refl_wsx,
                "Rules that preserve reflection symmetry, taking '|' as the axis.\n\n"
                "For any pattern and its leftside-right mirror image, if evolved under such a rule they will remain in mirror.\n\n"
                "For any leftside-right symmetric pattern, if evolved under such a rule it will remain leftside-right symmetric.");
            scope.append("-", &subsets.native_refl_asd, "Similar to '|'; the axis is '-'.");
            scope.append("\\", &subsets.native_refl_qsc, "Similar to '|'; the axis is '\\'.");
            scope.append("/", &subsets.native_refl_esz, "Similar to '|'; the axis is '/'.");
            scope.append("C2", &subsets.native_C2, "Rules that preserve 2-fold rotational symmetry.");
            scope.append("C4", &subsets.native_C4, "4-fold rotational symmetry. This is a strict subset of 'C2'.");
        }
        {
            terms_scope scope(m_terms_s, terms_totalistic);
            scope.append(
                "Tot", &subsets.native_tot_exc_s,
                "Outer-totalistic MAP rules, i.e. rules whose values are only dependent on 's' and the sum of surrounding cells. This is a strict subset of isotropic rules ('Iso').\n\n"
                "(This is also known as life-like rules, and is where the B/S notation applies.)");
            scope.append(
                "Tot(+s)", &subsets.native_tot_inc_s,
                "Inner-totalistic MAP rules, i.e. rules whose values are only dependent on the sum of all cells (including 's'). This is a strict subset of outer-totalistic rules ('Tot').");
            scope.append("Hex", &subsets.hex_tot_exc_s, "Outer-totalistic hexagonal rules.");
            scope.append("Hex(+s)", &subsets.hex_tot_inc_s, "Inner-totalistic hexagonal rules.");
            scope.append("Jvn", &subsets.jvn_tot_exc_s, "Outer-totalistic von Neumann rules.");
            scope.append("Jvn(+s)", &subsets.jvn_tot_inc_s, "Inner-totalistic von Neumann rules.");
        }
        {
            // q w -    q w
            // a s d ~ a s d
            // - x c    x c
            terms_scope scope(m_terms_s, terms_hex);
            scope.append(
                "Iso", &subsets.hex_isotropic,
                "Rules that emulate isotropic hexagonal rules.\n\n"
                "All sets in this line are strict subsets of 'Hex'; for windows displaying such rules, you can hover and press 6 to better view the symmetries in the hexagonal space.\n\n"
                "(Some of these sets behave like subsets of native-symmetry sets; however, they are not conceptually related.)");
            scope.append(
                "a-d", &subsets.hex_refl_asd,
                "Rules that emulate reflection symmetry in the hexagonal tiling, taking the axis from 'a' to 'd' (a-to-d).");
            scope.append("q-c", &subsets.hex_refl_qsc, "Similar to 'a-d'; the axis is q-to-c.");
            scope.append("w-x", &subsets.hex_refl_wsx, "Similar to 'a-d'; the axis is w-to-x.");
            scope.append("a|q", &subsets.hex_refl_aq, "Similar to 'a-d'; the axis is vertical to a-to-q.");
            scope.append("q|w", &subsets.hex_refl_qw, "Similar to 'a-d'; the axis is vertical to q-to-w.");
            scope.append("w|d", &subsets.hex_refl_wd, "Similar to 'a-d'; the axis is vertical to w-to-d.");
            scope.append("C2", &subsets.hex_C2,
                         "Rules that emulate 2-fold rotational symmetry in the hexagonal tiling.");
            scope.append("C3", &subsets.hex_C3, "3-fold rotational symmetry.");
            scope.append("C6", &subsets.hex_C6,
                         "6-fold rotational symmetry. This is a strict subset of both 'C2' and 'C3'.");
        }
        assert(m_terms_s.size() <= reserve_cap);
        assert(m_terms_p.empty());

        if (init_s) {
            const auto pos = std::ranges::find_if(m_terms_s, [init_s](const termT& t) { return t.get_s() == init_s; });
            assert(pos != m_terms_s.end());
            if (pos != m_terms_s.end()) {
                select_single(*pos);
            }
        }
    }

    // !!TODO: incomplete; should support multiple p-sets...
    // TODO: use different colors?
    void load_capture(const aniso::ruleT& r, const aniso::lockT& l, const char* msg = nullptr) {
        assert(m_terms_p.size() <= 1);
        const aniso::partialT p{.rule = aniso::normalize(r, l), .lock = l};
        if (!p.is_universal()) {
            if (!m_terms_p.empty() && m_terms_p[0].selected) {
                m_terms_p[0].selected = false;
                update_current();
            }
            m_terms_p.clear();
            m_terms_p.push_back(termT( // TODO: when to use "pattern" vs "object"?
                p, "P", "(Experimental) rules that can reproduce the captured object in all phases.", &m_current));
            messenger::set_msg(msg ? msg : "Captured. (See 'P' in the set table.)");
        } else {
            messenger::set_msg("Ignored. (Universal set.)");
        }
    }

    void debug_capture_in_popup() {
        if (ImGui::Selectable("Example (gliders)")) {
            // Gliders in all directions. (Capture a glider & 'Iso' -> Copy)
            constexpr const char* str =
                "MAPARYSZhYAPEgQYBCgAAAAgABAEsAIAIgAQIDgAIAAgAACAAIAKACggACAAICAAIAACICogIAAAACAAAAAAAAAAA [//Z65r4SvEjQ4JCgABAQgIDRksSLAIwAwKDgAIDAwACKAIICqAKggICAAKCAAKAAiICoiKCIAACAIAAAIAAAAA]";
            const auto extr = aniso::extract_MAP_str(str, true);
            assert(extr.has_rule() && extr.has_lock());
            // TODO: should redesign desc & message...
            load_capture(extr.get_rule(), extr.get_lock(), "Updated.");
        }
        if (ImGui::Selectable("Example (gol only)")) { // For testing all-locked p-set.
            aniso::lockT l{};
            l.fill(true);
            load_capture(aniso::game_of_life(), l, "Updated.");
        }
        ImGui::BeginDisabled(aniso::none_locked(m_current.lock));
        if (ImGui::Selectable("Copy")) {
            const auto rule = aniso::normalize(m_current.get_rule(), m_current.lock);
            set_clipboard_and_notify(aniso::to_MAP_str(rule, &m_current.lock));
        }
        ImGui::EndDisabled();
        if (ImGui::Selectable("Paste")) {
            // TODO: read_clipboard vs load_clipboard(frame_main_token) are confusing names...
            if (const auto str = read_clipboard(); !str.empty()) {
                const auto extr = aniso::extract_MAP_str(str, true);
                if (extr.has_rule() && extr.has_lock()) {
                    load_capture(extr.get_rule(), extr.get_lock(), "Updated.");
                } else {
                    messenger::set_msg("Invalid.");
                }
            }
        }
    }

private:
    enum centerE { Selected, Includes, Disabled /*no-common*/, None };

    // (Follows `ImGui::Dummy` or `ImGui::InvisibleButton`.)
    // TODO: use ImGuiCol_ colors?
    static void put_term(bool contains_rule, centerE center, char icon /* '\0' ~ no icon */) {
        if (center == Disabled && !icon) {
            icon = '-';
        }

        imgui_ItemRectFilled(IM_COL32_BLACK);
        if (icon && (center == None || center == Disabled)) {
            const ImU32 col = center != Disabled ? IM_COL32_WHITE : IM_COL32_GREY(150, 255);
            imgui_ItemStr(col, {&icon, 1});
        } else {
            const ImU32 col = center == Selected   ? IM_COL32(65, 150, 255, 255) // Roughly _ButtonHovered
                              : center == Includes ? IM_COL32(25, 60, 100, 255)  // Roughly _Button
                                                   : IM_COL32_BLACK_TRANS;
            imgui_ItemRectFilled(col, 0.5f);
        }
        imgui_ItemRect(contains_rule ? IM_COL32(0, 255, 0, 255)   // Light green
                                     : IM_COL32(0, 100, 0, 255)); // Dull green
    }

public:
    // TODO: improve...
    static void about() {
        const auto explain = [](bool contains_rule, centerE center, std::string_view desc) {
            ImGui::Dummy(square_size());
            put_term(contains_rule, center, '\0');
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            ImGui::AlignTextToFramePadding(); // `Dummy` does not align automatically.
            imgui_Str(": ");
            ImGui::SameLine(0, 0);
            imgui_Str(desc);
        };

        imgui_Str(
            "[S] stands for the intersection of selected sets.\n"
            "A rule belongs to [S] iff it belongs to every selected set.\n\n"
            "Left-click a set to toggle selection. Multiple sets can be selected as long as their intersection is not empty. If no sets are selected, [S] will be the entire MAP set.\n\n"
            "Hold right mouse button + left-click to select a single set.\n\n");

        explain(false, None, "Not selected.");
        explain(false, Selected, "Selected.");
        explain(false, Includes, "Not selected, but [S] already belongs to this set.");
        explain(false, Disabled, "Not selectable, as its intersection with [S] is empty.");
        // explain(true, None, "The rule belongs to this set.");
        // explain(false, None, "The rule does not belong to this set.");
    }

    const aniso::subsetT_v2& get() const { return m_current; }

    [[deprecated]] bool clear() {
        bool update = false;
        for (termT& t : std::views::join(m_terms_ex)) {
            if (compare_update(t.selected, false)) {
                update = true;
            }
        }
        if (update) {
            update_current();
        }
        return update;
    }

    bool match(const aniso::ruleT& rule) {
        bool update = false;
        for (termT& t : std::views::join(m_terms_ex)) {
            if (compare_update(t.selected, t.contains(rule))) {
                update = true;
            }
        }
        if (update) {
            update_current();
        }
        return update;
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
                                                                                        const bool show_icon) {
                const bool contains_rule = mode.rule && term.contains(*mode.rule);
                const char icon = show_icon ? term->title[0] : '\0';
                if (!mode.select) {
                    ImGui::Dummy(square_size());
                    put_term(contains_rule, None, icon);
                    return;
                }

                assert_implies(!term->has_common, !term.selected);
                const bool selectable = r_down || term->has_common;

                ImGui::PushID(id++);
                ImGui::BeginDisabled(!selectable);
                if (ImGui::InvisibleButton("Subset", square_size())) {
                    if (r_down) {
                        const bool updated = select_single(term);
                        messenger::dot_if(!updated);
                    } else if (term->has_common) {
                        toggle(term);
                    }
                }
                ImGui::EndDisabled();
                ImGui::PopID();
                put_term(contains_rule,
                         term.selected       ? Selected
                         : term->includes    ? Includes
                         : !term->has_common ? Disabled
                                             : None,
                         icon);
                if (selectable && ImGui::IsItemHovered()) {
                    imgui_ItemRectFilled(ImGui::IsItemActive() ? IM_COL32_GREY(255, 55) : IM_COL32_GREY(255, 45));
                }
                if (mode.tooltip) {
                    imgui_ItemTooltip([&] {
                        if (const auto* s = term.get_s()) {
                            ImGui::Text("Groups:%d", s->p.k());
                        } else if (const auto* p = term.get_p()) {
                            ImGui::Text("Locked:%d/512", aniso::count_locked(p->lock)); // TODO: improve...
                        } else {
                            assert(false);
                        }
                        ImGui::Separator();
                        imgui_Str(term->desc);
                    });
                }
            };

            const auto checklist = [&](const std::span<termT> terms) {
                for (bool first = true; termT & t : terms) {
                    if (!std::exchange(first, false)) {
                        ImGui::SameLine();
                    }
                    ImGui::BeginGroup();
                    const float title_w = imgui_CalcTextSize(t->title).x;
                    const float button_w = ImGui::GetFrameHeight();
                    if (title_w < button_w) {
                        imgui_AddCursorPosX((button_w - title_w) / 2);
                    }
                    imgui_Str(t->title);
                    if (button_w < title_w) {
                        imgui_AddCursorPosX((title_w - button_w) / 2);
                    }
                    check(t, false);
                    ImGui::EndGroup();
                }
            };

            const auto put_row = [](const char* l_str, const auto& r_contents) {
                ImGui::TableNextRow();
                ImGui::TableNextColumn();
                imgui_Str(l_str);

                ImGui::TableNextColumn();
                r_contents();
            };

            put_row("Neighborhood\n& misc", [&] {
                ImGui::BeginGroup();
                ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(2, 2));
                const std::span<termT> ignore = terms_ignore.get(m_terms_s);
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
                checklist(terms_misc.get(m_terms_s));
                if (!m_terms_p.empty()) {
                    ImGui::SameLine();
                    checklist(m_terms_p);
                }
            });

            put_row("Native\nsymmetry", [&] { checklist(terms_native.get(m_terms_s)); });
            put_row("Totalistic", [&] { checklist(terms_totalistic.get(m_terms_s)); });
            put_row("q w -    q w\n"
                    "a s d ~ a s d\n"
                    "- x c    x c",
                    [&] { checklist(terms_hex.get(m_terms_s)); });

            ImGui::EndTable();
        }
    }

    void show_belongs(const aniso::ruleT& rule) const {
        const_cast<subset_selector*>(this)->select({.rule = &rule, .select = false, .tooltip = false});
    }
};

// TODO: remove...
// (Workaround to avoid affecting too many lines.)
#define subsetT subsetT_v2
// TODO: recheck use of k() vs free_k(); free_k() can be 0...
// TODO: update tooltips (groups -> free groups).

// (Used to require being used after regular tooltips; no longer necessary.)
static bool check_contains(const pass_rule::passT& pass, const aniso::subsetT& working_set) {
    if (const auto* rule = pass.rule(); rule && !working_set.contains(*rule)) {
        const bool hov = pass.hov_for_tooltip();
        if (hov || pass.deliv) {
            messenger::set_msg("The rule does not belong to [S].");
            if (hov) {
                messenger::set_once();
            }
            // else { messenger::set_auto_disappear(); }
        }
        return false;
    }
    return true;
}

// TODO: improve tooltips...
class rule_selector : no_copy {
    enum tagE { Zero, Identity, Other, Default, None };
    tagE m_tag = Default;

    aniso::ruleT m_other = aniso::game_of_life(); // (!= 'Zero' or 'Identity'.)
    aniso::ruleT m_default = {};

    tagE m_highlight = None;

    struct termT {
        tagE tag;
        const char* label;
        const char* desc;
    };
    static constexpr termT terms[4]{
        {Zero, "Zero", // TODO: about distance to 'Zero'...
         "The all-0 rule, i.e. the rule that maps cell to 0 in all cases.\n\n"
         "For any rule in any case, being same as this rule means the rule maps cell to 0, and being different means the rule maps cell to 1."},

        {Identity, "Identity",
         "The rule that preserves cell's value in all cases.\n\n"
         "For any rule in any case, being same as this rule means the cell will stay unchanged (0->0 or 1->1), and being different means the cell will \"flip\" (0->1 or 1->0)."},

        {Other, "Other",
         "Another rule that's neither 'Zero' nor 'Identity'.\n\n"
         "This is initially the Game of Life rule, and can be updated by dragging a rule to [R]. (If the rule equals 'Zero' or 'Identity', will select directly; otherwise will update and select this.)"},

        {Default, "Default",
         "A rule predefined in [S]. Depending on [S], the rule may equal 'Zero' or 'Identity', or sometimes neither. (For example, try '0v1 & Tot(+s)'.)\n\n"
         "If [S] changes and no longer contains [R], this will be selected automatically."},
    };

    const aniso::ruleT& get_rule(tagE tag) const {
        assert(tag != None);
        return tag == Zero       ? aniso::rule_all_zero() //
               : tag == Identity ? aniso::rule_identity()
               : tag == Other    ? m_other
                                 : m_default;
    }

    tagE resolve_tag(const aniso::ruleT& rule) const {
        return rule == aniso::rule_all_zero()   ? Zero //
               : rule == aniso::rule_identity() ? Identity
                                                : None;
    }

public:
    static void about() {
        imgui_Str(
            "[S] divides all cases into disjoint groups, and any two rules in [S] must have either all-same or all-different values in each group. Due to this structure, the \"distance\" between rules can be defined as the number of groups where they have different values.\n\n"
            "[R] stands for an arbitrary rule in [S] to \"observe\" other rules (measure relative distance and compare values). It doesn't affect rule generation directly.");
        // "(If you select 'Zero' or 'Identity', being same or different than [R] has natural interpretations.)"
    }

    const aniso::ruleT& get() const { return get_rule(m_tag); }

    void sync(const aniso::subsetT& working_set) {
        // TODO: whether to highlight when selecting `Default` but the rule is updated?
        m_default = working_set.get_rule();
        if (m_tag != Default && !working_set.contains(get())) {
            m_tag = Default;
            m_highlight = Default;
        }
    }

    void select(const aniso::subsetT& working_set) {
        assert(working_set.contains(m_default));
        assert(working_set.contains(get()));

        // TODO: improve...
        // (Used to require being used after regular tooltips; no longer necessary.)
        const auto peek_dist = [&](const aniso::ruleT& rule) {
            if (const aniso::ruleT* peek = pass_rule::peek()) {
                if (imgui_IsItemHoveredForTooltip(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem)) {
                    if (working_set.contains(*peek)) {
                        const int dist = aniso::distance(working_set, rule, *peek);
                        messenger::set_msg("Dist:{}{}", dist, dist == 0 ? " (same rule)" : "");
                        messenger::set_once();
                        highlight_item();
                    }
                    // TODO: display something when !contains?
                }
            }
        };

        ImGui::AlignTextToFramePadding();
        imgui_StrWithID("[R]");
        guide_mode::item_tooltip("Drag a rule here to update [R]. (See 'Other' for details.)");
        // pass_rule::source(get());
        if (const auto pass = pass_rule::dest(); check_contains(pass, working_set) && pass.deliv) {
            // TODO: whether to check-diff (vs peek_dist())?
            if (const tagE tag = resolve_tag(*pass.deliv); tag != None) {
                m_tag = tag;
            } else {
                m_other = *pass.deliv;
                m_tag = Other;
            }
            m_highlight = m_tag;
        }
        peek_dist(get());

        ImGui::SameLine(0, 0);
        imgui_Str(" ~");
        for (const auto& term : terms) {
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            if (term.tag == Default) {
                imgui_Str("|");
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            }

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
            if (m_highlight == term.tag) {
                highlight_item();
                m_highlight = None;
            }
            if (!belongs) {
                ImGui::PopStyleColor(4);
            }

            pass_rule::source(rule);
            imgui_ItemTooltip([&] {
                if (!belongs) {
                    imgui_Str("The rule does not belong to [S].");
                    ImGui::Separator();
                }
                imgui_Str(term.desc);
                previewer::preview(0, previewer::default_settings, rule);
            });
            if (belongs) {
                peek_dist(rule);
            }

            if (term.tag == Default) {
                const tagE tag = resolve_tag(m_default); // TODO: cache...
                ImGui::SameLine();
                imgui_StrDisabled(tag == Zero ? "(= Zero)" : tag == Identity ? "(= Identity)" : "(= neither)");
            }
        }

        assert(working_set.contains(get()));
    }
};

// Temp rules (+ 0/1-rev (& approx))
// TODO: support send-to-list?
// TODO: "misc" -> "temp"?
static open_state misc_window(const aniso::subsetT& working_set, bool& /*set_changed*/) {
    bool open = true;
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
    imgui_CenterNextWindow(ImGuiCond_FirstUseEver);

    if (auto window =
            imgui_Window("Misc list", &open, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
        static previewer::configT config{previewer::default_settings};
        static std::optional<aniso::ruleT> rules[10];
        static rec_for_rule rec{}; // TODO: whether to share the same recorder?
        const auto try_accept = [](std::optional<aniso::ruleT>& rule, const pass_rule::passT& pass) {
            if ((!rule.has_value() || check_diff(pass, *rule)) && pass.deliv) {
                rule = *pass.deliv;
                rec.add(*pass.deliv);
            }
        };
        if (rec.empty()) {
            const aniso::ruleT r = aniso::trans_reverse(aniso::game_of_life());
            rules[0] = r;
            rec.add(r);
        }

        if (double_click_button_small("Dump")) {
            rec.dump(config);
        }
        guide_mode::item_tooltip(rec_for_rule::about_dump);
        ImGui::SameLine();
        config.set("Settings", true /*small*/);

        ImGui::Separator();

        // `_AutoResizeX` doesn't work with `imgui_CenterNextWindow`...
        const int spacing_x = ImGui::GetStyle().ItemSpacing.x + 3;
        const auto child_size = [&]() -> ImVec2 {
            const auto& style = ImGui::GetStyle();
            // const int header_width =
            //    imgui_CalcTextSize("Clear0/1Approx").x + style.FramePadding.x * 6 + style.ItemSpacing.x * 2;
            const int header_width = imgui_CalcTextSize("Clear0/1").x + style.FramePadding.x * 4 + style.ItemSpacing.x;
            const int child_width = 2 * std::max(config.width(), header_width) + spacing_x + style.ScrollbarSize;
            const int child_height = config.height() * 2 + ImGui::GetTextLineHeight() * 2 + style.ItemSpacing.y * 4;
            return ImVec2(child_width, child_height);
        }();

        imgui_FillAvailRect(IM_COL32_GREY(24, 255)); // Child bg.
        if (auto child = imgui_ChildWindow("Page", child_size)) {
            set_scroll_with_up_down();
            constexpr int perline = 2;
            for (int i = 0; auto& rule : rules) {
                const int this_i = i++;
                if (this_i % perline != 0) {
                    ImGui::SameLine(0, spacing_x);
                } else if (this_i != 0) {
                    ImGui::Spacing(); // ImGui::Separator();
                }

                ImGui::BeginGroup();
                ImGui::PushID(this_i);
                ImGui::BeginDisabled(!rule.has_value());
                if (double_click_button_small("Clear")) {
                    rule.reset();
                }
                ImGui::SameLine();
                if (ImGui::SmallButton("0/1##Rev") && rule.has_value()) {
                    const aniso::ruleT r = aniso::trans_reverse(*rule);
                    try_accept(rule, &r);
                }
                if (this_i == 0) {
                    guide_mode::item_tooltip(
                        "Get the 0/1 reversal dual for the rule. That is, for any pattern, [applying the original rule -> flipping all values] has the same effect as [flipping all values -> applying the dual].\n\n"
                        "(If the rule is self-complementary, this will result in the same rule.)");
                }
                if constexpr (0) {
                    // !!TODO: not supported currently, as the effect is very tricky...
                    // (This is mainly useful for fitting a rule to p-set; not quite meaningful in other cases.)
                    // (Ideally should take only one step (instead of dragging to update -> double-clicking)...)
                    ImGui::SameLine();
                    if (double_click_button_small("Approx") && rule.has_value()) {
                        const aniso::ruleT r = aniso::approximate(working_set, *rule);
                        try_accept(rule, &r);
                    }
                    if (this_i == 0) {
                        guide_mode::item_tooltip(
                            "Get a rule in [S] with all groups satisfying the constraints unchanged.\n\n"
                            "(If the rule already belongs to [S], this will result in the same rule.)");
                    }
                }
                ImGui::EndDisabled();
                ImGui::PopID();

                // https://stackoverflow.com/questions/73817020/why-is-there-no-built-in-way-to-get-a-pointer-from-an-stdoptional
                previewer::preview_or_dummy(this_i, config, rule ? &*rule : nullptr);
                if (this_i == 1 && !rule.has_value()) {
                    guide_mode::item_tooltip("Drag a rule here for later use.");
                }
                try_accept(rule, pass_rule::dest());
                ImGui::EndGroup();
            }
        }
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
        imgui_FillAvailRect(IM_COL32_GREY(24, 255)); // Background.

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

    static constexpr const char* about_resizing =
        "Resize the window to change page size; double-click window's resize border to fit the contents.";
};

class target_rule : no_copy {
    rule_with_rec m_rule{};

public:
    operator const aniso::ruleT&() const { return m_rule.get(); }
    const aniso::ruleT& get() const { return m_rule.get(); }

    // TODO: instead of resetting silently, invalidate the rule and update to get_rule() only if actually used?
    void sync(const aniso::subsetT& working_set) {
        if (!m_rule.assigned() || !working_set.contains(m_rule.get())) {
            m_rule.set(working_set.get_rule());
        }
    }

    enum class effectE { None = 0, Same, Diff };
    using enum effectE;
    effectE try_accept(const pass_rule::passT& pass, const aniso::subsetT& working_set) {
        if (check_contains(pass, working_set)) {
            if ((!m_rule.assigned() || check_diff(pass, m_rule.get())) && pass.deliv) {
                m_rule.set(*pass.deliv);
                return Diff;
            } else if (pass.deliv) {
                return Same;
            }
        }
        return None;
    }

    // TODO: whether to enable?
    // Closing the target-rule window will turn off checkbox directly. (Undocumented in UI, but should be obvious enough.)
    static constexpr bool close_from_window = true;

    effectE display(bool& open, const char* title, const previewer::configT& settings,
                    const aniso::subsetT& working_set) {
        assert(open);
        ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
        imgui_CenterNextWindow(ImGuiCond_FirstUseEver);

        const ImGuiWindow* source = GImGui->CurrentWindow;
        effectE effect = None;
        if (auto window = imgui_Window(title, close_from_window ? &open : nullptr,
                                       ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings |
                                           ImGuiWindowFlags_NoFocusOnAppearing)) {
            set_above(source);
            bring_to_front_on_appearing();
            imgui_StrTooltip(
                "(...)",
                "This can be an arbitrary rule in [S]. Drag a rule to the preview window to apply the rule.\n\n"
                "(If [S] changes and no longer contains the rule, this will be reset to 'Default'.)");
            ImGui::SameLine();
            if (double_click_button_small("Dump")) {
                m_rule.rec().dump(settings);
            }
            guide_mode::item_tooltip(rec_for_rule::about_dump);
            // ImGui::SameLine(); // TODO: whether to enable settings in this window?
            // settings.set("Settings", true /*small*/);

            previewer::preview(0, settings, m_rule.get());
            effect = try_accept(pass_rule::dest(), working_set);
        } else { // Collapsed (title bar only).
            set_above(source);
        }
        return effect;
    }
};

// TODO: define as classes...
// TODO: support separate context? e.g. random ~ totalistic & random-access ~ isotropic
static open_state traverse_window(const aniso::subsetT& working_set, bool& set_changed) {
    bool open[2]{true, true}; // (Using separate flags for better sync.)
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
    imgui_CenterNextWindow(ImGuiCond_FirstUseEver);

    static page_adapter adapter{};
    ImGui::SetNextWindowSizeConstraints(adapter.min_req_size, ImVec2(FLT_MAX, FLT_MAX));
    imgui_Window::next_window_titlebar_tooltip = page_adapter::about_resizing;
    if (auto window = imgui_Window("Traverse [S] | [X]", &open[0],
                                   ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoScrollbar)) {
        static target_rule orderer{};
        static std::vector<aniso::ruleT> page{};
        static previewer::configT config{previewer::default_settings};
        if (std::exchange(set_changed, false)) {
            orderer.sync(working_set);
            page.clear();
        }
        if (orderer.display(open[1], "[X]", config, working_set) == target_rule::Diff) {
            page.clear();
        }

        enum roleE { First, Last };
        const auto reset_page = [&](const roleE role, const aniso::ruleT rule /*by value*/, bool dot = false) {
            assert(working_set.contains(rule));
            const auto old_size = page.size();
            const auto old_front = !page.empty() ? page.front() : aniso::ruleT{};
            page.clear();
            page.push_back(rule);

            const auto fill_next = [&] {
                while (page.size() < adapter.page_size) {
                    const auto next = aniso::flatten::next(working_set, orderer, page.back());
                    if (next == page.back()) { // Reaches the end of the sequence.
                        break;
                    }
                    page.push_back(next);
                }
                return page.size() == adapter.page_size;
            };
            const auto fill_prev = [&] {
                std::ranges::reverse(page); // To prevent repeated push-front.
                while (page.size() < adapter.page_size) {
                    const auto prev = aniso::flatten::prev(working_set, orderer, page.back());
                    if (prev == page.back()) {
                        break;
                    }
                    page.push_back(prev);
                }
                std::ranges::reverse(page);
                return page.size() == adapter.page_size;
            };
            if (role == First) {
                if (!fill_next()) {
                    fill_prev();
                }
            } else {
                if (!fill_prev()) {
                    fill_next();
                }
            }
            messenger::dot_if(dot && page.size() == old_size && page.front() == old_front);
        };

        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip(
            "(...)", // TODO: actually [free] groups; not updated as p-set is still experimental in this version.
            "This can iterate through all rules in [S] in the following order: firstly [X], then rules with distance = 1 to it, then 2, 3, ..., up to the largest distance (i.e. the number of groups in [S]).\n\n"
            "\"Dist\" in this window refers to distance to [X]. You can input a distance ('To') to get to the first rule with that distance to [X]. If [S] or [X] changes, the page will be cleared automatically.\n\n"
            "(This is mainly useful for sets with only a few groups. For larger sets, 'Random' and 'Edit-rule' may work better.)");
        //"(Some small subsets include self-complementary totalistic rules ('0v1 & Tot'), inner-totalistic rules ('Tot(+s)'), isotropic von-Neumann rules ('All & Jvn') and so on.)"
        ImGui::SameLine();
        switch (sequence::seq("<|", "<##Prev", ">##Next", "|>")) {
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
        config.set("Settings");
        ImGui::SameLine();
        menu_like_popup::button("To");
        menu_like_popup::popup([&] {
            static input_int input_dist{};
            if (ImGui::IsWindowAppearing()) {
                input_dist.clear();
                ImGui::ActivateItemByID(ImGui::GetID("##Dist"));
            }

            ImGui::AlignTextToFramePadding();
            imgui_Str("Dist ~ ");
            ImGui::SameLine(0, 0);
            ImGui::SetNextItemWidth(imgui_CalcButtonSize("Max:0000").x);
            if (const auto dist = input_dist.input(5, "##Dist", std::format("Max:{}", working_set.free_k()).c_str())) {
                // X check_diff().
                reset_page(First, aniso::flatten::first_d(working_set, orderer, *dist), true);
            };
        });
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
        rclick_popup::for_text([] {
            ImGui::BeginDisabled(page.empty());
            if (ImGui::Selectable("Clear")) {
                page.clear();
                page.shrink_to_fit();
            }
            ImGui::EndDisabled();
        });

        ImGui::Separator();

        if (adapter.try_resize(config.size_imvec()) && !page.empty()) {
            reset_page(First, page.front());
        }
        adapter.display([&](const int j) {
            assert(j >= 0);
            previewer::preview_or_dummy(j, config, j < page.size() ? &page[j] : nullptr);
            if (j == 0) {
                if (page.empty()) {
                    guide_mode::item_tooltip("Drag a rule here to get to its position.");
                }
                if (const auto pass = pass_rule::dest(); check_contains(pass, working_set) &&
                                                         (page.empty() || check_diff(pass, page.front())) &&
                                                         pass.deliv) {
                    reset_page(First, *pass.deliv, true /*still needed*/);
                }
            }
        });
    }
    return {open[0] && open[1]};
}

static open_state random_rule_window(const aniso::subsetT& working_set, bool& set_changed) {
    bool open[2]{true, true};
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
    imgui_CenterNextWindow(ImGuiCond_FirstUseEver);

    static page_adapter adapter{};
    ImGui::SetNextWindowSizeConstraints(adapter.min_req_size, ImVec2(FLT_MAX, FLT_MAX));
    imgui_Window::next_window_titlebar_tooltip = page_adapter::about_resizing;
    if (auto window = imgui_Window("Random rules | [Y]", &open[0],
                                   ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoScrollbar)) {
        static target_rule target{};
        static previewer::configT config{previewer::default_settings};
        if (std::exchange(set_changed, false)) {
            target.sync(working_set);
        }
        target.display(open[1], "[Y]", config, working_set);

        static bool exact_mode = false;
        static double rate = 0.29;
        // const int c_group = working_set->k();
        const int c_free = working_set.free_k();
        int free_dist = std::round(rate * c_free); // Intended distance.

        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip(
            "(...)",
            "When you are at the last page, '>>' can generate random rules in [S] with specified distance around/exactly to [Y].\n\n"
            "The generated rules are stored and can be accessed using '</>>'. (They won't be cleared automatically.)");
        // "(Unlike 'Traverse', nothing will happen immediately when [S] or [Y] is updated, as they only affect how to generate new rules.)"
        ImGui::SameLine();
        imgui_RadioButton("Around", &exact_mode, false);
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        imgui_RadioButton("Exactly", &exact_mode, true);
        ImGui::SameLine();
        ImGui::SetNextItemWidth(std::floor(item_width() * 0.9));
        // TODO: show different str if has-lock? "(Free) %d"
        if (imgui_StepSliderInt::fn("##Dist", &free_dist, 0, c_free) && c_free != 0) {
            rate = double(free_dist) / c_free;
            assert(std::round(rate * c_free) == free_dist);
        }

        static std::vector<aniso::compressT> rules{};
        static int page_no = 0;

        const auto calc_page = [&]() -> int { return (rules.size() + adapter.page_size - 1) / adapter.page_size; };
        const auto last_page = [&]() -> int { return rules.empty() ? 0 : calc_page() - 1; }; // ]
        assert(0 <= page_no && page_no <= last_page());
        const auto set_page = [&](const int page) { page_no = std::clamp(page, 0, last_page()); };
        const auto set_next_page = [&] {
            if (page_no < last_page()) {
                ++page_no;
                return;
            }

            const int count = (1 + (rules.size() / adapter.page_size)) * adapter.page_size - rules.size();
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

        // ImGui::SameLine();
        switch (sequence::seq("<|", "<##Prev", ">>##Next", "|>")) {
            case 0: set_page(0); break;
            case 1: set_page(page_no - 1); break;
            case 2: set_next_page(); break;
            case 3: set_page(INT_MAX); break;
        }

        ImGui::SameLine();
        config.set("Settings");
        ImGui::SameLine();
        if (!rules.empty()) {
            // TODO: will this be confusing when the page is resized?
            ImGui::Text("Pages:%d At:%d", calc_page(), page_no + 1);
        } else {
            ImGui::Text("Pages:%d At:N/A", calc_page());
        }
        rclick_popup::for_text([] {
            ImGui::BeginDisabled(rules.empty());
            if (ImGui::Selectable("Clear")) {
                rules.clear();
                rules.shrink_to_fit();
                page_no = 0;
            }
            ImGui::EndDisabled();
        });

        ImGui::Separator();

        // TODO: reconsider page-resized logic (seeking to the last page may still be confusing).
        if (adapter.try_resize(config.size_imvec())) {
            page_no = last_page();
        }
        adapter.display([&](const int j) {
            const int r = page_no * adapter.page_size + j;
            assert(r >= 0);
            previewer::preview_or_dummy(j, config, r < rules.size() ? &rules[r] : nullptr);
        });
    }
    return {open[0] && open[1]};
}

[[maybe_unused]] static open_state capture_still_life();

// TODO: refactor... (should not be pointer...)
static subset_selector* select_working_ptr = nullptr;

// TODO: show whether belongs to the working set && dist to the observer?
void previewer::_show_belongs(const aniso::ruleT& rule) {
    if (select_working_ptr) {
        select_working_ptr->show_belongs(rule);
    }
}

void load_capture(const aniso::ruleT& r, const aniso::lockT& l) {
    if (select_working_ptr) {
        select_working_ptr->load_capture(r, l);
    }
}

void edit_rule(frame_main_token) {
    static test_appearing appearing{};
    appearing.update();

    // Select working set ([S]).
    static subset_selector select_working{&aniso::subsets.native_isotropic};
    select_working_ptr = &select_working;
    {
        static bool collapse = false;
        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip("(...)", subset_selector::about);
        ImGui::SameLine();
        imgui_Str("[S]");
        if (!collapse) {
            guide_mode::item_tooltip("Drag a rule here to select all sets containing the rule.");
            if (const auto pass = pass_rule::dest(); pass.deliv) {
                const bool updated = select_working.match(*pass.deliv);
                messenger::dot_if(!updated);
            }
        }
        ImGui::SameLine();
        ImGui::Checkbox("Collapse", &collapse);
        if constexpr (debug_mode) {
            ImGui::SameLine();
            menu_like_popup::small_button("Exp1");
            menu_like_popup::popup([] { select_working.debug_capture_in_popup(); });
            ImGui::SameLine();
            static bool show_exp_2 = false;
            ImGui::Checkbox("Exp2", &show_exp_2);
            if (show_exp_2) {
                capture_still_life().reset_if_closed(show_exp_2);
            }
        }

        if (!collapse) {
            ImGui::Separator();
            select_working.select({.rule = pass_rule::peek(), .select = true, .tooltip = true});
        } else {
            ImGui::SameLine();
            menu_like_popup::button("Select");
            menu_like_popup::popup([] { select_working.select({.select = true, .tooltip = true}); });
        }
    }
    static std::array<bool, 6> set_changed_n{};
    const aniso::subsetT& working_set = select_working.get();
    if (select_working.set_changed_since_last_check()) {
        set_changed_n.fill(true);
    }

    ImGui::Separator();

    // Select observing rule ([R]).
    static rule_selector select_rule;
    {
        if (std::exchange(set_changed_n[0], false)) {
            select_rule.sync(working_set);
        }

        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip("(...)", rule_selector::about);
        ImGui::SameLine();
        select_rule.select(working_set);
    }
    const aniso::ruleT& observer = select_rule.get();

    ImGui::Separator();

    static bool show_random_access = false;
    // appearing.reset_if_appearing(show_random_access);
    static bool group_table_reset_scroll = false;
    bool show_random_access_window_open = true; // (Delay close effect for better sync.)
    static target_rule target{};
    static previewer::configT config{previewer::default_settings};
    {
        static bool show_misc = false;
        appearing.reset_if_appearing(show_misc);
        ImGui::Checkbox("Misc", &show_misc);
        guide_mode::item_tooltip("Store rules temporarily for misc use.");
        if (show_misc) {
            misc_window(working_set, set_changed_n[1]).reset_if_closed(show_misc);
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
            traverse_window(working_set, set_changed_n[2]).reset_if_closed(show_trav);
        }
    }
    ImGui::SameLine();
    {
        static bool show_rand = false;
        appearing.reset_if_appearing(show_rand);
        ImGui::Checkbox("Random", &show_rand);
        guide_mode::item_tooltip("Get random rules in [S].\n\n"
                                 "(This is mainly useful for large sets.)");
        if (show_rand) {
            random_rule_window(working_set, set_changed_n[3]).reset_if_closed(show_rand);
        }
    }
    ImGui::SameLine();
    {
        const bool show_random_access_old = show_random_access;
        ImGui::Checkbox("Edit-rule", &show_random_access);
        guide_mode::item_tooltip("Edit rules in [S] (by flipping values).\n\n"
                                 "(This is mainly useful for large sets.)");
        random_access_status::update();
        check_diff_no_msg = !show_random_access;
        if (target.try_accept(pass_rule::dest(/*accept drop if*/ !show_random_access, random_access_status::rule_id),
                              working_set) != target_rule::None) {
            show_random_access = true;
        }
        check_diff_no_msg = false;
        if (show_random_access != show_random_access_old) {
            group_table_reset_scroll = true;
        }
        if (show_random_access) {
            if (std::exchange(set_changed_n[4], false)) {
                target.sync(working_set);
            }
            random_access_status::begin_disabled();
            target.display(show_random_access_window_open, "[Z]", config, working_set);
            random_access_status::end_disabled();

            // TODO: use (?) or (...)? (It's getting unclear which is for which...)
            ImGui::SameLine();
            imgui_StrTooltip(
                "(?)",
                "The table displays all rules with distance = 1 to [Z] in [S].\n\n"
                "You can update [Z] either by dragging a rule to it, or clicking the group buttons. By clicking a button, you will flip [Z]'s values for that group. (Click the same button to flip back.)");
            ImGui::SameLine();
            config.set("Settings");
        }
    }

    // TODO: there should finally be one single if-show_random_access scope...
    // const bool working_contains = show_random_access /*workaround*/ && working_set.contains(target);
    assert_implies(show_random_access, working_set.contains(target));

    enum class displayE { Observer, Target, Comp };
    static displayE disp_mode = displayE::Observer;
    bool all_free = true;
    {
        const int c_group = working_set->k();
        const int c_free = working_set.free_k();
        ImGui::AlignTextToFramePadding();
        if (c_group == c_free) {
            ImGui::Text("Groups:%d", c_group);
        } else {
            all_free = false;
            ImGui::Text("(Free) groups:%d/%d", c_free, c_group); // TODO: improve...
        }
    }
    {
        using enum displayE;
        if (!show_random_access) {
            disp_mode = Observer;
        } else if (disp_mode == Observer) {
            disp_mode = Target;
        }

        // TODO: simplify if possible...
        static constexpr std::pair<displayE, const char*> terms[]{
            {Observer, "R##disp"}, {Target, "Z##disp"}, {Comp, "C##disp"}};
        float spacing = imgui_ItemSpacingX() * 3;
        for (const auto& [mode, label] : terms) {
            ImGui::SameLine(0, std::exchange(spacing, imgui_ItemInnerSpacingX()));
            const bool selectable = !show_random_access ? mode == Observer : mode != Observer;
            ImGui::BeginDisabled(!selectable);
            imgui_RadioButton(label, &disp_mode, mode);
            ImGui::EndDisabled();
            if (!selectable && mode != Observer) {
                imgui_ItemTooltip("For 'Edit-rule'.");
            }
        }
        ImGui::SameLine();
        imgui_StrTooltip("(?)", [] {
            ImGui::PushStyleVarY(ImGuiStyleVar_ItemSpacing, 0);
            imgui_StrPair("R : ", "Display values of [R] (value ~ 0/1).");
            imgui_StrPair("Z : ", "Display values of [Z] (for 'Edit-rule').");
            imgui_StrPair("C : ", "Compare [Z] with [R] (same ~ O, different ~ I).");
            // "For example, when [R] ~ 'Identity', O/I can be interpreted as whether cell will flip (I ~ will flip)."
            ImGui::PopStyleVar();
        });
    }
    if (show_random_access) {
        ImGui::SameLine(0, imgui_ItemSpacingX() * 3);
        ImGui::Text("Dist:%d", aniso::distance(working_set, observer, target));
        ImGui::SameLine();
        imgui_StrTooltip("(?)",
                         "Distance between [Z] and [R], i.e. the number of groups where they have different values.");
    }

    ImGui::Separator();

    if (std::exchange(set_changed_n[5], false)) {
        group_table_reset_scroll = true;
    }
    if (std::exchange(group_table_reset_scroll, false)) {
        ImGui::SetNextWindowScroll({0, 0});
    }
    imgui_FillAvailRect(IM_COL32_GREY(24, 255)); // Child bg.
    if (auto child = imgui_ChildWindow("Groups")) {
        set_scroll_with_up_down();
        aniso::ruleT temp_target = show_random_access ? target.get() : aniso::ruleT{};
        const bool comp_mode = disp_mode == displayE::Comp;
        const auto disp = [&] {
            using dispT = aniso::codeT_to<bool>;
            if (comp_mode) {
                assert(show_random_access);
                const aniso::ruleT &a = observer, &b = target.get();
                return dispT::create([&](auto code) { return a[code] != b[code]; });
            } else {
                // (Avoiding `bit_cast`.)
                assert_implies(disp_mode == displayE::Target, show_random_access);
                const aniso::ruleT& r = disp_mode == displayE::Target ? target.get() : observer;
                return dispT::create([&](auto code) { return bool(r[code]); });
            }
        }();
        const char* const labels_disp[2]{comp_mode ? "-O" : "-0", comp_mode ? "-I" : "-1"};
        const char* const labels_disp_from_to[2]{comp_mode ? "-O -> I:" : "-0 -> 1:",
                                                 comp_mode ? "-I -> O:" : "-1 -> 0:"};

        const int button_zoom = std::ceil(ImGui::GetFrameHeight() / 3);
        const int image_zoom = button_zoom;
        constexpr ImVec2 button_padding = {2, 2};
        constexpr float image_padding = 1;
        const auto code_button_with_label = [&](const aniso::codeT code, bool* hov = nullptr) -> bool {
            assert(show_random_access);
            ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, button_padding);
            const bool hit = code_button(code, button_zoom);
            ImGui::PopStyleVar();
            if (hov) {
                *hov = imgui_IsItemHoveredForTooltip();
            }
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            imgui_AddCursorPosY((ImGui::GetItemRectSize().y - ImGui::GetTextLineHeight()) / 2); // Align vertically.
            imgui_Str(labels_disp_from_to[disp[code]]);
            return hit;
        };
        const auto code_image_with_label = [&](const aniso::codeT code, bool* hov = nullptr) {
            ImGui::PushStyleColor(ImGuiCol_Border, ImVec4(0.5, 0.5, 0.5, 1));
            ImGui::PushStyleVar(ImGuiStyleVar_ImageBorderSize, image_padding);
            code_image(code, image_zoom);
            ImGui::PopStyleVar();
            ImGui::PopStyleColor();
            if (hov) {
                *hov = imgui_IsItemHoveredForTooltip();
            }
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            imgui_AddCursorPosY((ImGui::GetItemRectSize().y - ImGui::GetTextLineHeight()) / 2); // Align vertically.
            imgui_Str(labels_disp[disp[code]]);
        };
        const int group_size_x = [&]() -> int {
            if (show_random_access) {
                int size = button_padding.x * 2 + 3 * button_zoom; // button-size
                size += imgui_ItemInnerSpacingX() + imgui_CalcTextSize(labels_disp_from_to[0]).x;
                return std::max(size, config.width());
            } else {
                return image_padding * 2 + 3 * image_zoom + imgui_ItemInnerSpacingX() +
                       imgui_CalcTextSize(labels_disp[0]).x;
            }
        }();
        const int spacing_x = ImGui::GetStyle().ItemSpacing.x + (show_random_access ? 3 : 5);
        const int perline = fit_count(ImGui::GetContentRegionAvail().x, group_size_x, spacing_x);

        // TODO: support sorting (partitioning) by values...
        auto groups = working_set->groups_to_vec();
        if (!all_free) {
            // (Free groups come first. (pred() -> true precedes -> false))
            std::ranges::stable_partition(groups, [&](const auto& group) { return !working_set.lock[group[0]]; });
        }
        for (int n = 0; const aniso::groupT& group : groups) {
            const int this_n = n++;
            if (this_n % perline != 0) {
                ImGui::SameLine(0, spacing_x);
            } else if (this_n != 0) {
                ImGui::Spacing(); // ImGui::Separator();
            }

            const aniso::codeT head = group[0];
            const bool locked = working_set.lock[head];
            bool hov = false;
            if (show_random_access) {
                aniso::flip_values_r(group, temp_target);
                // !!TODO: support exploring rules in locked groups...
                // Support flipping locked values directly? (But how should [Z] be specified?)
                // Or support disabling the sorting? (So users can unselect p-set without losing track.)
                ImGui::BeginGroup();
                ImGui::BeginDisabled(locked);
                if (code_button_with_label(head, &hov)) {
                    if (target.try_accept(&temp_target, working_set) != target_rule::Diff) { // Must succeed.
                        assert(false);
                    }
                }
                ImGui::EndDisabled();
                random_access_status::begin_disabled();
                const bool displayed = previewer::preview(/*id ~*/ this_n, config, temp_target);
                random_access_status::end_disabled();
                if (locked && displayed) { // TODO: improve...
                    imgui_ItemRectFilled(IM_COL32_GREY(128, 64 /*48*/));
                }
                ImGui::EndGroup();
                aniso::flip_values_r(group, temp_target); // Restore.
            } else {
                ImGui::BeginDisabled(locked);
                code_image_with_label(head, &hov);
                ImGui::EndDisabled();
            }

            // !!TODO: sometimes noisy; should be able to turn off this tooltip.
            if (hov && ImGui::BeginTooltip()) { // No text wrapping.
                // TODO: whether to use disabled style if locked?
                ImGui::BeginDisabled(locked);
                const int group_size = group.size();
                ImGui::Text("%s:%d", locked ? "(Locked) cases" : "Cases", group_size);
                ImGui::Separator();
                constexpr int perline2 = 8; // -Wshadow
                constexpr int max_to_show = perline2 * 6;
                for (int n2 = 0; const aniso::codeT code : group.first(std::min(group_size, max_to_show))) {
                    if (n2++ % perline2 != 0) {
                        ImGui::SameLine();
                    }
                    code_image_with_label(code);
                }
                if (group_size > max_to_show) {
                    imgui_Str("...");
                }
                ImGui::EndDisabled();
                ImGui::EndTooltip();
            }
        }
    }
    if (show_random_access && !show_random_access_window_open) {
        show_random_access = false;
        group_table_reset_scroll = true;
    }
}

// !!TODO: support in release mode...
static void capture_still_life_impl() {
    enum stateE { Any_background, O, I, O_background, I_background };

    // (Follows `ImGui::Dummy` or `ImGui::InvisibleButton`.)
    static const auto put_col = [](stateE state, bool disabled = false, ImVec2 min = ImGui::GetItemRectMin(),
                                   ImVec2 max = ImGui::GetItemRectMax()) {
        static constexpr ImU32 cols[5]{IM_COL32_GREY(80, 255),      // Any_bg
                                       IM_COL32_BLACK,              // O
                                       IM_COL32_WHITE,              // I
                                       IM_COL32(80, 0, 80, 255),    // O_bg
                                       IM_COL32(200, 0, 200, 255)}; // I_bg
        assert_implies(disabled, state == Any_background);
        auto& drawlist = *ImGui::GetWindowDrawList();
        drawlist.AddRectFilled(min, max, disabled ? IM_COL32_GREY(60, 255) : cols[state]);
        drawlist.AddRect(min, max, IM_COL32_GREY(160, 255));
    };

    constexpr int r = 8;
    // static stateE board[r][r]{/* Any_background... */};
    // Infuriatingly UB due to strict aliasing rule:
    // std::ranges::fill(&board[0][0], &board[0][0] + r * r, Any_background);
    static std::array<stateE, r * r> board_data{/* Any_background... */};
    const auto board = [](int y, int x) -> stateE& { return board_data[y * r + x]; };

    static stateE state_lbutton = I;
    constexpr stateE state_rbutton = Any_background;

    // ImGui::AlignTextToFramePadding();
    imgui_StrTooltip("(...)", [] {
        const auto explain = [](stateE s, const char* desc) {
            ImGui::Dummy(square_size());
            put_col(s);
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            ImGui::AlignTextToFramePadding(); // `Dummy` does not align automatically.
            imgui_Str(": ");
            ImGui::SameLine(0, 0);
            imgui_Str(desc);
        };

        imgui_Str("Left-click a cell to set the value.\n"
                  "Right-click to set to any-background.\n"
                  "Scroll in the board to change the value for left-click.\n\n"
                  "Use 'Apply' to capture the pattern as still life.\n\n");
        explain(O, "Supposed to remain 0.");
        explain(I, "Supposed to remain 1.");
        explain(O_background, "Background 0. (Not part of pattern.)");
        explain(I_background, "Background 1. (Not part of pattern.)");
        explain(
            Any_background,
            "Any background. (Not part of pattern; being surround by this means the pattern can stay unchanged in all cases.)");
    });
    ImGui::SameLine();
    if (double_click_button_small("Example")) {
        static_assert(r == 8);
        const auto old_data = board_data;
        board_data.fill(Any_background);
        for (int y = 0; y < 4; ++y) {
            for (int x = 0; x < 4; ++x) {
                board(y + 2, x + 2) = ((x == 1 || x == 2) && (y == 1 || y == 2)) ? O : I;
            }
        }
        messenger::dot_if(old_data == board_data);
    }
    ImGui::SameLine();
    ImGui::BeginDisabled(std::ranges::all_of(board_data, [](const auto s) { return s == Any_background; }));
    if (double_click_button_small("Clear")) {
        board_data.fill(Any_background);
    }
    ImGui::EndDisabled();
    ImGui::SameLine();
    ImGui::BeginDisabled(std::ranges::none_of(board_data, [](const auto s) { return s == O || s == I; }));
    const bool apply = double_click_button_small("Apply");
    ImGui::EndDisabled();

    ImGui::Separator();

    const ImVec2 cell_button_size = square_size();
    ImGui::InvisibleButton("Board", cell_button_size * ImVec2(r, r),
                           ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
    {
        const ImVec2 button_min = ImGui::GetItemRectMin();
        const bool button_hovered = ImGui::IsItemHovered();
        const ImVec2 mouse_pos = ImGui::GetMousePos(); // Needn't be valid.
        assert_implies(button_hovered, !apply);
        for (int y = 0; y < r; ++y) {
            for (int x = 0; x < r; ++x) {
                const ImVec2 cell_min = button_min + cell_button_size * ImVec2(x, y);
                const ImVec2 cell_max = cell_min + cell_button_size;
                const bool editable = y >= 1 && y < r - 1 && x >= 1 && x < r - 1;
                stateE& state = board(y, x);
                if (editable && button_hovered && ImRect(cell_min, cell_max).Contains(mouse_pos)) {
                    if (ImGui::IsMouseDown(ImGuiMouseButton_Right)) {
                        state = state_rbutton;
                    } else if (ImGui::IsMouseDown(ImGuiMouseButton_Left)) {
                        state = state_lbutton;
                    }
                }
                put_col(state, !editable /*-> disabled*/, cell_min, cell_max);
            }
        }
        if (button_hovered) {
            if (imgui_MouseScrollingDown()) {
                state_lbutton = (stateE)std::min((int)I_background, state_lbutton + 1);
            } else if (imgui_MouseScrollingUp()) {
                state_lbutton = (stateE)std::max((int)O, state_lbutton - 1);
            }
        }
    }

    ImGui::SameLine();
    ImGui::BeginGroup();
    for (const stateE s : {O, I, O_background, I_background}) {
        ImGui::PushID(s);
        imgui_RadioButton("##State", &state_lbutton, s);
        ImGui::PopID();
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        ImGui::Dummy(square_size());
        put_col(s);
    }
    ImGui::EndGroup();

    if (apply && select_working_ptr) {
        aniso::partialT p{};
        for (int y = 1; y < r - 1; ++y) {
            for (int x = 1; x < r - 1; ++x) {
                if (!(board(y, x) == O || board(y, x) == I)) {
                    continue;
                }

                // For example:
                //  O   O_b  I                 001       001
                // [Any] O   O  will set both [0]00 and [1]00
                //  I_b  I  I_b                111       111
                for (const auto code : aniso::each_code) {
                    const auto imbue = [](aniso::cellT& c, stateE state) {
                        if (state == O || state == O_background) {
                            c = {0};
                        } else if (state == I || state == I_background) {
                            c = {1};
                        }
                    };

                    aniso::situT situ = aniso::decode(code);
                    imbue(situ.q, board(y - 1, x - 1));
                    imbue(situ.w, board(y - 1, x));
                    imbue(situ.e, board(y - 1, x + 1));
                    imbue(situ.a, board(y, x - 1));
                    imbue(situ.s, board(y, x));
                    imbue(situ.d, board(y, x + 1));
                    imbue(situ.z, board(y + 1, x - 1));
                    imbue(situ.x, board(y + 1, x));
                    imbue(situ.c, board(y + 1, x + 1));
                    p.set(aniso::encode(situ), aniso::cellT(board(y, x) == O ? 0 : 1));
                }
            }
        }
        select_working_ptr->load_capture(p.rule, p.lock);
        // board_data.fill(Any_background); // TODO: whether to clear?
    }
}

static open_state capture_still_life() {
    bool open = true;
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
    imgui_CenterNextWindow(ImGuiCond_FirstUseEver);
    if (auto window = imgui_Window("Capture (still life)", &open,
                                   ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
        capture_still_life_impl();
    }
    return {open};
}
