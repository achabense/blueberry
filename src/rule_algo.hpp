#pragma once

#include <ranges>

#include "rule.hpp"

namespace aniso {
    using diffT = codeT::map_to<bool, 3>;

    inline diffT operator^(const ruleT& a, const ruleT& b) {
        diffT diff;
        for (const codeT code : each_code) {
            diff[code] = a[code] != b[code];
        }
        return diff;
    }

    // Equivalence relation for codeT ({0...511}), in the form of union-find set.
    class equivT {
        mutable codeT::map_to<codeT> m_par;

    public:
        equivT() {
            for (const codeT code : each_code) {
                m_par[code] = code;
            }
        }

        codeT head_for(codeT c) const {
            if (m_par[c] == c) {
                return c;
            } else {
                return m_par[c] = head_for(m_par[c]);
            }
        }

        bool has_eq(codeT a, codeT b) const { return head_for(a) == head_for(b); }
        bool has_eq(const equivT& other) const {
            return for_each_code_all_of([&](codeT code) { //
                return has_eq(code, other.m_par[code]);
            });
        }

        void add_eq(codeT a, codeT b) { m_par[head_for(a)] = head_for(b); }
        void add_eq(const equivT& other) {
            for (const codeT code : each_code) {
                add_eq(code, other.m_par[code]);
            }
        }
    };

    static_assert(std::is_trivially_copyable_v<equivT>);

    inline equivT operator|(const equivT& a, const equivT& b) {
        equivT c = a;
        c.add_eq(b);
        return c;
    }

    using groupT = std::span<const codeT>;

    inline void flip_values_r(const groupT& group, ruleT& r) {
        for (const codeT c : group) {
            r[c] = !r[c];
        }
    }

    [[nodiscard]] inline ruleT flip_values_v(const groupT& group, const ruleT& r) {
        ruleT rule = r;
        flip_values_r(group, rule);
        return rule;
    }

    inline bool all_same_or_different(const groupT& group, const ruleT& a, const ruleT& b) {
        const bool v = a[group[0]] == b[group[0]];
        return std::ranges::all_of(group.subspan(1), [&, v](const codeT code) { //
            return v == (a[code] == b[code]);
        });
    }

    [[deprecated]] inline bool all_same_or_different(const groupT& group, const ruleT& a, const ruleT& b,
                                                     const lockT& l) {
        for (int v = -1; const codeT c : group) {
            if (l[c]) {
                if (v == -1) {
                    v = a[c] == b[c];
                } else if (v != (a[c] == b[c])) {
                    return false;
                }
            }
        }
        return true;
    }

    class partitionT {
        int m_k{}; // Number of groups.

        // Map codeT of the same group to the same integer ∈ [0, m_k).
        codeT::map_to<int16_t> m_map{};

        // (Permutation of all codeT.)
        // Store codeT of the same group consecutively to provide span.
        std::array<codeT, 512> m_data{};

        struct group_pos {
            int16_t pos, size;
            groupT get(const decltype(m_data)& data) const { return groupT(data.data() + pos, size); }
        };
        std::vector<group_pos> m_groups{};

    public:
        // ~ "user-declared" to avoid implicit moving (to avoid `m_groups` being emptied):
        partitionT(const partitionT&) = default;
        partitionT& operator=(const partitionT&) = default;

        /*implicit*/ partitionT(const equivT& eq) { assign(eq); }
        partitionT& operator=(const equivT& eq) = delete; // -> `assign(eq)`

        groupT group_for(codeT code) const { return m_groups[m_map[code]].get(m_data); }
        // <-> `group_for(code)[0]`:
        codeT head_for(codeT code) const { return m_data[m_groups[m_map[code]].pos]; }

        int k() const { return m_k; }
        auto groups() const { //
            return std::views::transform(m_groups, [&](const group_pos& pos) { return pos.get(m_data); });
        }
        auto heads() const { //
            return std::views::transform(m_groups, [&](const group_pos& pos) { return m_data[pos.pos]; });
        }

        bool has_group(const groupT& group) const {
            const int v = m_map[group[0]];
            return std::ranges::all_of(group.subspan(1), [&, v](const codeT code) { //
                return v == m_map[code];
            });
        }

        friend bool operator==(const partitionT& a, const partitionT& b) { return a.m_map == b.m_map; }

        void assign(const equivT& eq) {
            m_k = 0;
            m_map.fill(-1);
            for (const codeT code : each_code) {
                const codeT head = eq.head_for(code);
                if (m_map[head] == -1) {
                    m_map[head] = m_k++;
                }
                m_map[code] = m_map[head];
            }
            // m_k is now the number of groups in the partition.

            std::vector<int16_t> count(m_k, 0);
            for (const codeT code : each_code) {
                ++count[m_map[code]];
            }

            std::vector<int16_t> pos(m_k, 0);
            for (int j = 1; j < m_k; ++j) {
                pos[j] = pos[j - 1] + count[j - 1];
            }

            m_groups.resize(m_k);
            for (int j = 0; j < m_k; ++j) {
                m_groups[j] = {pos[j], count[j]};
            }

            for (const codeT code : each_code) {
                int j = m_map[code];
                m_data[pos[j]++] = code;
            }
        }
    };

    inline void add_eq(equivT& eq, const groupT& group) {
        const codeT head = group[0];
        for (const codeT code : group.subspan(1)) {
            eq.add_eq(head, code);
        }
    }

    inline void add_eq(equivT& a, const equivT& b) = delete; // -> `a.add_eq(b)`

    inline void add_eq(equivT& eq, const partitionT& p) {
        for (const groupT& group : p.groups()) {
            add_eq(eq, group);
        }
    }

    inline equivT operator|(const partitionT& a, const partitionT& b) {
        equivT eq{};
        add_eq(eq, a);
        add_eq(eq, b);
        return eq;
    }

    // A rule belongs to `subsetT` s = (r, p) iff it's all-same or all-different than (r) for each group in (p).
    // (As a result, any rule in the set can equally serve as (r); there is no difference which rule is used.)
    // It can be proven that the intersection of `subsetT` (if not empty) is also `subsetT` (see below).
    struct subsetT {
        ruleT rule;
        partitionT p;

        const partitionT* operator->() const { return &p; }
        bool contains(const ruleT& r) const {
            return std::ranges::all_of(p.groups(),
                                       [&](const groupT& group) { return all_same_or_different(group, rule, r); });
        }
        bool includes(const subsetT& other) const {
            return contains(other.rule) && // this.p is refinement of other.p ~>
                   std::ranges::all_of(p.groups(), [&](const groupT& group) { return other.p.has_group(group); });
        }

        // <-> `a.includes(b) && b.includes(a)`:
        friend bool operator==(const subsetT& a, const subsetT& b) { return a.contains(b.rule) && a.p == b.p; }

        static subsetT universal() { return {.rule{}, .p{equivT{}}}; }
    };

    // Look for a rule that belongs to both a and b.
    // The result belongs to both a and b iff such a rule really exists.
    inline ruleT common_rule_unchecked(const subsetT& a, const subsetT& b) {
        ruleT common{};
        codeT::map_to<bool> assigned{};

        // Assign values according to equivalence relations, without checking for conflicts.
        codeT::map_to<bool> a_side_checked{}, b_side_checked{}; // To reduce time-complexity.
        const auto try_assign = [&](const codeT code, const cellT v, const auto& self) -> void {
            assert(!assigned[code]);
            assigned[code] = true;
            common[code] = v;
            if (!std::exchange(a_side_checked[a->head_for(code)], true)) {
                const bool same_as_a_side = a.rule[code] == v;
                for (const codeT c : a->group_for(code)) {
                    if (!assigned[c]) {
                        self(c, same_as_a_side ? a.rule[c] : !a.rule[c], self);
                    }
                }
            }
            if (!std::exchange(b_side_checked[b->head_for(code)], true)) {
                const bool same_as_b_side = b.rule[code] == v;
                for (const codeT c : b->group_for(code)) {
                    if (!assigned[c]) {
                        self(c, same_as_b_side ? b.rule[c] : !b.rule[c], self);
                    }
                }
            }
        };

        for_assertion(const partitionT par_both = a.p | b.p);
        for (const codeT code : each_code) {
            if (!assigned[code]) {
                // Suppose there really exists r (that belongs to both a and b), then by flipping all its values in any group of (a.p | b.p), the result must still belong to both a and b, as any group of (a.p | b.p) corresponds to one or multiple (entire) groups of (a.p) or (b.p).
                // So there doesn't exist a case where only one value is possible in the intersection - when no cases in a group are assigned, for any specific case there is no restriction for the value (either {0} or {1} will be ok; always using {0} here), and by deciding the value, the values for other cases in the group (of (a.p | b.p)) are decided transitively.
                assert(std::ranges::none_of(par_both.group_for(code), [&](codeT c) { return assigned[c]; }));
                try_assign(code, {0}, try_assign);
                assert(std::ranges::all_of(par_both.group_for(code), [&](codeT c) { return assigned[c]; }));
            }
        }

        assert(for_each_code_all_of([&](codeT code) { return assigned[code]; }));
        return common;
    }

    // The intersection of any two subsetT a and b (a & b) (if not empty) must be just another subsetT:
    // Suppose `common_rule_unchecked` find a common rule (r), so (a & b) is not empty, then there is c = (r, a.p | b.p) = (a & b):
    // 1. Any rule in c can be gotten by flipping all values from (r) for some groups in (a.p | b.p). As every group in (a.p | b.p) corresponds to one or multiple (entire) groups in (a.p) amd (b.p), the rule also belongs to both (a) and (b) (so (a & b)). So c belongs to (a & b).
    // 2. For any rule in (a & b), for any case, its value is either same or different than (r). Due to equivalence relation, if a rule belongs to both (a) and (b), the sameness applies transitively to the entire group in (a.p | b.p). As a result, the rule can be flipped from (r) by groups of (a.p | b.p). So (a & b) belongs to c.

    inline bool has_common(const subsetT& a, const subsetT& b) {
        if (a.contains(b.rule) || b.contains(a.rule)) {
            return true;
        }
        const ruleT common = common_rule_unchecked(a, b);
        return a.contains(common) && b.contains(common);
    }

    // If `has_common(a, b)`, return their intersection; otherwise, the result is (safe but) meaningless.
    inline subsetT operator&(const subsetT& a, const subsetT& b) {
        const ruleT common = a.contains(b.rule)   ? b.rule //
                             : b.contains(a.rule) ? a.rule
                                                  : common_rule_unchecked(a, b);
        assert(a.contains(common) && b.contains(common));
        return subsetT{.rule = common, .p = a.p | b.p};
    }

    inline int distance(const subsetT& subset, const ruleT& a, const ruleT& b) {
        assert(subset.contains(a) && subset.contains(b));
        int dist = 0;
        for (const codeT code : subset->heads()) {
            dist += a[code] != b[code];
        }
        return dist;
    }

    inline void approximate_r(const subsetT& subset, ruleT& rule) {
        for (const groupT& group : subset->groups()) {
            if (!all_same_or_different(group, subset.rule, rule)) {
                for (const codeT code : group) {
                    rule[code] = subset.rule[code];
                }
            }
        }
    }

    [[nodiscard]] inline ruleT approximate_v(const subsetT& subset, const ruleT& rule) {
        ruleT r = rule;
        approximate_r(subset, r);
        return r;
    }

    inline ruleT transform(const subsetT& subset, const ruleT& rel, const ruleT* pos,
                           const func_ref<void(bool*, bool*)> fn) {
        assert(subset.contains(rel));
        assert_implies(pos, subset.contains(*pos));
        assert(subset->k() <= 512);
        std::array<bool, 512> diff{}; // vector-bool.
        if (pos) {
            for (int j = 0; const codeT code : subset->heads()) {
                diff[j++] = rel[code] != (*pos)[code];
            }
        }
        fn(diff.data(), diff.data() + subset->k());

        ruleT rule = rel;
        for (int j = 0; const groupT& group : subset->groups()) {
            if (diff[j++]) {
                flip_values_r(group, rule);
            }
        }
        assert(subset.contains(rule));
        return rule;
    }

    inline ruleT randomize_c(const subsetT& subset, const ruleT& rel, std::mt19937& rand, int count) {
        return transform(subset, rel, nullptr, [&rand, count](bool* begin, bool* end) {
            const int c = std::clamp(count, 0, int(end - begin));
            std::fill_n(begin, c, bool(1));
            std::fill(begin + c, end, bool(0));
            std::shuffle(begin, end, rand);
        });
    }

    inline ruleT randomize_p(const subsetT& subset, const ruleT& rel, std::mt19937& rand, double p) {
        return transform(subset, rel, nullptr, [&rand, p](bool* begin, bool* end) {
            std::bernoulli_distribution dist(std::clamp(p, 0.0, 1.0));
            std::generate(begin, end, [&] { return dist(rand); });
        });
    }

    struct seq_mixed : no_create {
        static ruleT first(const subsetT& subset, const ruleT& rel) {
            return transform(subset, rel, nullptr, [](bool* begin, bool* end) { std::fill(begin, end, bool(0)); });
        }

        static ruleT last(const subsetT& subset, const ruleT& rel) {
            return transform(subset, rel, nullptr, [](bool* begin, bool* end) { std::fill(begin, end, bool(1)); });
        }

        // The result will be the first rule with distance = n to `rel` in the sequence.
        static ruleT seek_n(const subsetT& subset, const ruleT& rel, int n) {
            return transform(subset, rel, nullptr, [n](bool* begin, bool* end) {
                const int c = std::clamp(n, 0, int(end - begin));
                std::fill_n(begin, c, bool(1));
                std::fill(begin + c, end, bool(0));
            });
        }

        static ruleT next(const subsetT& subset, const ruleT& rel, const ruleT& pos) {
            return transform(subset, rel, &pos, [](bool* begin, bool* end) {
                if (!std::next_permutation(begin, end, std::greater<>{})) {
                    // 1100... -> 1110..., or stop at 111... (last())
                    bool* first_0 = std::find(begin, end, bool(0));
                    if (first_0 != end) {
                        *first_0 = 1;
                    }
                }
            });
        }

        static ruleT prev(const subsetT& subset, const ruleT& rel, const ruleT& pos) {
            return transform(subset, rel, &pos, [](bool* begin, bool* end) {
                if (!std::prev_permutation(begin, end, std::greater<>{})) {
                    // ...0111 -> ...0011, or stop at 000... (first())
                    bool* first_1 = std::find(begin, end, bool(1));
                    if (first_1 != end) {
                        *first_1 = 0;
                    }
                }
            });
        }
    };

    // Any-rule ^ rule_all_zero -> diff shows the rule's actual values.
    inline const ruleT rule_all_zero{};
    // Any-rule ^ rule_identity -> diff shows whether the cell will "flip" in each case.
    inline const ruleT rule_identity{make_rule([](codeT c) { return c.get(codeT::bpos_s); })};

    // A mapperT maps each codeT to another codeT.
    // Especially, mapperT{"qweasdzxc"} maps any codeT to the same value.
    class mapperT {
        struct takeT {
            enum tagE : int8_t { O, I, Get, NGet };
            tagE tag;
            codeT::bposE bpos;
            cellT operator()(codeT code) const {
                switch (tag) {
                    case O: return {0};
                    case I: return {1};
                    case Get: return code.get(bpos);
                    default: assert(tag == NGet); return !code.get(bpos);
                }
            }
        };

        takeT q{}, w{}, e{};
        takeT a{}, s{}, d{};
        takeT z{}, x{}, c{};

        // https://stackoverflow.com/questions/67320438/how-to-fail-a-consteval-function
        static void wrong_format() {}

    public:
        codeT operator()(codeT code) const {
            return encode({q(code), w(code), e(code), //
                           a(code), s(code), d(code), //
                           z(code), x(code), c(code)});
        }

        consteval explicit mapperT(const char* str) {
            for (takeT* take : {&q, &w, &e, &a, &s, &d, &z, &x, &c}) {
                // [01], or [qweasdzxc], or ![qweasdzxc].
                if (*str == '0') {
                    ++str;
                    *take = {takeT::O, {}};
                } else if (*str == '1') {
                    ++str;
                    *take = {takeT::I, {}};
                } else {
                    takeT::tagE tag = takeT::Get;
                    if (*str == '!') {
                        ++str;
                        tag = takeT::NGet;
                    }
                    switch (*str++) {
                        case 'q': *take = {tag, codeT::bpos_q}; break;
                        case 'w': *take = {tag, codeT::bpos_w}; break;
                        case 'e': *take = {tag, codeT::bpos_e}; break;
                        case 'a': *take = {tag, codeT::bpos_a}; break;
                        case 's': *take = {tag, codeT::bpos_s}; break;
                        case 'd': *take = {tag, codeT::bpos_d}; break;
                        case 'z': *take = {tag, codeT::bpos_z}; break;
                        case 'x': *take = {tag, codeT::bpos_x}; break;
                        case 'c': *take = {tag, codeT::bpos_c}; break;
                        default: wrong_format();
                    }
                }
            }
            if (*str != '\0') {
                wrong_format();
            }
        }
    };

    // consteval mapperT operator""_mp(const char* str, size_t) { return mapperT(str); }

    static_assert(std::is_trivially_copyable_v<mapperT>);

    // A pair of mapperT defines an equivalence relation.
    inline void add_eq(equivT& eq, const mapperT& a, const mapperT& b) {
        for (const codeT code : each_code) {
            eq.add_eq(a(code), b(code));
        }
    }

    // mp_identity(any code) -> the same code
    inline constexpr mapperT mp_identity("qweasdzxc");

    // The following mappers are defined relative to mp_identity.
    // That is, the effects actually refer to the effects of mapperT_pair{mp_identity, mp_*}.

    // Independence.
    // For example, mp_ignore_q * rule_all_zero -> rules whose values are "independent of q".
    // Note that `mp_ignore_s * rule_all_zero` is different from `mp_ignore_s * rule_identity`.
    inline constexpr mapperT mp_ignore_q("0weasdzxc");
    inline constexpr mapperT mp_ignore_w("q0easdzxc");
    inline constexpr mapperT mp_ignore_e("qw0asdzxc");
    inline constexpr mapperT mp_ignore_a("qwe0sdzxc");
    inline constexpr mapperT mp_ignore_s("qwea0dzxc");
    inline constexpr mapperT mp_ignore_d("qweas0zxc");
    inline constexpr mapperT mp_ignore_z("qweasd0xc");
    inline constexpr mapperT mp_ignore_x("qweasdz0c");
    inline constexpr mapperT mp_ignore_c("qweasdzx0");

    // Native symmetry.
    // For example, rules in `mp_refl_asd * rule_all_zero` can preserve "upside-down" symmetry in all cases.
    inline constexpr mapperT mp_refl_asd("zxc"
                                         "asd"
                                         "qwe"); // '-'
    inline constexpr mapperT mp_refl_wsx("ewq"
                                         "dsa"
                                         "cxz"); // '|'
    inline constexpr mapperT mp_refl_qsc("qaz"
                                         "wsx"
                                         "edc"); // '\'
    inline constexpr mapperT mp_refl_esz("cde"
                                         "xsw"
                                         "zaq"); // '/'
    inline constexpr mapperT mp_C2("cxz"
                                   "dsa"
                                   "ewq"); // 180
    inline constexpr mapperT mp_C4("zaq"
                                   "xsw"
                                   "cde"); // 90 (clockwise)

    // Native totalistic.
    // The `C8` mapper came from misconception of rotational symmetry (not more special than common C4 rules),
    // but can serve to define totalistic rules.
    inline constexpr mapperT mp_C8("aqw"
                                   "zse"
                                   "xcd"); // "45" (clockwise)
    inline constexpr mapperT mp_tot_exc_s("wqe"
                                          "asd"
                                          "zxc"); // swap(q,w); *C8 -> totalistic, excluding s
    inline constexpr mapperT mp_tot_inc_s("qse"
                                          "awd"
                                          "zxc"); // swap(w,s); *C8 -> totalistic, including s

    // * rule_identity -> self-complementary rules.
    // (Where every pair of [code] and [mp_reverse(code)] have the same "flip-ness")
    inline constexpr mapperT mp_reverse("!q!w!e"
                                        "!a!s!d"
                                        "!z!x!c");

    // Hexagonal emulation and emulated symmetry.
    // q w -     q w
    // a s d -> a s d
    // - x c     x c
    inline constexpr mapperT mp_hex_ignore("qw0"
                                           "asd"
                                           "0xc"); // ignore_(e,z)

    inline constexpr mapperT mp_hex_refl_asd("xc0"
                                             "asd"
                                             "0qw"); // swap(q,x), swap(w,c)
    inline constexpr mapperT mp_hex_refl_qsc("qa0"
                                             "wsx"
                                             "0dc"); // swap(a,w), swap(x,d)
    inline constexpr mapperT mp_hex_refl_wsx("dw0"
                                             "csq"
                                             "0xa"); // swap(q,d), swap(a,c)

    inline constexpr mapperT mp_hex_refl_aq("ax0"
                                            "qsc"
                                            "0wd"); // swap(a,q), swap(x,w), swap(c,d)
    inline constexpr mapperT mp_hex_refl_qw("wq0"
                                            "dsa"
                                            "0cx"); // swap(q,w), swap(a,d), swap(x,c)
    inline constexpr mapperT mp_hex_refl_wd("cd0"
                                            "xsw"
                                            "0aq"); // swap(w,d), swap(q,c), swap(a,x)

    inline constexpr mapperT mp_hex_C2("cx0"
                                       "dsa"
                                       "0wq"); // 180
    inline constexpr mapperT mp_hex_C3("xa0"
                                       "csq"
                                       "0dw"); // 120 (clockwise)
    inline constexpr mapperT mp_hex_C6("aq0"
                                       "xsw"
                                       "0cd"); // 60 (clockwise)

    // Hexagonal totalistic.
    inline constexpr mapperT mp_hex_tot_exc_s("wq0"
                                              "asd"
                                              "0xc"); // swap(q,w); *C6 -> totalistic, excluding s
    inline constexpr mapperT mp_hex_tot_inc_s("qs0"
                                              "awd"
                                              "0xc"); // swap(w,s); *C6 -> totalistic, including s

    // It's also valid to emulate hexagonal neighborhood by ignoring "q" and "c".
    // However, the program is not going to support this, as it makes the program more complicated
    // without bringing essentially different discoveries.

    // - w e     w e
    // a s d -> a s d
    // z x -     z x
    [[deprecated]] inline constexpr mapperT mp_hex2_ignore("0we"
                                                           "asd"
                                                           "zx0"); // ignore_(q,c)

    inline constexpr mapperT mp_ignore_qezc("0w0"
                                            "asd"
                                            "0x0");
    inline constexpr mapperT mp_ignore_wadx("q0e"
                                            "0s0"
                                            "z0c");

    // von Neumann emulation.
    // The neighborhood works naturally with native symmetry.
    inline constexpr mapperT mp_jvn_ignore = mp_ignore_qezc;

    inline constexpr mapperT mp_jvn_tot_exc_s("0d0"
                                              "asw"
                                              "0x0"); // swap(w,d); *C4 -> totalistic, excluding s
    inline constexpr mapperT mp_jvn_tot_inc_s("0s0"
                                              "awd"
                                              "0x0"); // swap(w,s); *C4 -> totalistic, including s

    inline subsetT make_subset(std::initializer_list<mapperT> mappers, const ruleT& rule = rule_all_zero) {
        equivT eq{};
        for (const mapperT& m : mappers) {
            add_eq(eq, m, mp_identity);
        }
        return subsetT{.rule = rule, .p = eq};
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_mappers = [] {
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
        };

        inline const testT test_subset_intersection = [] {
            subsetT sc = make_subset({mp_ignore_s}, rule_all_zero) & make_subset({mp_reverse}, rule_identity);

            // (`maskT` used to refer to the discoverd rule.)
            // 2024/1/20 2AM
            // There is NO problem in the algorithm.
            // It's just that, in this situation the maskT has a strong bias, so that it's too easy to generate
            // rules in a certain direction...
            using enum codeT::bposE;
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_q); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_w); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_e); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_a); })));
            assert(!sc.contains(rule_identity));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_d); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_z); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_x); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_c); })));
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    // 0/1-reversal dual.
    inline ruleT trans_reverse(const ruleT& rule) {
        ruleT rev{};
        for (const codeT code : each_code) {
            const codeT code_rev = mp_reverse(code);
            const cellT s = code.get(codeT::bpos_s);
            rev[code_rev] = (rule[code] == s) ? !s : !(!s); // So that ->
            assert((rev[code_rev] == !s) == (rule[code] == s));
        }
        return rev;
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_trans_reverse = [] {
            ruleT rule = make_rule([](auto) { return cellT(testT::rand() & 1); });
            assert(rule == trans_reverse(trans_reverse(rule)));
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    // (Currently not supported in gui, as these can map `hex` rules to `hex2` set.)
    [[deprecated]] inline ruleT trans_left_right(const ruleT& rule) {
        ruleT lr{};
        for (const codeT code : each_code) {
            lr[mp_refl_wsx(code) /* | */] = rule[code];
        }
        return lr;
    }

    [[deprecated]] inline ruleT trans_rotate_90(const ruleT& rule) {
        ruleT ro{};
        for (const codeT code : each_code) {
            ro[mp_C4(code)] = rule[code];
        }
        return ro;
    }

    // !!TODO: (v0.9.9) re-support value constraints in the gui.
    // -> Support intersection with subsetT (should be selectable in the set table).
    // -> Support generating partialT from pattern & support random-access editing in the gui.

    // (Previously `moldT`.)
    // A `partialT` stands for a another kind of MAP subset where, a rule belongs to the set iff it has the same "locked" values.
    // Take the "free" glider in gol for example. During all of its phases, the glider involves only a small subset of all cases (l), and a rule allows for the same glider iff it belongs to partialT(gol,l). (Not taking account of reactions with other patterns e.g. glider collision.)
    struct partialT {
        ruleT rule{};
        lockT lock{};

        void set(codeT code, cellT v) {
            rule[code] = v;
            lock[code] = true;
        }

        bool contains(const ruleT& r) const {
            return for_each_code_all_of([&](codeT code) { //
                return !lock[code] || rule[code] == r[code];
            });
        }

        // Less restrictive than `other`.
        bool includes(const partialT& other) const {
            return for_each_code_all_of([&](codeT code) { //
                return !lock[code] || (other.lock[code] && rule[code] == other.rule[code]);
            });
        }

        friend bool operator==(const partialT& a, const partialT& b) {
            return for_each_code_all_of([&](codeT code) {
                if (a.lock[code] != b.lock[code]) {
                    return false;
                }
                return !a.lock[code] || a.rule[code] == b.rule[code];
            });
        }
    };
} //  namespace aniso
