#pragma once

#include <ranges>

#include "rule.hpp"

namespace aniso {
    inline bool none_locked(const lockT& l) { //
        return for_each_code_all_of([&](const codeT code) { return !l[code]; });
    }

    inline int count_locked(const lockT& l) {
        int c = 0;
        for (const codeT code : each_code) {
            c += l[code];
        }
        return c;
    }

    inline ruleT normalize(const ruleT& r, const lockT& l) { //
        return ruleT::create([&](const codeT code) { return l[code] ? r[code] : cellT{0}; });
    }

    // Equivalence relation for codeT ({0...511}), in the form of union-find set.
    class equivT {
        using parT = codeT_to<codeT>;
        mutable parT m_par = parT::create([](codeT c) { return c; }); // m_par[c] == c

    public:
        codeT head_for(const codeT c) const {
            if (m_par[c] == c) {
                return c;
            } else {
                return m_par[c] = head_for(m_par[c]);
            }
        }

        bool has_eq(const codeT a, const codeT b) const { return head_for(a) == head_for(b); }
        bool has_eq(const equivT& other) const {
            return for_each_code_all_of([&](const codeT code) { //
                return has_eq(code, other.m_par[code]);
            });
        }

        void add_eq(const codeT a, const codeT b) { m_par[head_for(a)] = head_for(b); }
        void add_eq(const equivT& other) {
            for (const codeT code : each_code) {
                add_eq(code, other.m_par[code]);
            }
        }
    };

    static_assert(std::is_trivially_copyable_v<equivT>);

    inline void operator|=(equivT& a, const equivT& b) { a.add_eq(b); }

    inline equivT operator|(const equivT& a, const equivT& b) {
        equivT c = a;
        c |= b;
        return c;
    }

    using groupT = std::span<const codeT>;

    // <-> `std::ranges::all_of(group, pred)`:
    inline bool for_each_code_all_of(const groupT& group, const auto& pred) {
        for (const codeT c : group) {
            if (!pred(c)) {
                return false;
            }
        }
        return true;
    }

    inline void flip_values_r(const groupT& group, ruleT& r) {
        for (const codeT c : group) {
            r[c] = !r[c];
        }
    }

    inline const codeT* find_locked(const groupT& group, const lockT& l, const bool v = true) {
        for (const codeT& c : group) {
            if (l[c] == v) {
                return &c;
            }
        }
        return nullptr;
    }

    inline bool any_locked(const groupT& group, const lockT& l) { return find_locked(group, l); }
    inline bool all_locked(const groupT& group, const lockT& l) { return !find_locked(group, l, false); }
    inline bool none_locked(const groupT& group, const lockT& l) { return !any_locked(group, l); }

    inline void lock_all(const groupT& group, lockT& l, const bool v = true) {
        for (const codeT c : group) {
            l[c] = v;
        }
    }

    inline bool all_same(const groupT& group, const ruleT& a, const ruleT& b) {
        return for_each_code_all_of(group, [&](const codeT code) { //
            return a[code] == b[code];
        });
    }

    inline bool all_same_or_different(const groupT& group, const ruleT& a, const ruleT& b) {
        const bool v = a[group[0]] == b[group[0]];
        return for_each_code_all_of(group.subspan(1), [&, v](const codeT code) { //
            return v == (a[code] == b[code]);
        });
    }

    inline bool all_same_or_different_locked(const groupT& group, const ruleT& a, const ruleT& b, const lockT& l) {
        for (int v = -1; const codeT c : group) {
            if (l[c]) {
                const int eq = a[c] == b[c];
                if (v == -1) {
                    v = eq;
                } else if (v != eq) {
                    return false;
                }
            }
        }
        return true;
    }

    class partitionT {
        int m_k{}; // Number of groups.

        // Map codeT of the same group to the same integer ∈ [0, m_k).
        codeT_to<int16_t> m_map{};

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

        explicit partitionT() { reset(); }
        /*implicit*/ partitionT(const equivT& eq) { assign(eq); }
        partitionT& operator=(const equivT& eq) = delete; // -> `assign(eq)`

        groupT group_for(const codeT code) const { return m_groups[m_map[code]].get(m_data); }
        // <-> `group_for(code)[0]`:
        codeT head_for(const codeT code) const { return m_data[m_groups[m_map[code]].pos]; }

        int k() const { return m_k; }
        auto groups() const { //
            return std::views::transform(m_groups, [&](const group_pos& pos) { return pos.get(m_data); });
        }
        auto heads() const { //
            return std::views::transform(m_groups, [&](const group_pos& pos) { return m_data[pos.pos]; });
        }

        std::vector<groupT> groups_to_vec() const {
            const auto g = groups();
            return std::vector<groupT>(g.begin(), g.end());
        }

        // <-> `std::ranges::all_of(groups(), pred)`:
        bool for_each_group_all_of(const auto& pred) const {
            for (const group_pos& group : m_groups) {
                if (!pred(group.get(m_data))) {
                    return false;
                }
            }
            return true;
        }

        bool has_group(const groupT& group) const {
            const int v = m_map[group[0]];
            return for_each_code_all_of(group.subspan(1), [&, v](const codeT code) { //
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

            build_from_map();
        }

        // <-> `assign(equivT{})`:
        void reset() {
            m_k = 0;
            for (const codeT code : each_code) {
                m_map[code] = m_k++;
            }
            assert(m_k == 512);

            build_from_map();
        }

    private:
        void build_from_map() {
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

    ANISO_DECLARE_TEST(test_partition_defl);

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

    inline void operator|=(partitionT& a, const partitionT& b) { a.assign(a | b); }

    // A rule belongs to `subsetT` s = (r, p) iff it's all-same or all-different than (r) for each group in (p).
    // (As a result, any rule in the set can equally serve as (r); there is no difference which rule is used.)
    // It can be proven that the intersection of `subsetT` (if not empty) is also `subsetT` (see below).
    struct subsetT {
        ruleT rule{};
        partitionT p{};
        // static constexpr lockT lock{};

        const partitionT* operator->() const { return &p; }
        bool contains(const ruleT& r) const {
            return p.for_each_group_all_of([&](const groupT& group) { return all_same_or_different(group, rule, r); });
        }
        bool includes(const subsetT& other) const {
            // return contains(other.rule) && // this.p is refinement of other.p ~>
            //        p.for_each_group_all_of([&](const groupT& group) { return other.p.has_group(group); });
            return p.for_each_group_all_of([&](const groupT& group) { //
                return all_same_or_different(group, rule, other.rule) && other.p.has_group(group);
            });
        }

        // <-> `a.includes(b) && b.includes(a)`:
        friend bool operator==(const subsetT& a, const subsetT& b) { return a.contains(b.rule) && a.p == b.p; }

        bool is_universal() const { return p.k() == 512; }
        void reset() {
            rule.reset();
            p.reset();
        }

        // (To be compatible with `subsetT_v2`.)
        const ruleT& get_rule() const { return rule; }
        int free_k() const { return p.k(); }
        auto free_heads() const { return p.heads(); }
        auto free_groups() const { return p.groups(); }
    };

    // Look for a rule that belongs to both a and b.
    // The result belongs to both a and b iff such a rule really exists.
    // (x_lock: if provided, the result must have same values as x.rule in the locked cases.)
    inline ruleT common_rule_unchecked(const subsetT& a, const subsetT& b, //
                                       const lockT* a_lock = nullptr, const lockT* b_lock = nullptr) {
        ruleT common{};
        codeT_to<bool> assigned{};

        // Assign values according to equivalence relations, without checking for conflicts.
        codeT_to<bool> a_side_checked{}, b_side_checked{}; // To reduce time-complexity.
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

#ifdef YDEBUG
        const partitionT p_ab = a.p | b.p;
#endif
        if (a_lock) {
            for (const codeT code : each_code) {
                if ((*a_lock)[code] && !assigned[code]) {
                    try_assign(code, a.rule[code], try_assign);
                }
            }
        }
        if (b_lock) {
            for (const codeT code : each_code) {
                if ((*b_lock)[code] && !assigned[code]) {
                    try_assign(code, b.rule[code], try_assign);
                }
            }
        }
        for (const codeT code : each_code) {
            if (!assigned[code]) {
                // Suppose there really exists r (that belongs to both a and b), then by flipping all its values in any group of (a.p | b.p), the result must still belong to both a and b, as any group of (a.p | b.p) corresponds to one or multiple (entire) groups of (a.p) or (b.p).
                // So there doesn't exist a case where only one value is possible in the intersection - when no cases in a group are assigned, for any specific case there is no restriction for the value (either {0} or {1} will be ok; always using {0} here), and by deciding the value, the values for other cases in the group (of (a.p | b.p)) are decided transitively.
                assert(for_each_code_all_of(p_ab.group_for(code), [&](const codeT c) { return !assigned[c]; }));
                try_assign(code, {0}, try_assign);
                assert(for_each_code_all_of(p_ab.group_for(code), [&](const codeT c) { return assigned[c]; }));
            }
        }

        assert(for_each_code_all_of([&](const codeT code) { return assigned[code]; }));
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

    // TODO: should not be assert-only...
    inline ruleT common_rule(const subsetT& a, const subsetT& b) {
        const ruleT common = a.contains(b.rule)   ? b.rule //
                             : b.contains(a.rule) ? a.rule
                                                  : common_rule_unchecked(a, b);
        assert(a.contains(common) && b.contains(common));
        return common;
    }

    // If `has_common(a, b)`, return their intersection; otherwise, the result is (safe but) meaningless.
    inline subsetT operator&(const subsetT& a, const subsetT& b) { //
        return subsetT{.rule = common_rule(a, b), .p = a.p | b.p};
    }

    inline void operator&=(subsetT& a, const subsetT& b) {
        a.rule = common_rule(a, b);
        a.p |= b.p;
    }

    // const auto& subset ~ subsetT / subsetT_v2.
    inline int distance(const auto& subset, const ruleT& a, const ruleT& b) {
        assert(subset.contains(a) && subset.contains(b));
        int dist = 0;
        for (const codeT code : subset.free_heads()) {
            dist += a[code] != b[code];
        }
        return dist;
    }

    inline ruleT approximate(const auto& subset, const ruleT& rule) {
        ruleT r = subset.get_rule();
        for (const groupT& group : subset.free_groups()) {
            if (all_same_or_different(group, r, rule)) {
                for (const codeT code : group) {
                    r[code] = rule[code];
                }
            }
        }
        assert(subset.contains(r));
        return r;
    }

    inline ruleT transform(const auto& subset, const ruleT& rel, const ruleT* pos,
                           const func_ref<void(bool*, bool*)> fn) {
        assert(subset.contains(rel));
        assert_implies(pos, subset.contains(*pos));
        assert(subset->k() <= 512);   // free_k <= k <= 512.
        std::array<bool, 512> diff{}; // vector-bool.
        if (pos) {
            for (int j = 0; const codeT code : subset.free_heads()) {
                diff[j++] = rel[code] != (*pos)[code];
            }
        }
        fn(diff.data(), diff.data() + subset.free_k());

        ruleT rule = rel;
        for (int j = 0; const groupT& group : subset.free_groups()) {
            if (diff[j++]) {
                flip_values_r(group, rule);
            }
        }
        assert(subset.contains(rule));
        return rule;
    }

    inline ruleT random_rule_c(const auto& subset, const ruleT& rel, std::mt19937& rand, int count) {
        return transform(subset, rel, nullptr, [&rand, count](bool* begin, bool* end) {
            const int c = std::clamp(count, 0, int(end - begin));
            std::fill_n(begin, c, bool(1));
            std::fill(begin + c, end, bool(0));
            std::shuffle(begin, end, rand);
        });
    }

    inline ruleT random_rule_p(const auto& subset, const ruleT& rel, std::mt19937& rand, double p) {
        return transform(subset, rel, nullptr, [&rand, p](bool* begin, bool* end) {
            std::bernoulli_distribution dist(std::clamp(p, 0.0, 1.0));
            std::generate(begin, end, [&] { return dist(rand); });
        });
    }

    struct flatten : no_create {
        static ruleT first(const auto& subset, const ruleT& rel) {
            return transform(subset, rel, nullptr, [](bool* begin, bool* end) { std::fill(begin, end, bool(0)); });
        }

        static ruleT last(const auto& subset, const ruleT& rel) {
            return transform(subset, rel, nullptr, [](bool* begin, bool* end) { std::fill(begin, end, bool(1)); });
        }

        // First rule with distance = d to `rel` in the sequence.
        static ruleT first_d(const auto& subset, const ruleT& rel, int d) {
            return transform(subset, rel, nullptr, [d](bool* begin, bool* end) {
                const int c = std::clamp(d, 0, int(end - begin));
                std::fill_n(begin, c, bool(1));
                std::fill(begin + c, end, bool(0));
            });
        }

        static ruleT next(const auto& subset, const ruleT& rel, const ruleT& pos) {
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

        static ruleT prev(const auto& subset, const ruleT& rel, const ruleT& pos) {
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

    // (Due to a nasty init-order bug in clang-cl, these have to be either constexpr or wrapped by functions.)
    // (Otherwise, if defined as inline const (non-constexpr), the sets defined in "edit_rule.cpp" may be initialized before rules.)

    // Any-rule ^ rule_all_zero -> rule's actual values (same = 0, diff = 1).
    inline constexpr ruleT rule_all_zero = {};
    // Any-rule ^ rule_identity -> whether the cell will "flip" (same = no-flip, diff = flip).
    inline constexpr ruleT rule_identity = create_rule_copy_from(codeT::bpos_s);

    // A mapperT maps each codeT to another codeT.
    // Especially, mapperT{"qweasdzxc"} maps any codeT to the same value.
    class mapperT {
        struct takeT {
            enum tagE : int8_t { O, I, Get, NGet };
            tagE tag;
            codeT::bposE bpos;
            cellT operator()(const codeT code) const {
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
        codeT operator()(const codeT code) const {
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
            // add_eq(eq, m, mp_identity);
            for (const codeT code : each_code) {
                eq.add_eq(m(code), code);
            }
        }
        return subsetT{.rule = rule, .p = eq};
    }

    ANISO_DECLARE_TEST(test_mappers);
    ANISO_DECLARE_TEST(test_subset_intersection);

    // 0/1-reversal dual.
    inline ruleT trans_reverse(const ruleT& rule) {
        return ruleT::create([&rule](const codeT code) {
            // (dual[code] == code.s) == (rule[rev] == rev.s)
            const cellT s = code.get(codeT::bpos_s);
            const codeT rev = mp_reverse(code);
            return rule[rev] == rev.get(codeT::bpos_s) ? s : !s;
        });
    }

    ANISO_DECLARE_TEST(test_trans_reverse);

    // (Currently not supported in gui, as these can map `hex` rules to `hex2` set.)
    [[deprecated]] inline ruleT trans_left_right(const ruleT& rule) { //
        return ruleT::create([&rule](const codeT code) { return rule[mp_refl_wsx(code) /* | */]; });
    }

    [[deprecated]] inline ruleT trans_rotate_90(const ruleT& rule) { //
        return ruleT::create([&rule](const codeT code) { return rule[mp_C4(code)]; });
    }

    // A `partialT` stands for a another kind of MAP subset where, a rule belongs to the set iff it has the same "locked" values.
    // Take the "free" glider in gol for example. During all of its phases, the glider involves only a small subset of all cases (l), and a rule allows for the same glider iff it belongs to partialT(gol,l). (Not taking account of reactions with other patterns e.g. glider collision.)
    struct partialT {
        ruleT rule{};
        lockT lock{};

        void set(const codeT code, const cellT v) {
            rule[code] = v;
            lock[code] = true;
        }

        bool contains(const ruleT& r) const {
            return for_each_code_all_of([&](const codeT code) { //
                return !lock[code] || rule[code] == r[code];
            });
        }
        bool includes(const partialT& other) const {
            return for_each_code_all_of([&](const codeT code) { // Less restrictive.
                return !lock[code] || (other.lock[code] && rule[code] == other.rule[code]);
            });
        }

        friend bool operator==(const partialT& a, const partialT& b) {
            return for_each_code_all_of([&](const codeT code) { //
                return a.lock[code] == b.lock[code] && (!a.lock[code] || a.rule[code] == b.rule[code]);
            });
        }

        bool is_universal() const { return none_locked(lock); }
        void reset() {
            rule.reset();
            lock.reset();
        }
    };

    inline bool has_common(const partialT& a, const partialT& b) {
        return for_each_code_all_of([&](const codeT code) { // No conflicts.
            return !(a.lock[code] && b.lock[code] && a.rule[code] != b.rule[code]);
        });
    }

    inline void operator&=(partialT& a, const partialT& b) {
        for (const codeT code : each_code) {
            if (b.lock[code]) {
                // <-> `assert(has_common(a, b))`:
                assert(!a.lock[code] || a.rule[code] == b.rule[code]);
                a.set(code, b.rule[code]);
            }
        }
    }

    inline partialT operator&(const partialT& a, const partialT& b) {
        partialT c = a;
        c &= b;
        return c;
    }

    // TODO: under-tested...
    // TODO: ideally should be based on rule & p directly (but easy to expose partialT & subsetT).
    struct subsetT_v2 {
        // ruleT rule{};
        // partitionT p{};
        subsetT s{};
        lockT lock{}; // All-true/false in each group.

        const partitionT* operator->() const { return &s.p; }
        bool contains(const ruleT& r) const {
            return s->for_each_group_all_of([&](const groupT& group) { //
                return lock[group[0]] ? all_same(group, s.rule, r) : all_same_or_different(group, s.rule, r);
            });
        }

        bool includes(const subsetT_v2&) = delete;
        friend bool operator==(const subsetT_v2& a, const subsetT_v2& b) = delete;

        bool is_universal() const { return s->k() == 512 && none_locked(lock); }
        void reset() {
            s.reset();
            lock.reset();
        }

        const ruleT& get_rule() const { return s.rule; }
        // !!TODO: expensive; should optimize...
        int free_k() const { //
            return std::ranges::count_if(s->heads(), [&](codeT c) { return !lock[c]; });
        }
        auto free_heads() const { //
            return std::views::filter(s->heads(), [&](codeT c) { return !lock[c]; });
        }
        auto free_groups() const { //
            return std::views::filter(s->groups(), [&](const groupT& g) { return !lock[g[0]]; });
        }
    };

    inline bool has_common(const subsetT& a, const partialT& b) {
        return a->for_each_group_all_of([&](const groupT& group) { //
            return all_same_or_different_locked(group, a.rule, b.rule, b.lock);
        });
    }

    inline subsetT_v2 operator&(const subsetT& a, const partialT& b) {
        assert(has_common(a, b));
        ruleT rule = a.rule;
        lockT lock = {};
        for (const groupT& group : a->groups()) {
            if (const codeT* pos = find_locked(group, b.lock)) {
                if (rule[*pos] != b.rule[*pos]) {
                    flip_values_r(group, rule);
                }
                lock_all(group, lock);
            }
        }
        return {.s{.rule = rule, .p = a.p}, .lock = lock};
    }

    inline bool includes(const subsetT& a, const subsetT_v2& b) {
        return a.contains(b.s.rule) && a->for_each_group_all_of([&b](const groupT& group) { //
            return all_locked(group, b.lock) || b->has_group(group);
        });
    }

    // TODO: equivalent to `a.includes({b.rule, b.lock})`...
    inline bool includes(const partialT& a, const subsetT_v2& b) {
        return for_each_code_all_of([&](const codeT code) { // Less restrictive.
            return !a.lock[code] || (b.lock[code] && a.rule[code] == b.s.rule[code]);
        });
    }

    inline bool has_common(const partialT& a, const subsetT_v2& b) {
        return b->for_each_group_all_of([&](const groupT& group) {
            if (!b.lock[group[0]]) {
                return all_same_or_different_locked(group, b.s.rule, a.rule, a.lock);
            } else {
                return for_each_code_all_of(group, [&](const codeT code) { // No conflicts.
                    return !a.lock[code] || a.rule[code] == b.s.rule[code];
                });
            }
        });
    }

    inline bool has_common(const subsetT& a, const subsetT_v2& b) {
        if (a.contains(b.s.rule) || b.contains(a.rule)) {
            return true;
        }
        const ruleT common = common_rule_unchecked(a, b.s, nullptr, &b.lock);
        return a.contains(common) && b.contains(common);
    }

    void operator&=(subsetT_v2& a, const subsetT& b) = delete;
    void operator&=(subsetT_v2& a, const partialT& b) = delete;

    ANISO_DECLARE_TEST(test_both_compiles);

} // namespace aniso
