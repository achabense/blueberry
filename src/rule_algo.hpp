#pragma once

#include "rule.hpp"

// TODO: add summary about this header, especially subsetT.
namespace aniso {
    // TODO: defining `maskT` to emphasis which rule serves as the mask; it might be more
    // convenient to use `ruleT` directly.

    // A maskT is an arbitrary ruleT selected to do XOR mask for other rules.
    // The result reflects how the rule is different from the masking rule.
    struct maskT : public ruleT {};
    using ruleT_masked = codeT::map_to<bool, 3>;

    inline void operator^(const maskT&, const maskT&) = delete;

    // TODO: masking -> comparing; ^ -> != (also for `masked_by_a/b`)
    inline ruleT_masked operator^(const maskT& mask, const ruleT& rule) {
        ruleT_masked r{};
        for_each_code([&](codeT code) { r[code] = mask[code] ^ rule[code]; });
        return r;
    }

    inline ruleT operator^(const maskT& mask, const ruleT_masked& r) {
        ruleT rule{};
        for_each_code([&](codeT code) { rule[code] = mask[code] ^ r[code]; });
        return rule;
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_maskT = [] {
            const ruleT a = make_rule([](auto) { return cellT(testT::rand() & 1); });
            const ruleT b = make_rule([](auto) { return cellT(testT::rand() & 1); });
            const maskT a_as_mask{a}, b_as_mask{b};

            assert(a == (b_as_mask ^ (b_as_mask ^ a)));
            assert(b == (a_as_mask ^ (a_as_mask ^ b)));
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    // Equivalence relation for codeT ({0...511}), in the form of union-find set.
    class equivT {
        friend class partitionT;

        mutable codeT::map_to<codeT> parof;

        codeT headof(codeT c) const {
            if (parof[c] == c) {
                return c;
            } else {
                return parof[c] = headof(parof[c]);
            }
        }

    public:
        equivT() {
            for_each_code([&](codeT code) { parof[code] = code; });
        }

        // Test whether `r` has the same value in each group.
        bool test(const ruleT_masked& r) const {
            return for_each_code_all_of([&](codeT code) { //
                return r[code] == r[parof[code]];
            });
        }

        // Test whether `r` has the same value for locked codes in each group.
        bool test(const ruleT_masked& r, const lockT& lock) const {
            codeT::map_to<int> record;
            record.fill(-1);

            return for_each_code_all_of([&](codeT code) {
                if (!lock[code]) {
                    return true;
                }

                int& rep = record[headof(code)];
                if (rep == -1) {
                    rep = r[code];
                }
                return rep == r[code];
            });
        }

        void add_eq(codeT a, codeT b) { parof[headof(a)] = headof(b); }
        void add_eq(const equivT& other) {
            for_each_code([&](codeT code) { add_eq(code, other.parof[code]); });
        }

        bool has_eq(codeT a, codeT b) const { return headof(a) == headof(b); }
        bool has_eq(const equivT& other) const {
            return for_each_code_all_of([&](codeT code) { //
                return has_eq(code, other.parof[code]);
            });
        }

        friend equivT operator|(const equivT& a, const equivT& b) {
            equivT c = a;
            c.add_eq(b);
            return c;
        }
    };

    using groupT = std::span<const codeT>;

    inline void flip_values(const groupT& group, ruleT& r) {
        for (const codeT c : group) {
            r[c] = !r[c];
        }
    }

    class partitionT {
        equivT m_eq;
        int m_k{}; // `m_eq` has `m_k` groups.

        // Map codeT to an integer ∈ [0, m_k) (which represents the group the code belongs to).
        codeT::map_to<int> m_map{};

        // (Not a codeT::map_to<codeT>)
        // Store codeT of the same group consecutively (to provide codeT span).
        std::array<codeT, 512> m_data{};

        struct group_pos {
            int pos, size;
        };
        std::vector<group_pos> m_groups{};

        groupT jth_group(int j) const {
            const auto [pos, size] = m_groups[j];
            return groupT(m_data.data() + pos, size);
        }

    public:
        // ~ "user-declared" to avoid implicit moving (to avoid `m_groups` being emptied):
        partitionT(const partitionT&) = default;
        partitionT& operator=(const partitionT&) = default;

        groupT group_for(codeT code) const { return jth_group(m_map[code]); }

        int k() const { return m_k; }
        void for_each_group(const auto& fn) const {
            for (int j = 0; j < m_k; ++j) {
                if constexpr (requires { fn(jth_group(j)); }) {
                    fn(jth_group(j));
                } else {
                    fn(j, jth_group(j));
                }
            }
        }
        std::vector<groupT> groups() const {
            std::vector<groupT> vec(m_k);
            for (int j = 0; j < m_k; ++j) {
                vec[j] = jth_group(j);
            }
            return vec;
        }

        /*implicit*/ partitionT(const equivT& eq) : m_eq(eq) {
            m_k = 0;
            m_map.fill(-1);
            for_each_code([&](codeT code) {
                const codeT head = eq.headof(code);
                if (m_map[head] == -1) {
                    m_map[head] = m_k++;
                }
                m_map[code] = m_map[head];
            });
            // m_k is now the number of groups in the partition.

            std::vector<int> count(m_k, 0);
            for_each_code([&](codeT code) { ++count[m_map[code]]; });

            std::vector<int> pos(m_k, 0);
            for (int j = 1; j < m_k; ++j) {
                pos[j] = pos[j - 1] + count[j - 1];
            }

            m_groups.resize(m_k);
            for (int j = 0; j < m_k; ++j) {
                m_groups[j] = {pos[j], count[j]};
            }

            for_each_code([&](codeT code) {
                int j = m_map[code];
                m_data[pos[j]++] = code;
            });
        }

        bool test(const ruleT_masked& r) const { return m_eq.test(r); }
        bool test(const ruleT_masked& r, const lockT& lock) const { return m_eq.test(r, lock); }

        bool is_refinement_of(const partitionT& other) const { return other.m_eq.has_eq(m_eq); }

        friend partitionT operator|(const partitionT& a, const partitionT& b) { //
            return partitionT(a.m_eq | b.m_eq);
        }
    };

    // A `subsetT` (s = {} or (m, p)) defines a subset of all MAP rules, where:
    // A rule (r) belongs to a non-empty subset iff (m ^ r) has the same value for each group in (p).
    // (As a result, any rule in the subset can equally serve as the mask. There is no difference which rule is used.)
    // It can be proven that the intersection of `subsetT` is also `subsetT` (see below).
    class subsetT {
        struct nonemptyT {
            maskT mask;
            partitionT par;

            bool contains(const ruleT& rule) const { return par.test(mask ^ rule); }
            bool includes(const nonemptyT& other) const {
                return contains(other.mask) && par.is_refinement_of(other.par);
            }
        };

        std::optional<nonemptyT> m_set;

    public:
        explicit subsetT(const maskT& mask, const equivT& eq) { m_set.emplace(mask, eq); }
        explicit subsetT(const maskT& mask, const partitionT& par) { m_set.emplace(mask, par); }

        explicit subsetT() : m_set{std::nullopt} {}
        // The entire MAP set:
        static subsetT universal() { return subsetT{maskT{}, equivT{}}; }

        bool empty() const { return !m_set.has_value(); }
        bool contains(const ruleT& rule) const { return m_set && m_set->contains(rule); }
        bool includes(const subsetT& other) const { return other.empty() || (m_set && m_set->includes(*other.m_set)); }
        bool equals(const subsetT& other) const { return includes(other) && other.includes(*this); }

        friend bool operator==(const subsetT& a, const subsetT& b) { //
            return (a.empty() && b.empty()) || (a.includes(b) && b.includes(a));
        }

        const maskT& get_mask() const {
            assert(!empty());
            return m_set->mask;
        }
        const partitionT& get_par() const {
            assert(!empty());
            return m_set->par;
        }

        // Look for a rule that belongs to both a and b.
        static std::optional<maskT> common_rule(const subsetT& a_, const subsetT& b_) {
            if (a_.empty() || b_.empty()) {
                return std::nullopt;
            }

            const nonemptyT &a = *a_.m_set, &b = *b_.m_set;

            if (a.contains(b.mask)) {
                return b.mask;
            } else if (b.contains(a.mask)) {
                return a.mask;
            }

            maskT common{};
            codeT::map_to<bool> assigned{};

            // Assign values according to equivalence relations, without checking for consistency
            // in the groups.
            codeT::map_to<bool> a_checked{}, b_checked{}; // To reduce time-complexity.
            auto try_assign = [&](const codeT code, const cellT v, auto& self) -> void {
                if (!std::exchange(assigned[code], true)) {
                    common[code] = v;
                    if (!std::exchange(a_checked[a.par.group_for(code)[0]], true)) {
                        const bool masked_by_a = a.mask[code] ^ v;
                        for (const codeT c : a.par.group_for(code)) {
                            self(c, a.mask[c] ^ masked_by_a, self);
                        }
                    }
                    if (!std::exchange(b_checked[b.par.group_for(code)[0]], true)) {
                        const bool masked_by_b = b.mask[code] ^ v;
                        for (const codeT c : b.par.group_for(code)) {
                            self(c, b.mask[c] ^ masked_by_b, self);
                        }
                    }
                }
            };

#ifndef NDEBUG
            const partitionT par_both = a.par | b.par;
#endif
            for_each_code([&](codeT code) {
                if (!assigned[code]) {
                    assert(std::ranges::none_of(par_both.group_for(code), [&](codeT c) { return assigned[c]; }));
                    try_assign(code, {0}, try_assign);
                    assert(std::ranges::all_of(par_both.group_for(code), [&](codeT c) { return assigned[c]; }));
                }
            });

            assert(for_each_code_all_of([&](codeT code) { return assigned[code]; }));
            if (!a.contains(common) || !b.contains(common)) {
                return std::nullopt;
            }
            return common;
        }
    };

    // The intersection (&) of any two subsetT (a) and (b) is still another subsetT:
    // If there is no common rule, then (a & b) results in an empty set.
    // Otherwise, suppose there is a rule (r) known to belong to (a & b):
    // Let (p) = (a.par | b.par), and (s) = subsetT(r, p), then:
    // 1. (s) is included by (a & b), as:
    // Any rule in (s) can be gotten by flipping some groups in (p) from (r), and as (a.par) and (b.par)
    // are refinements of (p), the rule can also be viewed as flipping some groups in (a.par) or (b.par),
    // so the rule also belongs to (a) and (b).
    // 2. There cannot be a rule that belongs to (a & b) but not (s) (so (a & b) is included by (s)), as:
    // The values of a rule in any group from (p) are inter-dependent. If the value for any codeT (c) is fixed,
    // then the values for the group (c) belongs in (p) are also fixed accordingly.
    // As a result, the rule must be able to be flipped by whole groups from (r), so belongs to (s)
    inline subsetT operator&(const subsetT& a, const subsetT& b) {
        if (auto common = subsetT::common_rule(a, b)) {
            assert(!a.empty() && !b.empty());
            return subsetT{*common, a.get_par() | b.get_par()};
        }

        return subsetT{};
    }

    inline bool has_common(const subsetT& a, const subsetT& b) { //
        return subsetT::common_rule(a, b).has_value();
    }

    inline int distance(const subsetT& subset, const ruleT& a, const ruleT& b) {
        assert(subset.contains(a) && subset.contains(b));
        int d = 0;
        subset.get_par().for_each_group([&](const groupT& group) {
            if (a[group[0]] != b[group[0]]) {
                ++d;
            }
        });
        return d;
    }

    struct scanT {
        int c_0 = 0; // Same.
        int c_1 = 0; // Different.

        bool pure() const { return c_0 == 0 || c_1 == 0; }

        scanT() = default;
        scanT(const groupT& group, const maskT& mask, const ruleT& rule) {
            for (const codeT code : group) {
                mask[code] == rule[code] ? ++c_0 : ++c_1;
            }
        }

#if 0
        static std::vector<scanT> scanlist(const partitionT& par, const maskT& mask, const ruleT& rule) {
            std::vector<scanT> vec(par.k());
            par.for_each_group([&](int j, const groupT& group) { vec[j] = scanT(group, mask, rule); });
            return vec;
        }
#endif
    };

    // ~ scanT(...).pure().
    inline bool all_same_or_different(const groupT& group, const ruleT& a, const ruleT& b) {
        const bool v = a[group[0]] == b[group[0]];
        return std::ranges::all_of(group.subspan(1), [&, v](const codeT code) { //
            return v == (a[code] == b[code]);
        });
    }

#if 0
    // Return a rule in the subset that is closest to `rule` (as measured by the MAP set).
    // (If the rule already belongs to `subset`, the result will be exactly `rule`.)
    inline ruleT approximate(const subsetT& subset, const ruleT& rule) {
        const maskT& mask = subset.get_mask();
        const partitionT& par = subset.get_par();

        ruleT res{};
        par.for_each_group([&](const groupT& group) {
            const scanT scan(group, mask, rule);
            // v -> 0 (same as mask): resulting dist = c_1.
            // v -> 1 (different): resulting dist = c_0.
            // So if c_0 > c_1, selecting 0 can make the distance smaller (c_1).
            const bool v = scan.c_0 > scan.c_1 ? 0 : 1;
            for (const codeT code : group) {
                res[code] = mask[code] ^ v;
            }
        });

        assert_implies(subset.contains(rule), res == rule);
        return res;
    }
#else
    // The old impl cannot guarantee uniqueness (both values are ok when c_0=c_1, so the result is just impl-defined) or similarity (cannot be trivially implied by distance).

    // If rule belongs to {par, mask}, the result will be the rule itself.
    inline ruleT approximate(const partitionT& par, const maskT& mask, const ruleT& rule) {
        ruleT res{};
        par.for_each_group([&](const groupT& group) {
            if (all_same_or_different(group, mask, rule)) {
                for (const codeT code : group) {
                    res[code] = rule[code];
                }
            } else {
                for (const codeT code : group) {
                    res[code] = mask[code];
                }
            }
        });
        return res;
    }
#endif

    // Firstly get approximate(subset, rule), then transform the rule to another one.
    // The groups are listed as a sequence of values (relative to `mask`) and re-assigned by `fn`.
    inline ruleT transform(const subsetT& subset, const maskT& mask, const ruleT& rule,
                           const std::invocable<bool*, bool*> auto& fn) {
        assert(subset.contains(mask));

        const partitionT& par = subset.get_par();
        ruleT_masked r = mask ^ approximate(par, mask, rule);

        // `seq` is not a codeT::map_to<bool>.
        assert(par.k() <= 512);
        std::array<bool, 512> seq{};
        int z = 0;
        par.for_each_group([&](const groupT& group) { seq[z++] = r[group[0]]; });

        fn(seq.data(), seq.data() + z);

        z = 0;
        par.for_each_group([&](const groupT& group) {
            const bool seqz = seq[z++];
            for (const codeT code : group) {
                r[code] = seqz;
            }
        });

        const ruleT res = mask ^ r;
        assert(subset.contains(res));
        return res;
    }

    inline ruleT randomize_c(const subsetT& subset, const maskT& mask, std::mt19937& rand, int count) {
        return transform(subset, mask, {}, [&rand, count](bool* begin, bool* end) {
            const int c = std::clamp(count, 0, int(end - begin));
            std::fill_n(begin, c, 1);
            std::fill(begin + c, end, 0);
            std::shuffle(begin, end, rand);
        });
    }

    inline ruleT randomize_p(const subsetT& subset, const maskT& mask, std::mt19937& rand, double p) {
        return transform(subset, mask, {}, [&rand, p](bool* begin, bool* end) {
            std::bernoulli_distribution dist(std::clamp(p, 0.0, 1.0));
            std::generate(begin, end, [&] { return dist(rand); });
        });
    }

    struct seq_mixed {
        static ruleT first(const subsetT& subset, const maskT& mask) {
            return transform(subset, mask, {}, [](bool* begin, bool* end) { std::fill(begin, end, 0); });
        }

        static ruleT last(const subsetT& subset, const maskT& mask) {
            return transform(subset, mask, {}, [](bool* begin, bool* end) { std::fill(begin, end, 1); });
        }

        // The result will be the first rule with distance = n to the mask in the sequence.
        static ruleT seek_n(const subsetT& subset, const maskT& mask, int n) {
            return transform(subset, mask, {}, [n](bool* begin, bool* end) {
                const int c = std::clamp(n, 0, int(end - begin));
                std::fill_n(begin, c, 1);
                std::fill(begin + c, end, 0);
            });
        }

        static ruleT next(const subsetT& subset, const maskT& mask, const ruleT& rule) {
            assert(subset.contains(rule));

            return transform(subset, mask, rule, [](bool* begin, bool* end) {
                if (!std::next_permutation(begin, end, std::greater<>{})) {
                    // 1100... -> 1110..., or stop at 000... (first())
                    bool* first_0 = std::find(begin, end, 0);
                    if (first_0 != end) {
                        *first_0 = 1;
                    }
                }
            });
        }

        static ruleT prev(const subsetT& subset, const maskT& mask, const ruleT& rule) {
            assert(subset.contains(rule));

            return transform(subset, mask, rule, [](bool* begin, bool* end) {
                if (!std::prev_permutation(begin, end, std::greater<>{})) {
                    // ...0111 -> ...0011, or stop at 111... (last())
                    bool* first_1 = std::find(begin, end, 1);
                    if (first_1 != end) {
                        *first_1 = 0;
                    }
                }
            });
        }
    };

    // !!TODO: all the `moldT` related functions should be redesigned...

    // `lockT`, together with the associated rule, captures the idea that the locked values in the rule
    // are the "cause" for something to happen.
    // For example, suppose we find an oscillator in a rule. It is likely to only invoke a subset of all
    // codeT during all of its phases. We can record these invocations and say that's why the oscillator exists
    // in this rule.
    // The program uses `moldT` ~ (lockT, ruleT) pair as a constraint for generating new rules.
    struct moldT {
        ruleT rule{};
        lockT lock{};

        // Test whether `r` has the same values for all locked codes.
        bool compatible(const ruleT& r) const {
            return for_each_code_all_of([&](codeT code) { //
                return !lock[code] || rule[code] == r[code];
            });
        }

        friend bool operator==(const moldT&, const moldT&) = default;
    };

#if 0
    struct scanT {
        int free_0 = 0, free_1 = 0;
        int locked_0 = 0, locked_1 = 0; // 0/1 means masked value.

        bool any_locked() const { return locked_0 || locked_1; }
        bool all_locked() const { return !free_0 && !free_1; }

        bool all_0() const { return !free_1 && !locked_1; }
        bool all_1() const { return !free_0 && !locked_0; }

        scanT() = default;
        scanT(const groupT& group, const maskT& mask, const moldT& mold) {
            for (codeT code : group) {
                const bool v = mask[code] ^ mold.rule[code];
                if (mold.lock[code]) {
                    v ? ++locked_1 : ++locked_0;
                } else {
                    v ? ++free_1 : ++free_0;
                }
            }
        }

        static std::vector<scanT> scanlist(const partitionT& par, const maskT& mask, const moldT& mold) {
            std::vector<scanT> vec(par.k());
            par.for_each_group([&](int j, const groupT& group) { vec[j] = scanT(group, mask, mold); });
            return vec;
        }
    };

    // Test whether there exists any rule that belongs to both `subset` and `mold`.
    // (subset.contains(rule) && mold.compatible(rule))
    inline bool compatible(const subsetT& subset, const moldT& mold) {
        if (subset.empty()) {
            return false;
        }

        const ruleT_masked r = subset.get_mask() ^ mold.rule;
        return subset.get_par().test(r, mold.lock);
    }

    // Return a rule that belongs to `subset` and `mold` and is closest to `mold.rule` (as measured by
    // the MAP set.)
    // (If `mold.rule` already belongs to `subset`, the result will be exactly `mold.rule`.)
    inline ruleT approximate(const subsetT& subset, const moldT& mold) {
        assert(compatible(subset, mold));

        const maskT& mask = subset.get_mask();
        const partitionT& par = subset.get_par();

        ruleT_masked r{};
        par.for_each_group([&](const groupT& group) {
            const scanT scan(group, mask, mold);
            // If there are locks, `v` must be the locked value to guarantee `mold.compatible`.
            // Otherwise, if v = 0 the "distance" will be free_1; if v = 1 the "distance" will be free_0.
            // So for example, if free_0 = 9, free_1 = 3, then v should be 0 to make "distance" = free_1 = 3.
            assert(!(scan.locked_0 && scan.locked_1));
            const bool v = scan.locked_0 ? 0 : scan.locked_1 ? 1 : scan.free_0 > scan.free_1 ? 0 : 1;
            for (codeT code : group) {
                r[code] = v;
            }
        });

        const ruleT res = mask ^ r;
        assert(subset.contains(res) && mold.compatible(res));
        assert_implies(subset.contains(mold.rule), res == mold.rule);
        return res;
    }

    inline bool any_locked(const lockT& lock, const groupT& group) {
        return std::ranges::any_of(group, [&lock](codeT code) { return lock[code]; });
    }

    inline bool none_locked(const lockT& lock, const groupT& group) {
        return std::ranges::none_of(group, [&lock](codeT code) { return lock[code]; });
    }

    inline lockT enhance_lock(const subsetT& subset, const moldT& mold) {
        assert(subset.contains(mold.rule));

        lockT lock{};
        subset.get_par().for_each_group([&](const groupT& group) {
            if (any_locked(mold.lock, group)) {
                for (codeT code : group) {
                    lock[code] = true;
                }
            }
        });
        return lock;
    }

    // Firstly get rule = approximate(subset, mold), then transform the rule to another one:
    // 1. The locked groups are not affected.
    // 2. The free groups are listed as a sequence of values (relative to `mask`) and re-assigned by `fn`.
    inline ruleT transform(const subsetT& subset, const maskT& mask, const moldT& mold,
                           const std::invocable<bool*, bool*> auto& fn) {
        assert(subset.contains(mask));
        assert(compatible(subset, mold));

        const partitionT& par = subset.get_par();

        ruleT_masked r = mask ^ approximate(subset, mold);

        // `seq` is not a codeT::map_to<bool>.
        assert(par.k() <= 512);
        std::array<bool, 512> seq{};
        int z = 0;
        par.for_each_group([&](const groupT& group) {
            if (none_locked(mold.lock, group)) {
                seq[z] = r[group[0]];
                ++z;
            }
        });

        fn(seq.data(), seq.data() + z);

        z = 0;
        par.for_each_group([&](const groupT& group) {
            if (none_locked(mold.lock, group)) {
                for (codeT code : group) {
                    r[code] = seq[z];
                }
                ++z;
            }
        });

        const ruleT res = mask ^ r;
        assert(subset.contains(res) && mold.compatible(res));
        return res;
    }

    inline ruleT randomize_c(const subsetT& subset, const maskT& mask, const moldT& mold, std::mt19937& rand,
                             int count) {
        return transform(subset, mask, mold, [&rand, count](bool* begin, bool* end) {
            const int c = std::clamp(count, 0, int(end - begin));
            std::fill_n(begin, c, 1);
            std::fill(begin + c, end, 0);
            std::shuffle(begin, end, rand);
        });
    }

    inline ruleT randomize_p(const subsetT& subset, const maskT& mask, const moldT& mold, std::mt19937& rand,
                             double p) {
        return transform(subset, mask, mold, [&rand, p](bool* begin, bool* end) {
            std::bernoulli_distribution dist(std::clamp(p, 0.0, 1.0));
            std::generate(begin, end, [&] { return dist(rand); });
        });
    }

    struct seq_mixed {
        static ruleT first(const subsetT& subset, const maskT& mask, const moldT& mold) {
            return transform(subset, mask, mold, [](bool* begin, bool* end) { std::fill(begin, end, 0); });
        }

        static ruleT last(const subsetT& subset, const maskT& mask, const moldT& mold) {
            return transform(subset, mask, mold, [](bool* begin, bool* end) { std::fill(begin, end, 1); });
        }

        // The result will be the first rule with distance = n to the mask in the sequence.
        static ruleT seek_n(const subsetT& subset, const maskT& mask, const moldT& mold, int n) {
            return transform(subset, mask, mold, [n](bool* begin, bool* end) {
                const int c = std::clamp(n, 0, int(end - begin));
                std::fill_n(begin, c, 1);
                std::fill(begin + c, end, 0);
            });
        }

        static ruleT next(const subsetT& subset, const maskT& mask, const moldT& mold) {
            assert(subset.contains(mold.rule));

            return transform(subset, mask, mold, [](bool* begin, bool* end) {
                if (!std::next_permutation(begin, end, std::greater<>{})) {
                    // 1100... -> 1110..., or stop at 000... (first())
                    bool* first_0 = std::find(begin, end, 0);
                    if (first_0 != end) {
                        *first_0 = 1;
                    }
                }
            });
        }

        static ruleT prev(const subsetT& subset, const maskT& mask, const moldT& mold) {
            assert(subset.contains(mold.rule));

            return transform(subset, mask, mold, [](bool* begin, bool* end) {
                if (!std::prev_permutation(begin, end, std::greater<>{})) {
                    // ...0111 -> ...0011, or stop at 111... (last())
                    bool* first_1 = std::find(begin, end, 1);
                    if (first_1 != end) {
                        *first_1 = 0;
                    }
                }
            });
        }
    };
#endif

    // rule ^ mask_zero -> the masked values are identical to rule's.
    inline const maskT mask_zero{{}};
    // rule ^ mask_identity -> the masked values can mean whether the cell will "flip" in each situation.
    inline const maskT mask_identity{make_rule([](codeT c) { return c.get(codeT::bpos_s); })};

    // A mapperT maps each codeT to another codeT.
    // Especially, mapperT{"qweasdzxc"} maps any codeT to the same value.
    class mapperT {
        struct takeT {
            enum tagE { O, I, Get, NGet };
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

        takeT q, w, e;
        takeT a, s, d;
        takeT z, x, c;

    public:
        codeT operator()(codeT code) const {
            return encode({q(code), w(code), e(code), //
                           a(code), s(code), d(code), //
                           z(code), x(code), c(code)});
        }

        consteval mapperT(const char* str) {
            // [01], or [qweasdzxc], or ![qweasdzxc].
            auto parse = [&]() -> takeT {
                takeT::tagE tag = takeT::Get;
                switch (*str) {
                    case '0': ++str; return {takeT::O, {}};
                    case '1': ++str; return {takeT::I, {}};
                    case '!':
                        ++str;
                        tag = takeT::NGet;
                        break;
                }
                switch (*str++) {
                    case 'q': return {tag, codeT::bpos_q};
                    case 'w': return {tag, codeT::bpos_w};
                    case 'e': return {tag, codeT::bpos_e};
                    case 'a': return {tag, codeT::bpos_a};
                    case 's': return {tag, codeT::bpos_s};
                    case 'd': return {tag, codeT::bpos_d};
                    case 'z': return {tag, codeT::bpos_z};
                    case 'x': return {tag, codeT::bpos_x};
                    case 'c': return {tag, codeT::bpos_c};
                    default: throw 0;
                }
            };
            // ~ about `throw 0`:
            // https://stackoverflow.com/questions/67320438/how-to-fail-a-consteval-function
            q = parse(), w = parse(), e = parse();
            a = parse(), s = parse(), d = parse();
            z = parse(), x = parse(), c = parse();
            if (*str != '\0') {
                throw 0;
            }
        }
    };

    // A pair of mapperT defines an equivalence relation.
    inline void add_eq(equivT& eq, const mapperT& a, const mapperT& b) {
        for_each_code([&](codeT code) { eq.add_eq(a(code), b(code)); });
    }

    // mp_identity(any code) -> the same code
    inline constexpr mapperT mp_identity("qweasdzxc");

    // The following mappers are defined relative to mp_identity.
    // That is, the effects actually means the effects of mapperT_pair{mp_identity, mp_*}.

    // Independence.
    // For example, mp_ignore_q * mask_zero -> the rules where the values are "independent of q".
    // Notice that `mp_ignore_s * mask_zero` is different from `mp_ignore_s * mask_identity`.
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
    // For example, rules in `mp_refl_wsx * mask_zero` are able to preserve "leftside-right"
    // symmetry in all cases.
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
    // but is useful to help define totalistic rules.
    inline constexpr mapperT mp_C8("aqw"
                                   "zse"
                                   "xcd"); // "45" (clockwise)
    inline constexpr mapperT mp_tot_exc_s("wqe"
                                          "asd"
                                          "zxc"); // swap(q,w); *C8 -> totalistic, excluding s
    inline constexpr mapperT mp_tot_inc_s("qse"
                                          "awd"
                                          "zxc"); // swap(w,s); *C8 -> totalistic, including s

    // * mask_identity -> the self-complementary rules.
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

#if 0
        // It's also valid to emulate hexagonal neighborhood by ignoring "q" and "c".
        // However, the program is not going to support this, as it makes the program more complicated
        // without bringing essentialy different discoveries.

        // - w e     w e
        // a s d -> a s d
        // z x -     z x
        inline constexpr mapperT mp_hex2_ignore("0we"
                                                "asd"
                                                "zx0"); // ignore_(q,c)
#endif

    // Von-Neumann emulation.
    // The neighborhood works naturally with native symmetry.
    // For example, `mp_von_C4` is implicitly defined as mp_C4 * mp_von_ignore.
    inline constexpr mapperT mp_von_ignore("0w0"
                                           "asd"
                                           "0x0"); // ignore_(q,e,z,c)

    inline constexpr mapperT mp_von_tot_exc_s("0d0"
                                              "asw"
                                              "0x0"); // swap(w,d); *C4 -> totalistic, excluding s
    inline constexpr mapperT mp_von_tot_inc_s("0s0"
                                              "awd"
                                              "0x0"); // swap(w,s); *C4 -> totalistic, including s

    inline subsetT make_subset(std::initializer_list<mapperT> mappers, const maskT& mask = mask_zero) {
        equivT eq{};
        for (const mapperT& m : mappers) {
            add_eq(eq, m, mp_identity);
        }
        return subsetT{mask, eq};
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_mappers = [] {
            for_each_code([](codeT code) {
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
            });
        };

        inline const testT test_subset_intersection = [] {
            subsetT sc = make_subset({mp_ignore_s}, mask_zero) & make_subset({mp_reverse}, mask_identity);

            // 2024/1/20 2AM
            // There is NO problem in the algorithm.
            // It's just that, in this situation the maskT has a strong bias, so that it's too easy to generate
            // rules in a certain direction...
            using enum codeT::bposE;
            assert(!sc.empty());
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_q); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_w); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_e); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_a); })));
            assert(!sc.contains(mask_identity));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_d); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_z); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_x); })));
            assert(sc.contains(make_rule([](codeT c) { return c.get(bpos_c); })));
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    // 0/1-reversal dual.
    inline moldT trans_reverse(const moldT& mold) {
        moldT rev{};
        for_each_code([&](codeT code) {
            const auto [q, w, e, a, s, d, z, x, c] = decode(code);
            const codeT code_rev = encode({!q, !w, !e, //
                                           !a, !s, !d, //
                                           !z, !x, !c});
            rev.lock[code_rev] = mold.lock[code];
            rev.rule[code_rev] = (mold.rule[code] == s) ? !s : !(!s); // So that ->
            assert((rev.rule[code_rev] == !s) == (mold.rule[code] == s));
        });
        return rev;
    }

#if 0
    // The results are are easy to go out of control...
    // For example, rules in `hex` space will be mapped to `hex2` space, so all the symmetry checks become
    // unavailable...
    inline moldT trans_left_right(const moldT& mold) {
        moldT lr{};
        for_each_code([&](codeT code) {
            const codeT c = mp_refl_wsx(code); // |
            lr.lock[c] = mold.lock[code];
            lr.rule[c] = mold.rule[code];
        });
        return lr;
    }

    inline moldT trans_rotate(const moldT& mold) {
        moldT ro{};
        for_each_code([&](codeT code) {
            const codeT c = mp_C4(code);
            ro.lock[c] = mold.lock[code];
            ro.rule[c] = mold.rule[code];
        });
        return ro;
    }
#endif

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_trans_reverse = [] {
            moldT mold{};
            for_each_code([&](codeT code) {
                mold.rule[code] = cellT(testT::rand() & 1);
                mold.lock[code] = testT::rand() & 1;
            });
            assert(mold == trans_reverse(trans_reverse(mold)));
        };
    } // namespace _tests
#endif // ENABLE_TESTS

} //  namespace aniso
