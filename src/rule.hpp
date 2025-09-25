#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <climits>
#include <memory>
#include <optional>
#include <random>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "utils.hpp"

#ifdef YDEBUG
#define ENABLE_TESTS
#endif

#if defined(__clang__)
#define ALWAYS_INLINE [[clang::always_inline]]
#elif defined(__GNUC__)
#define ALWAYS_INLINE [[gnu::always_inline]]
#elif defined(_MSC_VER)
#define ALWAYS_INLINE [[msvc::forceinline]]
#else
#define ALWAYS_INLINE inline
#endif

#if 1
#if defined(__clang__)
#pragma clang diagnostic ignored "-Wsign-compare" // Comparision between int & size_t etc.
#elif defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wsign-compare"
#elif defined(_MSC_VER)
#pragma warning(disable : 4244 4267) // Conversion between int & size_t etc.
#endif
#endif

static_assert(INT_MAX >= INT32_MAX);

namespace aniso {

#ifdef ENABLE_TESTS
    namespace _tests {
        struct testT : no_copy {
            inline static std::mt19937 rand{(uint32_t)time(0)};
            testT(const auto& fn) noexcept { fn(); }
        };
    } // namespace _tests
#endif // ENABLE_TESTS

#if 1
    // TODO: -> uint8_t?
    struct cellT {
        bool val{};
        /*implicit*/ ALWAYS_INLINE operator int() const { return val; }
        // (`operator==` is delegated to comparing `int()` result.)

        // (Note: to allow for seamless switch between `bool`, `~` is not usable as bool(~bool(1)) == 1 instead of 0...)
        friend cellT operator!(cellT c) { return cellT(!c.val); }
        friend cellT operator~(cellT c) = delete; // -> !c
        friend cellT operator^(cellT c, bool b) { return cellT(c.val ^ b); }
        friend cellT operator^(bool b, cellT c) { return cellT(c.val ^ b); }

        friend cellT operator^(cellT a, cellT b) { return cellT(a.val ^ b.val); }
        friend cellT operator|(cellT a, cellT b) { return cellT(a.val | b.val); }
        friend cellT operator&(cellT a, cellT b) { return cellT(a.val & b.val); }
    };

    // enum cellT : bool {};
    // enum impl has no performance penalty in debug mode, but cannot be implicitly created from literals ({0} or {1}).
    // And scarily, the following assertion will fail for gcc... This is likely to be a compiler bug...
    // static_assert(static_cast<int>(static_cast<cellT>(2)) == 1);
#else
    using cellT = bool;
    // (Should have no performance difference in release mode.)
#endif

    static_assert(std::is_trivially_copyable_v<cellT>);

    // The state of cell `s` and its neighbors.
    // (The variables are named after the keys in the qwerty keyboard.)
    struct situT {
        cellT q, w, e;
        cellT a, s, d;
        cellT z, x, c;

        auto to_3x3() const { //
            return std::bit_cast<std::array<std::array<cellT, 3>, 3>>(*this);
        }
    };

    // `situT` encoded as an integer.
    struct codeT {
        int16_t val{};
        /*implicit*/ ALWAYS_INLINE operator int() const { /*assert(0 <= val && val < 512);*/ return val; }

        template <class T, int tag = 0>
        class map_to {
            // `array::operator[]` can slow down `apply_rule` by more than %20 in debug mode...
            std::conditional_t<debug_mode, T[512], std::array<T, 512>> m_map{};

        public:
            const T& operator[](codeT code) const { return m_map[code]; }
            T& operator[](codeT code) { return m_map[code]; }

            void fill(const T& v) { std::ranges::fill(m_map, v); }
            void reset() { fill(T{}); }

            friend bool operator==(const map_to&, const map_to&) = default;

            ALWAYS_INLINE T operator()(codeT code) const
                requires(std::is_same_v<T, cellT> && tag == 1)
            {
                // return m_map[code]; // (In debug mode) still results in worse codegen even force-inlined...
                return m_map[code.val];
            }

            // std::span<const T, 512> data() const { return m_map; }
            // std::span<T, 512> data() { return m_map; }
        };

        // clang-format off
        enum bposE : int8_t {
            bpos_q = 8, bpos_w = 7, bpos_e = 6,
            bpos_a = 5, bpos_s = 4, bpos_d = 3,
            bpos_z = 2, bpos_x = 1, bpos_c = 0
        };
        // clang-format on

        cellT get(bposE bpos) const { return cellT((val >> bpos) & 1); }
    };

    static_assert(std::is_trivially_copyable_v<codeT>);

    inline codeT encode(const situT& situ) {
        using enum codeT::bposE;
        const int code = (situ.q << bpos_q) | (situ.w << bpos_w) | (situ.e << bpos_e) | //
                         (situ.a << bpos_a) | (situ.s << bpos_s) | (situ.d << bpos_d) | //
                         (situ.z << bpos_z) | (situ.x << bpos_x) | (situ.c << bpos_c);
        assert(0 <= code && code < 512);
        return codeT(code);
    }

    inline situT decode(codeT code) {
        using enum codeT::bposE;
        return {code.get(bpos_q), code.get(bpos_w), code.get(bpos_e), //
                code.get(bpos_a), code.get(bpos_s), code.get(bpos_d), //
                code.get(bpos_z), code.get(bpos_x), code.get(bpos_c)};
    }

    namespace _misc {
        // https://en.cppreference.com/w/cpp/language/range-for
        struct each_code_ : no_copy {
            struct endT {};
            struct posT {
                int v{0};
                ALWAYS_INLINE bool operator==(endT) const { return v == 512; }
                ALWAYS_INLINE void operator++() { ++v; }
                ALWAYS_INLINE codeT operator*() const { return codeT(v); }
            };

            ALWAYS_INLINE static posT begin() { return {}; }
            ALWAYS_INLINE static endT end() { return {}; }
        };
    } // namespace _misc

    inline constexpr _misc::each_code_ each_code{};

    // (`std::views` codegen is extremely disappointing in debug mode (will make a lot of iterator fn calls).)
    // inline constexpr auto each_code = std::views::transform(std::views::iota(0, 512), [](int i) { return codeT(i); });

    inline bool for_each_code_all_of(const auto& pred) {
        for (int v = 0; v < 512; ++v) {
            if (!pred(codeT(v))) {
                return false;
            }
        }
        return true;
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_codeT = [] {
            for (const codeT code : each_code) {
                assert(encode(decode(code)) == code);
            }
            assert(for_each_code_all_of([](codeT code) { return encode(decode(code)) == code; }));
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    // Map `situT` (encoded as `codeT`) to the value `s` become at next generation.
    using ruleT = codeT::map_to<cellT, 1>;

    // While there doesn't have to be `!std::is_same_v<cellT, bool>`, there must be:
    static_assert(!std::is_same_v<ruleT, codeT::map_to<bool>>);
    static_assert(std::is_trivially_copyable_v<ruleT>);

    // The program saves `ruleT` as normal "MAP strings" (which is based on `q*256+w*128+...` encoding scheme),
    // so the output can be accepted by other programs like Golly.
    // See `append_MAP` and `from_MAP` below for details - the encoding scheme of `codeT` affects only internal
    // representation of `ruleT` etc in this program, and is independent of input/output.

    template <class T>
    concept rule_like = std::is_invocable_r_v<cellT, const T&, codeT>;

    static_assert(rule_like<ruleT>);

    inline ruleT make_rule(const rule_like auto& fn) {
        ruleT rule{};
        for (const codeT code : each_code) {
            rule[code] = fn(code);
        }
        return rule;
    }

    // "Convay's Game of Life" (B3/S23)
    inline ruleT game_of_life() {
        return make_rule([](const codeT code) -> cellT {
            const auto [q, w, e, a, s, d, z, x, c] = decode(code);
            switch (q + w + e + a + d + z + x + c) {
                case 2: return s;   // 2:S ~ 0->0, 1->1 ~ equal to "s".
                case 3: return {1}; // 3:BS ~ 0->1, 1->1 ~ always 1.
                default: return {0};
            }
        });
    }

    // Works in combination with ruleT to represent value constraints; see `partialT` in "rule_algo.hpp" for usage.
    using lockT = codeT::map_to<bool, 2>;

    namespace _misc {
        inline char to_base64(const uint8_t b6) {
            if (b6 < 26) {
                return 'A' + b6;
            } else if (b6 < 52) {
                return 'a' + b6 - 26;
            } else if (b6 < 62) {
                return '0' + b6 - 52;
            } else if (b6 == 62) {
                return '+';
            } else {
                assert(b6 == 63);
                return '/';
            }
        }

        inline uint8_t from_base64(const char ch) {
            if (ch >= 'A' && ch <= 'Z') {
                return ch - 'A';
            } else if (ch >= 'a' && ch <= 'z') {
                return 26 + ch - 'a';
            } else if (ch >= '0' && ch <= '9') {
                return 52 + ch - '0';
            } else if (ch == '+') {
                return 62;
            } else {
                assert(ch == '/');
                return 63;
            }
        }

        inline bool is_base64(const char ch) { //
            return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '+' ||
                   ch == '/';
        }

        inline int count_base64(const std::string_view str) { //
            return std::ranges::find_if_not(str, is_base64) - str.begin();
        }

        // https://golly.sourceforge.io/Help/Algorithms/QuickLife.html
        // "MAP string" is based on `q * 256 + w * 128 + ...` encoding scheme, which may differ from `codeT`'s.
        inline int transcode_MAP(const codeT code) {
            using enum codeT::bposE;
            if constexpr (bpos_q == 8 && bpos_w == 7 && bpos_e == 6 && //
                          bpos_a == 5 && bpos_s == 4 && bpos_d == 3 && //
                          bpos_z == 2 && bpos_x == 1 && bpos_c == 0) {
                return code.val;
            } else {
                const auto [q, w, e, a, s, d, z, x, c] = decode(code);
                return q * 256 + w * 128 + e * 64 + a * 32 + s * 16 + d * 8 + z * 4 + x * 2 + c * 1;
            }
        }

        inline constexpr int MAP_length = (512 + 5) / 6; // 86; not including "MAP" prefix.

        inline void append_MAP(std::string& str, const auto& source /* ruleT or lockT */) {
            std::array<bool, 512> MAP_data{};
            for (const codeT code : each_code) {
                MAP_data[transcode_MAP(code)] = source[code];
            }

            const auto get = [&MAP_data](int i) { return i < 512 ? MAP_data[i] : 0; };
            for (int i = 0; i < 512; i += 6) {
                const uint8_t b6 = (get(i + 5) << 0) | (get(i + 4) << 1) | (get(i + 3) << 2) | (get(i + 2) << 3) |
                                   (get(i + 1) << 4) | (get(i + 0) << 5);
                str += to_base64(b6);
            }
        }

        inline void from_MAP(std::string_view str, auto& dest /* ruleT or lockT */) {
            assert(str.size() >= MAP_length);

            std::array<bool, 512> MAP_data{};
            const auto put = [&MAP_data](int i, bool b) {
                if (i < 512) {
                    MAP_data[i] = b;
                }
            };

            int pos = 0;
            for (int i = 0; i < 512; i += 6) {
                const uint8_t b6 = from_base64(str[pos++]);
                put(i + 5, (b6 >> 0) & 1);
                put(i + 4, (b6 >> 1) & 1);
                put(i + 3, (b6 >> 2) & 1);
                put(i + 2, (b6 >> 3) & 1);
                put(i + 1, (b6 >> 4) & 1);
                put(i + 0, (b6 >> 5) & 1);
            }

            for (const codeT code : each_code) {
                using D = std::remove_cvref_t<decltype(dest[code])>;
                dest[code] = D(MAP_data[transcode_MAP(code)]);
            }
        }
    } // namespace _misc

    // Convert ruleT to a "MAP string".
    // Format: MAP...( [...])? (regex: "MAP([a-zA-Z0-9+/]{86})( \\[([a-zA-Z0-9+/]{86})\\])?")
    inline std::string to_MAP_str(const ruleT& rule, const lockT* lock = nullptr) {
        std::string str = "MAP";
        _misc::append_MAP(str, rule);
        if (lock) {
            str += " [";
            _misc::append_MAP(str, *lock);
            str += "]";
        }
        return str;
    }

    // Extract ruleT (and optionally lockT) from text.
    struct extrT {
        std::string_view prefix{};
        std::string_view rule_str{}; // "MAP..."
        std::string_view lock_str{}; // " [...]"
        std::string_view suffix{};

        bool has_rule() const {
            assert_implies(!rule_str.empty(), rule_str.size() == rule_len && rule_str.starts_with("MAP"));
            return !rule_str.empty();
        }

        bool has_lock() const {
            assert_implies(rule_str.empty(), lock_str.empty());
            assert_implies(!lock_str.empty(),
                           lock_str.size() == lock_len && lock_str.starts_with(" [") && lock_str.ends_with(']'));
            return !lock_str.empty();
        }

        ruleT get_rule() const {
            assert(has_rule());
            ruleT rule{};
            _misc::from_MAP(rule_str.substr(3 /*MAP*/, MAP_len), rule);
            return rule;
        }

        lockT get_lock() const {
            assert(has_lock());
            lockT lock{};
            _misc::from_MAP(lock_str.substr(2 /* [*/, MAP_len), lock);
            return lock;
        }

    private:
        static constexpr int MAP_len = _misc::MAP_length;
        static constexpr int rule_len = MAP_len + 3; // "MAP..."
        static constexpr int lock_len = MAP_len + 3; // " [...]"

    public:
        // data = prefix + rule_str + lock_str + suffix.
        // !has-rule -> data = prefix
        extrT(const std::span<const char> data, const bool with_lock) {
            const char* const begin = data.data();
            const char* const end = begin + data.size();
            std::string_view str{begin, end};
            for (;;) {
                if (const auto pos = str.find("MAP"); pos != str.npos && str.size() - pos >= rule_len) {
                    str.remove_prefix(pos);
                } else { // Not found.
                    this->prefix = {begin, end};
                    return;
                }

                if (const int len = _misc::count_base64(str.substr(3 /*MAP*/, MAP_len)); len < MAP_len) {
                    str.remove_prefix(3 /*MAP*/ + len);
                } else { // Matched.
                    this->prefix = {begin, str.data()};
                    this->rule_str = {str.data(), rule_len};
                    str.remove_prefix(rule_len);
                    if (with_lock && str.size() >= lock_len && str.starts_with(" [") &&
                        str[2 /* [*/ + MAP_len] == ']') {
                        if (_misc::count_base64(str.substr(2 /* [*/, MAP_len)) == MAP_len) {
                            this->lock_str = {str.data(), lock_len};
                            str.remove_prefix(lock_len);
                        }
                    }
                    assert(str.data() + str.size() == end);
                    this->suffix = str;
                    return;
                }
            }
        }
    };

    inline extrT extract_MAP_str(std::span<const char> data, bool with_lock = false) { //
        return extrT(data, with_lock);
    }

    inline std::optional<ruleT> extract_one_rule(std::string_view str) {
        if (const auto extr = extract_MAP_str(str); extr.has_rule()) {
            return extr.get_rule();
        }
        return std::nullopt;
    }

    inline std::vector<ruleT> extract_all_rules(std::string_view str) {
        std::vector<ruleT> rules;
        for (;;) {
            if (const auto extr = extract_MAP_str(str); extr.has_rule()) {
                rules.push_back(extr.get_rule());
                str = extr.suffix;
            } else {
                return rules;
            }
        }
    }

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_MAP_str = [] {
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
                const ruleT gol = game_of_life();
                assert(to_MAP_str(gol) == gol_str);

                const auto extr = extract_MAP_str(gol_str, true);
                assert(extr.prefix == "" && extr.suffix == "");
                assert(extr.get_rule() == gol);
                assert(!extr.has_lock());
            }

            {
                ruleT rule{};
                lockT lock{};
                for (const codeT code : each_code) {
                    rule[code] = cellT(testT::rand() & 1);
                    lock[code] = testT::rand() & 1;
                }
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
        };
    } // namespace _tests
#endif // ENABLE_TESTS

    class compressT {
        std::array<std::uint8_t, 64> m_data;

    public:
        compressT(const ruleT& rule) : m_data{} {
            for (const codeT c : each_code) {
                m_data[c.val >> 3] |= rule[c] << (c.val & 0b111);
            }
        }
        ruleT decompress() const {
            return make_rule([&](const codeT c) { //
                return cellT(1 & (m_data[c.val >> 3] >> (c.val & 0b111)));
            });
        }
        operator ruleT() const { return decompress(); }

        friend bool operator==(const compressT&, const compressT&) = default;

        size_t hash() const {
            return std::hash<std::string_view>{}( //
                {reinterpret_cast<const char*>(&m_data), sizeof(m_data)});
        }
    };

    static_assert(std::is_trivially_copyable_v<compressT>);

#ifdef ENABLE_TESTS
    namespace _tests {
        inline const testT test_compressT = [] {
            const ruleT a = make_rule([](auto) { return cellT(testT::rand() & 1); });
            const compressT b = a;
            assert(b == compressT(a));
            assert(b.decompress() == a);
        };
    } // namespace _tests
#endif
} // namespace aniso
