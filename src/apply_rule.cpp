#include <numbers>
#include <ranges>
#include <unordered_map>

#include "tile.hpp"

#include "common.hpp"

static ImVec2 to_imvec(const aniso::vecT& vec) { return ImVec2(vec.x, vec.y); }

template <float (&fn)(float) = floor>
static aniso::vecT from_imvec(const ImVec2& vec) {
    return {.x = int(fn(vec.x)), .y = int(fn(vec.y))};
}

static aniso::rangeT clamp_window(aniso::vecT size, aniso::vecT region_center, aniso::vecT region_size) {
    region_size = aniso::min(size, region_size);
    aniso::vecT begin = aniso::clamp(region_center - region_size / 2, {0, 0}, size - region_size);
    return {.begin = begin, .end = begin + region_size};
}

static bool strobing(const aniso::ruleT& rule) {
    constexpr aniso::codeT all_0{0}, all_1{511};
    return rule[all_0] == 1 && rule[all_1] == 0;
}

static constexpr int step_fast = 10;

static int adjust_step(int step, bool strobing) {
    if ((step % 2) && strobing) {
        return step + 1;
    }
    return step;
}

//           q w -
//           a s d
// Works for - x c case (~ rules in 'Hex' subset).
static void hex_image(const aniso::tile_const_ref source, const aniso::vecT /*source*/ center,
                      const aniso::vecT window_size, const double hex_d /*distance between adjacent centers*/) {
    const auto rel_pos = [](const double x, const double y /*normalized, relative to a center*/) -> aniso::vecT {
        // q w -     q w -
        // a s d ~  a s d (1,0)
        // - x c   - x c (1,1)
        //         (0,1)
        // When diameter = 1, there are:
        // s>x = (-1/2,sqrt3/2), s>d = (1,0), and
        // For any integers i and j, s>x * i + s>d * j points at hexagon centers (bijection).
        constexpr double _sqrt3_div_2 = std::numbers::sqrt3 / 2;
        constexpr double _2_div_sqrt3 = std::numbers::inv_sqrt3 * 2;

        // Let (x,y) = s>x * y2 + s>d * x2, there are:
        const double y2 = y * _2_div_sqrt3;
        const double x2 = x + y2 * 0.5;

        // And the point should belong to the hexagon with the nearest center:
        double min_dist_sqr = 100;
        int dx = 0, dy = 0;
        const auto find_nearest = [&](const double center_x2, const double center_y2 /*integral*/) {
            const double center_x = -0.5 * center_y2 + center_x2;
            const double center_y = _sqrt3_div_2 * center_y2;
            const double dist_sqr = (x - center_x) * (x - center_x) + (y - center_y) * (y - center_y);
            if (dist_sqr < min_dist_sqr) {
                min_dist_sqr = dist_sqr;
                dx = center_x2, dy = center_y2;
            }
        };

        const double x2_flr = floor(x2), y2_flr = floor(y2);
        find_nearest(x2_flr, y2_flr);
        find_nearest(x2_flr + 1, y2_flr);
        find_nearest(x2_flr, y2_flr + 1);
        find_nearest(x2_flr + 1, y2_flr + 1);
        return {.x = dx, .y = dy};
    };

    aniso::tileT dest(window_size);
    const auto dest_data = dest.data();
    const double _1_div_hex_d = 1.0 / hex_d;
    for (int y = 0; y < window_size.y; ++y) {
        for (int x = 0; x < window_size.x; ++x) {
            const auto pos =
                center + rel_pos((x - window_size.x / 2) * _1_div_hex_d, (y - window_size.y / 2) * _1_div_hex_d);
            if (source.contains(pos)) {
                dest_data.at(x, y) = source.at(pos);
            } else {
                dest_data.at(x, y) = aniso::cellT((x + y) & 1); // Checkerboard texture.
            }
        }
    }

    ImGui::Image(to_texture(dest_data, scaleE::Linear), to_imvec(window_size));
}

static bool want_hex_mode(const aniso::ruleT& rule) {
    if (shortcuts::no_ctrl() && shortcuts::global_flag(ImGuiKey_6)) {
        if (!rule_algo::is_hexagonal_rule(rule)) {
            // (But actually, the projection still corresponds to a range-2 hex rule.)
            messenger::set_msg("This rule does not belong to 'Hex' set.");
            // return false;
        }
        return true;
    }
    return false;
}

// TODO: error-prone. Should work in a way similar to `identify`.
// Copy the subrange and run as a torus space, recording all invoked mappings.
// This is only good at capturing simple, "self-contained" patterns (oscillators/spaceships).
// For more complex situations, the program has "open-capture" (`fake_apply`) to record
// areas frame-by-frame.
[[maybe_unused]] static aniso::lockT capture_closed(const aniso::tile_const_ref tile, const aniso::ruleT& rule) {
    aniso::lockT lock{};
    aniso::tileT torus(tile);

    // (wontfix) It's possible that the loop fails to catch all invocations in very rare cases,
    // due to that `limit` is not large enough.

    // Loop until there has been `limit` generations without newly invoked mappings.
    constexpr int limit = 120;
    for (int g = 0; g < limit; ++g) {
        aniso::lockT next = lock;
        torus.run_torus([&](const aniso::codeT code) {
            next[code] = true;
            return rule[code];
        });
        if (compare_update(lock, next)) {
            g = -1; // ++ -> 0
        }
    }

    return lock;
}

static bool check_border(const aniso::tile_const_ref tile, const aniso::vecT p_size) {
    if (!tile.size.both_gt(p_size * 2)) {
        messenger::set_msg("The area is too small. (Should be larger than {}*{}.)", p_size.x * 2, p_size.y * 2);
        return false;
    } else if (!aniso::has_enclosing_period(tile, p_size)) {
        messenger::set_msg("The area is not enclosed in {}*{} periodic background.", p_size.x, p_size.y);
        return false;
    }
    return true;
}

// Identify spaceships or oscillators in periodic (including pure) background. (Cannot deal with non-trivial
// objects like guns, puffers etc.)
// The area should be fully surrounded by periodic border, and contain a full phase of the object (one or
// several oscillators, or a single spaceship).
static void identify(const aniso::tile_const_ref tile, const aniso::ruleT& rule, const aniso::vecT period_size,
                     const bool require_matching_bg = true) {
    assert(period_size.both_lteq({4, 4}));
    if (!check_border(tile, period_size)) {
        return;
    }

    const aniso::tile_buf init_bg = tile.clip_corner(period_size);
    if (!aniso::torus_period(rule, init_bg.data(), 20).has_value()) {
        messenger::set_msg("The background is not temporally periodic.");
        return;
    }

    aniso::tilesetT matching_bg;
    if (require_matching_bg) {
        matching_bg.insert_rotation_group(init_bg);
    }

    // Empty-range -> invalid.
    static const auto locate_pattern_with_bg = [](const aniso::tile_const_ref tile, const aniso::vecT period_size,
                                                  const bool for_input = false) -> aniso::rangeT {
        // assert(aniso::has_enclosing_period(tile, period_size));
        const aniso::rangeT range = aniso::bounding_box(tile, period_size);
        if (range.empty()) {
            messenger::set_msg(for_input ? "The area contains no pattern." : "The pattern dies out.");
            return {};
        } else if (const auto size = range.size(); size.x > 3000 || size.y > 3000 || size.xy() > 400 * 400) {
            // For example, this can happen when the initial area contains a still life and a spaceship.
            messenger::set_msg(for_input ? "The area is too large." : "The pattern grows too large.");
            return {};
        } else if (!(range.begin.both_gteq(period_size) && (range.end + period_size).both_lteq(tile.size))) {
            assert(false);
            return {};
        } else {
            return {.begin = range.begin - period_size, .end = range.end + period_size};
        }
    };

    struct regionT {
        aniso::vecT period_size;
        aniso::tileT tile;
        aniso::rangeT range; // Range of pattern (including a layer of bg), relative to `tile`.
        aniso::vecT off;     // Pattern's begin pos, relative to the initial pattern.

        aniso::tile_const_ref get_pattern() const { return tile.data().clip(range); }

        bool run(const aniso::ruleT& rule) {
            const aniso::tile_const_ref pattern = get_pattern();
            const aniso::tile_const_ref background = pattern.clip_corner(period_size);
            const aniso::vecT padding = {1, 1};
            // (Ceiled for torus run. This can be avoided if `border_ref` is calculated manually, but that
            // will be a lot of code.)
            aniso::tileT next(aniso::divmul_ceil(range.size() + padding * 2, period_size));

            const aniso::rangeT relocate{.begin = padding, .end = padding + pattern.size};
            aniso::fill_outside(next.data(), relocate,
                                aniso::realign_from_to(background, padding, {0, 0}) /*relative to `next`*/);
            aniso::copy(next.data().clip(relocate), pattern);
            next.run_torus(rule);

            tile.swap(next);
            if (const auto next_range = locate_pattern_with_bg(tile.data(), period_size); //
                !next_range.empty()) {
                off = off - padding + next_range.begin;
                range = next_range;
                return true;
            }
            return false;
        }
    };

    const auto init_range = locate_pattern_with_bg(tile, period_size, true /*for-input*/);
    if (init_range.empty()) {
        return;
    }

    const aniso::tile_const_ref init_pattern = tile.clip(init_range);
    regionT region{.period_size = period_size,
                   .tile = aniso::tileT(init_pattern),
                   .range = {{0, 0}, init_pattern.size},
                   .off = {0, 0}};
    aniso::tileT smallest = aniso::tileT(init_pattern);

    constexpr int limit = 4000; // Max period to deal with.
    for (int g = 1; g <= limit; ++g) {
        if (!region.run(rule)) {
            return;
        }

        const aniso::tile_const_ref pattern = region.get_pattern();
        if ((!require_matching_bg || matching_bg.contains(pattern.clip_corner(period_size))) &&
            pattern.size.xy() < smallest.size().xy()) {
            smallest = aniso::tileT(pattern);
        }
        if (aniso::equal(init_pattern, pattern)) {
            std::string str;
            const bool is_oscillator = region.off == aniso::vecT{0, 0};
            if (is_oscillator && g == 1) {
                str = "#C Still life.\n";
            } else if (is_oscillator) {
                str = std::format("#C Oscillator. Period:{}.\n", g);
            } else {
                str = std::format("#C Spaceship. Period:{}. Offset(x,y):({},{}).\n", g, region.off.x, region.off.y);
            }
            assert(str.ends_with('\n'));
            // TODO: pad an extra layer of bg pattern?
            str += aniso::to_RLE_str(smallest.data(), &rule);
            ImGui::SetClipboardText(str.c_str());
            messenger::set_msg(std::move(str));
            return;
        }
    }
    // For example, this can happen the object really has a huge period, or the initial area doesn't
    // contain a full phase (fragments that evolves to full objects are not recognized), or whatever else.
    messenger::set_msg("Cannot identify.");
}

class percentT {
    int m_val; // ∈ [0, 100].
public:
    // percentT(double v) { m_val = round(std::clamp(v, 0.0, 1.0) * 100); }
    constexpr percentT(int p) { m_val = std::clamp(p, 0, 100); }
    percentT(double) = delete;

    double get() const { return m_val / 100.0; }

    void step_slide(const char* label, int min = 0, int max = 100, int step = 1) {
        assert(0 <= min && min < max && max <= 100);
        imgui_StepSliderInt::fn(label, &m_val, min, max, step,
                                [](int val) { return std::format("{:.2f}", val / 100.0); });
    }

    bool operator==(const percentT&) const = default;
};

struct initT {
    int seed;
    percentT density;
    percentT area;
    aniso::tile_buf background; // Periodic background.

    bool operator==(const initT&) const = default;

    void initialize(aniso::tileT& tile) const {
        assert(!tile.empty());
        aniso::fill(tile.data(), background);

        const aniso::vecT tile_size = tile.size();
        const auto range = clamp_window(tile_size, tile_size / 2, tile_size.mul_and_trunc(area.get()));
        if (!range.empty()) {
            // (Not caring about how the area is aligned with the background.)
            std::mt19937 rand{(uint32_t)seed};
            if (aniso::is_pure_0(background)) {
                aniso::random_fill(tile.data().clip(range), rand, density.get());
            } else {
                aniso::random_flip(tile.data().clip(range), rand, density.get());
            }
        }
    }
};

static_assert(std::is_trivially_copyable_v<initT>);

struct float_pair {
    float val;
    const char* str;
};

class zoomT {
    static constexpr float_pair terms[]{{0.5, "0.5"}, {1, "1"}, {2, "2"}, {3, "3"}, {4, "4"}, {5, "5"}};
    static constexpr int index_1 = 1;
    static constexpr int index_max = std::size(terms) - 1; // ]
    static_assert(terms[index_1].val == 1);

    int m_index; // = index_1;

    // Why is aggr init not allowed for private members, even inside class?
    constexpr zoomT(int i) : m_index{i} {}

public:
    constexpr zoomT() : m_index{index_1} {}
    bool operator==(const zoomT&) const = default;

    void slide(int di) { m_index = std::clamp(m_index + di, 0, index_max); }

    static constexpr zoomT min() { return zoomT{0}; }
    static constexpr zoomT max() { return zoomT{index_max}; }
    static std::span<const zoomT> list() {
        // return std::views::iota(0, index_max + 1) | std::views::transform([](int i) { return zoomT{i}; });
        static constexpr zoomT data[]{{0}, {1}, {2}, {3}, {4}, {5}};
        static_assert(std::size(data) == std::size(terms));
        return data;
    }

    constexpr operator float() const { return terms[m_index].val; }
    const char* str() const { return terms[m_index].str; }
};

// TODO: stop running when the space is moved out of scope?
// TODO: refactor; the code is horribly messy...
class runnerT : no_copy {
    class staged_rule {
        rule_with_rec rule = aniso::game_of_life();
        // std::optional<aniso::ruleT> next = std::nullopt;
        aniso::ruleT next = {};
        bool has_next = false;

    public:
        operator const aniso::ruleT&() const { return rule.get(); }
        const aniso::ruleT& get() const { return rule.get(); }
        const rec_for_rule& rec() const { return rule.rec(); }

        void set_next(const aniso::ruleT& r) {
            next = r;
            has_next = true;
        }
        bool update() {
            // TODO: restart as long as `has_next`?
            if (std::exchange(has_next, false) && rule.get() != next) {
                rule.set(next);
                return true;
            }
            return false;
        }
    };

    staged_rule current_rule{};

    struct paceT {
        int step = 1;
        global_timer::intervalT interval{init_zero_interval ? 0 : global_timer::min_nonzero_interval};
    };

    class ctrlT {
        paceT pace{};
        int extra_step = 0; // Workaround for +s/+1/+!.
        bool pause = false;
        bool extra_pause = false;
        bool skip_run = false; // Affects only auto mode.

        // TODO: the interop between different parts is pretty awkward...
        // m_paste.create -> push_pause_for_m_paste.
        // any interrupting write-access to pause -> invalidate stashed_pause.
        // m_paste.reset -> pop_pause_for_m_paste (~ restore if not invalidated).
        std::optional<bool> stashed_pause = std::nullopt;

    public:
        void push_pause_for_m_paste() { stashed_pause = std::exchange(pause, true); }
        void pop_pause_for_m_paste() {
            if (stashed_pause) {
                pause = *stashed_pause;
                stashed_pause.reset();
            }
        }

        int calc_step(const aniso::ruleT& rule, const bool newly_restarted, const bool extra_skip) {
            const bool ex_pause = std::exchange(extra_pause, false);
            const int ex_step = std::exchange(extra_step, 0);
            if (pause || ex_pause || extra_skip) {
                skip_run = true;
            }

            if (!pause && !ex_pause && newly_restarted) {
                // (Unless paused) skip displaying initial state for better visual.
                skip_run = true;
                return adjust_step(pace.step, strobing(rule));
            } else if (ex_step) {
                skip_run = true;
                return ex_step;
            } else if (!pause && !ex_pause && pace.interval.test() && !std::exchange(skip_run, false)) {
                return adjust_step(pace.step, strobing(rule));
            } else {
                return 0;
            }
        }

        void pause_for_this_frame() { extra_pause = true; }

        bool get_pause() const { return pause; }
        void set_pause(bool p) {
            pause = p;
            stashed_pause.reset();
        }
        void flip_pause() { set_pause(!pause); }

        void set_step_interval(const func_ref<void(paceT&, int&)> fn) { fn(pace, extra_step); }
    };

    ctrlT m_ctrl{};

    class torusT {
    public:
        static constexpr initT init_init{.seed = 0, .density = 50, .area = 50, .background = aniso::cellT{0}};
        static constexpr aniso::vecT init_size{.x = 600, .y = 400};
        static constexpr aniso::vecT min_size{.x = 30, .y = 20};
        static constexpr aniso::vecT max_size{.x = 1500, .y = 1000};

    private:
        initT m_init = init_init;
        aniso::tileT m_tile = {};
        int m_gen = 0;

        bool m_resized = true;
        bool m_newly_restarted = true;
        bool m_written = true;

    public:
        torusT() { resize_and_restart(init_size); }

        void restart() {
            assert_implies(m_newly_restarted, m_gen == 0);
            m_gen = 0;
            m_init.initialize(m_tile);
            m_newly_restarted = true;
            m_written = true;
        }
        // TODO: whether to try to avoid repeated init?
        // void restart_opt() {
        //     if (!m_newly_restarted) {
        //         restart();
        //     }
        // }

        aniso::tile_const_ref read_only() const { return m_tile.data(); }
        aniso::tile_const_ref read_only(const aniso::rangeT& range) const { return m_tile.data().clip(range); }

        aniso::tile_ref write_only() {
            m_newly_restarted = false;
            m_written = true;
            return m_tile.data();
        }
        aniso::tile_ref write_only(const aniso::rangeT& range) {
            m_newly_restarted = false;
            m_written = true;
            return m_tile.data().clip(range);
        }

        void read_and_maybe_write(const func_ref<bool(aniso::tile_ref)> fn) {
            if (fn(m_tile.data())) {
                m_newly_restarted = false;
                m_written = true;
            }
        }

        void rotate_00_to(int dx, int dy) {
            if (dx != 0 || dy != 0) {
                m_newly_restarted = false;
                m_written = true;
                aniso::tileT temp(m_tile.size());
                aniso::rotate_copy_00_to(temp.data(), m_tile.data(), {.x = dx, .y = dy});
                m_tile.swap(temp);
            }
        }

        int gen() const { return m_gen; }
        double density() const { return double(aniso::count(m_tile.data())) / m_tile.size().xy(); }

        aniso::vecT size() const {
            assert(m_tile.size() == calc_size(m_tile.size()));
            return m_tile.size();
        }

        aniso::vecT calc_size(const aniso::vecT size) const {
            const aniso::vecT n_size =
                aniso::divmul_floor(aniso::clamp(size, min_size, max_size), m_init.background.size());
            if (!n_size.both_gteq(min_size)) [[unlikely]] {
                return aniso::divmul_ceil(min_size, m_init.background.size());
            }
            return n_size;
        }

        bool resize(const aniso::vecT size) {
            if (m_tile.resize(calc_size(size))) {
                m_resized = true;
                restart();
                return true;
            }
            return false;
        }
        bool resized_since_last_check() { return std::exchange(m_resized, false); }

    private:
        void resize_and_restart(const aniso::vecT size) {
            if (!resize(size)) {
                restart();
            }
        }

    public:
        const initT& get_init() const { return m_init; }
        bool set_init(const initT& init) {
            if (compare_update(m_init, init)) {
                resize_and_restart(m_tile.size()); // In case the background is resized.
                return true;
            }
            return false;
        }
        bool resize_and_set_init(const aniso::vecT& size, const initT& init) {
            if (compare_update(m_init, init)) {
                resize_and_restart(size);
                return true;
            } else {
                return resize(size);
            }
        }

        void run(const aniso::ruleT& rule, ctrlT& ctrl) {
            const int step =
                ctrl.calc_step(rule, std::exchange(m_newly_restarted, false), std::exchange(m_written, false));
            for (int c = 0; c < step; ++c) {
                m_tile.run_torus(rule);
                ++m_gen;
            }
        }
    };

    torusT m_torus{}; // Space.

    // space-pos == corner-pos + canvas-pos / zoom
    struct coordT {
        zoomT zoom{};
        ImVec2 corner_pos = {0, 0}; // Space.
        ImVec2 to_space(ImVec2 canvas_pos) const { return corner_pos + canvas_pos / zoom; }
        ImVec2 to_canvas(ImVec2 space_pos) const { return (space_pos - corner_pos) * zoom; }
        void bind(const ImVec2 space_pos, const ImVec2 canvas_pos) { corner_pos = space_pos - canvas_pos / zoom; }
    };

    coordT m_coord{};
    ImVec2 to_rotate = {0, 0};

    struct pasteT {
        std::optional<aniso::ruleT> rule = std::nullopt;
        aniso::tileT tile{};
        aniso::vecT beg{0, 0};
        aniso::blitE mode = aniso::blitE::Copy;
        inline static bool paste_once = true; // TODO: static or per object?

        aniso::vecT size() const { return tile.size(); }
    };
    std::optional<pasteT> m_paste = std::nullopt;

    struct selectT {
        bool active = true;
        aniso::vecT beg{0, 0}, end{0, 0}; // [] instead of [).

        aniso::rangeT to_range() const {
            const auto [xmin, xmax] = std::minmax(beg.x, end.x);
            const auto [ymin, ymax] = std::minmax(beg.y, end.y);

            return {.begin{xmin, ymin}, .end{xmax + 1, ymax + 1}};
        }

        int width() const { return std::abs(beg.x - end.x) + 1; }
        int height() const { return std::abs(beg.y - end.y) + 1; }
        aniso::vecT size() const { return {.x = width(), .y = height()}; }
    };
    std::optional<selectT> m_sel = std::nullopt;

    bool locate_center = true;
    bool find_suitable_zoom = true;

    // Any state change (size or init) -> reset area & pattern.
    // Any attempt to resize (not including init-bg) -> reset pos.
    void reset_pos() {
        locate_center = true;
        find_suitable_zoom = true;
    }
    void reset_m_paste() {
        m_paste.reset();
        m_ctrl.pop_pause_for_m_paste();
    }
    void reset_m_paste_and_m_sel() {
        reset_m_paste();
        m_sel.reset();
    }

    rule_snapshot m_snapshot{}; // For `current_rule`.
    test_appearing m_appearing{};

public:
    void set_rule(const aniso::ruleT& rule) { current_rule.set_next(rule); }

    void set_state(const aniso::vecT& size, const initT& init) {
        reset_pos();
        if (m_torus.resize_and_set_init(size, init)) {
            reset_m_paste_and_m_sel();
        }
    }

    // (wontfix) Not designed to support multiple instances. (For example, some settings use static variables.)
    void display() {
        if (m_appearing.update()) {
            reset_m_paste();
            m_snapshot.clear();
        }
        if (current_rule.update()) {
            m_torus.restart();
            m_ctrl.set_pause(false);
        }

        bool highlight_canvas = false;
        {
            // !!TODO: 'Space ops' is also a bad name...
            ImGui::AlignTextToFramePadding();
            if (imgui_StrTooltip(
                    "(...)",
                    "The \"main window\" (as highlighted below) provides strictly wider control than preview windows, and is able to operate on patterns (like saving and pasting; open 'Space ops' for details).\n\n"
                    "All spaces use torus topology, i.e. information can go across boundaries directly, so the entire space can be treated as a periodic unit in an infinite space.")) {
                highlight_canvas = true;
            }
            ImGui::SameLine();

            const ImGuiID map_id = ImGui::GetID("MAP-str");
            imgui_StrWithID(aniso::to_MAP_str(current_rule), map_id);
            if (!pass_rule::source(current_rule)) {
                assert(ImGui::GetItemID() == map_id);
                rclick_popup::popup(map_id, [&] {
                    if (ImGui::Selectable("Copy rule")) {
                        copy_rule::copy(current_rule);
                    }

                    selectable_to_take_snapshot("Recent", current_rule.rec(), m_snapshot);
                });
            }
            if (const auto* deliv = pass_rule::dest().get_deliv()) {
                current_rule.set_next(*deliv);
                messenger::dot();
            }
            guide_mode::item_tooltip(
                "MAP-string for the displayed rule; drag to send the rule elsewhere; drag a rule here to replace; open menu for recent rules.");
            if (m_snapshot) {
                display_snapshot("Recent - main window", m_snapshot, current_rule.rec(),
                                 {{.get = [&]() -> decltype(auto) { return current_rule.get(); },
                                   .set = [&](const aniso::ruleT& r) { current_rule.set_next(r); }}});
            }

            ImGui::Separator();
        }

        std::optional<zoomT> resize_fullscreen_tooltip = std::nullopt;
        std::optional<zoomT> resize_fullscreen = std::nullopt;

        constexpr const char* canvas_name = "Canvas";
        const ImGuiID canvas_id = ImGui::GetID(canvas_name);
        // The shortcuts are available only when the canvas is hovered or held.
        // (`keys_avail` is needed for hover case, as the canvas can still be hovered when another text-input is active (won't actually happen now).)
        const bool canvas_hovered_or_held =
            ((ImGui::GetActiveID() == canvas_id) || shortcuts::keys_avail()) && (ImGui::GetHoveredID() == canvas_id);

        const auto set_init_state_in_popup = [&] {
            const bool reset = ImGui::Button("Reset") && messenger::dot();
            ImGui::SameLine();
            const bool restart = ImGui::Button("Restart") || (shortcuts::keys_avail_and_no_ctrl() &&
                                                              shortcuts::test_pressed_and_highlight(ImGuiKey_R));
            ImGui::SameLine();
            if (imgui_CheckboxV("Pause", m_ctrl.get_pause()) ||
                (shortcuts::keys_avail_and_no_ctrl() && shortcuts::test_pressed_and_highlight(ImGuiKey_Space))) {
                m_ctrl.flip_pause();
            }
            ImGui::SameLine();
            imgui_StrTooltip("(?)", "The space will pause and restart if you 'Restart' or change init settings.\n\n"
                                    "The shortcuts for 'Restart' ('R') and 'Pause' ('Space') also work here.");

            initT init = reset ? torusT::init_init : m_torus.get_init();

            ImGui::PushItemWidth(item_width);
            imgui_StepSliderInt::fn("Seed", &init.seed, 0, 29);
            init.density.step_slide("Density");
            init.area.step_slide("Area");
            ImGui::PopItemWidth();

            ImGui::Separator();

            ImGui::AlignTextToFramePadding();
            imgui_StrTooltip("(...)", "Left-click a cell to set it to 1 (white).\n"
                                      "Right-click to set to 0 (black).\n\n"
                                      "'Ctrl' and left-click a cell to resize to that position.");
            ImGui::SameLine();
            imgui_Str("Background");
            ImGui::SameLine();
            if (ImGui::SmallButton("0/1")) {
                aniso::flip(init.background.data());
            }

            // There are:
            // demo_size.z is a multiple of any i <= max_period.z, and
            // cell_button_size.z * max_period.z == demo_size.z * demo_zoom (so the images have the same
            // size as the board)
            constexpr aniso::vecT max_period{.x = 4, .y = 4};
            constexpr aniso::vecT demo_size{.x = 24, .y = 24};
            constexpr int demo_zoom = 3;
            constexpr ImVec2 cell_button_size{18, 18};
            // static_assert(max_period.x * max_period.y == init.background.capacity()); This works, but why?
            static_assert(max_period.x * max_period.y == aniso::tile_buf::capacity());

            ImGui::InvisibleButton("##Board", cell_button_size * to_imvec(max_period),
                                   ImGuiButtonFlags_MouseButtonLeft |
                                       ImGuiButtonFlags_MouseButtonRight); // So right-click can activate the button.
            {
                const ImVec2 button_beg = ImGui::GetItemRectMin();
                const bool button_hovered = ImGui::IsItemHovered();
                const ImVec2 mouse_pos = ImGui::GetMousePos();
                ImDrawList& drawlist = *ImGui::GetWindowDrawList();
                const aniso::tile_ref data = init.background.data();
                std::optional<aniso::vecT> resize = std::nullopt;
                for (int y = 0; y < max_period.y; ++y) {
                    for (int x = 0; x < max_period.x; ++x) {
                        const bool in_range = x < data.size.x && y < data.size.y;

                        const ImVec2 cell_beg = button_beg + cell_button_size * ImVec2(x, y);
                        const ImVec2 cell_end = cell_beg + cell_button_size;
                        drawlist.AddRectFilled(cell_beg, cell_end,
                                               in_range ? (data.at(x, y) ? IM_COL32_WHITE : IM_COL32_BLACK)
                                                        : IM_COL32_GREY(60, 255));
                        drawlist.AddRect(cell_beg, cell_end, IM_COL32_GREY(160, 255));
                        if (button_hovered && ImRect(cell_beg, cell_end).Contains(mouse_pos) /*[)*/) {
                            if (shortcuts::ctrl()) {
                                if (ImGui::IsMouseClicked(ImGuiMouseButton_Left)) {
                                    resize = {.x = x + 1, .y = y + 1};
                                }
                            } else if (in_range) {
                                if (ImGui::IsMouseDown(ImGuiMouseButton_Right)) {
                                    data.at(x, y) = {0};
                                } else if (ImGui::IsMouseDown(ImGuiMouseButton_Left)) {
                                    data.at(x, y) = {1};
                                }
                            }
                        }
                    }
                }
                if (resize && init.background.size() != *resize) {
                    init.background.resize(*resize);
                }
            }

            {
                aniso::tileT demo(demo_size);
                aniso::fill(demo.data(), init.background);
                static aniso::tileT curr;
                static bool skip_run = true;
                if (ImGui::IsWindowAppearing() || curr.empty() || init.background != m_torus.get_init().background) {
                    curr = aniso::tileT(demo);
                    skip_run = true;
                }

                ImGui::SameLine(0, 0);
                imgui_Str(" ~ ");
                ImGui::SameLine(0, 0);
                ImGui::Image(to_texture(demo.data(), scaleE::Nearest), to_imvec(demo.size() * demo_zoom));
                imgui_ItemRect(IM_COL32_GREY(160, 255));
                ImGui::SameLine(0, 0);
                imgui_Str(" ~ ");
                ImGui::SameLine(0, 0);
                ImGui::Image(to_texture(curr.data(), scaleE::Nearest), to_imvec(curr.size() * demo_zoom));
                imgui_ItemRect(IM_COL32_GREY(160, 255));

                if (global_timer::test(200) && !std::exchange(skip_run, false)) {
                    curr.run_torus(current_rule.get());
                }
            }

            if (restart) {
                m_torus.restart();
            }
            if (m_torus.set_init(init) || restart) {
                m_ctrl.set_pause(true);
                reset_m_paste_and_m_sel();
            };
        };

        const auto input_size = [&] {
            static input_int input_x{}, input_y{};

            const float inner_spacing = imgui_ItemInnerSpacingX();
            const aniso::vecT size = m_torus.size();

            ImGui::AlignTextToFramePadding();
            imgui_Str("Size ~");
            ImGui::SameLine(0, inner_spacing);
            ImGui::SetNextItemWidth(floor((item_width - inner_spacing) / 2));
            const auto ix = input_x.input(5, "##Width", std::format("Width:{}", size.x).c_str());
            ImGui::SameLine(0, inner_spacing);
            ImGui::SetNextItemWidth(ceil((item_width - inner_spacing) / 2));
            const auto iy = input_y.input(5, "##Height", std::format("Height:{}", size.y).c_str());
            // Bruh...
            // ImGui::SameLine();
            // imgui_StrTooltip(
            //     "(?)", "Press 'Enter' to resize; if only one side is specified, the other will use the current size.");

            if (ix || iy) {
                reset_pos();

                // Both values will be flushed if either receives the enter key.
                if (m_torus.resize({.x = ix.value_or(input_x.flush().value_or(size.x)),
                                    .y = iy.value_or(input_y.flush().value_or(size.y))})) {
                    reset_m_paste_and_m_sel();
                }
            }
        };

        const auto select_zoom = [&] {
            ImGui::AlignTextToFramePadding();
            imgui_Str("Zoom ~");
            for (const zoomT& z : zoomT::list()) {
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                if (ImGui::RadioButton(z.str(), z == m_coord.zoom)) {
                    resize_fullscreen = z;
                }
                if (imgui_IsItemHoveredForTooltip()) {
                    resize_fullscreen_tooltip = z;
                }
            }
            ImGui::SameLine();
            if (imgui_StrTooltip("(?)", "Click a radio button to resize the space to fit the screen.\n\n"
                                        "(Scroll in the main window to zoom in/out without resizing.)")) {
                highlight_canvas = true;
            }
        };

        ImGui::PushItemWidth(item_width);
        ImGui::BeginGroup();
        m_ctrl.set_step_interval([&](paceT& pace, int& extra_step) {
            ImGui::AlignTextToFramePadding();
            if (imgui_StrTooltip("(...)", "Restart : R\n"
                                          "Pause   : Space\n"
                                          "+s/+1/+!: S/D/F (repeatable)\n"
                                          "-/+ Step    : 1/2 (repeatable)\n"
                                          "-/+ Interval: 3/4 (repeatable)\n\n"
                                          "These shortcuts work only when the main window is hovered.")) {
                highlight_canvas = true;
            }

            const bool enable_shortcuts = canvas_hovered_or_held && shortcuts::no_ctrl();
            const auto item_shortcut = [enable_shortcuts](ImGuiKey key, bool repeat) {
                return enable_shortcuts && shortcuts::test_pressed_and_highlight(key, repeat);
            };

            ImGui::SameLine();
            if (ImGui::Button("Restart") || item_shortcut(ImGuiKey_R, false)) {
                m_torus.restart();
            }
            ImGui::SameLine();
            if (imgui_CheckboxV("Pause", m_ctrl.get_pause()) || item_shortcut(ImGuiKey_Space, false)) {
                m_ctrl.flip_pause();
            }
            ImGui::PushItemFlag(ImGuiItemFlags_ButtonRepeat, true);
            ImGui::SameLine();
            if (ImGui::Button("+s") || item_shortcut(ImGuiKey_S, true)) {
                extra_step = m_ctrl.get_pause() ? adjust_step(pace.step, strobing(current_rule)) : 0;
                m_ctrl.set_pause(true);
            }
            ImGui::SameLine();
            if (ImGui::Button("+1") || item_shortcut(ImGuiKey_D, true)) {
                extra_step = 1;
            }
            ImGui::SameLine();
            ImGui::Button("+!");
            if ((ImGui::IsItemActive() && ImGui::IsItemHovered() /* && ImGui::IsMouseDown(ImGuiMouseButton_Left)*/) ||
                (enable_shortcuts && shortcuts::test_down_and_highlight(ImGuiKey_F))) {
                extra_step = adjust_step(step_fast, strobing(current_rule));
            }
            ImGui::PopItemFlag(); // ImGuiItemFlags_ButtonRepeat
            ImGui::SameLine();
            imgui_StrTooltip("(?)", [] {
                imgui_StrPair(
                    "+s: ",
                    "Run manually, i.e. firstly pause the space, then advance generation by 'Step' afterwards.");
                imgui_StrPair("+1: ", "Advance generation by 1 (instead of 'Step').");
                imgui_StrPair("+!: ", "Speed up manually, i.e. advance generation by 10 in every frame.");
            });
            static_assert(step_fast == 10); // Used in tooltip.

            ImGui::Separator(); // To align with the left panel.

            const auto to_str = [is_strobing = strobing(current_rule)](int step) {
                if (!is_strobing) {
                    return std::to_string(step);
                } else {
                    return std::format("{} -> {}", step, adjust_step(step, is_strobing));
                }
            };

            if (enable_shortcuts) {
                imgui_StepSliderInt::next_shortcuts = {ImGuiKey_1, ImGuiKey_2};
            }
            imgui_StepSliderInt::fn("Step", &pace.step, 1, 100, 1, to_str);
            ImGui::SameLine();
            imgui_StrTooltip(
                "(?)", // !!TODO: rewrite...
                "For auto-mode, '+s' and '+!', if the rule maps all-0 case to 1 and all-1 case to 0, the step will be ceiled to 2*n (e.g. 1->2, 2->2) to avoid large spans of 0/1 areas flashing between two colors.\n\n"
                "('+1' is not affected, and can serve to change the parity of generation in all cases.)\n\n"
                "Such rules are called \"strobing rules\". There also exist rules that are non-strobing (so the adjustment won't take place) but can still develop non-trivial flashing areas. To avoid flashing effect for these rules, you can manually set a suitable step (likely 2*n).");
            if (enable_shortcuts) {
                imgui_StepSliderInt::next_shortcuts = {ImGuiKey_3, ImGuiKey_4};
            }
            pace.interval.step_slide("Interval", 0, 400);
        });
        ImGui::EndGroup();
        ImGui::SameLine(floor(1.5 * item_width));
        ImGui::BeginGroup();
        menu_like_popup::button("Init state");
        menu_like_popup::popup(set_init_state_in_popup);
        ImGui::SameLine();
        menu_like_popup::button("Reset");
        menu_like_popup::popup([&] {
            int id = 0;
            if (imgui_SelectableStyledButtonEx(id++, "Pos") && messenger::dot()) {
                reset_pos();
            }
            static_assert(torusT::init_size == aniso::vecT{600, 400});
            if (imgui_SelectableStyledButtonEx(id++, "Size (600*400)") && messenger::dot()) {
                reset_pos();
                if (m_torus.resize(torusT::init_size)) {
                    reset_m_paste_and_m_sel();
                }
            }
            if (imgui_SelectableStyledButtonEx(id++, "Init state") && messenger::dot()) {
                if (m_torus.set_init(torusT::init_init)) {
                    reset_m_paste_and_m_sel();
                }
            }
            if (imgui_SelectableStyledButtonEx(id++, "Size & state") && messenger::dot()) {
                reset_pos();
                if (m_torus.resize_and_set_init(torusT::init_size, torusT::init_init)) {
                    reset_m_paste_and_m_sel();
                }
            }
        });
        ImGui::SameLine();
        if (ImGui::Button("Reset pos") && messenger::dot()) {
            reset_pos();
        }
        guide_mode::item_tooltip(
            "Center the space, and select suitable zoom for it. (As if the space is newly resized.)");

        ImGui::Spacing(); // To align with the separator.

        input_size();
        select_zoom();

        ImGui::EndGroup();
        ImGui::PopItemWidth();

        ImGui::Separator();

        ImGui::AlignTextToFramePadding();
        if (imgui_StrTooltip(
                "(...)",
                "Scroll in the main window to zoom in/out.\n\n"
                "Drag with left button to move the space; 'Ctrl' and drag to \"rotate\" the space.\n\n"
                "Drag with right button to select area; single right-click to unselect.\n\n"
                "See 'Space ops' for more operations (e.g. 'C' to copy selected area, 'V' to load pattern). The shortcuts (including 'V') work only when the main window is hovered.")) {
            highlight_canvas = true;
        }
        ImGui::SameLine();
        static bool show_op_window = false;
        m_appearing.reset_if_appearing(show_op_window);
        ImGui::Checkbox("Space ops", &show_op_window);
        const int wide_spacing = imgui_ItemSpacingX() * 3; // imgui_CalcCharWidth(' ') * 3;
        ImGui::SameLine(0, wide_spacing);
        if (m_sel) {
            ImGui::Text("Selected:%d*%d", m_sel->width(), m_sel->height());
        } else {
            imgui_Str("Selected:N/A");
        }
        // ImGui::SameLine(0, wide_spacing); // TODO: looks good, but can stutter when selecting area...
        ImGui::SameLine(floor(1.5 * item_width));
        if constexpr (debug_mode) { // TODO: whether to show density?
            ImGui::Text("Generation:%d   Density:%.3f", m_torus.gen(), m_torus.density());
        } else {
            ImGui::Text("Generation:%d", m_torus.gen());
        }

        if (m_paste) {
            bool open = true;
            ImGui::SetNextWindowPos(ImGui::GetCursorScreenPos(), ImGuiCond_Appearing);
            imgui_Window::next_window_titlebar_tooltip = "Close the window, or press 'V' again to discard the pattern.";
            const std::string title = std::format("Pattern:{}*{}###Pattern", m_paste->size().x, m_paste->size().y);
            if (auto window = imgui_Window(title.c_str(), &open,
                                           ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoCollapse |
                                               ImGuiWindowFlags_AlwaysAutoResize)) {
                imgui_Str("Double-click to paste.");
                if (m_paste->rule && *(m_paste->rule) != current_rule) { // TODO: improve...
                    ImGui::SameLine(0, imgui_ItemSpacingX() * 2);
                    imgui_StrWithID("[Rule]");
                    if (!pass_rule::source(*(m_paste->rule))) {
                        imgui_ItemTooltip([&] {
                            imgui_Str("The pattern specified a different rule.");
                            previewer::preview(-1, previewer::default_settings, *(m_paste->rule));
                        });
                    }
                    ImGui::SameLine();
                    if (double_click_button_small("Apply")) {
                        current_rule.set_next(*(m_paste->rule));
                    }
                }

                // ImGui::Separator();

                imgui_RadioButton("Once", &m_paste->paste_once, true);
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("Multi", &m_paste->paste_once, false);

                ImGui::SameLine(0, 0), imgui_Str(" | "), ImGui::SameLine(0, 0);

                imgui_RadioButton("Copy##Mode", &m_paste->mode, aniso::blitE::Copy);
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("Or##Mode", &m_paste->mode, aniso::blitE::Or);
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("And##Mode", &m_paste->mode, aniso::blitE::And);
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("Xor##Mode", &m_paste->mode, aniso::blitE::Xor);
                ImGui::SameLine();
                imgui_StrTooltip("(?)", "Once:  Paste once (clear automatically after pasting).\n"
                                        "Multi: Paste multiple times.\n\n"
                                        "Copy:  Paste values directly.\n"
                                        "Or/And/Xor: Perform binary op.\n"
                                        "(Use Or/And to treat black/white cells as transparent bg.)\n\n"
                                        "Right-click the main window to switch between Copy/Or/And/Xor.");
                if (canvas_hovered_or_held && ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
                    switch (m_paste->mode) {
                        case aniso::blitE::Copy: m_paste->mode = aniso::blitE::Or; break;
                        case aniso::blitE::Or: m_paste->mode = aniso::blitE::And; break;
                        case aniso::blitE::And: m_paste->mode = aniso::blitE::Xor; break;
                        case aniso::blitE::Xor: m_paste->mode = aniso::blitE::Copy; break;
                        default: assert(false);
                    }
                }
            }
            if (!open) {
                reset_m_paste();
            }
        }

        // ImGui::Separator();

        {
            // (Values of GetContentRegionAvail() can be negative...)
            constexpr ImVec2 min_canvas_size{torusT::min_size.x * zoomT::max(), torusT::min_size.y * zoomT::max()};
            ImGui::InvisibleButton(canvas_name, ImMax(min_canvas_size, ImGui::GetContentRegionAvail()),
                                   ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
            const ImVec2 canvas_min = ImGui::GetItemRectMin();
            const ImVec2 canvas_max = ImGui::GetItemRectMax();
            const ImVec2 canvas_size = ImGui::GetItemRectSize();
            assert(canvas_id == ImGui::GetItemID());

            const bool active = ImGui::IsItemActive();
            const bool hovered = ImGui::IsItemHovered();
            const bool l_down = ImGui::IsMouseDown(ImGuiMouseButton_Left);
            const bool r_down = ImGui::IsMouseDown(ImGuiMouseButton_Right);
            if (active) {
                m_ctrl.pause_for_this_frame();
            }

            const auto fullscreen_size = [&](const zoomT& z) {
                // return m_torus.calc_size(from_imvec((canvas_size - ImVec2(20, 20)) / z));
                // (v So that 220*160 will choose zoom = 2...)
                return m_torus.calc_size(from_imvec((canvas_size - (z > 2 ? ImVec2(70, 70) : ImVec2(50, 50))) / z));
                // return m_torus.calc_size(from_imvec((canvas_size - ImVec2(30, 30) * std::max((float)z, 1.0f)) / z));
            };

            if (resize_fullscreen_tooltip) {
                if (ImGui::BeginTooltip()) {
                    const auto size = fullscreen_size(*resize_fullscreen_tooltip);
                    ImGui::Text("%d*%d", size.x, size.y);
                    ImGui::EndTooltip();
                }
            }
            if (resize_fullscreen) {
                reset_pos();
                m_coord.zoom = *resize_fullscreen;
                if (m_torus.resize(fullscreen_size(m_coord.zoom))) {
                    reset_m_paste_and_m_sel();
                    find_suitable_zoom = false;
                }
            }
            if (std::exchange(find_suitable_zoom, false)) {
                // Select the largest zoom that can hold the entire tile.
                m_coord.zoom = zoomT::min();
                for (const zoomT& z : std::views::reverse(zoomT::list())) {
                    if (fullscreen_size(z).both_gteq(m_torus.size())) {
                        m_coord.zoom = z;
                        break;
                    }
                }
            }

            // `m_torus` won't resize now.
            const aniso::vecT tile_size = m_torus.size();
            if (m_torus.resized_since_last_check()) {
                assert(!m_paste && !m_sel);
                reset_m_paste_and_m_sel(); // Defensive.
            }

            if (std::exchange(locate_center, false)) {
                m_coord.bind(to_imvec(tile_size) / 2, canvas_size / 2);
                to_rotate = {0, 0};
            }

            if (m_sel && m_sel->active && (!r_down || m_paste || ImGui::IsItemDeactivated())) {
                m_sel->active = false;
                // Allow a single right-click to unselect the area.
                // (`bounding_box` has no size check like this. This is intentional.)
                if (m_sel->size().xy() <= 2) {
                    m_sel.reset();
                }
            }

            std::optional<aniso::vecT> zoom_center = std::nullopt; // Not clamped.
            bool hex_mode = false;

            if (hovered) {
                const ImGuiIO& io = ImGui::GetIO();
                assert(ImGui::IsMousePosValid(&io.MousePos));
                const ImVec2 mouse_pos = io.MousePos - canvas_min;

                if (active && (!m_paste ? l_down && !r_down : l_down || r_down)) {
                    if (io.KeyCtrl) {
                        to_rotate += io.MouseDelta / m_coord.zoom;
                        const int dx = to_rotate.x, dy = to_rotate.y; // Truncate.
                        if (dx || dy) {
                            to_rotate -= ImVec2(dx, dy);
                            m_torus.rotate_00_to(dx, dy);
                        }
                    } else {
                        m_coord.corner_pos -= io.MouseDelta / m_coord.zoom;
                    }
                } else if (active && !m_paste && l_down && r_down) { // No rotating.
                    m_coord.corner_pos -= io.MouseDelta / m_coord.zoom;
                }

                if (imgui_MouseScrolling()) {
                    to_rotate = {0, 0};

                    const ImVec2 space_pos = m_coord.to_space(mouse_pos);
                    const ImVec2 space_pos_clamped = ImClamp(space_pos, {0, 0}, to_imvec(tile_size));
                    const ImVec2 mouse_pos_clamped = m_coord.to_canvas(space_pos_clamped); // Nearest point.
                    if (imgui_MouseScrollingDown()) {
                        m_coord.zoom.slide(-1);
                    } else if (imgui_MouseScrollingUp()) {
                        m_coord.zoom.slide(1);
                    }
                    m_coord.bind(space_pos_clamped, mouse_pos_clamped);
                }

                const aniso::vecT cel_pos = from_imvec(m_coord.to_space(mouse_pos));

                // (`want_hex_mode` should be tested only when the zoom window is really going to be shown.)
                if (!m_paste && !(m_sel && m_sel->active && m_sel->to_range().size().xy() > 2)) {
                    if (imgui_IsItemHoveredForTooltip() && cel_pos.both_gteq({-10, -10}) &&
                        cel_pos.both_lt(tile_size.plus(10, 10))) {
                        hex_mode = want_hex_mode(current_rule);
                        if (hex_mode || m_coord.zoom <= 1) {
                            zoom_center = cel_pos;
                        }
                    }
                }

                if (m_paste) {
                    assert(m_paste->size().both_lteq(tile_size));
                    m_paste->beg = aniso::clamp(cel_pos - m_paste->size() / 2, {0, 0}, tile_size - m_paste->size());
                } else {
                    if (ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
                        const aniso::vecT pos = aniso::clamp(cel_pos, {0, 0}, tile_size.minus(1, 1));
                        m_sel = {.active = true, .beg = pos, .end = pos};
                    } else if (m_sel && m_sel->active && r_down) {
                        m_sel->end = aniso::clamp(cel_pos, {0, 0}, tile_size.minus(1, 1));
                    }
                }
            }

            m_torus.run(current_rule, m_ctrl);

            // Render.
            {
                const ImVec2 screen_min = ImFloor(canvas_min + m_coord.to_canvas({0, 0}));
                const ImVec2 screen_max = screen_min + to_imvec(tile_size) * m_coord.zoom;

                ImDrawList& drawlist = *ImGui::GetWindowDrawList();
                drawlist.PushClipRect(canvas_min, canvas_max);
                drawlist.AddRectFilled(canvas_min, canvas_max, IM_COL32_GREY(24, 255));

                const scaleE scale_mode = m_coord.zoom < 1 ? scaleE::Linear : scaleE::Nearest;
                if (!m_paste) {
                    const ImTextureID texture = to_texture(m_torus.read_only(), scale_mode);

                    drawlist.AddImage(texture, screen_min, screen_max);
                    if (zoom_center.has_value()) {
                        ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, {0, 0});
                        if (ImGui::BeginTooltip()) {
                            const aniso::rangeT clamped = clamp_window(tile_size, *zoom_center, {80, 60});
                            assert(!clamped.empty());

                            if (hex_mode) {
                                // Using *zoom_center instead of (clamped.begin + .end) / 2, as otherwise the
                                // bottom-left corner cannot be fully shown.
                                hex_image(m_torus.read_only(), *zoom_center, clamped.size() * 3,
                                          std::max(double(m_coord.zoom), 3.0));
                            } else if (scale_mode == scaleE::Nearest) {
                                ImGui::Image(texture, to_imvec(clamped.size() * 3),
                                             to_imvec(clamped.begin) / to_imvec(tile_size),
                                             to_imvec(clamped.end) / to_imvec(tile_size));
                            } else {
                                // TODO: is it possible to reuse the texture in a different scale mode?
                                // (Related: https://github.com/ocornut/imgui/issues/7616)
                                ImGui::Image(to_texture(m_torus.read_only(clamped), scaleE::Nearest),
                                             to_imvec(clamped.size() * 3));
                            }

                            // (wontfix) It's too hard to correctly show selected area in hex mode.
                            if (!hex_mode && m_sel) {
                                const aniso::rangeT sel = aniso::common(clamped, m_sel->to_range());
                                if (!sel.empty()) {
                                    const ImVec2 zoom_min = ImGui::GetItemRectMin();
                                    ImGui::GetWindowDrawList()->AddRectFilled(
                                        zoom_min + to_imvec((sel.begin - clamped.begin) * 3),
                                        zoom_min + to_imvec((sel.end - clamped.begin) * 3), IM_COL32(0, 255, 0, 40));
                                }
                            }
                            ImGui::EndTooltip();
                        }
                        ImGui::PopStyleVar();
                    }
                } else {
                    assert(!zoom_center);
                    assert(m_paste->size().both_lteq(tile_size));
                    const aniso::vecT paste_beg = aniso::clamp(m_paste->beg, {0, 0}, tile_size - m_paste->size());
                    const aniso::vecT paste_end = paste_beg + m_paste->size();
                    m_paste->beg = paste_beg;

                    ImTextureID texture = 0;
                    // (wontfix) Wasteful, but after all this works...
                    m_torus.read_and_maybe_write([&](const aniso::tile_ref tile) {
                        const aniso::tile_ref paste_area = tile.clip({paste_beg, paste_end});
                        aniso::tileT temp(paste_area);
                        // !!TODO: (v0.9.9) support specifying align req.
                        aniso::blit(paste_area, m_paste->tile.data(), m_paste->mode);
                        texture = to_texture(tile, scale_mode);
                        if (hovered && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left)) {
                            if (m_paste->paste_once) {
                                reset_m_paste();
                            } else {
                                messenger::dot();
                            }
                            return true;
                        } else { // Restore.
                            aniso::copy(paste_area, temp.data());
                            return false;
                        }
                    });

                    drawlist.AddImage(texture, screen_min, screen_max);
                    // (It's ok to render for this frame even if `m_paste` has been consumed.)
                    const ImVec2 paste_min = screen_min + to_imvec(paste_beg) * m_coord.zoom;
                    const ImVec2 paste_max = screen_min + to_imvec(paste_end) * m_coord.zoom;
                    drawlist.AddRectFilled(paste_min, paste_max, IM_COL32(255, 0, 0, 60));
                }

                if (m_sel) {
                    const auto [sel_beg, sel_end] = m_sel->to_range();
                    const ImVec2 sel_min = screen_min + to_imvec(sel_beg) * m_coord.zoom;
                    const ImVec2 sel_max = screen_min + to_imvec(sel_end) * m_coord.zoom;
                    drawlist.AddRectFilled(sel_min, sel_max, IM_COL32(0, 255, 0, !m_paste ? 40 : 20));
                    // drawlist->AddRect(sel_min, sel_max, IM_COL32(0, 255, 0, 160));
                }
                drawlist.AddRect(screen_min, screen_max, previewer::default_border_color());
                drawlist.PopClipRect();

                if (hovered || highlight_canvas) {
                    imgui_ItemRect(ImGui::GetColorU32(ImGuiCol_Separator));
                }
            }

            range_operations(canvas_hovered_or_held, show_op_window);

            assert(tile_size == m_torus.size());
            assert(!m_torus.resized_since_last_check());
        }
    }

private:
    void range_operations(const bool canvas_hovered_or_held, bool& show_op_window) {
        // TODO: disable some operations if `m_paste.has_value`?
        // TODO: set pause for some operations?
        struct op_term : no_copy {
            void (*op)(runnerT& self);
            const char* key_label;
            ImGuiKey key;
            bool req_sel;

            // (Should respect -Wreorder-ctor)
            consteval op_term(ImGuiKey key, const char* key_label, bool req_sel, void (*op)(runnerT& self))
                : op(op), key_label(key_label), key(key), req_sel(req_sel) {}

            bool check_sel(const runnerT& self) const { return !req_sel || self.m_sel; }
            void apply(runnerT& self) const {
                if (check_sel(self)) {
                    op(self);
                }
            }
        };

        static percentT flip_den = 50;
        static constexpr op_term op_random_flip{
            ImGuiKey_Equal, "+ (=)", true,
            [](runnerT& self) {
                assert(self.m_sel);
                static std::mt19937 rand = rand_source::create();
                aniso::random_flip(self.m_torus.write_only(self.m_sel->to_range()), rand, flip_den.get());
            } // (Without a comment here, the clang-format result will be extremely ugly...)
        };
        static constexpr op_term op_flip{
            ImGuiKey_Minus, "- (_)", true,
            [](runnerT& self) {
                assert(self.m_sel);
                aniso::flip(self.m_torus.write_only(self.m_sel->to_range()));
            } //
        };

        static int background = 0; // TODO: -> enum.
        static constexpr op_term op_clear_inside{
            ImGuiKey_Backspace, "Backspace", true,
            [](runnerT& self) {
                assert(self.m_sel);
                const auto sel_area = self.m_torus.write_only(self.m_sel->to_range());
                if (background == 0 || background == 1) {
                    aniso::fill(sel_area, aniso::cellT(background));
                } else {
                    const aniso::vecT p_size{2, 2};
                    if (check_border(sel_area, p_size)) {
                        aniso::self_repeat(sel_area, p_size);
                    }
                }
            } //
        };
        static constexpr op_term op_clear_outside{
            ImGuiKey_0, "0 (zero)", true,
            [](runnerT& self) {
                assert(self.m_sel);
                if (background == 0 || background == 1) {
                    aniso::fill_outside(self.m_torus.write_only(), self.m_sel->to_range(), aniso::cellT(background));
                } else {
                    const aniso::rangeT sel_range = self.m_sel->to_range();
                    const auto sel_area = self.m_torus.read_only(sel_range);
                    const aniso::vecT p_size{2, 2};
                    if (check_border(sel_area, p_size)) {
                        aniso::fill_outside(
                            self.m_torus.write_only(), sel_range,
                            aniso::realign_from_to(sel_area.clip_corner(p_size), sel_range.begin, {0, 0}));
                    }
                }
            } //
        };

        static constexpr op_term op_select_all{
            ImGuiKey_A, "A", false,
            [](runnerT& self) {
                const aniso::vecT tile_size = self.m_torus.size();
                auto& m_sel = self.m_sel;
                if (!m_sel || m_sel->size() != tile_size) {
                    m_sel = {.active = false, .beg = {0, 0}, .end = tile_size.minus(1, 1)};
                } else {
                    m_sel.reset();
                }
            } //
        };
        static constexpr op_term op_bounding_box{
            ImGuiKey_B, "B", true,
            [](runnerT& self) {
                assert(self.m_sel);
                const aniso::rangeT sel_range = self.m_sel->to_range();
                const aniso::tile_const_ref sel_area = self.m_torus.read_only(sel_range);
                const aniso::vecT p_size{2, 2};
                if (check_border(sel_area, p_size)) {
                    const aniso::rangeT bound = aniso::bounding_box(sel_area, p_size);
                    if (!bound.empty()) {
                        self.m_sel = {.active = false,
                                      .beg = sel_range.begin + bound.begin - p_size,
                                      .end = sel_range.begin + bound.end.minus(1, 1) + p_size};
                    } else {
                        // m_sel.reset();
                        messenger::set_msg("The area contains no pattern.");
                    }
                }
            } //
        };

        // !!TODO: (v0.9.9) enhance to background sampling...
        static constexpr op_term op_test_bg_period{
            ImGuiKey_P, "P", true,
            [](runnerT& self) {
                assert(self.m_sel);
                const aniso::tile_const_ref sel_area = self.m_torus.read_only(self.m_sel->to_range());
                if (const auto p_size = aniso::spatial_period_full_area(sel_area, {30, 30})) {
                    if (const auto period = aniso::torus_period(self.current_rule, sel_area.clip_corner(*p_size), 60)) {
                        messenger::set_msg("Period: x = {}, y = {}, p = {}", p_size->x, p_size->y, *period);
                    } else {
                        messenger::set_msg("Spatial period: x = {}, y = {} (not temporally periodic)", p_size->x,
                                           p_size->y);
                    }
                } else {
                    // (The too-large case is considered impossible to occur naturally.)
                    messenger::set_msg("The area is too small, or not spatially periodic.");
                }
            } //
        };

        static constexpr bool add_rule = true; // TODO: whether to support specifying this?
        static constexpr bool show_rle = true;
        static constexpr op_term op_copy{
            ImGuiKey_C, "C", true,
            [](runnerT& self) {
                assert(self.m_sel);
                std::string rle_str = aniso::to_RLE_str(self.m_torus.read_only(self.m_sel->to_range()),
                                                        add_rule ? &self.current_rule.get() : nullptr);
                ImGui::SetClipboardText(rle_str.c_str());
                if constexpr (show_rle) { // TODO: sometimes noisy...
                    messenger::set_msg(std::move(rle_str));
                } else { // (But this doesn't work well with 'Cut'...)
                    messenger::set_msg("Copied.");
                }
            } //
        };
        static constexpr op_term op_cut{
            ImGuiKey_X, "X", true,
            [](runnerT& self) {
                assert(self.m_sel);
                const aniso::rangeT sel_range = self.m_sel->to_range();
                const aniso::vecT p_size{2, 2};
                if (check_border(self.m_torus.read_only(sel_range), p_size)) {
                    op_copy.op(self);
                    aniso::self_repeat(self.m_torus.write_only(sel_range), p_size);
                }
            } //
        };
        static constexpr op_term op_identify{
            ImGuiKey_I, "I (i)", true,
            [](runnerT& self) {
                assert(self.m_sel);
                const aniso::vecT p_size{2, 2};
                identify(self.m_torus.read_only(self.m_sel->to_range()), self.current_rule, p_size);
            } //
        };

        static constexpr op_term op_paste{
            ImGuiKey_V, "V", false,
            [](runnerT& self) {
                if (self.m_sel) {
                    self.m_sel->active = false;
                }
                if (self.m_paste) {
                    self.reset_m_paste();
                } else if (std::string_view text = read_clipboard(); !text.empty()) {
                    // (Not interested in whether the header has correct format.)
                    const std::string_view header = aniso::strip_RLE_header(text);
                    aniso::from_RLE_str(text, [&](const aniso::prepareT size) -> std::optional<aniso::tile_ref> {
                        if (size.empty()) {
                            messenger::set_msg("Found no pattern (RLE-string).");
                            return std::nullopt;
                        } else if (const aniso::vecT tile_size = self.m_torus.size(); //
                                   size.x > tile_size.x || size.y > tile_size.y) {
                            messenger::set_msg("The space is not large enough for the pattern.\n"
                                               "Space size: x = {}, y = {}\n"
                                               "Pattern size: x = {}, y = {}",
                                               tile_size.x, tile_size.y, size.x, size.y);
                            return std::nullopt;
                        } else {
                            self.m_ctrl.push_pause_for_m_paste();
                            self.m_paste = {.rule = aniso::extract_one_rule(header),
                                            .tile = aniso::tileT(aniso::vecT{.x = (int)size.x, .y = (int)size.y}),
                                            .beg = {0, 0},
                                            .mode = aniso::blitE::Copy};
                            return self.m_paste->tile.data();
                        }
                    });
                }
            } //
        };

        const op_term* op = nullptr;
        const op_term* op_highlight = nullptr;

        if (canvas_hovered_or_held && shortcuts::no_ctrl()) {
            static constexpr const op_term* op_list[]{
                &op_random_flip,    &op_flip, &op_clear_inside, &op_clear_outside, &op_select_all, &op_bounding_box,
                &op_test_bg_period, &op_copy, &op_cut,          &op_identify,      &op_paste};
            for (const op_term* t : op_list) {
                if (shortcuts::test_pressed(t->key, false)) {
                    op_highlight = t;
                    if (t->check_sel(*this)) {
                        op = t;
                    } else {
                        messenger::set_msg("No selected area.");
                    }
                    break;
                }
            }
        }
        if (show_op_window) {
            ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
            if (ImGui::IsMousePosValid()) {
                ImGui::SetNextWindowPos(ImGui::GetMousePos() + ImVec2(2, 2), ImGuiCond_Appearing);
            }
            if (auto window = imgui_Window("Space operations", &show_op_window,
                                           ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
                const auto term = [&](const char* label, const op_term& t) {
                    const bool valid = t.check_sel(*this);
                    // Was `ImGui::MenuItem(label, shortcut, nullptr, valid)`.
                    ImGui::BeginDisabled(!valid);
                    if (imgui_SelectableStyledButton(label, false, t.key_label)) {
                        assert(valid);
                        op = &t;
                    } else if (op_highlight == &t) {
                        highlight_item();
                    }
                    ImGui::EndDisabled();
                    if (!valid) {
                        imgui_ItemTooltip("No selected area.");
                    }
                };

                flip_den.step_slide("Density");
                term("Random flip", op_random_flip);
                term("Flip", op_flip);

                ImGui::Separator();

                ImGui::AlignTextToFramePadding();
                imgui_Str("Background ~");
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("0", &background, 0);
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("1", &background, 1);
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("C", &background, 2);
                ImGui::SameLine();
                imgui_StrTooltip("(?)", [] {
                    imgui_Str(
                        "For 'Clear inside/outside':\n"
                        "0/1: Fill area with 0 or 1 (unconditionally).\n"
                        "C:   If the area is enclosed in 2*2 periodic bg, fill area with the bg; otherwise do nothing.\n\n"
                        "The \"2*2 periodic bg\" includes the following patterns:");

                    static constexpr std::array<aniso::cellT, 4> bg_data[]{
                        {{{0}, {0}, {0}, {0}}}, {{{1}, {1}, {1}, {1}}}, {{{0}, {1}, {1}, {0}}}, {{{1}, {0}, {0}, {0}}},
                        {{{0}, {1}, {1}, {1}}}, {{{1}, {0}, {1}, {0}}}, {{{1}, {1}, {0}, {0}}},
                    };
                    constexpr int total = std::size(bg_data);
                    constexpr aniso::vecT demo_size{.x = 16, .y = 16};
                    constexpr int demo_zoom = 3;

                    aniso::tileT demo_tile({.x = demo_size.x, .y = demo_size.y * total});
                    for (int i = 0; i < total; ++i) {
                        const aniso::vecT pos{.x = 0, .y = demo_size.y * i};
                        aniso::fill(demo_tile.data().clip({pos, pos + demo_size}), {{bg_data[i].data(), {2, 2}}});
                    }
                    const ImTextureID texture = to_texture(demo_tile.data(), scaleE::Nearest);
                    for (int i = 0; i < total; ++i) {
                        if (i != 0) {
                            ImGui::SameLine();
                        }
                        ImGui::Image(texture, to_imvec(demo_size * demo_zoom), ImVec2(0, float(i) / total),
                                     ImVec2(1, float(i + 1) / total));
                        imgui_ItemRect(IM_COL32_GREY(160, 255));
                        if constexpr (debug_mode_log_aware) {
                            if (i == 0 && GImGui->LogEnabled) {
                                imgui_LogRenderedText(ImGui::GetItemRectMin(), ".....");
                            }
                        }
                    }
                });
                term("Clear inside", op_clear_inside);
                term("Clear outside", op_clear_outside);

                ImGui::Separator();

                term("Select all", op_select_all);
                term("Bound", op_bounding_box);
                guide_mode::item_tooltip(
                    "The area should be enclosed in 2*2 periodic background.\n\n"
                    "Get the bounding box for the pattern; the resulting bounding box will include a layer of background.");
                term("Test background", op_test_bg_period);
                guide_mode::item_tooltip("Test the size and period of periodic background.");

                ImGui::Separator();

                // ImGui::Checkbox("Rule info", &add_rule);
                // ImGui::SameLine();
                // imgui_StrTooltip(
                //     "(?)", "Whether to include rule info ('rule = ...') in the header for the patterns.\n\n"
                //            "(This applies to 'Copy' and 'Cut'. 'Identify' will always include rule info.)");
                term("Copy", op_copy);
                guide_mode::item_tooltip("Copy selected area (as RLE-string) to the clipboard.");
                term("Cut", op_cut);
                guide_mode::item_tooltip("The area should be enclosed in 2*2 periodic background.\n\n"
                                         "Copy selected area and clear area with the background.");
                term("Identify", op_identify);
                guide_mode::item_tooltip(
                    "The area should be enclosed in 2*2 periodic background.\n\n"
                    "Identify a single oscillator or spaceship in the area, and copy its smallest phase to the clipboard.");

                ImGui::Separator();

                term("Paste", op_paste);
                guide_mode::item_tooltip("Load pattern (RLE-string) from the clipboard.\n\n"
                                         "(To load list of rules, use the 'Clipboard' window instead.)");
            }
        }

        if (op) {
            op->apply(*this);
        }
    }
};

static runnerT runner;
static test_active runner_active;
void apply_rule(frame_main_token) {
    runner.display();
    runner_active.update();
}

class previewer_data : no_create {
    friend class previewer;

    static constexpr initT init_init{.seed = 0, .density = 50, .area = 100, .background = aniso::cellT{0}};
    inline static initT global_init = init_init; // Globally shared.

    struct termT {
        bool active = false;
        bool skip_run = false;  // (Affects only auto mode.)
        initT init = init_init; // (Whichever is ok; just to allow for default construction.)
        aniso::ruleT rule = {};
        aniso::tileT tile = {};
    };

    inline static std::unordered_map<uint64_t, termT> terms;
};

void previewer::configT::_set() {
    // ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, {3, 2});
    ImGui::PushItemWidth(item_width);

    const auto to_tile_size = [&](int size) { return std::to_string(int(size / zoom_)); };
    imgui_StepSliderInt::fn("Width", &width_, 120, 280, 20, to_tile_size);
    imgui_StepSliderInt::fn("Height", &height_, 120, 280, 20, to_tile_size);
    ImGui::AlignTextToFramePadding();
    imgui_Str("Zoom ~"); // TODO: should this be "zoom" or "scale"?
    static constexpr float_pair terms[]{{0.5, "0.5"}, {1, "1"}, {2, "2"}};
    for (const auto& [val, str] : terms) {
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        imgui_RadioButton(str, &zoom_, val);
    }

    ImGui::Separator();
    imgui_StepSliderInt::fn("Step", &step, 1, 30);
    interval.step_slide("Interval", 0, 400);

    ImGui::Separator();
    menu_like_popup::button("Init state");
    menu_like_popup::popup([] {
        initT& init = previewer_data::global_init;
        if (ImGui::Button("Reset") && messenger::dot()) {
            init = previewer_data::init_init;
        }
        ImGui::SameLine();
        imgui_Str("Background ~ ");
        ImGui::SameLine(0, 0);
        ImGui::Dummy(square_size());
        imgui_ItemRectFilled(IM_COL32_BLACK);
        imgui_ItemRect(IM_COL32_GREY(160, 255));

        imgui_StepSliderInt::fn("Seed", &init.seed, 0, 9);
        init.density.step_slide("Density", 10, 100, 10);
        init.area.step_slide("Area", 10, 100, 10);
    });
    ImGui::SameLine();
    imgui_StrTooltip("(?)", "Shared by all groups.");

    ImGui::PopItemWidth();
    // ImGui::PopStyleVar();
}

void previewer::begin_frame(frame_main_token) {
    if (!previewer_data::terms.empty()) {
        // According to https://en.cppreference.com/w/cpp/container/unordered_map/erase_if
        // There seems no requirement on the predicate.
        std::erase_if(previewer_data::terms, [](std::pair<const uint64_t, previewer_data::termT>& pair) {
            return !std::exchange(pair.second.active, false);
        });
    }
}

// TODO: allow setting the step and interval with shortcuts when the window is hovered?
void previewer::_preview(const uint64_t id, const configT& config, const aniso::ruleT& rule) {
    assert(ImGui::GetItemRectSize() == config.size_imvec());
    assert(ImGui::IsItemVisible());
    const int frame = ImGui::GetFrameCount();
    config.update_op(frame);

    previewer_data::termT& term = previewer_data::terms[id];
    if (term.active) [[unlikely]] { // ID conflict
        assert(false);
        return;
    }
    term.active = true;

    const aniso::vecT tile_size{.x = int(config.width_ / config.zoom_), .y = int(config.height_ / config.zoom_)};

    // TODO: (though the actual behaviors are ok) these op logics are quite messy...
    const bool passing = pass_rule::source(rule);
    rclick_popup::hoverE hov = rclick_popup::None;
    bool restart_from_menu = false;
    if (!passing) {
        const id_pair popup_id{ImGui::GetItemID(), (ImGuiID)(intptr_t)&term}; // Absolutely impossible to clash.
        hov = rclick_popup::popup_no_highlight(popup_id, [&] {
            // !!TODO: rewrite...
            imgui_StrTooltip("(...)", "Drag to send the rule elsewhere.\n"
                                      "Hold to pause.\n\n"
                                      "Similar to space window's 'Restart' and '+s/+1/+!':\n"
                                      "'R' to restart. (+'A' to apply to the entire group.)\n"
                                      "Hold (pause) + 'S' to run manually.\n"
                                      "'D' to advance generation by 1.\n"
                                      "'F' to speed up. (+'A' to apply to the entire group.)\n\n"
                                      "For rules that belong to 'Hex' set:\n"
                                      "'6' to see the projected view in hexagonal space.\n"
                                      "(This also applies to the space window.)");
            ImGui::SameLine();
            // imgui_StrTooltip("Belongs", [&] { _show_belongs(rule); });
            imgui_StrDisabled("Belongs");
            if (ImGui::BeginItemTooltip()) {
                _show_belongs(rule);
                ImGui::EndTooltip();
            }
            ImGui::Separator();

            if (ImGui::Selectable("Copy rule")) {
                copy_rule::copy(rule);
            }
            guide_mode::item_tooltip("Copy (as MAP-string) to the clipboard. "
                                     "Equivalent to sending the rule to '[C]' after 'Clipboard'.");
            if (ImGui::Selectable("Restart")) {
                restart_from_menu = true;
            }
            guide_mode::item_tooltip("Equivalent to the shortcut ('R').");

            if (runner_active) {
                // if (ImGui::Selectable("Apply (rule)") && messenger::dot()) {
                //     runner.set_rule(rule);
                // }
                // I really want to use "Copy state", but that can be misleading...
                if (ImGui::Selectable("Apply") && messenger::dot()) {
                    runner.set_rule(rule);
                    runner.set_state(tile_size, previewer_data::global_init);
                    // runner.set_state(term.tile.size(), term.init);
                }
                guide_mode::item_tooltip(
                    "Apply rule and space state to the main window, so you can operate on the same patterns (seen in this window).\n\n"
                    "(Send rule to the MAP-string ('MAP...') to apply only the rule.)");
            }
        });
    }
    assert_implies(passing, hov == rclick_popup::None);

    const bool hovered = hov == rclick_popup::Hovered; // ImGui::IsItemHovered();
    const bool active = ImGui::IsItemActive();
    const bool has_group_op = config.group_op.has_value();
    configT::opT op = has_group_op ? *config.group_op : configT::opT{};
    if (hovered && (active || shortcuts::keys_avail())) {
        // (Using unfiltered shortcut for `p_f` for smoother inter with seq op (<</>>).)
        const bool no_ctrl = shortcuts::no_ctrl();
        const configT::opT op2 = {.pause = active,
                                  .restart = no_ctrl && shortcuts::test_pressed(ImGuiKey_R),
                                  .p_s = no_ctrl && shortcuts::test_pressed(ImGuiKey_S, true),
                                  .p_1 = no_ctrl && shortcuts::test_pressed(ImGuiKey_D, true),
                                  .p_f = no_ctrl && shortcuts::global_flag(ImGuiKey_F)};
        if (no_ctrl && shortcuts::global_flag(ImGuiKey_A)) {
            config.group_op_next = op2;
            config.group_op_next_frame = frame + 1;
        } else if (!has_group_op) {
            op = op2;
        }
    }

    const bool restart = restart_from_menu + op.restart + // No short-circuiting.
                         term.tile.resize(tile_size) + compare_update(term.init, previewer_data::global_init) +
                         compare_update(term.rule, rule);
    if (restart) {
        term.init.initialize(term.tile);
    }

    const int step = [&] {
        const bool pause = op.pause || passing;
        if (pause) {
            term.skip_run = true;
        }

        if (!pause && restart) {
            // (Unless paused, skip displaying initial state for better visual.)
            term.skip_run = true;
            return adjust_step(config.step, strobing(rule));
        } else if (pause && op.p_s) {
            term.skip_run = true;
            return adjust_step(config.step, strobing(rule));
        } else if (op.p_1) {
            term.skip_run = true;
            return 1;
        } else if (op.p_f) {
            term.skip_run = true;
            return adjust_step(step_fast, strobing(rule));
        } else if (!pause && config.interval.test() && !std::exchange(term.skip_run, false)) {
            return adjust_step(config.step, strobing(rule));
        } else {
            return 0;
        }
    }();
    if (step != 0) {
        for (int i = 0; i < step; ++i) {
            term.tile.run_torus(rule);
        }
    }

    const scaleE scale_mode = config.zoom_ >= 1 ? scaleE::Nearest : scaleE::Linear;
    const ImTextureID texture = to_texture(term.tile.data(), scale_mode);
    ImGui::GetWindowDrawList()->AddImage(texture, ImGui::GetItemRectMin(), ImGui::GetItemRectMax());
    imgui_ItemRect(has_group_op                ? rclick_popup::highlight_col(false)
                   : hov == rclick_popup::None ? default_border_color()
                                               : rclick_popup::highlight_col(hov == rclick_popup::PopupVisible));

    const bool popup_invisible = hov == rclick_popup::PopupInvisible;
    const bool tooltip = !has_group_op && (hovered || popup_invisible) &&
                         imgui_IsItemHoveredForTooltip(popup_invisible ? ImGuiHoveredFlags_AllowWhenBlockedByPopup
                                                                       : ImGuiHoveredFlags_None);
    bool hex_mode = false;
    if (tooltip && ((hex_mode = want_hex_mode(rule)) || config.zoom_ <= 1)) {
        assert(ImGui::IsMousePosValid());
        const aniso::vecT pos = from_imvec((ImGui::GetMousePos() - ImGui::GetItemRectMin()) / config.zoom_);
        ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, {0, 0});
        if (ImGui::BeginTooltip()) {
            const aniso::rangeT clamped = clamp_window(tile_size, pos, {64, 48});
            assert(!clamped.empty());
            if (hex_mode) {
                // Using `pos` instead of (clamped.begin + .end) / 2, as otherwise the bottom-left
                // corner cannot be fully shown.
                hex_image(term.tile.data(), pos, clamped.size() * 3, 3);
            } else if (scale_mode == scaleE::Nearest) {
                ImGui::Image(texture, to_imvec(clamped.size() * 3), to_imvec(clamped.begin) / to_imvec(tile_size),
                             to_imvec(clamped.end) / to_imvec(tile_size));
            } else {
                ImGui::Image(to_texture(term.tile.data().clip(clamped), scaleE::Nearest), to_imvec(clamped.size() * 3));
            }
            ImGui::EndTooltip();
        }
        ImGui::PopStyleVar();
    }
    if constexpr (debug_mode_log_aware) {
        if (GImGui->LogEnabled) {
            imgui_LogRenderedText(ImGui::GetItemRectMin(), aniso::to_MAP_str(rule));
        }
    }
}
