#include <numbers>
#include <unordered_map>

#include "tile.hpp"

#include "common.hpp"

static ImVec2 to_imvec(const aniso::vecT& vec) { return ImVec2(vec.x, vec.y); }

static aniso::vecT from_imvec_floor(const ImVec2& vec) { return {.x = int(floor(vec.x)), .y = int(floor(vec.y))}; }

static aniso::rangeT clamp_window(aniso::vecT size, aniso::vecT region_center, aniso::vecT region_size) {
    region_size = aniso::min(size, region_size);
    aniso::vecT begin = aniso::clamp(region_center - region_size / 2, {0, 0}, size - region_size);
    return {.begin = begin, .end = begin + region_size};
}

static bool strobing(const aniso::ruleT& rule) {
    constexpr aniso::codeT all_0{0}, all_1{511};
    return rule[all_0] == 1 && rule[all_1] == 0;
}

static const int step_fast = 10;

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
        auto find_nearest = [&](const double center_x2, const double center_y2 /*integral*/) {
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

// TODO: whether to hard-block when !is-hex-rule ?
#if 1
// `is_hexagonal_rule` is not strictly necessary, but it ensures that the projected view is
// always meaningful.
static bool want_hex_mode(const aniso::ruleT& rule) {
    // return shortcuts::global_flag(ImGuiKey_6);

    if (shortcuts::global_flag(ImGuiKey_6)) {
        if (rule_algo::is_hexagonal_rule(rule)) {
            return true;
        }
        messenger::set_msg("This rule does not belong to 'Hex' subset.");
    }
    return false;
}
#else
static bool want_hex_mode(const aniso::ruleT& rule) {
    if (shortcuts::global_flag(ImGuiKey_6)) {
        if (!rule_algo::is_hexagonal_rule(rule)) {
            // (The dynamics can still be produced by a higher-ranged hexagonal rule, but that's beyond the scope of this program.)
            messenger::set_msg(
                "This rule does not belong to 'Hex' subset. As a result, the dynamics of the projected view cannot be produced by an actual range-1 hexagonal rule.");
        }
        return true;
    }
    return false;
}
#endif

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
    const int limit = 120;
    for (int g = limit; g > 0; --g) {
        torus.run_torus([&](const aniso::codeT code) {
            if (!lock[code]) {
                g = limit;
                lock[code] = true;
            }
            return rule[code];
        });
    }

    return lock;
}

// Identify spaceships or oscillators in 2*2 periodic (including pure) background. (Cannot deal with non-trivial
// objects like guns, puffers etc.)
// The area should be fully surrounded by 2*2 periodic border, and contain a full phase of the object (one or
// several oscillators, or a single spaceship).
// TODO: revamp & extend period support to up to 4*4 (notice the error messages are currently hard-coded).
static void identify(const aniso::tile_const_ref tile, const aniso::ruleT& rule,
                     const bool require_matching_background = true) {
    static constexpr aniso::vecT period_size{2, 2};
    struct periodT {
        std::array<aniso::cellT, period_size.x * period_size.y> m_data{};

        bool operator==(const periodT&) const = default;

        static aniso::vecT size() { return period_size; }
        aniso::tile_ref data() { return {m_data.data(), period_size}; }
        aniso::tile_const_ref data() const { return {m_data.data(), period_size}; }

        bool is_periodic(const aniso::ruleT& rule) const {
            const int max_period = 1 << (period_size.x * period_size.y);
            return aniso::torus_period(rule, data(), max_period).has_value();
        }
        bool rotate_equal(const periodT& b) const {
            for (int dy = 0; dy < period_size.y; ++dy) {
                for (int dx = 0; dx < period_size.x; ++dx) {
                    periodT ro{};
                    aniso::rotate_copy_00_to(ro.data(), data(), {.x = dx, .y = dy});
                    if (ro == b) {
                        return true;
                    }
                }
            }
            return false;
        }
    };

    static const auto take_corner = [](const aniso::tile_const_ref tile) {
        assert(tile.size.both_gteq(period_size));
        periodT p{};
        aniso::copy(p.data(), tile.clip_corner(period_size));
        return p;
    };
    static const auto locate_pattern = [](const aniso::tile_const_ref tile,
                                          const bool for_input = false) -> std::optional<aniso::rangeT> {
        assert(tile.size.both_gt(period_size * 2));
        const aniso::rangeT range = aniso::bounding_box(tile, take_corner(tile).data());
        if (range.empty()) {
            messenger::set_msg(for_input ? "The area contains nothing." : "The pattern dies out.");
            return {};
        } else if (!(range.begin.both_gteq(period_size) && range.end.both_lteq(tile.size - period_size))) {
            if (for_input) {
                messenger::set_msg("The area is not enclosed in 2*2 periodic background.");
            } else {
                assert(false); // Guaranteed by `regionT::run`.
            }
            return {};
        } else if (const auto size = range.size(); size.x > 3000 || size.y > 3000 || size.xy() > 400 * 400) {
            // For example, this can happen when the initial area contains a still life and a spaceship.
            messenger::set_msg(for_input ? "The area is too large." : "The pattern grows too large.");
            return {};
        }
        return aniso::rangeT{.begin = range.begin - period_size, .end = range.end + period_size};
    };
    struct regionT {
        aniso::tileT tile;
        aniso::rangeT range; // Range of pattern (including the background border), relative to the tile.
        aniso::vecT off;     // Pattern's begin pos, relative to the initial pattern.
        bool run(const aniso::ruleT& rule) {
            const aniso::tile_const_ref pattern = tile.data().clip(range);
            const aniso::tile_const_ref background = pattern.clip_corner(period_size);
            const aniso::vecT padding = {1, 1};
            // (Ceiled for torus run. This can be avoided if `border_ref` is calculated manually, but that
            // will be a lot of code.)
            aniso::tileT next(aniso::divmul_ceil(range.size() + padding * 2, period_size));

            periodT aligned{}; // Aligned to next.data().at(0, 0).
            aniso::rotate_copy_00_to(aligned.data(), background, padding);
            const aniso::rangeT relocate{.begin = padding, .end = padding + pattern.size};
            aniso::fill_outside(next.data(), relocate, aligned.data());
            aniso::copy(next.data().clip(relocate), pattern);
            next.run_torus(rule);

            tile.swap(next);
            if (const auto next_range = locate_pattern(tile.data())) {
                off = off - padding + next_range->begin;
                range = *next_range;
                return true;
            }
            return false;
        }
    };

    if (!tile.size.both_gt(period_size * 2)) {
        messenger::set_msg("The area is too small. (Should be larger than 4*4.)");
        return;
    }

    const periodT init_background = take_corner(tile);
    const std::optional<aniso::rangeT> init_range = locate_pattern(tile, true);
    if (!init_range) {
        return;
    } else if (!init_background.is_periodic(rule)) {
        messenger::set_msg("The background is not temporally periodic.");
        return;
    }

    const aniso::tile_const_ref init_pattern = tile.clip(*init_range);
    regionT region{.tile = aniso::tileT(init_pattern), .range = {{0, 0}, init_pattern.size}, .off = {0, 0}};
    aniso::tileT smallest = region.tile;

    const int limit = 4000; // Max period to deal with.
    for (int g = 1; g <= limit; ++g) {
        if (!region.run(rule)) {
            return;
        }

        const aniso::tile_const_ref pattern = region.tile.data().clip(region.range);
        if ((!require_matching_background || init_background.rotate_equal(take_corner(pattern))) &&
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
    percentT(double v) { m_val = round(std::clamp(v, 0.0, 1.0) * 100); }

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
    percentT area; // TODO: support more strategies (separate x/y, and fixed size)?

    // TODO: avoid dynamic allocation?
    aniso::tileT background; // Periodic background.

    bool operator==(const initT&) const = default;

    void initialize(aniso::tileT& tile) const {
        assert(!tile.empty() && !background.empty());
        aniso::fill(tile.data(), background.data());

        const aniso::vecT tile_size = tile.size();
        const auto range = clamp_window(tile_size, tile_size / 2, from_imvec_floor(to_imvec(tile_size) * area.get()));
        if (!range.empty()) {
            // (Not caring about how the area is aligned with the background.)
            std::mt19937 rand{(uint32_t)seed};
            aniso::random_flip(tile.data().clip(range), rand, density.get());
        }
    }

    // Background ~ 0.
    static void initialize(aniso::tileT& tile, int seed, percentT density, percentT area) {
        assert(!tile.empty());
        aniso::fill(tile.data(), {0});

        const aniso::vecT tile_size = tile.size();
        const auto range = clamp_window(tile_size, tile_size / 2, from_imvec_floor(to_imvec(tile_size) * area.get()));
        if (!range.empty()) {
            // (Using `random_fill` as the background is 0.)
            std::mt19937 rand{(uint32_t)seed};
            aniso::random_fill(tile.data().clip(range), rand, density.get());
        }
    }
};

struct float_pair {
    float val;
    const char* str;
};

class zoomT {
    static constexpr float_pair terms[]{{0.5, "0.5"}, {1, "1"}, {2, "2"}, {3, "3"}, {4, "4"}, {5, "5"}};
    static constexpr int index_1 = 1;
    static constexpr int index_max = std::size(terms) - 1; // ]
    int m_index = index_1;

public:
    void set_1() {
        static_assert(terms[index_1].val == 1);
        m_index = index_1;
    }
    void slide(int di) { m_index = std::clamp(m_index + di, 0, index_max); }
    void select(const func_ref<bool(bool, const float_pair&)> fn) {
        int n_index = m_index;
        for (int i = 0; const auto& term : terms) {
            const int this_i = i++;
            if (fn(m_index == this_i, term)) {
                n_index = this_i;
            }
        }
        m_index = n_index;
    }

    static constexpr float min() { return terms[0].val; }
    static constexpr float max() { return terms[index_max].val; }
    operator float() const {
        assert(m_index >= 0 && m_index <= index_max);
        return terms[m_index].val;
    }
};

// TODO: refactor; the code is horribly messy...
class runnerT {
    static constexpr aniso::vecT size_min{.x = 20, .y = 15};
    static constexpr aniso::vecT size_max{.x = 1600, .y = 1200};
    static constexpr ImVec2 min_canvas_size{size_min.x * zoomT::max(), size_min.y* zoomT::max()};

    class staged_rule {
        rule_with_rec rule = aniso::game_of_life();
        std::optional<aniso::ruleT> next = std::nullopt;

    public:
        operator const aniso::ruleT&() const { return rule; }
        const aniso::ruleT& get() const { return rule; }
        const rec_for_rule* operator->() const { return rule.operator->(); }

        void set_next(const aniso::ruleT& r) {
            if (rule == r) {
                messenger::set_msg("Identical.");
            } else {
                next = r;
            }
        }
        bool begin_frame() {
            if (next) {
                rule.set(*next);
                next.reset();
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
        int extra_step = 0;
        bool pause = false;
        bool extra_pause = false;

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

        void begin_frame() {
            extra_step = 0;
            extra_pause = false;
        }

        bool tick() const { return extra_step != 0 || pace.interval.test(); }
        int calc_step(const aniso::ruleT& rule) const {
            if (extra_step) {
                return extra_step;
            } else if (!pause && !extra_pause && pace.interval.test()) {
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

    // TODO: ideally the space window should skip the initial state as well (if not paused).
    // (This change is not as easy to make as it seems, as the current impl is toooo fragile...)
    class torusT {
        initT m_init{.seed = 0, .density = 0.5, .area = 0.5, .background = aniso::tileT{{.x = 1, .y = 1}}};
        aniso::tileT m_torus{};
        int m_gen = 0;

        bool skip_next = false;
        bool m_resized = true; // Init ~ resized.

    public:
        torusT() { resize_and_restart({.x = 600, .y = 400}); }

        void restart() {
            m_gen = 0;
            m_init.initialize(m_torus);
            skip_next = true;
        }

        aniso::tile_const_ref read_only() const { return m_torus.data(); }
        aniso::tile_const_ref read_only(const aniso::rangeT& range) const { return m_torus.data().clip(range); }

        aniso::tile_ref write_only() {
            skip_next = true;
            return m_torus.data();
        }
        aniso::tile_ref write_only(const aniso::rangeT& range) {
            skip_next = true;
            return m_torus.data().clip(range);
        }

        void read_and_maybe_write(const func_ref<bool(aniso::tile_ref)> fn) {
            if (fn(m_torus.data())) {
                skip_next = true;
            }
        }

        void rotate_00_to(int dx, int dy) {
            if (dx != 0 || dy != 0) {
                aniso::tileT temp(m_torus.size());
                aniso::rotate_copy_00_to(temp.data(), m_torus.data(), {.x = dx, .y = dy});
                m_torus.swap(temp);
            }
        }

        // int area() const { return m_torus.size().xy(); }
        int gen() const { return m_gen; }
        aniso::vecT size() const {
            assert(m_torus.size() == calc_size(m_torus.size()));
            return m_torus.size();
        }

        aniso::vecT calc_size(const aniso::vecT size) const {
            const aniso::vecT n_size =
                aniso::divmul_floor(aniso::clamp(size, size_min, size_max), m_init.background.size());
            if (!n_size.both_gteq(size_min)) [[unlikely]] {
                return aniso::divmul_ceil(size_min, m_init.background.size());
            }
            return n_size;
        }

        void resize(const aniso::vecT size, bool force_restart = false) {
            const aniso::vecT n_size = calc_size(size);
            if (m_torus.size() != n_size) {
                m_torus.resize(n_size);
                m_resized = true;
                restart();
            } else if (force_restart) {
                restart();
            }
        }
        void resize_and_restart(const aniso::vecT size) { resize(size, true); }
        bool resized_since_last_check() { return std::exchange(m_resized, false); }

        bool set_init(const func_ref<bool(initT&)> fn) {
            const initT old_init = m_init;
            if (fn(m_init) || old_init != m_init) {
                resize_and_restart(m_torus.size()); // In case the background is resized.
                return true;
            }
            return false;
        }

        void run(const aniso::ruleT& rule, const ctrlT& ctrl) {
            if (skip_next && ctrl.tick()) {
                skip_next = false;
                return;
            }

            const int step = ctrl.calc_step(rule);
            for (int c = 0; c < step; ++c) {
                m_torus.run_torus(rule);
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
    };
    std::optional<selectT> m_sel = std::nullopt;

public:
    void set_next_rule(const aniso::ruleT& rule) { current_rule.set_next(rule); }

    // TODO: (wontfix?) there cannot actually be multiple instances in the program.
    // For example, there are a lot of static variables in `display`, and the keyboard controls are not designed
    // for per-object use.
    void display() {
        m_ctrl.begin_frame();
        const bool rule_changed = current_rule.begin_frame();
        if (rule_changed) {
            m_torus.restart();
            m_ctrl.set_pause(false);
        }

        {
            ImGui::AlignTextToFramePadding();
            imgui_StrTooltip("(...)", "!!TODO (about space window)");
            ImGui::SameLine();

            const ImGuiID map_id = ImGui::GetID("MAP-str");
            imgui_StrWithID(copy_rule::to_str(current_rule), map_id);
            if (!pass_rule::source(current_rule)) {
                assert(ImGui::GetItemID() == map_id);
                rclick_popup::popup(map_id, [&] {
                    if (ImGui::Selectable("Copy rule")) {
                        copy_rule::copy(current_rule);
                    }
                    current_rule->selectable_to_take_snapshot("Recent", "right panel");
                });
            }
            if (const auto pass = pass_rule::dest(ImGuiKey_2, '2')) {
                if (*pass.any() == current_rule) {
                    pass.tooltip_or_message("Identical.");
                } else if (pass.deliv) {
                    current_rule.set_next(*pass.deliv); // TODO: avoid comparing in `set_next`.
                }
            }
            guide_mode::item_tooltip("MAP-string for ... !!TODO");
            ImGui::Separator();
        }

        bool resize_fullscreen = false;
        bool locate_center = false;
        bool find_suitable_zoom = false;
        bool highlight_canvas = false;
        {
            static bool first = true;
            if (std::exchange(first, false)) {
                locate_center = true;
                find_suitable_zoom = true;
            }
        }

        const char* const canvas_name = "Canvas";
        const ImGuiID canvas_id = ImGui::GetID(canvas_name);
        // The shortcuts are available only when the canvas is hovered or held.
        // (`keys_avail` is needed for hover case, as the canvas can still be hovered when another text-input is active (won't actually happen now).)
        const bool canvas_hovered_or_held =
            ((ImGui::GetActiveID() == canvas_id) || shortcuts::keys_avail()) && (ImGui::GetHoveredID() == canvas_id);

        auto set_init_state = [&](initT& init) {
            // TODO: support reset-all?
            const bool force_restart =
                ImGui::Button("Restart") ||
                (shortcuts::keys_avail() && shortcuts::test_pressed(ImGuiKey_R) && shortcuts::highlight());
            ImGui::SameLine();
            if (imgui_CheckboxV("Pause", m_ctrl.get_pause()) ||
                (shortcuts::keys_avail() && shortcuts::test_pressed(ImGuiKey_Space) && shortcuts::highlight())) {
                m_ctrl.flip_pause();
            }
            ImGui::SameLine();
            imgui_StrTooltip("(?)", "The space will pause and restart if you 'Restart' or change init settings.\n\n"
                                    "The shortcuts for 'Restart' ('R') and 'Pause' ('Space') also work here.");

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

            // There are:
            // demo_size.z is a multiple of any i <= max_period.z, and
            // cell_button_size.z * max_period.z == demo_size.z * demo_zoom (so the images have the same
            // size as the board)
            constexpr aniso::vecT max_period{.x = 4, .y = 4};
            constexpr aniso::vecT demo_size{.x = 24, .y = 24};
            constexpr int demo_zoom = 3;
            constexpr ImVec2 cell_button_size{18, 18};

            std::optional<aniso::vecT> resize{};
            const aniso::tile_ref data = init.background.data();
            ImGui::InvisibleButton("##Board", cell_button_size * to_imvec(max_period),
                                   ImGuiButtonFlags_MouseButtonLeft |
                                       ImGuiButtonFlags_MouseButtonRight); // So right-click can activate the button.
            {
                const ImVec2 button_beg = ImGui::GetItemRectMin();
                const bool button_hovered = ImGui::IsItemHovered();
                const ImVec2 mouse_pos = ImGui::GetMousePos();
                ImDrawList* const drawlist = ImGui::GetWindowDrawList();
                for (int y = 0; y < max_period.y; ++y) {
                    for (int x = 0; x < max_period.x; ++x) {
                        const bool in_range = x < data.size.x && y < data.size.y;

                        const ImVec2 cell_beg = button_beg + cell_button_size * ImVec2(x, y);
                        const ImVec2 cell_end = cell_beg + cell_button_size;
                        drawlist->AddRectFilled(cell_beg, cell_end,
                                                in_range ? (data.at(x, y) ? IM_COL32_WHITE : IM_COL32_BLACK)
                                                         : IM_COL32_GREY(60, 255));
                        drawlist->AddRect(cell_beg, cell_end, IM_COL32_GREY(160, 255));
                        if (button_hovered && ImRect(cell_beg, cell_end).Contains(mouse_pos) /*[)*/) {
                            if (ImGui::GetIO().KeyCtrl) {
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
            }

            {
                aniso::tileT demo(demo_size);
                aniso::fill(demo.data(), data);

                ImGui::SameLine(0, 0);
                imgui_Str(" ~ ");
                ImGui::SameLine(0, 0);
                ImGui::Image(to_texture(demo.data(), scaleE::Nearest), to_imvec(demo.size() * demo_zoom));
                imgui_ItemRect(IM_COL32_GREY(160, 255));

                static aniso::tileT init, curr;
                static bool skip_next = false;
                if (rule_changed // Defensive, won't actually happen now.
                    || ImGui::IsWindowAppearing() || init != demo) {
                    init = aniso::tileT(demo);
                    curr = aniso::tileT(demo);
                    skip_next = true;
                }

                ImGui::SameLine(0, 0);
                imgui_Str(" ~ ");
                ImGui::SameLine(0, 0);
                ImGui::Image(to_texture(curr.data(), scaleE::Nearest), to_imvec(curr.size() * demo_zoom));
                imgui_ItemRect(IM_COL32_GREY(160, 255));

                if (global_timer::test(200) && !std::exchange(skip_next, false)) {
                    curr.run_torus(current_rule.get());
                }
            }

            if (resize && init.background.size() != *resize) {
                aniso::tileT resized(*resize); // Already 0-filled.
                const aniso::vecT common = aniso::min(resized.size(), init.background.size());
                aniso::copy(resized.data().clip_corner(common), init.background.data().clip_corner(common));
                init.background.swap(resized);
            }

            return force_restart;
        };

        auto input_size = [&] {
            static input_int input_x{}, input_y{};

            const float inner_spacing = imgui_ItemInnerSpacingX();
            const aniso::vecT size = m_torus.size();

            // TODO: whether / how to add hint ('enter' to resize)?
            ImGui::AlignTextToFramePadding();
            imgui_Str("Size ~");
            ImGui::SameLine(0, inner_spacing);
            ImGui::SetNextItemWidth(floor((item_width - inner_spacing) / 2));
            const auto ix = input_x.input("##Width", std::format("Width:{}", size.x).c_str());
            ImGui::SameLine(0, inner_spacing);
            ImGui::SetNextItemWidth(ceil((item_width - inner_spacing) / 2));
            const auto iy = input_y.input("##Height", std::format("Height:{}", size.y).c_str());

            if (ix || iy) {
                locate_center = true;
                find_suitable_zoom = true;

                // Both values will be flushed if either receives the enter key.
                m_torus.resize({.x = ix.value_or(input_x.flush().value_or(size.x)),
                                .y = iy.value_or(input_y.flush().value_or(size.y))});
            }
        };

        auto select_zoom = [&] {
            ImGui::AlignTextToFramePadding();
            imgui_Str("Zoom ~");
            m_coord.zoom.select([&](const bool is_cur, const float_pair& z) {
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                if (ImGui::RadioButton(z.str, is_cur)) {
                    resize_fullscreen = true;
                    return true;
                }
                return false;
            });
            ImGui::SameLine();
            if (imgui_StrTooltip("(?)", "The buttons are for resizing the space to full-screen.\n\n"
                                        "(Scroll in the space window to zoom in/out without resizing.)")) {
                highlight_canvas = true;
            }
        };

        ImGui::PushItemWidth(item_width);
        ImGui::BeginGroup();
        m_ctrl.set_step_interval([&](paceT& pace, int& extra_step) {
            bool tooltip_hovered = false;
            ImGui::AlignTextToFramePadding();
            if (imgui_StrTooltip(
                    "(...)",
                    "Restart : R\n"
                    "Pause   : Space\n"
                    "+s/+1/+!: S/D/F (repeatable)\n"
                    "-/+ Step    : 1/2 (repeatable)\n"
                    "-/+ Interval: 3/4 (repeatable)\n\n"
                    "These shortcuts are available only when the space window is hovered, or when this tooltip is displayed.")) {
                highlight_canvas = true;
                tooltip_hovered = true;
            }

            const bool enable_shortcuts = canvas_hovered_or_held || (tooltip_hovered && shortcuts::keys_avail());
            auto item_shortcut = [enable_shortcuts](ImGuiKey key, bool repeat) {
                return enable_shortcuts && shortcuts::test_pressed(key, repeat) && shortcuts::highlight();
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
                (enable_shortcuts && shortcuts::test_down(ImGuiKey_F) && shortcuts::highlight())) {
                extra_step = adjust_step(std::max(pace.step, step_fast), strobing(current_rule));
            }
            ImGui::PopItemFlag(); // ImGuiItemFlags_ButtonRepeat
            ImGui::SameLine();
            imgui_StrTooltip("(?)", [] {
                imgui_Str("+s: ");
                ImGui::SameLine(0, 0);
                imgui_Str("Manual mode (firstly pause the space, then advance generation by step afterwards).");
                imgui_Str("+1: ");
                ImGui::SameLine(0, 0);
                imgui_Str("Advance generation by 1 (instead of step).");
                imgui_Str("+!: ");
                ImGui::SameLine(0, 0);
                imgui_Str("Fast mode (advance generation by max(10,step) in every frame).");
            });

            ImGui::Separator(); // To align with the left panel.

            const auto to_str = [is_strobing = strobing(current_rule)](int step) {
                if (!is_strobing) {
                    return std::to_string(step);
                } else {
                    return std::format("{} -> {}", step, adjust_step(step, is_strobing));
                }
            };

            // TODO: recheck this design... Ideally these sliders should use locally-defined `item_shortcut`.
            imgui_StepSliderInt::set_shortcuts(ImGuiKey_1, ImGuiKey_2, enable_shortcuts);
            imgui_StepSliderInt::fn("Step", &pace.step, 1, 100, 1, to_str);
            imgui_StepSliderInt::reset_shortcuts();
            ImGui::SameLine();
            imgui_StrTooltip(
                "(?)",
                "If the current rule maps '000...' to 1 and '111...' to 0 (aka \"strobing rule\"), the step will be ceiled to 2*n (e.g. 1->2, 2->2) to avoid large spans of 0/1 areas flashing between two colors.\n\n"
                "The adjustment also applies to '+s' and '+!', but does not affect '+1' (so '+1' can serve to change the parity of generation).\n\n"
                "Sometimes you may also find rules that are non-strobing (so the adjustment won't take place) but can develop non-trivial flashing areas. The effect can usually be avoided by manually setting a 2*n step.");
            // TODO: the last sec is terrible but I have no idea how to improve it...

            imgui_StepSliderInt::set_shortcuts(ImGuiKey_3, ImGuiKey_4, enable_shortcuts);
            pace.interval.step_slide("Interval", 0, 400);
            imgui_StepSliderInt::reset_shortcuts();
        });
        ImGui::EndGroup();
        ImGui::SameLine(floor(1.5 * item_width));
        ImGui::BeginGroup();
        menu_like_popup::button("Init state");
        menu_like_popup::popup([&] {
            if (m_torus.set_init(set_init_state)) {
                m_ctrl.set_pause(true);
                m_sel.reset();
                m_paste.reset();
                m_ctrl.pop_pause_for_m_paste();
            }
        });
        ImGui::SameLine();
        if (ImGui::Button("Reset pos")) { // !!TODO: (v0.9.9) -> menu for resetting pos, size etc..
            locate_center = true;
            find_suitable_zoom = true;
        }
        guide_mode::item_tooltip(
            "Move the space to the center, and select suitable zoom for it. (As if the space is newly resized.)");
        ImGui::SameLine();
        ImGui::Text("Generation:%d", m_torus.gen());

        ImGui::Spacing(); // To align with the separator.

        input_size();
        select_zoom();

        ImGui::EndGroup();
        ImGui::PopItemWidth();

        ImGui::Separator();

        ImGui::AlignTextToFramePadding();
        if (imgui_StrTooltip(
                "(...)",
                "Scroll in the space window to zoom in/out.\n\n"
                "Drag with left button to move the space; 'Ctrl' and drag to \"rotate\" the space.\n\n"
                "Drag with right button to select area; single right-click to unselect.\n\n"
                "See 'Space ops' for more operations (e.g. 'C' to copy selected area, 'V' to load pattern). The shortcuts (including 'V') are available only when the space window is hovered.")) {
            highlight_canvas = true;
        }
        ImGui::SameLine();
        static bool show_op_window = false;
        ImGui::Checkbox("Space ops", &show_op_window);
        const int wide_spacing = ImGui::CalcTextSize(" ").x * 2;
        ImGui::SameLine(0, wide_spacing);
        if (m_paste) {
            const aniso::vecT size = m_paste->size();
            ImGui::Text("Pattern:%d*%d", size.x, size.y);
        } else {
            imgui_Str("Pattern:N/A");
        }
        ImGui::SameLine(0, wide_spacing);
        if (m_sel) {
            ImGui::Text("Area:%d*%d", m_sel->width(), m_sel->height());
        } else {
            imgui_Str("Area:N/A");
        }

        if (m_paste) {
            ImGui::SetNextWindowPos(ImGui::GetCursorScreenPos(), ImGuiCond_Appearing);
            if (auto window = imgui_Window("Pasted", nullptr,
                                           ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoDecoration |
                                               ImGuiWindowFlags_AlwaysAutoResize)) {
                imgui_StrTooltip("Double-click to decide where to paste.",
                                 "Use 'Clear' (or press 'V' again) to cancel pasting.\n\n"
                                 "Turn off 'Auto clear' to paste multiple times.");

                ImGui::PushStyleVarY(ImGuiStyleVar_FramePadding, 0);
                ImGui::Checkbox("Auto clear", &m_paste->paste_once);
                ImGui::PopStyleVar();
                ImGui::SameLine();
                const bool clear = double_click_button_small("Clear");
                if (m_paste->rule && m_paste->rule != current_rule) {
                    // !!TODO: redesign... this can be a rule-source...
                    ImGui::SameLine();
                    if (double_click_button_small("Rule")) {
                        current_rule.set_next(*(m_paste->rule));
                    }
                    ImGui::SameLine();
                    imgui_StrTooltip("(?)", "The pattern specified a different rule.");
                }

                ImGui::AlignTextToFramePadding();
                imgui_Str("Paste mode ~");
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("Copy##Mode", &m_paste->mode, aniso::blitE::Copy);
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("Or##Mode", &m_paste->mode, aniso::blitE::Or);
                ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                imgui_RadioButton("And##Mode", &m_paste->mode, aniso::blitE::And);
                ImGui::SameLine();
                imgui_StrTooltip(
                    "(?)",
                    "Use 'Copy' mode to copy the pattern directly.\n\n"
                    "Use 'Or' mode to treat black cells as transparent background. ('And' ~ white background.)\n\n"
                    "(Only 'Copy' works for periodic background.)");
                if (clear) {
                    m_paste.reset();
                    m_ctrl.pop_pause_for_m_paste();
                }
            }
        }

        {
            // (Values of GetContentRegionAvail() can be negative...)
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

            if (resize_fullscreen) {
                locate_center = true;
                m_torus.resize(from_imvec_floor((canvas_size - ImVec2(20, 20)) / m_coord.zoom));
            } else if (find_suitable_zoom) {
                // (wontfix) Poorly written, but works...
                // Select the largest zoom that can hold the entire tile.
                m_coord.zoom.slide(-100); // -> smallest.
                m_coord.zoom.select([&](bool, const float_pair& z) {
                    const aniso::vecT size =
                        m_torus.calc_size(from_imvec_floor((canvas_size - ImVec2(20, 20)) / z.val));
                    return size.both_gteq(m_torus.size());
                });
            }

            // `m_torus` won't resize now.
            const aniso::vecT tile_size = m_torus.size();
            if (m_torus.resized_since_last_check()) {
                m_sel.reset();
                m_paste.reset();
                m_ctrl.pop_pause_for_m_paste();
            }

            if (locate_center) {
                m_coord.bind(to_imvec(tile_size) / 2, canvas_size / 2);
                to_rotate = {0, 0};
            }

            if (m_sel && m_sel->active && (!r_down || m_paste || ImGui::IsItemDeactivated())) {
                m_sel->active = false;
                // Allow a single right-click to unselect the area.
                // (`bounding_box` has no size check like this. This is intentional.)
                if (m_sel->width() * m_sel->height() <= 2) {
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

                const aniso::vecT cel_pos = from_imvec_floor(m_coord.to_space(mouse_pos));

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

            // Render.
            {
                const ImVec2 screen_min = ImFloor(canvas_min + m_coord.to_canvas({0, 0}));
                const ImVec2 screen_max = screen_min + to_imvec(tile_size) * m_coord.zoom;

                ImDrawList* const drawlist = ImGui::GetWindowDrawList();
                drawlist->PushClipRect(canvas_min, canvas_max);
                drawlist->AddRectFilled(canvas_min, canvas_max, IM_COL32_GREY(24, 255));

                const scaleE scale_mode = m_coord.zoom < 1 ? scaleE::Linear : scaleE::Nearest;
                if (!m_paste) {
                    const ImTextureID texture = to_texture(m_torus.read_only(), scale_mode);

                    drawlist->AddImage(texture, screen_min, screen_max);
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
                        // An ideal way to copy patterns with arbitrary periodic background will be:
                        // Detect backgrounds of the pattern and the target area (?not practical?).
                        // 'copy_diff' only if the backgrounds are the same and aligns properly.
                        aniso::blit(paste_area, m_paste->tile.data(), m_paste->mode);
                        texture = to_texture(tile, scale_mode);
                        if (hovered && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left)) {
                            if (m_paste->paste_once) {
                                m_paste.reset();
                                m_ctrl.pop_pause_for_m_paste();
                            } else {
                                messenger::set_msg("Pasted.");
                            }
                            return true;
                        } else { // Restore.
                            aniso::copy(paste_area, temp.data());
                            return false;
                        }
                    });

                    drawlist->AddImage(texture, screen_min, screen_max);
                    // (It's ok to render for this frame even if `m_paste` has been consumed.)
                    const ImVec2 paste_min = screen_min + to_imvec(paste_beg) * m_coord.zoom;
                    const ImVec2 paste_max = screen_min + to_imvec(paste_end) * m_coord.zoom;
                    drawlist->AddRectFilled(paste_min, paste_max, IM_COL32(255, 0, 0, 60));
                }

                if (m_sel) {
                    const auto [sel_beg, sel_end] = m_sel->to_range();
                    const ImVec2 sel_min = screen_min + to_imvec(sel_beg) * m_coord.zoom;
                    const ImVec2 sel_max = screen_min + to_imvec(sel_end) * m_coord.zoom;
                    drawlist->AddRectFilled(sel_min, sel_max, IM_COL32(0, 255, 0, !m_paste ? 40 : 20));
                    // drawlist->AddRect(sel_min, sel_max, IM_COL32(0, 255, 0, 160));
                }
                drawlist->AddRect(screen_min, screen_max, previewer::default_border_color());
                drawlist->PopClipRect();

                // `skip` is a workaround to make the highlight appear in the same frame with the tooltip.
                // (Tooltips will be hidden for one extra frame before appearing.)
                static bool skip = true;
                if (!highlight_canvas) {
                    skip = true;
                } else if (std::exchange(skip, false)) {
                    highlight_canvas = false;
                }

                if (hovered || highlight_canvas) {
                    imgui_ItemRect(ImGui::GetColorU32(ImGuiCol_Separator));
                }
            }

            // Range operations.
            {
                enum class operationE {
                    _none,
                    // _capture_closed,
                    // _capture_open,
                    _random_fill,
                    _clear_inside,
                    _clear_outside,
                    _select_all,
                    _bounding_box,
                    _test_bg_period, // !!TODO: (v0.9.9?v0.9.8) enhance to background sampling
                    _copy,
                    _cut,
                    _identify,
                    _paste
                };
                using enum operationE;

                struct op_term {
                    operationE op;
                    bool use_sel;
                    ImGuiKey key;
                    const char* key_label;
                };
                constexpr op_term op_random_fill{_random_fill, true, ImGuiKey_Equal, "+ (=)"};
                constexpr op_term op_clear_inside{_clear_inside, true, ImGuiKey_Backspace, "Backspace"};
                constexpr op_term op_clear_outside{_clear_outside, true, ImGuiKey_0, "0 (zero)"};
                constexpr op_term op_select_all{_select_all, false, ImGuiKey_A, "A"};
                constexpr op_term op_bounding_box{_bounding_box, true, ImGuiKey_B, "B"};
                constexpr op_term op_test_bg_period{_test_bg_period, true, ImGuiKey_P, "P"};
                constexpr op_term op_copy{_copy, true, ImGuiKey_C, "C"};
                constexpr op_term op_cut{_cut, true, ImGuiKey_X, "X"};
                constexpr op_term op_identify{_identify, true, ImGuiKey_I, "I (i)"};
                constexpr op_term op_paste{_paste, false, ImGuiKey_V, "V"};

                static aniso::cellT background{0};
                static percentT fill_den = 0.5; // Random-fill.
                static bool add_rule = true;    // Copy / cut.
                auto copy_sel = [&] {
                    if (m_sel) {
                        std::string rle_str = aniso::to_RLE_str(m_torus.read_only(m_sel->to_range()),
                                                                add_rule ? &current_rule.get() : nullptr);
                        ImGui::SetClipboardText(rle_str.c_str());

                        messenger::set_msg(std::move(rle_str));
                    }
                };

                operationE op = _none;
                operationE op_highlight = _none;

                if (canvas_hovered_or_held) {
                    for (const op_term& t :
                         {op_random_fill, op_clear_inside, op_clear_outside, op_select_all, op_bounding_box,
                          op_test_bg_period, op_copy, op_cut, op_identify, op_paste}) {
                        if (shortcuts::test_pressed(t.key, false)) {
                            op_highlight = t.op;
                            const bool valid = !t.use_sel || m_sel.has_value();
                            if (valid) {
                                op = t.op;
                            } else {
                                messenger::set_msg("There is no selected area.");
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
                    if (auto window =
                            imgui_Window("Space operations", &show_op_window,
                                         ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
                        auto term = [&](const char* label, const op_term& t) {
                            const bool valid = !t.use_sel || m_sel.has_value();
                            // Was `ImGui::MenuItem(label, shortcut, nullptr, valid)`.
                            ImGui::BeginDisabled(!valid);
                            if (imgui_SelectableStyledButton(label, false, t.key_label)) {
                                assert(valid);
                                op = t.op;
                            } else if (op_highlight == t.op) {
                                shortcuts::highlight();
                            }
                            ImGui::EndDisabled();
                            if (!valid) {
                                imgui_ItemTooltip("There is no selected area.");
                            }
                        };

                        ImGui::AlignTextToFramePadding();
                        imgui_Str("Background ~");
                        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                        imgui_RadioButton("0", &background, {0});
                        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                        imgui_RadioButton("1", &background, {1});
                        ImGui::SameLine();
                        imgui_StrTooltip("(?)",
                                         "'Clear inside/outside' and 'Cut' will clear the range with this value.");

                        // Filling.
                        ImGui::Separator();
                        fill_den.step_slide("Fill density");
                        term("Random fill", op_random_fill);
                        term("Clear inside", op_clear_inside);
                        term("Clear outside", op_clear_outside);

                        ImGui::Separator();
                        term("Select all", op_select_all);
                        term("Bound", op_bounding_box);
                        guide_mode::item_tooltip(
                            "Get the bounding box for the pattern. (The pattern should be enclosed in 2*2 periodic background.)");
                        term("Test background", op_test_bg_period);
                        guide_mode::item_tooltip("Test the size and period of periodic background.");

                        // Copy/Cut/Identify.
                        ImGui::Separator();
                        ImGui::Checkbox("Rule info", &add_rule);
                        ImGui::SameLine();
                        imgui_StrTooltip(
                            "(?)", "Whether to include rule info ('rule = ...') in the header for the patterns.\n\n"
                                   "(This applies to 'Copy' and 'Cut'. 'Identify' will always include rule info.)");
                        term("Copy", op_copy);
                        term("Cut", op_cut);
                        term("Identify", op_identify);
                        guide_mode::item_tooltip(
                            "Identify a single oscillator or spaceship in 2*2 periodic background "
                            "(e.g., pure white, pure black, striped, or checkerboard background), and "
                            "copy its smallest phase to the clipboard.");

                        // Paste.
                        ImGui::Separator();
                        term("Paste", op_paste);
                    }
                }

                // TODO: disable some operations if `m_paste.has_value`?
                if (op == _random_fill && m_sel) {
                    // TODO: or `random_flip`?
                    static std::mt19937 rand = rand_source::create();
                    aniso::random_fill(m_torus.write_only(m_sel->to_range()), rand, fill_den.get());
                } else if (op == _clear_inside && m_sel) {
                    aniso::fill(m_torus.write_only(m_sel->to_range()), background);
                } else if (op == _clear_outside && m_sel) {
                    aniso::fill_outside(m_torus.write_only(), m_sel->to_range(), background);
                } else if (op == _select_all) {
                    if (!m_sel || m_sel->width() != tile_size.x || m_sel->height() != tile_size.y) {
                        m_sel = {.active = false, .beg = {0, 0}, .end = tile_size.minus(1, 1)};
                    } else {
                        m_sel.reset();
                    }
                } else if (op == _bounding_box && m_sel) {
                    // TODO: temporarily sharing the same logic with `identify`...
                    const aniso::rangeT sel_range = m_sel->to_range();
                    const aniso::tile_const_ref sel_area = m_torus.read_only(sel_range);
                    const aniso::vecT p_size{2, 2};
                    if (aniso::has_enclosing_period(sel_area, p_size)) {
                        aniso::rangeT bound = aniso::bounding_box(sel_area, sel_area.clip_corner(p_size));
                        if (!bound.empty()) {
                            bound.begin -= p_size;
                            bound.end += p_size;
                            // (Pad another layer of bg pattern if possible; this behaves well but skipped to keep same as `identify`.)
                            if (false && bound.begin.both_gteq(p_size) && bound.end.both_lteq(sel_area.size - p_size)) {
                                bound.begin -= p_size;
                                bound.end += p_size;
                            }
                            m_sel = {.active = false,
                                     .beg = sel_range.begin + bound.begin,
                                     .end = sel_range.begin + bound.end.minus(1, 1)};
                        } else {
                            // m_sel.reset();
                            messenger::set_msg("The area contains nothing.");
                        }
                    } else {
                        messenger::set_msg("The area is not enclosed in 2*2 periodic background.");
                    }
                } else if (op == _test_bg_period && m_sel) {
                    const aniso::tile_const_ref sel_area = m_torus.read_only(m_sel->to_range());
                    if (const auto p_size = aniso::spatial_period_full_area(sel_area, {30, 30})) {
                        if (const auto period = aniso::torus_period(current_rule, sel_area.clip_corner(*p_size), 60)) {
                            messenger::set_msg("Period: x = {}, y = {}, p = {}", p_size->x, p_size->y, *period);
                        } else {
                            messenger::set_msg("Spatial period: x = {}, y = {} (not temporally periodic)", p_size->x,
                                               p_size->y);
                        }
                    } else {
                        // (The too-large case is considered impossible to occur naturally.)
                        messenger::set_msg("The selected area is too small, or not spatially periodic.");
                    }
                } else if (op == _copy && m_sel) {
                    copy_sel();
                } else if (op == _cut && m_sel) {
                    copy_sel();
                    aniso::fill(m_torus.write_only(m_sel->to_range()), background);
                } else if (op == _paste) {
                    if (m_sel) {
                        m_sel->active = false;
                    }
                    // TODO: whether to support toggling?
                    if (m_paste) {
                        m_paste.reset();
                        m_ctrl.pop_pause_for_m_paste();
                    } else if (std::string_view text = read_clipboard(); !text.empty()) {
                        std::optional<aniso::ruleT> rule = std::nullopt;
                        text = aniso::strip_RLE_header(text, &rule);
                        aniso::from_RLE_str(text, [&](const aniso::prepareT p_size) -> std::optional<aniso::tile_ref> {
                            if (p_size.empty()) {
                                // TODO: whether to tell apart [no pattern] vs [wrong format e.g. not ending with '!']?
                                // (At least, explain expected format in this case?)
                                messenger::set_msg("Found no pattern.\n\n"
                                                   "('V' is for pasting patterns. If you want to read rules from the "
                                                   "clipboard, use the 'Clipboard' window instead.)");
                                return std::nullopt;
                            } else if (p_size.x > tile_size.x || p_size.y > tile_size.y) {
                                messenger::set_msg("The space is not large enough for the pattern.\n"
                                                   "Space size: x = {}, y = {}\n"
                                                   "Pattern size: x = {}, y = {}",
                                                   tile_size.x, tile_size.y, p_size.x, p_size.y);
                                return std::nullopt;
                            } else {
                                m_ctrl.push_pause_for_m_paste();
                                m_paste = {.rule = rule,
                                           .tile = aniso::tileT(aniso::vecT{.x = (int)p_size.x, .y = (int)p_size.y}),
                                           .beg = {0, 0},
                                           .mode = aniso::blitE::Copy};
                                return m_paste->tile.data();
                            }
                        });
                    }
                } else if (op == _identify) {
                    identify(m_torus.read_only(m_sel->to_range()), current_rule);
                }
            }

            assert(tile_size == m_torus.size());
        }

        assert(!m_torus.resized_since_last_check());
        m_torus.run(current_rule, m_ctrl);
    }
};

static runnerT runner;
void apply_rule(frame_main_token) { runner.display(); }

// TODO: let users decide which to be globally shared?
class global_config : no_create {
    friend class previewer;

    inline static int seed = 0;
    inline static percentT density = 0.5;
    inline static percentT area = 1.0;

    static void reset() {
        seed = 0;
        density = 0.5;
        area = 1.0;
    }
};

void previewer::configT::_set() {
    // ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, {3, 2});
    ImGui::PushItemWidth(item_width);

#if 0
    // TODO: what to reset? size/zoom or size/zoom/seed/step?
    if (ImGui::Button("Reset")) {
        _reset_size_zoom();
    }
    guide_mode::item_tooltip("(Width, height and zoom.)");
    ImGui::Separator();
#endif

    imgui_StepSliderInt::fn("Width", &width_, 120, 280, 20);
    imgui_StepSliderInt::fn("Height", &height_, 120, 280, 20);
    ImGui::AlignTextToFramePadding();
    imgui_Str("Zoom ~"); // TODO: should this be "zoom" or "scale"?
    for (const auto& [val, str] : std::initializer_list<float_pair>{{0.5, "0.5"}, {1, "1"}, {2, "2"}}) {
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        imgui_RadioButton(str, &zoom_, val);
    }
    ImGui::SameLine();
    imgui_StrTooltip(
        "(?)",
        "Unlike space window's 'Size ~' (which refers to space size), 'Width' and 'Height' here refer to image size (in pixels) and are unrelated to zoom setting.");

    ImGui::Separator();
    imgui_StepSliderInt::fn("Step", &step, 1, 30);
    interval.step_slide("Interval", 0, 400);

    ImGui::Separator();
    menu_like_popup::button("Init state");
    menu_like_popup::popup([] {
        if (ImGui::Button("Reset")) {
            global_config::reset();
        }
        ImGui::SameLine();
        imgui_StrTooltip("(?)", "These settings are shared by all preview windows across the program.");
        imgui_StepSliderInt::fn("Seed", &global_config::seed, 0, 9);
        global_config::density.step_slide("Density", 10, 100, 10);
        global_config::area.step_slide("Area", 10, 100, 10);
        imgui_Str("Background ~ default (single black cell)");
    });

    ImGui::PopItemWidth();
    // ImGui::PopStyleVar();
}

class previewer_data : no_create {
    friend class previewer;

    struct termT {
        bool active = false;
        bool skip_run = false;
        int seed = 0;
        percentT density = 0.5;
        percentT area = 0;
        aniso::ruleT rule = {};
        aniso::tileT tile = {};
    };

    inline static std::unordered_map<uint64_t, termT> terms;
};

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
void previewer::_preview(uint64_t id, const configT& config, const aniso::ruleT& rule) {
    assert(ImGui::GetItemRectSize() == config.size_imvec());
    assert(ImGui::IsItemVisible());
    static auto keys_avail_and_window_hovered = [] { //
        // (Window-hovered should go before test-pressed to avoid messing with shortcut precedence.)
        // TODO: let this be tested by configT?
        return (shortcuts::keys_avail() || imgui_IsBgHeld()) && ImGui::IsWindowHovered();
    };

    previewer_data::termT& term = previewer_data::terms[id];
    if (term.active) [[unlikely]] { // ID conflict
        assert(false);
        return;
    }
    term.active = true;

    // TODO: (though the actual behaviors are ok) these op logics are quite messy...
    const bool passing = pass_rule::source(rule);
    const aniso::vecT tile_size{.x = int(config.width_ / config.zoom_), .y = int(config.height_ / config.zoom_)};
    const bool hovered = !passing && ImGui::IsItemHovered();
    const bool active = ImGui::IsItemActive();
    const bool enable_shortcuts = shortcuts::global_flag(ImGuiKey_A) ? keys_avail_and_window_hovered()
                                                                     : (hovered && (active || shortcuts::keys_avail()));
    assert_implies(passing, !enable_shortcuts);

    const bool restart = (enable_shortcuts && shortcuts::test_pressed(ImGuiKey_R)) || //
                         term.tile.size() != tile_size || term.seed != global_config::seed ||
                         term.density != global_config::density || term.area != global_config::area ||
                         term.rule != rule;
    if (restart) {
        term.tile.resize(tile_size);
        term.seed = global_config::seed;
        term.density = global_config::density;
        term.area = global_config::area;
        term.rule = rule;
        initT::initialize(term.tile, global_config::seed, global_config::density, global_config::area);
        term.skip_run = true;
    }

    // (`IsItemActive` does not work as preview-window is based on `Dummy`.)
    const bool pause = passing || active;
    const bool fast = enable_shortcuts &&
                      shortcuts::global_flag(ImGuiKey_F); // Using unfiltered version for smoother inter with <</>>.
    if (fast || (!pause && (restart || (config.interval.test() && !std::exchange(term.skip_run, false))))) {
        const int p = adjust_step(fast ? std::max(config.step, step_fast) : config.step, strobing(rule));
        for (int i = 0; i < p; ++i) {
            term.tile.run_torus(rule);
        }
    }
    // Unless paused, the initial state will not be shown. This is intentional for better visual effect.

    const scaleE scale_mode = config.zoom_ >= 1 ? scaleE::Nearest : scaleE::Linear;
    const ImTextureID texture = to_texture(term.tile.data(), scale_mode);
    bool hex_mode = false;
    ImGui::GetWindowDrawList()->AddImage(texture, ImGui::GetItemRectMin(), ImGui::GetItemRectMax());
    if (hovered && imgui_IsItemHoveredForTooltip() && ((hex_mode = want_hex_mode(rule)) || config.zoom_ <= 1)) {
        assert(ImGui::IsMousePosValid());
        const aniso::vecT pos = from_imvec_floor((ImGui::GetMousePos() - ImGui::GetItemRectMin()) / config.zoom_);
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

    ImU32 border_col = default_border_color();
    if (!passing) {
        const id_pair popup_id{ImGui::GetItemID(), (ImGuiID)(intptr_t)&term}; // Absolutely impossible to clash.
        const auto hov = rclick_popup::popup_no_highlight(popup_id, [&] {
            if (ImGui::Selectable("Copy rule")) {
                copy_rule::copy(rule);
            }
            // !!TODO: either remove, or enhance to copying size & init state as well.
            if (ImGui::Selectable("To right panel")) {
                runner.set_next_rule(rule);
            }

            imgui_StrTooltip(
                "(...)",
                "Hold to pause.\n"
                "'R' to restart. (+ 'A' to apply to the entire group.)\n"
                "'F' to speed up. (+ 'A' to apply to the entire group.)\n\n"
                "If the rule belongs to 'Hex' subset:\n"
                "'6' to see the projected view in the real hexagonal space. (This also applies to the space window.)");
        });
        if (hov != rclick_popup::None) {
            border_col = rclick_popup::highlight_col(hov == rclick_popup::Popup);
        }
    }

    imgui_ItemRect(border_col);
}
