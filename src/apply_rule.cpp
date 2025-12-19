#include <numbers>
#include <ranges>
#include <unordered_map>

#include "tile.hpp"

#include "common.hpp"

static ImVec2 to_imvec(const aniso::vecT& vec) { return ImVec2(vec.x, vec.y); }

// (-> trunc as this doesn't need strict rounding mode.)
// (Though working in practice, the standard doesn't require `std::floor` etc to be addressable.)
// template <float (&fn)(float) = std::floor>
static aniso::vecT from_imvec(const ImVec2& vec) { return {.x = int(vec.x), .y = int(vec.y)}; }

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

        const double x2_flr = std::floor(x2), y2_flr = std::floor(y2);
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
    if (shortcuts::global_flag(ImGuiKey_6)) {
        if (!rule_algo::is_hexagonal_rule(rule)) {
            // (But actually, the projection still corresponds to a range-2 hex rule.)
            messenger::set_msg("The rule does not belong to 'Hex'.");
            messenger::set_once();
            // return false;
        }
        return true;
    }
    return false;
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

// !!TODO: support sampling manually (`fake_apply`)...

// Identify spaceships or oscillators in periodic (including pure) background. (Cannot deal with non-trivial
// objects like guns, puffers etc.)
// The area should be fully surrounded by periodic border, and contain a full phase of the object (one or
// several oscillators, or a single spaceship).
struct identify_result {
    aniso::tileT pattern; // Smallest phase.
    aniso::vecT offset;
    int period;
    aniso::lockT rec;
};
static std::optional<identify_result> identify(const aniso::tile_const_ref tile, const aniso::ruleT& rule,
                                               const aniso::vecT period_size, const bool require_matching_bg = true) {
    assert(period_size.both_lteq({4, 4}));
    if (!check_border(tile, period_size)) {
        return std::nullopt;
    }

    const aniso::tile_buf init_bg = tile.clip_corner(period_size);
    if (!aniso::torus_period(rule, init_bg.data(), 20).has_value()) {
        messenger::set_msg("The background is not temporally periodic.");
        return std::nullopt;
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
            messenger::set_msg(for_input ? "The area contains nothing." : "The pattern dies out.");
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
        aniso::lockT rec;

        aniso::tile_const_ref get_pattern() const { return tile.data(range); }

        bool run(const aniso::ruleT& rule) {
            const aniso::tile_const_ref pattern = get_pattern();
            const aniso::tile_const_ref background = pattern.clip_corner(period_size);
            const aniso::vecT padding = {1, 1};
            // (Ceiled for torus run.)
            aniso::tileT next(aniso::divmul_ceil(range.size() + padding * 2, period_size));

            const aniso::rangeT relocate{.begin = padding, .end = padding + pattern.size};
            aniso::fill_outside(next.data(), relocate,
                                aniso::realign_from_to(background, padding, {0, 0}) /*relative to `next`*/);
            aniso::copy(next.data(relocate), pattern);
            next.run_torus(rule, &rec);

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
        return std::nullopt;
    }

    const aniso::tile_const_ref init_pattern = tile.clip(init_range);
    regionT region{.period_size = period_size,
                   .tile = aniso::tileT(init_pattern),
                   .range = {{0, 0}, init_pattern.size},
                   .off = {0, 0},
                   .rec = {}};
    aniso::tileT smallest = aniso::tileT(init_pattern);

    constexpr int limit = 4000; // Max period to deal with.
    for (int g = 1; g <= limit; ++g) {
        if (!region.run(rule)) {
            return std::nullopt;
        }

        const aniso::tile_const_ref pattern = region.get_pattern();
        if ((!require_matching_bg || matching_bg.contains(pattern.clip_corner(period_size))) &&
            pattern.area() < smallest.area()) {
            smallest = aniso::tileT(pattern);
        }
        if (init_pattern == pattern) {
            // TODO: pad an extra layer of bg pattern?
            return {{.pattern = std::move(smallest), .offset = region.off, .period = g, .rec = region.rec}};
        }
    }
    // For example, this can happen the object really has a huge period, or the initial area doesn't
    // contain a full phase (fragments that evolves to full objects are not recognized), or whatever else.
    messenger::set_msg("Cannot identify.");
    return std::nullopt;
}

class percentT {
    int m_val; // ∈ [0, 100].
public:
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
            if (aniso::is_pure(background, {0})) {
                aniso::random_fill(tile.data(range), rand, density.get());
            } else {
                aniso::random_flip(tile.data(range), rand, density.get());
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
// !!TODO: reconsider, when to reset pos? When to reset sel and pattern? When to restart the space?
class runnerT : no_copy {
    class handlerT : no_copy {
        runnerT& self;
        bool enabled = false; // Disabled during member init.

    public:
        handlerT(runnerT& self) : self{self} {}
        void enable() { enabled = true; }

        void on_resized() {
            if (enabled) {
                self.m_paste.reset();
                self.m_sel.reset();
            }
        }

        // void on_restarted() { if (enabled) { self.m_ctrl.set_pause(false); } }

        void on_pattern_set() {
            assert(enabled);
            self.m_ctrl.set_pause(true);
        }
    };

    handlerT m_handler{*this};

    // TODO: use `rule_with_rec` directly?
    class targetT : no_copy {
        rule_with_rec rule = aniso::game_of_life;

    public:
        operator const aniso::ruleT&() const { return rule.get(); }
        const aniso::ruleT& get() const { return rule.get(); }
        void set(const aniso::ruleT& r) { rule.set(r); }

        const rec_for_rule& rec() const { return rule.rec(); }
    };

    targetT m_rule{};

    class ctrlT : no_copy {
    public:
        int step = 1; // Auto mode.
        int interval = global_timer::default_interval;

        enum stepE { None, Ps, P1, Pf }; // Workaround for +s/+1/+!.
        stepE extra_step = None;
        bool extra_pause = false;

    private:
        bool pause = false; // TODO: needn't be private now.
        bool delay = false; // Affects only auto mode.

    public:
        int calc_step(const aniso::ruleT& rule, const bool newly_restarted, const bool ex_delay) {
            if constexpr (global_timer::debug_frame_interval) {
                if (global_timer::test(interval)) {
                    static int last = 0;
                    const int frame = ImGui::GetFrameCount();
                    messenger::set_msg("{}", frame - std::exchange(last, frame));
                }
            }

            const stepE ex_step = std::exchange(extra_step, None);
            const bool ex_pause = std::exchange(extra_pause, false) || pause;
            if (ex_pause || ex_delay) {
                delay = true;
            }

            if (!ex_pause && newly_restarted) {
                // (Unless paused) skip displaying initial state for better visual.
                delay = true;
                return adjust_step(step, strobing(rule));
            } else if (ex_step == Ps) {
                delay = true;
                set_pause(true);
                return adjust_step(step, strobing(rule));
            } else if (ex_step == P1) {
                delay = true;
                return 1;
            } else if (ex_step == Pf) {
                delay = true;
                return adjust_step(step_fast, strobing(rule));
            } else if (!ex_pause && global_timer::test(interval) && !std::exchange(delay, false)) {
                return adjust_step(step, strobing(rule));
            } else {
                return 0;
            }
        }

        bool get_pause() const { return pause; }
        void set_pause(bool p) { pause = p; }
        void flip_pause() { pause = !pause; }
    };

    ctrlT m_ctrl{};

    class torusT : no_copy {
    public:
        static constexpr initT init_init{.seed = 0, .density = 50, .area = 50, .background = aniso::cellT{0}};
        static constexpr aniso::vecT init_size{.x = 600, .y = 400};
        static constexpr aniso::vecT min_size{.x = 30, .y = 20};
        static constexpr aniso::vecT max_size{.x = 1500, .y = 1000};

    private:
        handlerT& m_handler;

        initT m_init = init_init;
        aniso::tileT m_tile = {};
        int m_gen = 0;

        bool m_restarted = true; // -> skip displaying initial state if not paused.
        bool m_written = false;
        bool m_delay = true;

        void mark_written() {
            m_restarted = false;
            m_written = true;
            m_delay = true;
        }
        void resize(const aniso::vecT& size) {
            if (m_tile.resize(align(size))) {
                m_handler.on_resized();
            }
        }

    public:
        torusT(handlerT& m_handler) : m_handler{m_handler} { //
            restart(init_size);
        }

        void restart() {
            m_gen = 0;
            m_init.initialize(m_tile);
            m_restarted = true;
            m_written = false;
            m_delay = true;
            // m_handler.on_restarted();
        }
        void restart(const aniso::vecT& size) {
            resize(size);
            restart();
        }
        void restart(const initT& init) {
            m_init = init;
            restart(m_tile.size()); // Instead of plain restart() (in case the background is resized).
        }
        void restart(const aniso::vecT& size, const initT& init) {
            m_init = init;
            restart(size);
        }

        // TODO: whether to try to avoid repeated init?
        bool restart_has_effect() const { return m_gen != 0 || m_written; }

        bool try_resize(const aniso::vecT& size) {
            if (m_tile.size() != align(size)) {
                restart(size);
                return true;
            }
            return false;
        }

        aniso::tile_const_ref read_only() const { return m_tile.data(); }
        aniso::tile_const_ref read_only(const aniso::rangeT& range) const { return m_tile.data(range); }

        aniso::tile_ref write_only() {
            mark_written();
            return m_tile.data();
        }
        aniso::tile_ref write_only(const aniso::rangeT& range) {
            mark_written();
            return m_tile.data(range);
        }

        void read_and_maybe_write(const auto& fn) {
            if (fn(m_tile.data())) {
                mark_written();
            }
        }

        void rotate_00_to(int dx, int dy) {
            if (dx != 0 || dy != 0) {
                mark_written();
                aniso::tileT temp(m_tile.size());
                aniso::rotate_copy_00_to(temp.data(), m_tile.data(), {.x = dx, .y = dy});
                m_tile.swap(temp);
            }
        }

        int gen() const { return m_gen; }
        double density() const { return double(aniso::count(m_tile.data())) / m_tile.area(); }
        double density(const aniso::rangeT& range) const {
            return double(aniso::count(m_tile.data(range))) / range.size().xy();
        }

        aniso::vecT size() const {
            assert(m_tile.size() == align(m_tile.size()));
            return m_tile.size();
        }

        aniso::vecT align(const aniso::vecT& size) const {
            const aniso::vecT n_size =
                aniso::divmul_floor(aniso::clamp(size, min_size, max_size), m_init.background.size());
            if (!n_size.both_gteq(min_size)) [[unlikely]] {
                return aniso::divmul_ceil(min_size, m_init.background.size());
            }
            return n_size;
        }

        const initT& get_init() const { return m_init; }

        void run(const aniso::ruleT& rule, ctrlT& ctrl) {
            const int step = ctrl.calc_step(rule, std::exchange(m_restarted, false), std::exchange(m_delay, false));
            for (int c = 0; c < step; ++c) {
                m_tile.run_torus(rule);
                ++m_gen;
            }
        }
    };

    torusT m_torus{m_handler}; // Space.

    // !!TODO: recheck (especially rounding)...
    // space-pos == corner-pos + canvas-pos / zoom
    struct coordT {
        zoomT zoom{};
        ImVec2 corner_pos = {0, 0}; // Space.
        ImVec2 to_space(ImVec2 canvas_pos) const { return corner_pos + canvas_pos / zoom; }
        ImVec2 to_canvas(ImVec2 space_pos) const { return (space_pos - corner_pos) * zoom; }
        bool bind(const ImVec2 space_pos, const ImVec2 canvas_pos) {
            return compare_update(corner_pos, space_pos - canvas_pos / zoom);
        }

        ImVec2 to_rotate = {0, 0};
        bool reset_pos = true;
        bool dot_if_no_effect = false; // Workaround for dot feedback.

        // Cannot serve to reset pos immediately (needs up-to-date size).
        ImVec2 last_known_canvas_size = {0, 0};
    };

    coordT m_coord{};

    void reset_pos(const bool dot = false) {
        m_coord.reset_pos = true;
        m_coord.dot_if_no_effect = dot;
    }

    aniso::vecT fullscreen_size(const zoomT& z, ImVec2 canvas_size) const {
        // TODO: using {70, 70} so that 220*160 (via 'Mirror') can select zoom = 2 by default...
        // Too fragile.
        return m_torus.align(from_imvec((canvas_size - ImVec2(70, 70)) / z));
    };

    // !!TODO: enhance to pattern list (& support more sources like text-page & sel op).
    // (Used to be std::optional.)
    class pasteT : no_copy {
        handlerT& m_handler;
        bool has_value = false;

        struct dataT {
            bool newly_assigned = true;
            std::optional<aniso::ruleT> rule = std::nullopt;
            aniso::tileT tile = {};
            aniso::vecT beg = {0, 0};
            aniso::blitE mode = aniso::blitE::Copy;
            bool paste_once = true;
            aniso::vecT size() const { return tile.size(); }
        } data{};

    public:
        pasteT(handlerT& m_handler) : m_handler{m_handler} {}

        explicit operator bool() const { return has_value; }
        dataT* operator->() {
            assert(has_value);
            return &data;
        }

        void set(std::optional<aniso::ruleT> rule_, aniso::tileT tile_) {
            m_handler.on_pattern_set();
            has_value = true;

            data.newly_assigned = true;
            data.rule = std::move(rule_);
            data.tile = std::move(tile_);
            data.beg = {0, 0};
            data.mode = aniso::blitE::Copy;
            // TODO: whether to reset `paste_once`?
        }
        void reset() {
            if (std::exchange(has_value, false)) {
                data.tile.clear();
            }
        }
    };

    pasteT m_paste{m_handler};

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
        int area() const { return width() * height(); }
    };

    std::optional<selectT> m_sel = std::nullopt;

    test_appearing m_appearing{};

public:
    runnerT() { m_handler.enable(); }

    void try_accept(const pass_rule::passT& pass) {
        if (check_diff(pass, m_rule) && pass.deliv) {
            m_rule.set(*pass.deliv);
            m_ctrl.set_pause(false);
            m_torus.restart();
        }
    }
    void set_rule_and_state(const aniso::ruleT& rule, const aniso::vecT& size, const initT& init) {
        reset_pos();
        m_rule.set(rule); // No need to compare (always restarts).
        m_ctrl.set_pause(false);
        m_torus.restart(size, init);
    }

    // Not designed to support multiple instances. (For example, some settings use static variables.)
    void display() {
        if (m_appearing.update()) {
            reset_pos();
            m_ctrl.set_pause(false);
            m_torus.restart();
            m_paste.reset();
            m_sel.reset();
        }

        bool highlight_canvas = false;
        {
            ImGui::AlignTextToFramePadding();
            if (imgui_StrTooltip(
                    "(...)",
                    "The \"pattern editor\" (as highlighted) has wider control than preview windows, and is able to operate on patterns (see 'Edit-pattern' for supported operations).\n\n"
                    "All spaces use torus topology, i.e. information can go across boundaries directly, so the entire space can be imagined as a periodic unit in an infinite space.")) {
                highlight_canvas = true;
            }
            ImGui::SameLine();

            imgui_StrWithID(aniso::to_MAP_str(m_rule), ImGui::GetID("MAP-str"));
            pass_rule::source(m_rule);
            try_accept(pass_rule::dest());
            rclick_popup::for_text([&] {
                if (ImGui::Selectable("Copy rule")) {
                    copy_rule::copy(m_rule);
                }
                if (random_access_status::available()) {
                    if (ImGui::Selectable("Send to rule editor")) {
                        pass_rule::set_extra(m_rule, random_access_status::rule_id);
                    }
                }
                if (ImGui::Selectable("Dump")) {
                    m_rule.rec().dump(previewer::default_settings);
                }
                guide_mode::item_tooltip(rec_for_rule::about_dump);
            });
            guide_mode::item_tooltip("MAP-string for the displayed rule.\n\n"
                                     "Drag a rule here to apply the rule; drag from here to send the rule elsewhere.");

            ImGui::Separator();
        }

        constexpr const char* canvas_name = "Canvas";
        const ImGuiID canvas_id = ImGui::GetID(canvas_name);
        // (Requiring active test to disable shortcuts when an input field is active.)
        const bool canvas_hovered_or_held = imgui_IsItemOrNoneActive(canvas_id) && (ImGui::GetHoveredID() == canvas_id);

        const auto set_init_state_in_popup = [&] {
            const auto item_shortcut = [enable_shortcuts = shortcuts::no_active()](ImGuiKey key) {
                return enable_shortcuts && shortcuts::test_pressed(key, false) && highlight_item();
            };

            // !!TODO: looks strange if the dot (appears at mouse pos) is triggered by shortcut.
            // (Should either remove dot, or always display dot at item center.)
            const bool restart = ImGui::Button("Restart") || item_shortcut(ImGuiKey_R);
            ImGui::SameLine();
            if (imgui_CheckboxV("Pause", m_ctrl.get_pause()) || item_shortcut(ImGuiKey_Space)) {
                m_ctrl.flip_pause();
            }
            ImGui::SameLine();
            imgui_StrTooltip("(?)",
                             "The space will restart and pause if you click 'Restart' or change init settings.\n\n"
                             "Restart : R\n"
                             "Pause   : Space");

            initT init = m_torus.get_init();

            ImGui::PushItemWidth(item_width());
            imgui_StepSliderInt::fn("Seed", &init.seed, 0, 29);
            init.density.step_slide("Density");
            init.area.step_slide("Area");
            ImGui::PopItemWidth();

            ImGui::Separator();

            if (ImGui::TreeNodeEx("Background", ImGuiTreeNodeFlags_NoTreePushOnOpen | ImGuiTreeNodeFlags_AllowOverlap |
                                                    ImGuiTreeNodeFlags_NoAutoOpenOnLog)) {
                ImGui::SameLine(0, imgui_ItemSpacingX() * 3);
                if (ImGui::SmallButton("Flip")) {
                    aniso::flip(init.background.data());
                }
                ImGui::SameLine();
                imgui_StrTooltip("(?)", "Left-click a cell to flip its value (drag to color more cells).\n\n"
                                        "Hold right mouse button + left-click to resize.");

                // There are:
                // demo_size.z is a multiple of any i <= max_period.z, and
                // cell_button_size.z * max_period.z == demo_size.z * demo_zoom (so the images have the same
                // size as the board)
                constexpr aniso::vecT max_period{.x = 4, .y = 4};
                constexpr aniso::vecT demo_size{.x = 24, .y = 24};
                constexpr int demo_zoom = 3;
                constexpr ImVec2 cell_button_size{18, 18}; // TODO: use font-based size (not quite easy...)
                // static_assert(max_period.x * max_period.y == init.background.capacity()); This works, but why?
                static_assert(max_period.x * max_period.y == aniso::tile_buf::capacity());

                ImGui::InvisibleButton("Board", cell_button_size * to_imvec(max_period));
                {
                    const ImVec2 button_beg = ImGui::GetItemRectMin();
                    const bool button_hovered = ImGui::IsItemHovered();
                    const ImVec2 mouse_pos = ImGui::GetMousePos(); // Needn't be valid.
                    ImDrawList& drawlist = *ImGui::GetWindowDrawList();
                    const aniso::tile_ref data = init.background.data();
                    std::optional<aniso::vecT> hover_pos = std::nullopt;
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
                                hover_pos = {.x = x, .y = y};
                            }
                        }
                    }
                    if (hover_pos) {
                        static std::optional<aniso::cellT> col = std::nullopt;
                        static double col_time = -1; // Ideally should be frame count (but not provided).
                        if (col && col_time != ImGui::GetIO().MouseClickedTime[ImGuiMouseButton_Left]) {
                            col.reset();
                        }

                        const bool l_clicked = ImGui::IsMouseClicked(ImGuiMouseButton_Left);
                        if (l_clicked && ImGui::IsMouseDown(ImGuiMouseButton_Right)) {
                            messenger::dot_if(init.background.size() == hover_pos->plus(1, 1));
                            init.background.resize(hover_pos->plus(1, 1));
                            col.reset();
                        } else if (hover_pos->both_lt(data.size)) {
                            aniso::cellT& cell = data.at(*hover_pos);
                            if (l_clicked) {
                                cell = !cell;
                                col = cell;
                                col_time = ImGui::GetIO().MouseClickedTime[ImGuiMouseButton_Left];
                            } else if (col && ImGui::IsMouseDown(ImGuiMouseButton_Left)) {
                                cell = *col;
                            }
                        }
                    }
                }

                {
                    aniso::tileT demo(demo_size);
                    aniso::fill(demo.data(), init.background);
                    static aniso::tileT curr;
                    static bool delay = true;
                    static test_appearing appearing;
                    if (appearing.update() || curr.empty() || init.background != m_torus.get_init().background) {
                        curr = aniso::tileT(demo);
                        delay = true;
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

                    if (global_timer::test(200) && !std::exchange(delay, false)) {
                        curr.run_torus(m_rule);
                    }
                }
            }

            if (restart) {
                messenger::dot_if(m_ctrl.get_pause() && !m_torus.restart_has_effect());
                m_torus.restart();
                m_ctrl.set_pause(true);
            } else if (m_torus.get_init() != init) {
                m_torus.restart(init);
                m_ctrl.set_pause(true);
            }
        };

        ImGui::BeginGroup();
        {
            const bool enable_shortcuts = canvas_hovered_or_held;
            const auto item_shortcut = [enable_shortcuts](ImGuiKey key, bool repeat) {
                return enable_shortcuts && shortcuts::test_pressed(key, repeat) && highlight_item();
            };

            // !!TODO: support keeping tooltips open? (Or just display in regular windows?)
            ImGui::AlignTextToFramePadding();
            if (imgui_StrTooltip("(...)", "Restart  : R\n"
                                          "Pause    : Space\n"
                                          "+s/+1/+! : S/D/F\n"
                                          "-/+ Step     : 1/2\n"
                                          "-/+ Interval : 3/4\n\n"
                                          "These shortcuts work only when the editor is hovered.")) {
                highlight_canvas = true;
            }
            ImGui::SameLine();
            if (ImGui::Button("Restart") || item_shortcut(ImGuiKey_R, false)) {
                m_ctrl.set_pause(false);
                m_torus.restart();
            }
            ImGui::SameLine();
            if (imgui_CheckboxV("Pause", m_ctrl.get_pause()) || item_shortcut(ImGuiKey_Space, false)) {
                m_ctrl.flip_pause();
            }
            ImGui::PushItemFlag(ImGuiItemFlags_ButtonRepeat, true);
            ImGui::SameLine();
            if (ImGui::Button("+s") || item_shortcut(ImGuiKey_S, true)) {
                m_ctrl.extra_step = ctrlT::Ps;
            }
            ImGui::SameLine();
            if (ImGui::Button("+1") || item_shortcut(ImGuiKey_D, true)) {
                m_ctrl.extra_step = ctrlT::P1;
            }
            ImGui::SameLine();
            ImGui::Button("+!");
            if ((ImGui::IsItemActive() && ImGui::IsItemHovered() /* && ImGui::IsMouseDown(ImGuiMouseButton_Left)*/) ||
                (enable_shortcuts && shortcuts::test_down(ImGuiKey_F) && highlight_item())) {
                m_ctrl.extra_step = ctrlT::Pf;
            }
            ImGui::PopItemFlag(); // ImGuiItemFlags_ButtonRepeat
            ImGui::SameLine();
            imgui_StrTooltip("(?)", [] {
                ImGui::PushStyleVarY(ImGuiStyleVar_ItemSpacing, 0);
                imgui_StrPair("+s : ", "Run manually, i.e. pause and advance generation by 'Step'.");
                imgui_StrPair("+1 : ", "Advance generation by 1 (instead of 'Step').");
                imgui_StrPair("+! : ", "Speed up manually, i.e. advance generation by 10 every frame.");
                ImGui::PopStyleVar();
            });
            static_assert(step_fast == 10); // Used in tooltip.

            ImGui::Separator(); // To align with the left panel.

            ImGui::PushItemWidth(item_width());
            const auto to_str = [is_strobing = strobing(m_rule)](int step) {
                if (!is_strobing) {
                    return std::to_string(step);
                } else {
                    return std::format("{} -> {}", step, adjust_step(step, is_strobing));
                }
            };

            if (enable_shortcuts) {
                imgui_StepSliderInt::next_shortcuts = {ImGuiKey_1, ImGuiKey_2};
            }
            imgui_StepSliderInt::fn("Step", &m_ctrl.step, 1, 100, 1, to_str);
            ImGui::SameLine();
            imgui_StrTooltip(
                "(?)",
                "For auto-mode, '+s' and '+!', if the rule maps all-0 case to 1 and all-1 case to 0, the step will be ceiled to 2*n (e.g. 1->2, 2->2) to avoid large spans of 0/1 areas flashing between two colors.\n\n"
                "Such rules are called \"strobing rules\". There also exist rules that are non-strobing (so the adjustment won't take place) but can develop non-trivial flashing areas. If you encounter such rules, to avoid the flashing effect you can manually try some different steps.\n\n"
                "('+1' is not affected by the adjustment, and can serve to change the parity of generation in all cases.)");
            if (enable_shortcuts) {
                imgui_StepSliderInt::next_shortcuts = {ImGuiKey_3, ImGuiKey_4};
            }
            global_timer::step_slide("Interval", m_ctrl.interval, 0, 500);
            ImGui::SameLine();
            imgui_StrTooltip("(?)", "Min interval for auto-mode. (0 ms ~ the space will run at each frame.)");
            ImGui::PopItemWidth();
        }
        ImGui::EndGroup();

        ImGui::SameLine(0, imgui_ItemSpacingX() * 4);
        // const float right_col_abs_pos = imgui_GetCursorScreenPosX();

        ImGui::BeginGroup();
        menu_like_popup::button("Init state");
        menu_like_popup::popup(set_init_state_in_popup);
        ImGui::SameLine();
        menu_like_popup::button("Resize");
        menu_like_popup::popup([&] {
            static input_int input_x{}, input_y{};
            if (ImGui::IsWindowAppearing()) {
                input_x.clear();
                input_y.clear();
                ImGui::ActivateItemByID(ImGui::GetID("##Width"));
            }

            const aniso::vecT size = m_torus.size();
            ImGui::AlignTextToFramePadding();
            imgui_Str("Size ~ ");
            ImGui::SameLine(0, 0);
            ImGui::SetNextItemWidth(imgui_CalcButtonSize("Width:00000").x);
            const auto ix = input_x.input(5, "##Width", std::format("Width:{}", size.x).c_str());
            ImGui::SameLine(0, imgui_ItemInnerSpacingX());
            ImGui::SetNextItemWidth(imgui_CalcButtonSize("Height:00000").x);
            const auto iy = input_y.input(5, "##Height", std::format("Height:{}", size.y).c_str());
            if (ix || iy) {
                reset_pos();
                // Both values will be flushed if either receives the enter key.
                if (m_torus.try_resize({.x = ix.value_or(input_x.flush().value_or(size.x)),
                                        .y = iy.value_or(input_y.flush().value_or(size.y))})) {
                    m_ctrl.set_pause(false);
                } else {
                    // TODO: also set_pause(false)?
                    messenger::dot();
                }
            }

            for (bool first = true; const zoomT& z : zoomT::list()) {
                if (!std::exchange(first, false)) {
                    ImGui::SameLine(0, imgui_ItemInnerSpacingX());
                }
                // (`last_known_canvas_size` is 99.99% reliable here due to UI logic.)
                const aniso::vecT n_size = fullscreen_size(z, m_coord.last_known_canvas_size);
                if (ImGui::RadioButton(z.str(), m_torus.size() == n_size)) { // Instead of comparing zoom.
                    reset_pos();
                    if (m_torus.try_resize(n_size)) {
                        m_ctrl.set_pause(false);
                    }
                    // No need for dot.
                }
                imgui_ItemTooltip([&] { ImGui::Text("%d*%d", n_size.x, n_size.y); });
            }
            ImGui::SameLine();
            if (imgui_StrTooltip("(?)", "Click a radio button to resize the space to fit the screen.\n\n"
                                        "(Scroll in the editor to zoom in/out without resizing.)")) {
                highlight_canvas = true;
            }
        });
        ImGui::SameLine();
        if (ImGui::Button("Reset pos")) {
            reset_pos(true);
        }
        if (guide_mode::item_tooltip(
                "Center the space and display in suitable zoom (as if the space is newly resized).")) {
            highlight_canvas = true;
        }

        ImGui::Spacing(); // To align with the separator.

        // !!TODO: workaround to display updated info. (`m_torus.run()` happens after canvas button.)
        // (Currently the sync logic is messy and heavily constrained; should redesign if possible...)
        const ImVec2 supposed_abs_pos = ImGui::GetCursorScreenPos();
        const auto unfortunately_deferred = [&] {
            ImGui::BeginGroup();
            ImGui::AlignTextToFramePadding();
            const aniso::vecT size = m_torus.size();
            ImGui::Text("Size:%d*%d   Zoom:%s", size.x, size.y, m_coord.zoom.str());

            ImGui::AlignTextToFramePadding();
            ImGui::Text("Generation:%d   Density:%.3f", m_torus.gen(),
                        m_sel ? m_torus.density(m_sel->to_range()) : m_torus.density());
            // TODO: has no stable offset (can break hover)...
            // ImGui::SameLine();
            // imgui_StrTooltip("(?)", "Density of the entire space (or the selected area).");
            ImGui::EndGroup();
        };
        ImGui::EndGroup();

        ImGui::Separator();

        ImGui::AlignTextToFramePadding();
        // & '6' to see the projected view in hexagonal space.
        if (imgui_StrTooltip("(...)", "Scroll in the editor to zoom in/out.\n\n"
                                      "Left-click and drag to move the space.\n"
                                      "Ctrl + drag to rotate the space.\n\n"
                                      "Right-click and drag to select area.\n"
                                      "Single right-click to unselect.")) {
            highlight_canvas = true;
        }
        ImGui::SameLine();
        static bool show_op_window = false;
        m_appearing.reset_if_appearing(show_op_window);
        ImGui::Checkbox("Edit-pattern", &show_op_window);
        ImGui::SameLine(0, imgui_ItemSpacingX() * 3); // imgui_CalcCharWidth(' ') * 3;
        if (m_sel) {
            ImGui::Text("Selected:%d*%d", m_sel->width(), m_sel->height());
        } else {
            imgui_Str("Selected:N/A");
        }
        rclick_popup::for_text([&] {
            ImGui::BeginDisabled(!m_sel.has_value());
            if (ImGui::Selectable("Unselect")) { // "Clear" would be misleading here.
                m_sel.reset();
            }
            ImGui::EndDisabled();
        });

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
            m_coord.last_known_canvas_size = canvas_size;

            const bool active = ImGui::IsItemActive();
            const bool hovered = ImGui::IsItemHovered();
            const bool l_down = ImGui::IsMouseDown(ImGuiMouseButton_Left);
            const bool r_down = ImGui::IsMouseDown(ImGuiMouseButton_Right);
            if (active) {
                m_ctrl.extra_pause = true; // Pause for this frame.
            }

            // `m_torus` won't resize now.
            const aniso::vecT tile_size = m_torus.size();

            if (std::exchange(m_coord.reset_pos, false)) {
                // Select the largest zoom that can hold the entire space.
                zoomT largest = zoomT::min();
                for (const zoomT& z : std::views::reverse(zoomT::list())) {
                    if (fullscreen_size(z, canvas_size).both_gteq(tile_size)) {
                        largest = z;
                        break;
                    }
                }
                const bool a = compare_update(m_coord.zoom, largest);
                // Center the space.
                const bool b = m_coord.bind(to_imvec(tile_size) / 2, canvas_size / 2);
                messenger::dot_if(m_coord.dot_if_no_effect && !a && !b);
            }

            if (m_sel && m_sel->active && (!r_down || m_paste || ImGui::IsItemDeactivated())) {
                m_sel->active = false;
                // Allow a single right-click to unselect the area.
                // (`bounding_box` has no size check like this. This is intentional.)
                if (m_sel->area() <= 2) {
                    m_sel.reset();
                }
            }

            std::optional<aniso::vecT> zoom_center = std::nullopt; // Not clamped.
            bool hex_mode = false;

            if (!active || !hovered) {
                m_coord.to_rotate = {0, 0};
            }
            if (hovered && ImGui::IsMousePosValid()) {
                const auto& io = ImGui::GetIO();
                const ImVec2 mouse_pos = io.MousePos - canvas_min;

                if (active && (!m_paste ? l_down && !r_down : l_down || r_down)) {
                    if (io.KeyCtrl) {
                        m_coord.to_rotate += io.MouseDelta / m_coord.zoom;
                        const int dx = m_coord.to_rotate.x; // Truncate.
                        const int dy = m_coord.to_rotate.y;
                        if (dx || dy) {
                            m_coord.to_rotate -= ImVec2(dx, dy);
                            m_torus.rotate_00_to(dx, dy);
                        }
                    } else {
                        m_coord.corner_pos -= io.MouseDelta / m_coord.zoom;
                    }
                } else if (active && !m_paste && l_down && r_down) { // No rotating.
                    m_coord.corner_pos -= io.MouseDelta / m_coord.zoom;
                }

                // TODO: require Ctrl? This is often triggered by accident...
                if (/*io.KeyCtrl &&*/ imgui_MouseScrolling()) {
                    m_coord.to_rotate = {0, 0};

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
                if (!m_paste && !(m_sel && m_sel->active && m_sel->area() > 2)) {
                    if (imgui_IsItemHoveredForTooltip() && cel_pos.both_gteq({-10, -10}) &&
                        cel_pos.both_lt(tile_size.plus(10, 10))) {
                        hex_mode = want_hex_mode(m_rule);
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

            m_torus.run(m_rule, m_ctrl);

            // Render.
            {
                const ImVec2 screen_min = ImFloor(canvas_min + m_coord.to_canvas({0, 0}));
                const ImVec2 screen_max = screen_min + to_imvec(tile_size) * m_coord.zoom;

                ImDrawList& drawlist = *ImGui::GetWindowDrawList();
                drawlist.PushClipRect(canvas_min, canvas_max);
                drawlist.AddRectFilled(canvas_min, canvas_max, IM_COL32_GREY(24, 255)); // Background.

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

                            // TODO: too hard to correctly show selected area in hex mode...
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
                    // (Wasteful, but after all this works...)
                    m_torus.read_and_maybe_write([&](const aniso::tile_ref tile) {
                        const aniso::vecT paste_beg = aniso::clamp(m_paste->beg, {0, 0}, tile_size - m_paste->size());
                        const aniso::vecT paste_end = paste_beg + m_paste->size();
                        m_paste->beg = paste_beg;

                        const aniso::tile_ref paste_area = tile.clip({paste_beg, paste_end});
                        aniso::tileT temp(paste_area);
                        // !!TODO: support specifying align req.
                        aniso::blit(paste_area, m_paste->tile.data(), m_paste->mode);
                        {
                            drawlist.AddImage(to_texture(tile, scale_mode), screen_min, screen_max);
                            const ImVec2 paste_min = screen_min + to_imvec(paste_beg) * m_coord.zoom;
                            const ImVec2 paste_max = screen_min + to_imvec(paste_end) * m_coord.zoom;
                            drawlist.AddRectFilled(paste_min, paste_max, IM_COL32(255, 0, 0, 60));
                        }
                        if (hovered && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left)) {
                            messenger::dot_if(!m_paste->paste_once);
                            if (m_paste->paste_once) {
                                m_paste.reset();
                            }
                            return true;
                        } else { // Restore.
                            aniso::copy(paste_area, temp.data());
                            return false;
                        }
                    });
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

            ImGui::SetCursorScreenPos(supposed_abs_pos);
            unfortunately_deferred();
            // Note: next widget will have wrong pos. (Working as nothing follows in this window.)

            const bool canvas_hovered_or_held_ex =
                canvas_hovered_or_held && (active || !ImGui::IsAnyItemActive()) && hovered;
            edit_pattern(canvas_hovered_or_held_ex, show_op_window);
            if (m_paste) {
                pattern_settings(canvas_hovered_or_held_ex, /*init pos*/ canvas_min);
            }

            assert(tile_size == m_torus.size());
        }
    }

    // TODO: support plain-text format as well?
    void load_pattern(std::string_view text) {
        // (Not interested in whether the header has correct format.)
        const std::string_view header = aniso::strip_RLE_header(text);
        aniso::from_RLE_str(text, [&](const aniso::prepareT size) -> std::optional<aniso::tile_ref> {
            if (size.empty()) {
                messenger::set_msg("Found no pattern (RLE-string).");
                return std::nullopt;
            } else if (const aniso::vecT tile_size = m_torus.size(); //
                       size.x > tile_size.x || size.y > tile_size.y) {
                messenger::set_msg("The space is not large enough for the pattern.\n"
                                   "Space size: x = {}, y = {}\n"
                                   "Pattern size: x = {}, y = {}",
                                   tile_size.x, tile_size.y, size.x, size.y);
                return std::nullopt;
            } else {
                m_paste.set(aniso::extract_one_rule(header),
                            aniso::tileT(aniso::vecT{.x = (int)size.x, .y = (int)size.y}));
                return m_paste->tile.data();
            }
        });
    }

private:
    void edit_pattern(const bool canvas_hovered_or_held, bool& show_op_window) {
        // TODO: disable some operations if `m_paste.has_value`?
        // TODO: set pause for some operations?
        struct op_term : no_copy {
            const char* (*check)(const runnerT& self);
            void (*op)(runnerT& self);
            const char* key_label;
            ImGuiKey key;

            // (Should respect -Wreorder-ctor)
            consteval op_term(ImGuiKey key, const char* key_label, const char* (*check)(const runnerT& self),
                              void (*op)(runnerT& self))
                : check(check), op(op), key_label(key_label), key(key) {}

            void apply(runnerT& self) const {
                if (const char* msg = check(self)) {
                    messenger::set_msg(msg);
                } else {
                    op(self);
                }
            }
        };

        static constexpr auto check_nothing = [](const runnerT&) -> const char* { return nullptr; };
        static constexpr auto check_sel = [](const runnerT& self) -> const char* {
            return self.m_sel ? nullptr : "No selected area.";
        };
        static constexpr auto check_for_extract = [](const runnerT& self) -> const char* {
            return self.m_paste || self.m_sel ? nullptr : "No selected area.";
        };

        static percentT flip_den = 50;
        static constexpr op_term op_random_flip{
            ImGuiKey_Equal, "+ (=)", check_sel,
            [](runnerT& self) {
                assert(self.m_sel);
                static std::mt19937 rand = rand_source::create();
                aniso::random_flip(self.m_torus.write_only(self.m_sel->to_range()), rand, flip_den.get());
            } // (Without a comment here, the clang-format result will be extremely ugly...)
        };
        static constexpr op_term op_flip{
            ImGuiKey_Minus, "- (_)", check_sel,
            [](runnerT& self) {
                assert(self.m_sel);
                aniso::flip(self.m_torus.write_only(self.m_sel->to_range()));
            } //
        };

        // !!TODO: support variable p_size...
        static int background = 0; // TODO: -> enum.
        static constexpr op_term op_clear_inside{
            ImGuiKey_Backspace, "Backspace", check_sel,
            [](runnerT& self) {
                assert(self.m_sel);
                const auto sel_area = self.m_torus.write_only(self.m_sel->to_range());
                if (background == 0 || background == 1) {
                    aniso::fill(sel_area, aniso::cellT(background));
                } else {
                    constexpr aniso::vecT p_size{2, 2};
                    if (check_border(sel_area, p_size)) {
                        aniso::self_repeat(sel_area, p_size);
                    }
                }
            } //
        };
        static constexpr op_term op_clear_outside{
            ImGuiKey_0, "0 (zero)", check_sel,
            [](runnerT& self) {
                assert(self.m_sel);
                if (background == 0 || background == 1) {
                    aniso::fill_outside(self.m_torus.write_only(), self.m_sel->to_range(), aniso::cellT(background));
                } else {
                    const aniso::rangeT sel_range = self.m_sel->to_range();
                    const auto sel_area = self.m_torus.read_only(sel_range);
                    constexpr aniso::vecT p_size{2, 2};
                    if (check_border(sel_area, p_size)) {
                        aniso::fill_outside(
                            self.m_torus.write_only(), sel_range,
                            aniso::realign_from_to(sel_area.clip_corner(p_size), sel_range.begin, {0, 0}));
                    }
                }
            } //
        };

        static constexpr op_term op_select_all{
            ImGuiKey_A, "A", check_nothing,
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
            ImGuiKey_B, "B", check_sel,
            [](runnerT& self) {
                assert(self.m_sel);
                const aniso::rangeT sel_range = self.m_sel->to_range();
                const auto sel_area = self.m_torus.read_only(sel_range);
                constexpr aniso::vecT p_size{2, 2};
                if (check_border(sel_area, p_size)) {
                    const aniso::rangeT bound = aniso::bounding_box(sel_area, p_size);
                    if (!bound.empty()) {
                        const aniso::rangeT range{.begin = sel_range.begin + bound.begin - p_size,
                                                  .end = sel_range.begin + bound.end + p_size};
                        // Then whether to show dot for clear functions? No way...
                        // messenger::dot_if(self.m_sel && self.m_sel->to_range() == range);
                        self.m_sel = {.active = false, .beg = range.begin, .end = range.end.minus(1, 1)};
                    } else {
                        // m_sel.reset();
                        messenger::set_msg("The area contains nothing.");
                    }
                }
            } //
        };

#if 0
        // !!TODO: enhance to background sampling...
        static constexpr op_term op_test_bg_period{
            ImGuiKey_P, "P", check_sel,
            [](runnerT& self) {
                assert(self.m_sel);
                const auto sel_area = self.m_torus.read_only(self.m_sel->to_range());
                if (const auto p_size = aniso::spatial_period_full_area(sel_area, {30, 30})) {
                    if (const auto period = aniso::torus_period(self.m_rule, sel_area.clip_corner(*p_size), 60)) {
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
#endif

        static constexpr bool add_rule = true; // TODO: whether to support specifying this?
        static constexpr op_term op_copy{
            ImGuiKey_C, "C", check_sel,
            [](runnerT& self) {
                assert(self.m_sel);
                const auto sel_area = self.m_torus.read_only(self.m_sel->to_range());
                const std::string rle_str = aniso::to_RLE_str(sel_area, add_rule ? &self.m_rule.get() : nullptr);
                set_clipboard_and_notify(rle_str);
#ifdef YDEBUG
                aniso::tileT test(sel_area.size);
                aniso::from_RLE_str(rle_str, [&](const aniso::prepareT size) {
                    assert(size.x == sel_area.size.x && size.y == sel_area.size.y);
                    return test.data();
                });
                assert(test == sel_area);
#endif // YDEBUG
            } //
        };
        static constexpr op_term op_extract{
            ImGuiKey_X, "X", check_for_extract,
            [](runnerT& self) {
                if (self.m_sel) {
                    self.m_sel->active = false;
                }
                if (self.m_paste) { // (Undocumented.)
                    self.m_paste.reset();
                } else {
                    assert(self.m_sel);
                    const auto sel_area = self.m_torus.read_only(self.m_sel->to_range());
                    self.m_paste.set(self.m_rule, aniso::tileT(sel_area));
                }
            } //
        };
        static constexpr op_term op_identify{
            ImGuiKey_I, "I (i)", check_sel,
            [](runnerT& self) {
                assert(self.m_sel);
                constexpr aniso::vecT p_size{2, 2};
                if (const auto result = identify(self.m_torus.read_only(self.m_sel->to_range()), self.m_rule, p_size)) {
                    const auto& [pattern, offset, period, _] = *result;
                    const bool no_offset = offset == aniso::vecT{0, 0};
                    std::string desc;
                    if (no_offset && period == 1) {
                        desc = "still life";
                    } else if (no_offset) {
                        desc = std::format("oscillator, period = {}", period);
                    } else {
                        desc = std::format("spaceship, period = {}, offset = ({},{})", period, offset.x, offset.y);
                    }
                    std::string rle_str = "#C " + desc + '\n' + aniso::to_RLE_str(pattern.data(), &self.m_rule.get());
                    if (set_clipboard(rle_str)) {
                        if constexpr (0) {
                            messenger::set_msg(std::move(rle_str));
                        } else {
                            desc[0] = std::toupper(desc[0]);
                            desc += '.';
                            messenger::set_msg(std::move(desc));
                        }
                    }
                }
            } //
        };

        // TODO: support pasting rule directly (if the text contains only map-str)?
        static constexpr op_term op_paste{
            ImGuiKey_V, "V", check_nothing,
            [](runnerT& self) {
                if (self.m_sel) {
                    self.m_sel->active = false;
                }
                if (self.m_paste) { // (Undocumented.)
                    self.m_paste.reset();
                } else if (const auto text = read_clipboard(); !text.empty()) {
                    self.load_pattern(text);
                }
            } //
        };

        // TODO: combine with 'I'? (identify -> display a window to support both ops)
        static constexpr op_term op_capture{
            ImGuiKey_P, "P", check_sel,
            [](runnerT& self) {
                // TODO: conceptually should test "set-table-avail" (currently not necessary)...
                if (!random_access_status::available()) {
                    return;
                }
                assert(self.m_sel);
                const auto sel_area = self.m_torus.read_only(self.m_sel->to_range());
                constexpr aniso::vecT p_size{2, 2};
                // TODO: should be dealt-with in `identify` directly...
                // (Workaround to support capturing pure bg.)
                if (!check_border(sel_area, p_size)) {
                    return;
                } else if (aniso::is_spatially_periodic(sel_area, p_size)) {
                    aniso::lockT rec{};
                    if (aniso::torus_period(self.m_rule, sel_area.clip_corner(p_size), 20, &rec)) {
                        load_capture(self.m_rule, rec);
                        return;
                    }
                }

                if (const auto result = identify(self.m_torus.read_only(self.m_sel->to_range()), self.m_rule, p_size)) {
                    load_capture(self.m_rule, result->rec);
                }
            } //
        };

        const op_term* op = nullptr;

        if (canvas_hovered_or_held) {
            static constexpr const op_term* op_list[]{
                &op_random_flip, &op_flip,    &op_clear_inside, &op_clear_outside, &op_select_all, &op_bounding_box,
                &op_copy,        &op_extract, &op_identify,     &op_paste,         &op_capture,
            };
            for (const op_term* t : op_list) {
                if (shortcuts::test_pressed(t->key, false)) {
                    op = t;
                    break;
                }
            }
        }
        if (show_op_window) {
            ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
            imgui_CenterNextWindow(ImGuiCond_FirstUseEver);
            imgui_Window::next_window_titlebar_tooltip =
                "The shortcuts work only when the editor is hovered (and this window doesn't need to stay open).";
            if (auto window = imgui_Window("Edit pattern (settings)", &show_op_window,
                                           ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
                ImGui::PushItemWidth(item_width());

                const auto term = [&](const char* label, const op_term& t) {
                    const char* msg = t.check(*this);
                    // Was `ImGui::MenuItem(label, shortcut, nullptr, valid)`.
                    ImGui::BeginDisabled(msg);
                    if (imgui_SelectableStyledButton(label, false, t.key_label)) {
                        op = &t;
                    } else if (op == &t) { // Triggered by shortcut.
                        highlight_item();
                    }
                    ImGui::EndDisabled();
                    if (msg) {
                        imgui_ItemTooltip(msg);
                    }
                };

                flip_den.step_slide("Density");
                term("Random flip", op_random_flip);
                guide_mode::item_tooltip("Flip random cells in the selected area (rate controlled by 'Density').");
                term("Flip", op_flip);
                guide_mode::item_tooltip("Flip all cells in the selected area.");

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
                    ImGui::PushStyleVarY(ImGuiStyleVar_ItemSpacing, 0);
                    imgui_Str("For 'Clear inside/outside':");
                    imgui_StrPair("0/1 : ", "Fill area with 0 or 1 (unconditionally).");
                    imgui_StrPair(
                        "C   : ",
                        "Fill area with \"2*2 bg pattern\" (listed below) if the area is enclosed by the pattern (otherwise do nothing).");
                    imgui_Str(""); // For spacing.
                    ImGui::PopStyleVar();

                    // TODO: use font-based size...
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
                        aniso::fill(demo_tile.data({pos, pos + demo_size}), {{bg_data[i].data(), {2, 2}}});
                    }
                    const ImTextureID texture = to_texture(demo_tile.data(), scaleE::Nearest);
                    for (int i = 0; i < total; ++i) {
                        if (i != 0) {
                            ImGui::SameLine();
                        }
                        ImGui::Image(texture, to_imvec(demo_size * demo_zoom), ImVec2(0, float(i) / total),
                                     ImVec2(1, float(i + 1) / total));
                        // TODO: use image border (ImGuiStyleVar_ImageBorderSize) instead?
                        imgui_ItemRect(IM_COL32_GREY(160, 255));
                    }
                });
                term("Clear inside", op_clear_inside);
                term("Clear outside", op_clear_outside);

                ImGui::Separator();

                term("Select all", op_select_all);
                term("Bound", op_bounding_box);
                guide_mode::item_tooltip(
                    "Get the bounding box for the pattern. The resulting bounding box will include a layer of bg pattern.\n\n"
                    "(The area should be enclosed in 2*2 bg pattern.)");
                // term("Test background", op_test_bg_period);
                // guide_mode::item_tooltip("Test the size and period of periodic background.");

                ImGui::Separator();

                term("Copy", op_copy);
                guide_mode::item_tooltip("Copy selected area to the clipboard (as RLE-string).");
                term("Extract", op_extract);
                guide_mode::item_tooltip("Extract selected area without copying to the clipboard.");
                term("Identify", op_identify);
                guide_mode::item_tooltip(
                    "Identify a single object in the area, and copy its smallest phase to the clipboard. (Supported objects include still life, oscillator and spaceship.)\n\n"
                    "(The area should be enclosed in 2*2 bg pattern.)");
                term("Paste", op_paste);
                guide_mode::item_tooltip("Load pattern (RLE-string) from the clipboard.");

                if (random_access_status::available()) {
                    ImGui::Separator();

                    term("Capture", op_capture);
                    guide_mode::item_tooltip( // (Or the bg itself if the area contains nothing.)
                        "(Experimental) identify a single object in the area and record all invoked cases. The record can serve to generate rules that can reproduce the same object (in all phases).\n\n"
                        "(The area should be enclosed in 2*2 bg pattern.)");
                }

                ImGui::PopItemWidth();
            }
        }

        if (op) {
            op->apply(*this);
        }
    }

    void pattern_settings(const bool canvas_hovered_or_held, const ImVec2 init_pos) {
        assert(m_paste);
        // TODO: the effect should be more obvious...
        if (std::exchange(m_paste->newly_assigned, false)) {
            // ImGui::SetNextWindowCollapsed(false);
            ImGui::SetNextWindowFocus();
        }
        bool open = true;
        ImGui::SetNextWindowPos(init_pos, ImGuiCond_Appearing);
        imgui_Window::next_window_titlebar_tooltip = "Close the window to discard the pattern.";
        const std::string title = std::format("Pattern:{}*{}###Pattern", m_paste->size().x, m_paste->size().y);
        if (auto window = imgui_Window(title.c_str(), &open,
                                       ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_NoCollapse |
                                           ImGuiWindowFlags_AlwaysAutoResize)) {
            imgui_Str("Double-click to paste.");
            if (m_paste->rule && *(m_paste->rule) != m_rule) { // TODO: improve...
                ImGui::SameLine(0, imgui_ItemSpacingX() * 2);
                imgui_StrWithID("[Rule]");
                pass_rule::source(*(m_paste->rule));
                imgui_ItemTooltip([&] {
                    imgui_Str("The pattern specified a different rule.");
                    previewer::preview(0, previewer::default_settings, *(m_paste->rule));
                });
                ImGui::SameLine();
                if (double_click_button_small("Apply")) {
                    try_accept(&*(m_paste->rule));
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
            imgui_StrTooltip("(?)", "Once  : Paste once (clear automatically after pasting).\n"
                                    "Multi : Paste multiple times.\n\n"
                                    "Copy       : Paste values directly.\n"
                                    "Or/And/Xor : Perform binary op.\n"
                                    "(Use Or/And to treat black/white cells as transparent bg.)\n\n"
                                    "Right-click in the editor to switch between Copy/Or/And/Xor.");
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
            m_paste.reset();
        }
    }
};

// TODO: rename -> (pattern_)editor
static runnerT runner;
void edit_pattern(frame_main_token) {
    pattern_editor_status::update();
    // pattern_editor_status::begin_disabled(); // TODO: whether to disable?
    runner.display();
    // pattern_editor_status::end_disabled();
}
void load_pattern(std::string_view text) { runner.load_pattern(text); }

// (Actually starts-with-pattern.)
bool has_pattern(std::string_view text) { return aniso::is_RLE_str(text); }

struct previewer::_global_data : no_create {
    static constexpr initT init_init{.seed = 0, .density = 50, .area = 100, .background = aniso::cellT{0}};
    inline static initT init = init_init; // Globally shared.

    struct termT {
#ifdef YDEBUG
        bool appearing = true; // (Implied by `tile.empty()`.)
#endif
        bool active = false;
        bool pause = false;
        bool delay = false; // (Affects only auto mode.)
        initT init = _global_data::init;
        aniso::ruleT rule = {};
        aniso::tileT tile = {};
    };

    inline static std::unordered_map<uint64_t, termT> terms;

    // (Workaround to support group op; configT should live across frames.)
    struct opT {
        bool extra_pause = false, restart = false, flip_pause = false, p_s = false, p_1 = false, p_f = false;
    };
    struct opT_ex {
        uintptr_t owner = 0; // (To avoid storing invalid ptr.)
        opT op = {};
    };
    static opT_ex group_op;
    static opT_ex group_op_next;

    static void begin_frame() {
        if (!terms.empty()) {
            // According to https://en.cppreference.com/w/cpp/container/unordered_map/erase_if
            // There seems no requirement on the predicate.
            std::erase_if(terms, [](std::pair<const uint64_t, termT>& pair) {
                return !std::exchange(pair.second.active, false);
            });
        }

        group_op.owner = std::exchange(group_op_next.owner, 0);
        group_op.op = group_op_next.op;
    }
};

// (Could be inline static; workaround for gcc & clang building...)
previewer::_global_data::opT_ex previewer::_global_data::group_op{};
previewer::_global_data::opT_ex previewer::_global_data::group_op_next{};

void previewer::configT::_set() {
    // ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, {3, 2});
    ImGui::PushItemWidth(item_width());

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
    global_timer::step_slide("Interval", interval, 0, 500);

    ImGui::Separator();
    if (ImGui::TreeNodeEx("Init state", ImGuiTreeNodeFlags_NoTreePushOnOpen | ImGuiTreeNodeFlags_AllowOverlap |
                                            ImGuiTreeNodeFlags_NoAutoOpenOnLog)) {
        ImGui::SameLine(0, imgui_ItemSpacingX() * 3);
        imgui_StrTooltip(
            "(?)", "These settings are shared by all preview windows.\n\n"
                   "(Changing init state will restart and pause all preview windows; press A+R/Space to resume all.)");

        // TODO: support both global and per-group setting mode?
        initT& init = _global_data::init;
        imgui_StepSliderInt::fn("Seed", &init.seed, 0, 9);
        init.density.step_slide("Density", 10, 100, 10);
        init.area.step_slide("Area", 10, 100, 10);

        ImGui::AlignTextToFramePadding();
        imgui_Str("Background ~ ");
        ImGui::SameLine(0, 0);
        ImGui::Dummy(square_size());
        imgui_ItemRectFilled(IM_COL32_BLACK);
        imgui_ItemRect(IM_COL32_GREY(160, 255));
    }

    ImGui::PopItemWidth();
    // ImGui::PopStyleVar();
}

void previewer::begin_frame(frame_main_token) { _global_data::begin_frame(); }

// TODO: allow setting the step and interval with shortcuts when the window is hovered?
// TODO: support send-to menu ops...
// TODO: support pause mode?
void previewer::_preview(const uint64_t id, const configT& config, const aniso::ruleT& rule) {
    assert(ImGui::GetItemID() != 0);
    assert(ImGui::GetItemRectSize() == config.size_imvec());
    assert(ImGui::IsItemVisible());

    _global_data::termT& term = _global_data::terms[id];
    if (term.active) [[unlikely]] { // ID conflict
        assert(false);
        return;
    }
    term.active = true;

    const aniso::vecT tile_size{.x = int(config.width_ / config.zoom_), .y = int(config.height_ / config.zoom_)};

    // TODO: (though the actual behaviors are ok) these op logics are quite messy...
    // TODO: support setting step/interval from menu?
    const bool passing = pass_rule::source(rule);
    bool restart_from_menu = false; // TODO: remove this?
    const rclick_popup::hoverE hov = rclick_popup::popup_no_highlight(ImGui::GetItemID(), [&] {
        // & '6' to see the projected view in hexagonal space.
        imgui_StrTooltip("(...)", "Drag to send the rule elsewhere.\n\n"
                                  "Restart  : R\n"
                                  "Pause    : Space\n"
                                  "+s/+1/+! : S/D/F\n\n"
                                  "Press G to apply shortcuts to the entire group.\n"
                                  "Press A to apply to all preview windows.");
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
        guide_mode::item_tooltip("Copy rule to the clipboard (as MAP-string).");
        if (ImGui::Selectable("Restart")) {
            restart_from_menu = true;
        }
        guide_mode::item_tooltip("Equivalent to the shortcut (R).");

        const bool rule_editor_avail = random_access_status::available();
        const bool pattern_editor_avail = pattern_editor_status::available();

        if ((rule_editor_avail || pattern_editor_avail) && imgui_BeginMenuFromPopup("Send to")) {
            if (rule_editor_avail) {
                if (ImGui::Selectable("Rule editor")) {
                    pass_rule::set_extra(rule, random_access_status::rule_id);
                }
                guide_mode::item_tooltip(
                    "Equivalent to sending rule to the 'Edit-rule' checkbox (or '[Z]' if it's turned on).");
            }
            if (pattern_editor_avail) {
                if (ImGui::Selectable("Pattern editor")) {
                    runner.try_accept(&rule);
                }
                guide_mode::item_tooltip(
                    "Equivalent to sending rule to the MAP-string ('MAP...').\n\n"
                    "(This does not duplicate space state, so you may see different patterns. Use 'Mirror' to operate on the same patterns.)");
            }
            ImGui::EndMenu();
        }
        if (pattern_editor_avail) {
            if (ImGui::Selectable("Mirror")) {
                runner.set_rule_and_state(rule, tile_size, term.init);
            }
            guide_mode::item_tooltip(
                "Send rule to the pattern editor & duplicate the space state (so you can operate on the same patterns).");
        }
    });
    assert_implies(passing, hov == rclick_popup::None);

    // (Stricter than `IsItemHovered()`; will be false when the popup is appearing (hidden).)
    const bool hovered = hov == rclick_popup::Hovered;
    // X assert(hovered == ImGui::IsItemHovered());
    const bool active = ImGui::IsItemActive();

    constexpr uintptr_t owner_all = 1;
    const bool has_group_op = _global_data::group_op.owner == owner_all || //
                              _global_data::group_op.owner == uintptr_t(&config);
    _global_data::opT op = has_group_op ? _global_data::group_op.op : _global_data::opT{};
    if (hovered && (active || shortcuts::no_active())) {
        // (Using unfiltered shortcut for `p_f` for smoother inter with seq op (<</>>).)
        const _global_data::opT op2 = {.extra_pause = active,
                                       .restart = shortcuts::test_pressed(ImGuiKey_R),
                                       .flip_pause = shortcuts::test_pressed(ImGuiKey_Space),
                                       .p_s = shortcuts::test_pressed(ImGuiKey_S, true),
                                       .p_1 = shortcuts::test_pressed(ImGuiKey_D, true),
                                       .p_f = shortcuts::global_flag(ImGuiKey_F)};
        if (shortcuts::global_flag(ImGuiKey_A)) {
            _global_data::group_op_next = {.owner = owner_all, .op = op2};
        } else if (shortcuts::global_flag(ImGuiKey_G)) {
            _global_data::group_op_next = {.owner = uintptr_t(&config), .op = op2};
        } else if (!has_group_op) {
            op = op2;
        }
    }

    const bool init_changed = compare_update(term.init, _global_data::init);
    const bool restart = restart_from_menu + op.restart + // No short-circuiting.
                         term.tile.resize(tile_size) + init_changed + compare_update(term.rule, rule);
    assert_implies(std::exchange(term.appearing, false), restart && !init_changed);
    if (restart) {
        term.pause = init_changed;
        term.init.initialize(term.tile);
    }
    if (op.flip_pause) {
        term.pause = !term.pause;
    }

    const int step = [&] {
        const bool pause = op.extra_pause || passing || term.pause;
        if (pause) {
            term.delay = true;
        }

        if (!pause && restart) {
            // (Unless paused, skip displaying initial state for better visual.)
            term.delay = true;
            return adjust_step(config.step, strobing(rule));
        } else if (op.p_s) {
            term.delay = true;
            term.pause = true;
            return adjust_step(config.step, strobing(rule));
        } else if (op.p_1) {
            term.delay = true;
            return 1;
        } else if (op.p_f) {
            term.delay = true;
            return adjust_step(step_fast, strobing(rule));
        } else if (!pause && global_timer::test(config.interval) && !std::exchange(term.delay, false)) {
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
    // TODO: add visual for pause state...
    // (Some rules converge fast, then it's not obvious whether the space is paused or not...)

    const bool popup_hidden = hov == rclick_popup::PopupHidden;
    const bool tooltip = !has_group_op && (hovered || popup_hidden) &&
                         imgui_IsItemHoveredForTooltip(popup_hidden ? ImGuiHoveredFlags_AllowWhenBlockedByPopup
                                                                    : ImGuiHoveredFlags_None);
    bool hex_mode = false;
    if (tooltip && ((hex_mode = want_hex_mode(rule)) || config.zoom_ <= 1) && ImGui::IsMousePosValid()) {
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
                ImGui::Image(to_texture(term.tile.data(clamped), scaleE::Nearest), to_imvec(clamped.size() * 3));
            }
            ImGui::EndTooltip();
        }
        ImGui::PopStyleVar();
    }
}
