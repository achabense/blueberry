#pragma once

#include <chrono>
#include <format>
#include <functional>

#include "rule.hpp"

#include "dear_imgui.hpp"

/* Not inline */ static const bool check_version = IMGUI_CHECKVERSION();

#if 0
// Enforce that ordinary string literals are encoded with utf-8.
// This requires certain compiler flags to be set (e.g. `/utf-8` in MSVC).
// Currently not necessary, as the program is not using non-ascii characters.
// (u8"..." is not usable in this project, as it becomes `char8_t[]` since C++20.)
inline void assert_utf8_encoding() {
    constexpr auto a = std::to_array("中文");
    constexpr auto b = std::to_array(u8"中文");

    static_assert(std::equal(a.begin(), a.end(), b.begin(), b.end(), [](auto l, auto r) {
        return static_cast<unsigned char>(l) == static_cast<unsigned char>(r);
    }));
}

// Experience in MSVC
// It turns out that there are still a lot of messy encoding problems even if `/utf-8` is specified.
// For example, how is main's argv / fs::path.string() / exception.what() / ... encoded?
// (Well, are there even guarantees that we can get a valid fs::path from main's argv?)
// To make things easy the program does not try to deal with these strings.
#endif

inline std::mt19937& global_mt19937() {
    static std::mt19937 rand{(uint32_t)time(0)};
    return rand;
}

// Managed by `main`.
bool set_home(const char* u8path = nullptr); // nullptr ~ filesystem::current_path.
void frame_main();

// Managed by `frame_main`.
class sync_point;
void load_file(sync_point&);
void load_clipboard(sync_point&);
void load_doc(sync_point&);
void edit_rule(sync_point&);
void apply_rule(sync_point&);

struct rule_algo {
    static aniso::ruleT trans_reverse(const aniso::ruleT&);
    static bool is_hexagonal_rule(const aniso::ruleT&);
};

// "tile_base.hpp"
namespace aniso::_misc {
    template <class>
    struct tile_ref_;
} // namespace aniso::_misc

// The texture is only valid for the current frame.
enum class scaleE { Nearest, Linear };
[[nodiscard]] ImTextureID make_screen(aniso::_misc::tile_ref_<const bool> tile, scaleE scale);

// ImGui::Image and ImGui::ImageButton for `codeT`.
void code_image(aniso::codeT code, int zoom, const ImVec4& tint_col = ImVec4(1, 1, 1, 1),
                const ImVec4& border_col = ImVec4(0, 0, 0, 0));
bool code_button(aniso::codeT code, int zoom, const ImVec4& bg_col = ImVec4(0, 0, 0, 0),
                 const ImVec4& tint_col = ImVec4(1, 1, 1, 1));

inline const int item_width = 220;

inline ImVec2 square_size() {
    const float r = ImGui::GetFrameHeight();
    return ImVec2(r, r);
}

inline float wrap_len() {
    // The same as the one in `HelpMarker` in "imgui_demo.cpp".
    return ImGui::GetFontSize() * 35.0f;
}

// TODO: consider using ImGui::Shortcut?
// Some features cannot easily be satisfied with `ImGui::Shortcut` and `ImGui::SetNextItemShortcut`.
struct shortcuts {
    static bool global_flag(ImGuiKey key) { //
        return !ImGui::GetIO().WantTextInput && ImGui::IsKeyDown(key);
    }

    static bool keys_avail() { //
        return !ImGui::GetIO().WantCaptureKeyboard && !ImGui::IsAnyItemActive();
    }

    static bool keys_avail_and_window_hoverable() { // Not blocked by popup.
        return keys_avail() && imgui_IsWindowHoverable();
    }

private:
    friend void frame_main();

    inline static ImGuiKey occupied = ImGuiKey_None;
    static void begin_frame() { occupied = ImGuiKey_None; }

    // Resolve shortcut competition when multiple keys are pressed.
    static bool filter(ImGuiKey key) {
        assert(key != ImGuiKey_None);
        if (occupied == ImGuiKey_None) {
            if (ImGui::IsKeyDown(key)) {
                occupied = key;
            }
            return true;
        }
        return occupied == key;
    }

public:
    static bool test(ImGuiKey key, bool repeat = false) { //
        return filter(key) && ImGui::IsKeyPressed(key, repeat);
    }

    static bool highlight(ImGuiID id = 0) {
        if (id == 0) {
            id = ImGui::GetItemID();
        }
        ImGui::NavHighlightActivated(id);
        return true;
    }

    // Should be called after the item.
    static bool item_shortcut(ImGuiKey key, bool repeat = false, std::optional<bool> cond = std::nullopt) {
        if (key != ImGuiKey_None && !imgui_TestItemFlag(ImGuiItemFlags_Disabled)) {
            if (cond.has_value() ? *cond : keys_avail_and_window_hoverable()) { // `value_or` won't short-circuit.
                return test(key, repeat) && highlight();
            }
        }
        return false;
    }

    // ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows | ImGuiFocusedFlags_NoPopupHierarchy)
    static bool window_focused() {
        if (const ImGuiWindow* focused = GImGui->NavWindow) {
            const ImGuiWindow* current = ImGui::GetCurrentWindowRead();
            assert(current);
            return current->RootWindow == focused->RootWindow;
        }
        return false;
    }
};

#if 0
inline void quick_info(std::string_view msg) {
    if (shortcuts::global_flag(ImGuiKey_H) && ImGui::IsItemVisible()) {
        imgui_ItemRect(IM_COL32_WHITE);

        assert(!msg.empty());
        const char *const text_beg = msg.data(), *const text_end = msg.data() + msg.size();
        const ImVec2 padding = ImGui::GetStyle().FramePadding;
        const ImVec2 msg_size = ImGui::CalcTextSize(text_beg, text_end) + padding * 2;
        const ImVec2 msg_min = [&]() -> ImVec2 {
            const ImVec2 min = ImGui::GetItemRectMin(), max = ImGui::GetItemRectMax();
            if (msg[0] == '<') {
                return ImVec2(max.x + ImGui::GetStyle().ItemInnerSpacing.x, min.y);
            } else if (msg[0] == '^') {
                return ImVec2(min.x, max.y + ImGui::GetStyle().ItemSpacing.y);
            } else {
                assert(msg[0] == 'v');
                return ImVec2(min.x, min.y - ImGui::GetStyle().ItemSpacing.y - msg_size.y);
            }
        }();
        const ImVec2 msg_max = msg_min + msg_size;
        // TODO: ideally, the message should be rendered on the foreground of individual windows...
        ImDrawList* const drawlist = ImGui::GetForegroundDrawList();
        drawlist->AddRectFilled(msg_min, msg_max, IM_COL32_GREY(48, 255));
        drawlist->AddRect(msg_min, msg_max, IM_COL32_WHITE);
        drawlist->AddText(msg_min + padding, IM_COL32_WHITE, text_beg, text_end);
    }
}
#endif

class guide_mode {
    inline static bool enable_tooltip = false;

    friend void frame_main();

    static void begin_frame() {
        if (shortcuts::keys_avail() && shortcuts::test(ImGuiKey_H)) {
            enable_tooltip = !enable_tooltip;
        }
    }

    static void _highlight() { //
        imgui_ItemRectFilled(ImGui::GetColorU32(ImGuiCol_PlotHistogram, 0.3f));
    }

public:
    // For use in combination with other tooltips.
    static bool enabled() { return enable_tooltip; }

    static void highlight() {
        if (enable_tooltip) {
            _highlight();
        }
    }

    static void item_tooltip(const std::string_view tooltip) {
        if (enable_tooltip) {
            _highlight();
            imgui_ItemTooltip(tooltip);
        } else {
            imgui_ItemTooltip_StrID = nullptr;
        }
    }
};

// There is intended to be at most one call to this function in each window hierarchy.
inline void set_scroll_by_up_down(float dy) {
    if (shortcuts::keys_avail() && shortcuts::window_focused()) {
        if (shortcuts::test(ImGuiKey_DownArrow, true)) {
            ImGui::SetScrollY(ImGui::GetScrollY() + dy);
            shortcuts::highlight(ImGui::GetWindowScrollbarID(ImGui::GetCurrentWindowRead(), ImGuiAxis_Y));
        } else if (shortcuts::test(ImGuiKey_UpArrow, true)) {
            ImGui::SetScrollY(ImGui::GetScrollY() - dy);
            shortcuts::highlight(ImGui::GetWindowScrollbarID(ImGui::GetCurrentWindowRead(), ImGuiAxis_Y));
        }
    }
}

#if 0
// (Referring to ImGui::BeginPopupContextItem().)
inline bool begin_popup_for_item(bool open, const char* str_id = nullptr) {
    if (GImGui->CurrentWindow->SkipItems) {
        return false;
    }
    const ImGuiID id = str_id ? ImGui::GetID(str_id) : ImGui::GetItemID();
    assert(id != 0);
    if (open) { // Instead of MouseReleased && ItemHovered
        ImGui::OpenPopupEx(id);
    }
    return ImGui::BeginPopupEx(id, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoTitleBar |
                                       ImGuiWindowFlags_NoSavedSettings);
}
#endif

// TODO: rename; `menu` is misleading...
// Looks like a common popup, and will appear like a menu (but with more consistent closing behavior).
// (Not meant to be used recursively; should end with `ImGui::EndPopup` instead of `EndMenu`.)
inline bool begin_menu_for_item() {
    const ImRect item_rect = imgui_GetItemRect();
    const ImGuiID item_id = ImGui::GetItemID();
    assert(item_id != 0); // Mainly designed for buttons.

    if (!ImGui::IsPopupOpen(item_id, 0) && imgui_IsItemOrNoneActive() && imgui_ItemHoveredForTooltip()) {
        ImGui::OpenPopupEx(item_id, ImGuiPopupFlags_NoReopen);
        assert(ImGui::IsPopupOpen(item_id, 0));
        ImGui::SetNextWindowPos(item_rect.GetTR(), ImGuiCond_Appearing); // Like a menu.
    }

    if (ImGui::BeginPopupEx(item_id, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoTitleBar |
                                         ImGuiWindowFlags_NoSavedSettings)) {
        ImGui::NavHighlightActivated(item_id);

        const ImVec2 mouse_pos = ImGui::GetMousePos();
        const auto window_rect = imgui_GetWindowRect();
        if (!window_rect.Contains(mouse_pos) && item_rect.Contains(mouse_pos)) {
#ifndef NDEBUG
            // Avoid closing the popup when the item is clicked; relying on the impl details of this function:
            (void)ImGui::UpdateMouseMovingWindowEndFrame;
            // Initially I tried to use modal popup to avoid the closing behavior, but that caused much more
            // trouble than it solved :|
#endif
            GImGui->IO.MouseClicked[0] = GImGui->IO.MouseClicked[1] = false;
        } else if (const ImVec2 pad = square_size(); !ImGui::IsAnyItemActive() &&
                                                     !item_rect.ContainsWithPad(mouse_pos, pad * 1.5) &&
                                                     !window_rect.ContainsWithPad(mouse_pos, pad * 2.5)) {
            ImGui::CloseCurrentPopup();
        }
        return true;
    }
    return false;
}

// Looks like `ImGui::Selectable` but behaves like a button (not designed for tables).
// (`menu_shortcut` is a workaround to mimic `MenuItem` in the range-ops window. Ideally, that window
// should be redesigned.)
inline bool imgui_SelectableStyledButton(const char* label, const bool selected = false,
                                         const char* menu_shortcut = nullptr) {
    assert(!GImGui->CurrentWindow->DC.IsSameLine);
    if (GImGui->CurrentWindow->SkipItems) {
        return false;
    }

    if (!selected) {
        ImGui::PushStyleColor(ImGuiCol_Button, IM_COL32_BLACK_TRANS);
    }
    static ImGuiID prev_id = 0;
    if (prev_id != 0 && prev_id == ImGui::GetItemID()) {
        // As if the last call used `ImGui::PushStyleVarY(ImGuiStyleVar_ItemSpacing, 0)`.
        // (PushStyleVar-ItemSpacing affects the spacing to the next item. See `ImGui::ItemSize` for details.)
        imgui_AddCursorPosY(-ImGui::GetStyle().ItemSpacing.y);
    }

    const float frame_padding_y = 2;
    ImGui::PushStyleVar(ImGuiStyleVar_ButtonTextAlign, {0, 0});
    ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, {0, frame_padding_y});

    bool ret = false;
    if (!menu_shortcut) {
        const ImVec2 label_size = ImGui::CalcTextSize(label, nullptr, true);
        const ImVec2 button_size = {std::max(ImGui::GetContentRegionAvail().x, label_size.x),
                                    label_size.y + 2 * frame_padding_y};
        ret = ImGui::Button(label, button_size);
    } else {
        // The label should fit in {ImGui::CalcItemWidth(), ImGui::GetFrameHeight()}. Not checked.
        const float w_before_shortcut = ImGui::CalcItemWidth() + imgui_ItemInnerSpacingX();
        const ImVec2 shortcut_size = ImGui::CalcTextSize(menu_shortcut);
        assert(shortcut_size.y == ImGui::GetFontSize()); // Single-line.
        const ImVec2 button_size = {std::max(ImGui::GetContentRegionAvail().x, w_before_shortcut + shortcut_size.x),
                                    ImGui::GetFrameHeight()};
        ret = ImGui::Button(label, button_size);
        const ImVec2 min = ImGui::GetItemRectMin();
        ImGui::GetWindowDrawList()->AddText({min.x + w_before_shortcut, min.y + frame_padding_y},
                                            ImGui::GetColorU32(ImGuiCol_TextDisabled), menu_shortcut);
    }

    ImGui::PopStyleVar(2);
    prev_id = ImGui::GetItemID();
    if (!selected) {
        ImGui::PopStyleColor();
    }

    return ret;
}

class sequence {
    enum tagE { None, First, Prev, Next, Last };

    // Workaround to avoid the current rule being changed in override mode.
    static bool extra_cond() { return !ImGui::IsKeyDown(ImGuiKey_Z) && !ImGui::IsKeyDown(ImGuiKey_X); }

    inline static ImGuiID bound_id = 0;
    inline static bool keep_bound_id = false;
    friend void frame_main();
    static void begin_frame() {
        if (!std::exchange(keep_bound_id, false)) {
            bound_id = 0;
        }
    }

    // (`disable` is a workaround for a sequence in `edit_rule`...)
    static tagE seq(const char* label_first, const char* label_prev, const char* label_next, const char* label_last,
                    const char* const disable) {
        tagE tag = None;

        if (ImGui::Button(label_first)) {
            tag = First;
        }
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());

        if (disable) {
            ImGui::BeginDisabled();
            ImGui::BeginGroup();
        }

        const ImGuiID id_prev = ImGui::GetID(label_prev);
        assert(id_prev != 0);

        const bool window_focused = shortcuts::window_focused();
        const bool pair_disabled = imgui_TestItemFlag(ImGuiItemFlags_Disabled);
        // The binding will be preserved if the window is blocked by its popups.
        // (Note: popups from other windows will still disable the binding.)
        const bool shortcut_avail =
            bound_id == id_prev && !pair_disabled &&
            ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows /*Including popup hierarchy*/);
        if (shortcut_avail) { // Otherwise, `bound_id` will become 0 at next frame.
            keep_bound_id = true;
        }
        auto button_with_shortcut = [shortcut_avail, window_focused](const char* label, ImGuiKey shortcut) {
            bool ret = ImGui::Button(label);
            if (shortcut_avail) {
                imgui_ItemRect(ImGui::GetColorU32(ImGuiCol_ButtonActive));
                if (!ret && window_focused && extra_cond() && shortcuts::item_shortcut(shortcut)) {
                    ret = true;
                }
            }
            return ret;
        };

        if (button_with_shortcut(label_prev, ImGuiKey_LeftArrow)) {
            tag = Prev;
        }
        ImGui::SameLine(0, 0), imgui_Str("/"), ImGui::SameLine(0, 0);
        if (button_with_shortcut(label_next, ImGuiKey_RightArrow)) {
            tag = Next;
        }
        if (disable) {
            ImGui::EndGroup();
            ImGui::EndDisabled();
            if (!imgui_TestItemFlag(ImGuiItemFlags_Disabled)) {
                imgui_ItemTooltip(disable);
            }
        }

        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        if (ImGui::Button(label_last)) {
            tag = Last;
        }

        if ((tag != None) ||
            (bound_id == 0 && window_focused && !pair_disabled && shortcuts::keys_avail() && extra_cond() &&
             (shortcuts::test(ImGuiKey_LeftArrow) || shortcuts::test(ImGuiKey_RightArrow)))) {
            bound_id = id_prev;
            keep_bound_id = true;
        }

        return tag;
    }

public:
    sequence() = delete;

    static void seq(const char* label_first, const char* label_prev, const char* label_next, const char* label_last,
                    const auto& act_first, const auto& act_prev, const auto& act_next, const auto& act_last,
                    const char* disable = nullptr) {
        switch (seq(label_first, label_prev, label_next, label_last, disable)) {
            case None: return;
            case First: act_first(); return;
            case Prev: act_prev(); return;
            case Next: act_next(); return;
            case Last: act_last(); return;
        }
    }
};

class imgui_StepSliderShortcuts {
    friend bool imgui_StepSliderInt(const char* label, int* v, int v_min, int v_max, const char* format);

    inline static ImGuiKey minus = ImGuiKey_None;
    inline static ImGuiKey plus = ImGuiKey_None;
    inline static std::optional<bool> cond = std::nullopt; // Shared by both buttons.

public:
    static void reset() {
        minus = plus = ImGuiKey_None;
        cond.reset();
    }

    static void set(ImGuiKey m, ImGuiKey p, std::optional<bool> c = std::nullopt) {
        minus = m;
        plus = p;
        cond = c;
    }
};

// (Referring to ImGui::InputScalar; moved here to support shortcuts.)
inline bool imgui_StepSliderInt(const char* label, int* v, int v_min, int v_max, const char* format = "%d") {
    if (GImGui->CurrentWindow->SkipItems) {
        return false;
    }

    int v2 = *v;

    const float r = ImGui::GetFrameHeight();
    const float s = imgui_ItemInnerSpacingX();
    ImGui::BeginGroup();
    ImGui::PushID(label);
    ImGui::SetNextItemWidth(std::max(1.0f, ImGui::CalcItemWidth() - 2 * (r + s)));
    ImGui::SliderInt("", &v2, v_min, v_max, format, ImGuiSliderFlags_NoInput);
    ImGui::PushItemFlag(ImGuiItemFlags_ButtonRepeat, true);
    ImGui::SameLine(0, s);
    // (`InputScalar` makes .FramePadding.x = y for these buttons, not added here.)
    if (ImGui::Button("-", ImVec2(r, r)) ||
        shortcuts::item_shortcut(imgui_StepSliderShortcuts::minus, true, imgui_StepSliderShortcuts::cond)) {
        --v2;
    }
    ImGui::SameLine(0, s);
    if (ImGui::Button("+", ImVec2(r, r)) ||
        shortcuts::item_shortcut(imgui_StepSliderShortcuts::plus, true, imgui_StepSliderShortcuts::cond)) {
        ++v2;
    }
    ImGui::PopItemFlag(); // ImGuiItemFlags_ButtonRepeat
    const char* label_end = ImGui::FindRenderedTextEnd(label);
    if (label != label_end) {
        ImGui::SameLine(0, s);
        imgui_Str(std::string_view(label, label_end));
    }
    ImGui::PopID();
    ImGui::EndGroup();

    v2 = std::clamp(v2, v_min, v_max);
    const bool changed = *v != v2;
    *v = v2;
    return changed;
}

class messenger {
    class messageT {
        std::string m_str{};
        std::optional<ImVec2> m_min{};
        int m_count{};

        using clockT = std::chrono::steady_clock;
        clockT::time_point m_time{};

    public:
        // (Defined as a workaround for gcc building.)
        // (Related: https://stackoverflow.com/questions/53408962)
        messageT() : m_str{}, m_min{}, m_count{}, m_time{} {}

        void set(std::string&& str) {
            m_min.reset();

            size_t subsize = 0;
            for (int line = 0; line < 20; ++line) {
                subsize = str.find_first_of('\n', subsize);
                if (subsize == str.npos) {
                    break;
                } else {
                    ++subsize; // Include the '\n'.
                }
            }

            if (subsize >= str.size()) {
                m_str = std::move(str);
            } else {
                m_str = str.substr(0, subsize) + ".....";
            }
        }

        // Won't interfere with normal tooltips or popups.
        void display() {
            if (m_str.empty()) {
                return;
            } else if (m_min) {
                if (const ImVec2 delta = ImGui::GetIO().MouseDelta; delta.x || delta.y) {
                    --m_count;
                }
                const bool t_expired = clockT::now() > m_time;
                const bool c_expired = m_count < 0;
                // TODO: ideally the callers of `set_msg` should be able to specify quitting cond.
                if (m_str.size() < 10 ? (c_expired || t_expired) : (c_expired && t_expired)) {
                    m_str.clear();
                    return;
                }
            }

            assert(!m_str.empty());
            const float text_wrap = wrap_len();
            const char *const text_beg = m_str.c_str(), *const text_end = m_str.c_str() + m_str.size();
            const ImVec2 padding = ImGui::GetStyle().WindowPadding;
            const ImVec2 window_size = ImGui::CalcTextSize(text_beg, text_end, false, text_wrap) + padding * 2;

            if (!m_min) {
                m_count = 10;
                m_time = clockT::now() + std::chrono::milliseconds(500);

                const ImVec2 main_size = ImGui::GetMainViewport()->Size;
                if (ImGui::IsMousePosValid()) {
                    const ImVec2 pos = ImClamp(ImGui::GetMousePos(), ImVec2{0, 0}, main_size);
                    ImGuiDir dir = ImGuiDir_None;
                    m_min = ImGui::FindBestWindowPosForPopupEx(pos, window_size, &dir, {padding, main_size - padding},
                                                               {pos - padding, pos + padding},
                                                               ImGuiPopupPositionPolicy_Default);
                } else {
                    m_min = ImFloor(main_size / 2 - window_size / 2);
                }
            }

            const ImVec2 window_min = *m_min;
            const ImVec2 window_max = window_min + window_size;
            ImDrawList* const drawlist = ImGui::GetForegroundDrawList();
            drawlist->AddRectFilled(window_min, window_max, ImGui::GetColorU32(ImGuiCol_PopupBg));
            drawlist->AddRect(window_min, window_max, ImGui::GetColorU32(ImGuiCol_Border));
            drawlist->AddText(nullptr, 0.0f, window_min + padding, ImGui::GetColorU32(ImGuiCol_Text), text_beg,
                              text_end, text_wrap);
        }
    };

    inline static messageT m_msg;

public:
    messenger() = delete;

    static void set_msg(std::string str) { m_msg.set(std::move(str)); }

    template <class... U>
    static void set_msg(std::format_string<const U&...> fmt, const U&... args) {
        m_msg.set(std::format(fmt, args...));
    }

    // Managed by `frame_main`.
    static void display() { m_msg.display(); }
};

// Preview rules.
class previewer {
public:
    previewer() = delete;

    // TODO: should be more flexible...
    class configT {
    public:
        enum sizeE : int { _160_160 = 0, _220_160, _220_220, _280_220, Count };

    private:
        struct termT {
            int w, h;
            const char* str;
        };
        static constexpr termT size_terms[Count]{
            {160, 160, "160*160"}, {220, 160, "220*160"}, {220, 220, "220*220"}, {280, 220, "280*220"}};

        friend previewer;
        sizeE size; // Cannot be `Count`.
        int seed;
        int step;
        void _set();

    public:
        /*implicit*/ configT(sizeE size) : size(size), seed(0), step(1) { assert(size >= 0 && size < Count); }

        int width() const { return size_terms[size].w; }
        int height() const { return size_terms[size].h; }
        ImVec2 size_imvec() const { return ImVec2(size_terms[size].w, size_terms[size].h); }

        void set(const char* label) {
            ImGui::Button(label);
            if (begin_menu_for_item()) {
                _set();
                ImGui::EndPopup();
            }
        }
    };

    static void dummy(const configT& config, const char* str = "--") {
        ImGui::Dummy(config.size_imvec());
        if (ImGui::IsItemVisible()) {
            imgui_ItemRectFilled(IM_COL32_BLACK);
            imgui_ItemRect(ImGui::GetColorU32(ImGuiCol_TableBorderStrong)); // Instead of `ImGuiCol_Border`

            if (str && *str != '\0') {
                imgui_ItemStr(ImGui::GetColorU32(ImGuiCol_TextDisabled), str);
            }
        }
    }

    static void preview(uint32_t id, const configT& config, const aniso::ruleT& rule, bool interactive = true) {
        ImGui::Dummy(config.size_imvec());
        if (ImGui::IsItemVisible()) {
            ImU32 border_col = ImGui::GetColorU32(ImGuiCol_TableBorderStrong);
            _preview((uint64_t(ImGui::GetID("")) << 32) | id, config, rule, interactive, border_col);
            imgui_ItemRect(border_col);
        }
    }

    static void preview(uint32_t id, const configT& config, const std::invocable<> auto& get_rule,
                        bool interactive = true) {
        ImGui::Dummy(config.size_imvec());
        if (ImGui::IsItemVisible()) {
            ImU32 border_col = ImGui::GetColorU32(ImGuiCol_TableBorderStrong);
            _preview((uint64_t(ImGui::GetID("")) << 32) | id, config, get_rule(), interactive, border_col);
            imgui_ItemRect(border_col);
        }
    }

private:
    enum class clearE { None, InActive, All };
    inline static clearE _preview_clear = clearE::All;

    friend void frame_main();
    static void begin_frame() {
        using enum clearE;
        _preview_clear = _preview_clear == None ? InActive : All;
    }

    static void _preview(uint64_t id, const configT& config, const aniso::ruleT& rule, bool interactive,
                         ImU32& border_col);
};

class sync_point {
    friend void frame_main();

    std::optional<aniso::ruleT> out_rule = std::nullopt;

    sync_point(const aniso::ruleT& rule) : rule{rule} {}

public:
    sync_point(const sync_point&) = delete;
    sync_point& operator=(const sync_point&) = delete;

    const aniso::ruleT rule;

    void set(const aniso::ruleT& rule) { out_rule.emplace(rule); }
};

// TODO: ideally this should attach to a `sync_point` object.
// (Would require previewer::preview to take `sync_point` parameter.)
// However, as there is only one `sync_point` in the program, it makes no actual difference for now.
struct sync_point_override {
    inline static aniso::ruleT rule{};
    inline static bool want_test_set = false;
    inline static bool want_test_run = false;

    static bool set(const aniso::ruleT& r, bool test_set, bool test_run) {
        want_test_set_next = test_set;
        want_test_run_next = test_run;
        if (want_test_set_next || want_test_run_next) {
            rule_next = r;
            return true;
        }
        return false;
    }

    static bool begin_frame(const aniso::ruleT* clear_if_same = nullptr) {
        bool same = false;
        if (clear_if_same && (want_test_set_next || want_test_run_next) && *clear_if_same == rule_next) {
            want_test_set_next = false;
            want_test_run_next = false;
            same = true;
        }

        want_test_set = std::exchange(want_test_set_next, false);
        want_test_run = std::exchange(want_test_run_next, false);
        if (want_test_set || want_test_run) {
            rule = rule_next;
        }
        return same;
    }

private:
    inline static aniso::ruleT rule_next{};
    inline static bool want_test_set_next = false;
    inline static bool want_test_run_next = false;
};

inline void set_clipboard_and_notify(const char* c_str) {
    ImGui::SetClipboardText(c_str);
    messenger::set_msg("Copied.");
}

inline void set_clipboard_and_notify(const std::string& str) { //
    set_clipboard_and_notify(str.c_str());
}

inline void set_msg_cleared(bool has_effect) {
    // if (has_effect) {
    messenger::set_msg("Cleared.");
    // }
}

// It's not obvious whether `ImGui::GetClipboardText` can return nullptr, and
// in some cases the function returns empty string "" for errors...
inline std::string_view read_clipboard() {
    const char* str = ImGui::GetClipboardText();
    if (!str || *str == '\0') {
        messenger::set_msg("Failed to read from the clipboard.");
        return {};
    }
    return str;
}

class global_timer {
    static constexpr int time_unit = 25;                // ms.
    static constexpr int min_time = 0, max_time = 1000; // ms.
    static_assert(max_time % time_unit == 0);

    using clockT = std::chrono::steady_clock;
    struct termT {
        clockT::time_point last;   // = {};
        bool active_at_this_frame; // = false; (Will cause trouble when building with gcc or clang...)
    };
    inline static termT terms[1 + (max_time / time_unit)]{};

    // TODO: what's the most suitable place to call this?
    friend void frame_main();
    static void begin_frame() {
        const clockT::time_point now = clockT::now();
        for (int i = 0; i < std::size(terms); ++i) {
            const int dur = i * time_unit;
            if (terms[i].last + std::chrono::milliseconds(dur) <= now) {
                terms[i].last = now;
                terms[i].active_at_this_frame = true;
            } else {
                terms[i].active_at_this_frame = false;
            }
        }
    }

public:
    // 0: will return true every frame.
    static constexpr int min_nonzero_interval = time_unit;

    // `timerT` with the same interval will always be activated at the same frame.
    struct timerT {
        int i; // terms[i].

    public:
        bool test() const { return terms[i].active_at_this_frame; }

        timerT(int ms) {
            assert(min_time <= ms && ms <= max_time);
            assert(ms % time_unit == 0);
            i = std::clamp(ms, min_time, max_time) / time_unit;
        }

        void slide_interval(const char* label, int min_ms, int max_ms) {
            assert(min_time <= min_ms && min_ms <= max_ms && max_ms <= max_time);
            assert(min_ms % time_unit == 0);
            assert(max_ms % time_unit == 0);
            imgui_StepSliderInt(label, &i, min_ms / time_unit, max_ms / time_unit,
                                std::format("{} ms", i * time_unit).c_str());
        }
    };
};

// TODO: the name is too casual but I cannot think of a very suitable one...
class input_int {
    char buf[6]{}; // 5 digits.

public:
    std::optional<int> flush() {
        if (buf[0] != '\0') {
            int v = 0;
            const bool has_val = std::from_chars(buf, std::end(buf), v).ec == std::errc{};
            buf[0] = '\0';
            if (has_val) {
                return v;
            }
        }
        return std::nullopt;
    }

    std::optional<int> input(const char* label, const char* hint = nullptr) {
        constexpr auto input_flags = ImGuiInputTextFlags_CallbackCharFilter | ImGuiInputTextFlags_EnterReturnsTrue;
        constexpr auto input_filter = [](ImGuiInputTextCallbackData* data) -> int {
            return (data->EventChar >= '0' && data->EventChar <= '9') ? 0 : 1;
        };

        if (hint ? ImGui::InputTextWithHint(label, hint, buf, std::size(buf), input_flags, input_filter)
                 : ImGui::InputText(label, buf, std::size(buf), input_flags, input_filter)) {
            return flush();
        }
        return std::nullopt;
    }
};
