#pragma once

#include <chrono>
#include <format>

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

    static_assert(std::ranges::equal(
        a, b, [](auto l, auto r) { return static_cast<unsigned char>(l) == static_cast<unsigned char>(r); }));
}

// Experience in MSVC
// It turns out that there are still a lot of messy encoding problems even if `/utf-8` is specified.
// For example, how is main's argv / fs::path.string() / exception.what() / ... encoded?
// (Well, are there even guarantees that we can get a valid fs::path from main's argv?)
// To make things easy the program does not try to deal with these strings.
#endif

struct [[nodiscard]] open_state {
    const bool open;
    bool closed() const { return !open; }
    void reset_if_closed(bool& flag) const {
        if (!open) {
            flag = false;
        }
    }
};

// Managed by `main`.
void frame_main();

class frame_main_token : no_copy {
    friend void frame_main();
    /*implicit*/ frame_main_token() = default;
};

open_state load_file(ImVec2 init_pos, frame_main_token);
open_state load_clipboard(ImVec2, bool paste, frame_main_token);
open_state load_doc(ImVec2, frame_main_token);

void edit_rule(frame_main_token);
void apply_rule(frame_main_token);

class rand_source : no_create {
    static uint32_t seed() { return time(0); }

public:
    static std::mt19937 create() { return std::mt19937{seed()}; }
    static void perturb(std::mt19937& rand) { rand.discard(1 + (seed() % 16)); }
};

class rule_algo : no_create {
public:
    // static aniso::ruleT trans_reverse(const aniso::ruleT&);
    static bool is_hexagonal_rule(const aniso::ruleT&);
};

// "tile_base.hpp"
namespace aniso::_misc {
    template <class>
    struct tile_ref_;
} // namespace aniso::_misc

inline namespace backend_fn {
    // The texture is only valid for the current frame.
    enum class scaleE { Nearest, Linear };
    ImTextureID to_texture(aniso::_misc::tile_ref_<const aniso::cellT> tile, scaleE scale);

    // ImGui::Image and ImGui::ImageButton for `codeT`.
    void code_image(aniso::codeT code, int zoom, const ImVec4& tint_col = ImVec4(1, 1, 1, 1),
                    const ImVec4& border_col = ImVec4(0, 0, 0, 0));
    bool code_button(aniso::codeT code, int zoom, const ImVec4& bg_col = ImVec4(0, 0, 0, 0),
                     const ImVec4& tint_col = ImVec4(1, 1, 1, 1));

    std::string home_path_utf8(); // "." ~ current path, empty ~ unavailable.
} // namespace backend_fn

inline const int item_width = 220;

inline ImVec2 square_size() {
    const float r = ImGui::GetFrameHeight();
    return ImVec2(r, r);
}

inline float wrap_len() {
    // The same as the one in `HelpMarker` in "imgui_demo.cpp".
    return ImGui::GetFontSize() * 35.0f;
}

inline ImVec2 clamp_window_pos(const ImVec2 pos, const ImVec2 size) {
    const ImVec2 padding = ImGui::GetStyle().WindowPadding;
    const ImVec2 min = padding;
    const ImVec2 max = ImGui::GetMainViewport()->Size - padding - size;
    return (min.x < max.x && min.y < max.y) ? ImClamp(pos, min, max) : pos;
}

// TODO: should finally be configurable in the program.
// inline const bool init_maximize_window = false;
inline const bool init_zero_interval = false;
inline const bool init_show_intro = true;
inline const bool init_compact_mode = false;

// TODO: consider using ImGui::Shortcut?
// Some features cannot easily be satisfied with `ImGui::Shortcut` and `ImGui::SetNextItemShortcut`.
class shortcuts : no_create {
public:
    static bool ctrl() { return GImGui->IO.KeyCtrl; }
    static bool no_ctrl() { return !GImGui->IO.KeyCtrl; }

    static bool global_flag(ImGuiKey key) { //
        return !GImGui->IO.WantTextInput && ImGui::IsKeyDown(key);
    }

    static bool keys_avail() {
        // return !ImGui::GetIO().WantCaptureKeyboard && !ImGui::IsAnyItemActive();
        return !GImGui->IO.WantCaptureKeyboard && !GImGui->ActiveId;
    }

    static bool keys_avail_and_no_ctrl() { return keys_avail() && no_ctrl(); }
    static bool keys_avail_and_window_hoverable() { // Not blocked by popup.
        return keys_avail() && imgui_IsWindowHoverable();
    }

private:
    inline static ImGuiKey occupied = ImGuiKey_None;

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
    static void begin_frame(frame_main_token) { occupied = ImGuiKey_None; }

    static bool test_pressed(ImGuiKey key, bool repeat = false) { //
        return filter(key) && ImGui::IsKeyPressed(key, repeat);
    }

    static bool test_down(ImGuiKey key) { //
        return filter(key) && ImGui::IsKeyDown(key);
    }

    static bool highlight(ImGuiID id = 0) {
        ImGui::NavHighlightActivated(id ? id : ImGui::GetItemID());
        return true;
    }
};

class guide_mode : no_create {
    inline static bool enable_tooltip = false;

public:
    static void flip_enable(frame_main_token) { enable_tooltip = !enable_tooltip; }

    static bool item_tooltip(const std::string_view tooltip) {
        if (enable_tooltip) {
            imgui_ItemRectFilled(ImGui::GetColorU32(ImGuiCol_PlotHistogram, 0.3f));
            return imgui_ItemTooltip(tooltip);
        } else {
            return false;
        }
    }
};

// Prevent window scrolling for one frame.
inline void lock_scroll() {
    // ImGui::SetKeyOwner(ImGuiKey_MouseWheelY, ImGuiKeyOwner_NoOwner, ImGuiInputFlags_LockThisFrame); Doesn't work.
    ImGui::SetKeyOwner(ImGuiKey_MouseWheelY, ImGuiKeyOwner_Any, ImGuiInputFlags_LockThisFrame);
    // After this call: LockThisFrame ~ true, OwnerCur ~ Any, OwnerNext ~ Any
    // Then in the next frame, when calling `NewFrame`:
    // In `UpdateKeyboardInputs`: LockThisFrame ~ false, OwnerCurr <- OwnerNext (Any).
    // Then in `UpdateMouseWheel`, `TestKeyOwner` requires !LockThisFrame && OwnerCurr == NoOwner.
    // So it's `Any != NoOwner` that prevents window scrolling.
}

inline bool may_scroll() { return ImGui::TestKeyOwner(ImGuiKey_MouseWheelY, ImGuiKeyOwner_NoOwner); }

// TODO: whether to support this?
// There is intended to be at most one call to this function in each window hierarchy.
[[deprecated]] inline void set_scroll_by_up_down(float dy) {
    if (shortcuts::no_ctrl() && may_scroll() && shortcuts::keys_avail() && imgui_IsWindowFocused()) {
        if (shortcuts::test_pressed(ImGuiKey_DownArrow, true)) {
            ImGui::SetScrollY(ImGui::GetScrollY() + dy);
            shortcuts::highlight(ImGui::GetWindowScrollbarID(GImGui->CurrentWindow, ImGuiAxis_Y));
        } else if (shortcuts::test_pressed(ImGuiKey_UpArrow, true)) {
            ImGui::SetScrollY(ImGui::GetScrollY() - dy);
            shortcuts::highlight(ImGui::GetWindowScrollbarID(GImGui->CurrentWindow, ImGuiAxis_Y));
        }
    }
}

[[deprecated]] inline void global_tooltip(const bool highlight, const func_ref<void()> func) {
    // TODO: are there simpler ways to prevent inheriting styles?
    // const ImGuiStyle old_style = GImGui->Style;
    // auto old_stack = GImGui->StyleVarStack;
    // ImGui::PopStyleVar(GImGui->StyleVarStack.size()); // Restore style vars.

    if (highlight) {
        ImGui::PushStyleColor(ImGuiCol_Border, IM_COL32(0, 128, 255, 255));
    }
    imgui_CenterNextWindow(ImGuiCond_Always);
    // Multiple tooltips: https://github.com/ocornut/imgui/issues/1345
    if (auto tooltip = imgui_Window("global_tooltip", nullptr,
                                    ImGuiWindowFlags_Tooltip | ImGuiWindowFlags_NoInputs | ImGuiWindowFlags_NoTitleBar |
                                        ImGuiWindowFlags_NoMove | ImGuiWindowFlags_NoResize |
                                        ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_AlwaysAutoResize)) {
        if (ImGui::IsWindowAppearing() ||
            ((ImGui::GetFrameCount() % 32) == 0) /*As normal tooltips may appear after this*/) {
            ImGui::BringWindowToDisplayFront(ImGui::GetCurrentWindow());
        }
        func();
        if (highlight) {
            const auto [min, max] = imgui_GetWindowRect();
            ImGui::GetWindowDrawList()->AddRectFilled(min, max, IM_COL32(0, 128, 255, 16));
        }
    }
    if (highlight) {
        ImGui::PopStyleColor();
    }

    // GImGui->Style = old_style;
    // GImGui->StyleVarStack.swap(old_stack);
}

// Looks like a common popup, and will appear like a menu (but with more consistent closing behavior).
// (Can be called recursively.)
class menu_like_popup : no_create {
    inline static ImGuiID expected_id = 0;

public:
    // (There seems no good way to apply the style change retroactively in `popup()`.)
    // (Used to be `NavHighlightActivated`, but that's problematic in some cases.)
    static void button(const char* label, bool small = false) {
        ImGui::PushStyleColor(ImGuiCol_ButtonActive, ImGui::GetStyleColorVec4(ImGuiCol_ButtonHovered));
        const bool is_open = ImGui::IsPopupOpen((expected_id = ImGui::GetID(label)), 0);
        if (is_open) {
            ImGui::PushStyleColor(ImGuiCol_Button, ImGui::GetStyleColorVec4(ImGuiCol_ButtonHovered));
        }
        small ? ImGui::SmallButton(label) : ImGui::Button(label);
        if (is_open) {
            ImGui::PopStyleColor();
        }
        ImGui::PopStyleColor();
    }

    static void small_button(const char* label) { button(label, true); }

    static void popup(const func_ref<void()> fn) {
        const ImRect item_rect = imgui_GetItemRect();
        const ImGuiID item_id = ImGui::GetItemID();
        assert(item_id != 0 && item_id == expected_id); // Must follow `(small_)button()`.

        if (!ImGui::IsPopupOpen(item_id, 0) && imgui_IsItemOrNoneActive() && imgui_IsItemHoveredForTooltip()) {
            ImGui::OpenPopupEx(item_id, ImGuiPopupFlags_NoReopen);
            assert(ImGui::IsPopupOpen(item_id, 0));
            ImGui::SetNextWindowPos(item_rect.GetTR(), ImGuiCond_Appearing); // Like a menu.
        }

        if (ImGui::BeginPopupEx(item_id, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoTitleBar |
                                             ImGuiWindowFlags_NoSavedSettings)) {
            if (imgui_IsWindowHoverable()) { // Topmost popup.
                const ImVec2 mouse_pos = ImGui::GetMousePos();
                const auto window_rect = imgui_GetWindowRect();
                if (!window_rect.Contains(mouse_pos)) {
                    // Disable mouse scrolling in other windows.
                    lock_scroll();
                }

                if (!window_rect.Contains(mouse_pos) && item_rect.Contains(mouse_pos)) {
                    // Avoid closing the popup when the item is clicked; relying on the impl details of this function:
                    (void)&ImGui::UpdateMouseMovingWindowEndFrame;
                    // Initially I tried to use modal popup to avoid the closing behavior, but that caused much more
                    // trouble than it solved :|

                    GImGui->IO.MouseClicked[0] = GImGui->IO.MouseClicked[1] = false;
                } else if (const ImVec2 pad = square_size(); !ImGui::IsAnyItemActive() &&
                                                             !item_rect.ContainsWithPad(mouse_pos, pad * 1.5) &&
                                                             !window_rect.ContainsWithPad(mouse_pos, pad * 2.5)) {
                    ImGui::CloseCurrentPopup();
                } else if (!item_rect.Contains(mouse_pos) && test_esc()) {
                    ImGui::CloseCurrentPopup();
                }
            }

            fn();
            ImGui::EndPopup();
        }
    }
};

struct id_pair {
    ImGuiID id0 = 0, id1 = 0;

    explicit id_pair() = default;
    /*implicit*/ id_pair(ImGuiID i, ImGuiID j = 1) : id0(i), id1(j) {}
    /*implicit*/ id_pair(const char* s, ImGuiID j = 1) : id0(ImGui::GetID(s)), id1(j) {}

    explicit operator bool() const { return id0 || id1; }
    friend bool operator==(const id_pair&, const id_pair&) = default;
};

class rclick_popup : no_create {
    inline static id_pair bound_id{}, bound_id_next{};
    inline static bool in_popup = false;

public:
    static void begin_frame(frame_main_token) {
        bound_id = std::exchange(bound_id_next, id_pair{});
        assert(!in_popup);
    }

    // Popups are hidden at the first frame due to auto-resize, but will still block items.
    enum class hoverE { None, Hovered, PopupInvisible, PopupVisible };
    using enum hoverE;

    [[nodiscard]] static hoverE popup_no_highlight(const id_pair id, const func_ref<void()> fn) {
        // assert(!in_popup); (Too strict; ok as long as never hovered (e.g. in tooltip).)
        const bool hovered = ImGui::IsItemHovered();
        if (!hovered && id != bound_id) {
            return None;
        }

        assert(!in_popup); // Cannot open recursively.
        hoverE hov = Hovered;

        // (Avoid creating windows for one-off usage.)
        constexpr const char* shared_popup = "Shared-popup";
        assert_implies(!bound_id, !ImGui::IsPopupOpen(shared_popup));
        // (This doesn't break assertion, which means skipping `BeginPopup` is able to close the popup as well.)
        // if (ImGui::IsKeyDown(ImGuiKey_Q)) {
        //     return;
        // }

        if (!bound_id && hovered && !ImGui::IsAnyItemActive() && ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
            ImGui::OpenPopup(shared_popup);
            bound_id = id;
        }

        if (bound_id == id) {
            if (ImGui::BeginPopup(shared_popup)) {
                bound_id_next = id;
                hov = ImGui::IsWindowAppearing() ? PopupInvisible : PopupVisible;
                assert_implies(hov == PopupInvisible, GImGui->CurrentWindow->Hidden);

                lock_scroll();
                in_popup = true;
                fn();
                in_popup = false;
                if (test_esc()) {
                    ImGui::CloseCurrentPopup();
                }
                ImGui::EndPopup();
            }
            // else: `bound_id` will become {} next frame.
        } else {
            // To respect ImGui::SetNextWindow... calls.
            GImGui->NextWindowData.ClearFlags();
        }

        return hov;
    }

    static ImU32 highlight_col(const bool bright) {
        return ImGui::GetColorU32(bright ? ImGuiCol_Text : ImGuiCol_TextDisabled);
    }

    static hoverE popup(const id_pair id, const func_ref<void()> fn) {
        const hoverE hov = popup_no_highlight(id, fn);
        if (hov != None) {
            imgui_ItemUnderline(highlight_col(hov == PopupVisible));
        }
        return hov;
    }

    static hoverE popup2(const func_ref<void()> fn) {
        const ImGuiID id = ImGui::GetItemID();
        assert(id);
        const hoverE hov = popup_no_highlight(id, fn);
        if (hov == PopupInvisible || hov == PopupVisible) {
            shortcuts::highlight(id);
        }
        return hov;
    }
};

class item_timer {
    ImGuiID id = 0;
    double due = 0;

public:
    void bind(double sec = 0.1) {
        id = imgui_GetItemIDNonZero();
        due = ImGui::GetTime() + sec;
    }

    bool test() {
        if (!id) {
            return false;
        }
        if (ImGui::GetTime() < due) {
            return id == imgui_GetItemIDNonZero();
        } else {
            id = 0;
            return false;
        }
    }
};

inline bool double_click_button_small(const char* label) {
    for (const auto col : {ImGuiCol_Button, ImGuiCol_ButtonActive, ImGuiCol_ButtonHovered}) {
        ImGui::PushStyleColor(col, ImLerp(ImGui::GetStyleColorVec4(col), ImVec4(1, 0, 0, 1), 0.2f));
    }
    ImGui::SmallButton(label);
    ImGui::PopStyleColor(3);
    return ImGui::IsItemHovered() && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left);
}

// TODO: should finally be replaced by Ex version.
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
        const ImVec2 label_size = imgui_CalcLabelSize(label);
        const ImVec2 button_size = {std::max(ImGui::GetContentRegionAvail().x, label_size.x),
                                    label_size.y + 2 * frame_padding_y};
        ret = ImGui::Button(label, button_size);
    } else {
        // The label should fit in {ImGui::CalcItemWidth(), ImGui::GetFrameHeight()}. Not checked.
        const float w_before_shortcut = ImGui::CalcItemWidth() + imgui_ItemInnerSpacingX();
        const ImVec2 shortcut_size = imgui_CalcTextSize(menu_shortcut);
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

// The actual item-id is id/##Sel, irrelevant to 'label'.
inline bool imgui_SelectableStyledButtonEx(const int id, const std::string_view label, const bool selected = false) {
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
    const ImVec2 label_size = imgui_CalcTextSize(label); // (Not trying to hide double-hash.)
    const ImVec2 button_size = {std::max(ImGui::GetContentRegionAvail().x, label_size.x),
                                label_size.y + 2 * frame_padding_y};
    ImGui::PushID(id);
    const bool ret = ImGui::Button("##Sel", button_size);
    ImGui::PopID();
    const auto rect = imgui_GetItemRect();
    ImGui::RenderTextClipped(rect.Min + ImVec2(0, frame_padding_y), rect.Max - ImVec2(0, frame_padding_y), label.data(),
                             label.data() + label.size(), &label_size, {0, 0} /*align*/, &rect);

    prev_id = ImGui::GetItemID();
    if (!selected) {
        ImGui::PopStyleColor();
    }

    return ret;
}

// (No longer need to be a class.)
class sequence : no_create {
public:
    // There can be at most one seq in each window hierarchy.
    // 0:first, 1:prev, 2:next, 3:last
    static int seq(const char* label_first, const char* label_prev, const char* label_next, const char* label_last) {
        enum tagE : int { None = -1, First, Prev, Next, Last };
        tagE tag = None;

        const bool not_disabled = !imgui_TestItemFlag(ImGuiItemFlags_Disabled);
        const bool shortcut_avail = not_disabled && imgui_IsWindowFocused(); // Not including popup hierarchy.
        const bool shortcut_visible =
            not_disabled && ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows); // Including popup.
        assert_implies(shortcut_avail, shortcut_visible);
        auto item_shortcut = [shortcut_avail](ImGuiKey key) {
            // (Used to require `may_scroll` to avoid previewed rule being changed by shortcut.)
            // (Perhaps no longer needed; kept as it does no harm.)
            return shortcut_avail && may_scroll() && shortcuts::no_ctrl() &&
                   shortcuts::keys_avail_and_window_hoverable() && shortcuts::test_pressed(key) &&
                   shortcuts::highlight();
        };

        if (ImGui::Button(label_first)) {
            tag = First;
        }
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        if (ImGui::Button(label_prev) || item_shortcut(ImGuiKey_LeftArrow)) {
            tag = Prev;
        }
        if (shortcut_visible) {
            imgui_ItemRect(ImGui::GetColorU32(ImGuiCol_ButtonActive /*, shortcut_avail ? 1.0f : 0.7f*/));
        }
        ImGui::SameLine(0, 0), imgui_Str("/"), ImGui::SameLine(0, 0);
        if (ImGui::Button(label_next) || item_shortcut(ImGuiKey_RightArrow)) {
            tag = Next;
        }
        if (shortcut_visible) {
            imgui_ItemRect(ImGui::GetColorU32(ImGuiCol_ButtonActive /*, shortcut_avail ? 1.0f : 0.7f*/));
        }
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        if (ImGui::Button(label_last)) {
            tag = Last;
        }

        return tag;
    }
};

class imgui_StepSliderInt : no_create {
    // (Avoiding direct use of `&std::to_string`; see https://stackoverflow.com/questions/55687044)
    static std::string to_str_default(int v) { return std::to_string(v); }

    // (Reduced from `ImGui::SliderBehaviorT`.)
    // (In practice this works well, but I'm not sure whether this is 100% accurate as the original function is too complex...)
    static int value_if_clicked(const float full_slider_width, const int v_max /*v_min ~ 0*/,
                                const float mouse_pos_rel_slider) {
        const float grab_padding = 2.0f; // (In `SliderBehaviorT`: "FIXME: Should be part of style.")
        const float slider_sz = full_slider_width - grab_padding * 2.0f;
        const float grab_sz = std::max(slider_sz / (v_max + 1), GImGui->Style.GrabMinSize);
        if (grab_sz >= slider_sz) {
            return 0;
        }

        const float ratio = (mouse_pos_rel_slider - grab_padding - grab_sz * 0.5f) / (slider_sz - grab_sz);
        // (Rounding in the same way (`int(f + 0.5)`) as `ImGui::ScaleValueFromRatioT`.)
        return (int)(v_max * std::clamp(ratio, 0.0f, 1.0f) + 0.5f);
    }

public:
    struct shortcutT {
        ImGuiKey minus;
        ImGuiKey plus;
    };

    inline static shortcutT next_shortcuts{ImGuiKey_None, ImGuiKey_None};

    // (Referring to ImGui::InputScalar.)
    static bool fn(const char* label, int* v, int v_min, int v_max, int v_step = 1,
                   const func_ref<std::string(int)> to_str = to_str_default) {
        const auto [minus, plus] = std::exchange(next_shortcuts, {ImGuiKey_None, ImGuiKey_None});
        if (GImGui->CurrentWindow->SkipItems) {
            return false;
        }

        assert(v_min < v_max && v_step > 0 && ((v_max - v_min) % v_step) == 0);
        const int u_max = (v_max - v_min) / v_step; // > 0.
        const int u_init = std::clamp((*v - v_min) / v_step, 0, u_max);
        const auto to_v = [u_max, v_min, v_step](int u) { return std::clamp(u, 0, u_max) * v_step + v_min; };
        int u = u_init;

        const float r = ImGui::GetFrameHeight();
        const float s = imgui_ItemInnerSpacingX();
        ImGui::BeginGroup();
        ImGui::PushID(label);
        ImGui::SetNextItemWidth(std::max(1.0f, ImGui::CalcItemWidth() - 2 * (r + s)));
        ImGui::SliderInt("", &u, 0, u_max, "" /*rendered below*/, ImGuiSliderFlags_NoInput);
        {
            const auto rect = imgui_GetItemRect();
            const std::string str = to_str(to_v(u));
            ImGui::RenderTextClipped(rect.Min, rect.Max, str.data(), str.data() + str.size(), nullptr,
                                     ImVec2(0.5f, 0.5f));
            imgui_ItemTooltip([&] {
                imgui_Str(to_str(to_v(value_if_clicked(rect.GetWidth(), u_max, ImGui::GetMousePos().x - rect.Min.x))));
            });
        }

        ImGui::PushItemFlag(ImGuiItemFlags_ButtonRepeat, true);
        ImGui::SameLine(0, s);
        // (`InputScalar` makes .FramePadding.x = y for these buttons, not added here.)
        if (ImGui::Button("-", ImVec2(r, r)) ||
            (minus != ImGuiKey_None && shortcuts::test_pressed(minus, true) && shortcuts::highlight())) {
            --u;
        }
        // imgui_ItemTooltip([&] { imgui_Str(to_str(to_v(u_init - 1))); }); // (too noisy)
        ImGui::SameLine(0, s);
        if (ImGui::Button("+", ImVec2(r, r)) ||
            (plus != ImGuiKey_None && shortcuts::test_pressed(plus, true) && shortcuts::highlight())) {
            ++u;
        }
        // imgui_ItemTooltip([&] { imgui_Str(to_str(to_v(u_init + 1))); });
        ImGui::PopItemFlag(); // ImGuiItemFlags_ButtonRepeat
        const char* label_end = ImGui::FindRenderedTextEnd(label);
        if (label != label_end) {
            ImGui::SameLine(0, s);
            imgui_Str(std::string_view(label, label_end));
        }
        ImGui::PopID();
        ImGui::EndGroup();

        return compare_update(*v, to_v(u));
    }
};

class messenger : no_create {
    class messageT {
        using clockT = std::chrono::steady_clock;

        std::string m_str{};
        std::optional<ImVec2> m_min{};
        int m_count{};
        clockT::time_point m_time{};

    public:
        // (Defined as a workaround for gcc building.)
        // (Related: https://stackoverflow.com/questions/53408962)
        messageT() : m_str{}, m_min{}, m_count{}, m_time{} {}

        void set(std::string&& str) {
            m_min.reset();

            // TODO: ideally should take account of wrap-len...
            size_t subsize = 0;
            for (int line = 0; line < 15; ++line) {
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
        void display_if_present() {
            if (m_str.empty()) {
                return;
            }
            const auto now = clockT::now();
            if (m_min) {
                if (const ImVec2 delta = ImGui::GetIO().MouseDelta; delta.x || delta.y) {
                    --m_count;
                }
                const bool t_expired = now > m_time;
                const bool c_expired = m_count < 0;
                // TODO: ideally the callers of `set_msg` should be able to specify quitting cond.
                if (m_str.size() < 15 ? (c_expired || t_expired) : (c_expired && t_expired)) {
                    m_str.clear();
                    return;
                }
            }

            assert(!m_str.empty());
            if (m_str == ".") {
                if (!m_min) {
                    m_count = 12;
                    m_time = now + std::chrono::milliseconds(400);
                    m_min = ImGui::GetMousePos();
                }
                assert(m_time >= now);
                const float radius =
                    4.0f * std::chrono::floor<std::chrono::milliseconds>(m_time - now).count() / 400.0f;
                ImGui::GetForegroundDrawList()->AddCircleFilled(*m_min - ImVec2(1, 1), radius,
                                                                IM_COL32(0, 255, 0, 255) /*light green*/);
                return;
            }

            const float text_wrap = wrap_len();
            const char *const text_beg = m_str.c_str(), *const text_end = text_beg + m_str.size();
            const ImVec2 window_padding = ImGui::GetStyle().WindowPadding;
            const ImVec2 window_size = ImGui::CalcTextSize(text_beg, text_end, false, text_wrap) + window_padding * 2;
            if (!m_min) {
                m_count = 12;
                m_time = now + std::chrono::milliseconds(600);
                // TODO: support specifying appearing pos?
                if (ImGui::IsMousePosValid()) [[likely]] {
                    m_min = clamp_window_pos(ImGui::GetMousePos() + window_padding, window_size);
                } else {
                    m_min = window_padding;
                }
            }

            const ImVec2 window_min = *m_min;
            const ImVec2 window_max = window_min + window_size;
            ImDrawList* const drawlist = ImGui::GetForegroundDrawList();
            drawlist->AddRectFilled(window_min, window_max, ImGui::GetColorU32(ImGuiCol_PopupBg));
            drawlist->AddRect(window_min, window_max, ImGui::GetColorU32(ImGuiCol_Border));
            drawlist->AddText(nullptr, 0.0f, window_min + window_padding, ImGui::GetColorU32(ImGuiCol_Text), text_beg,
                              text_end, text_wrap);
        }
    };

    inline static messageT m_msg;

public:
    static void set_msg(std::string str) { m_msg.set(std::move(str)); }

    template <class... U>
    static void set_msg(std::format_string<const U&...> fmt, const U&... args) {
        m_msg.set(std::format(fmt, args...));
    }

    static bool dot() {
        m_msg.set(".");
        return true;
    }

    static void display_msg_if_present(frame_main_token) { m_msg.display_if_present(); }
};

class global_timer : no_create {
    static constexpr int time_unit = 25;                // ms.
    static constexpr int min_time = 0, max_time = 1000; // ms.
    static_assert(max_time % time_unit == 0);

    using clockT = std::chrono::steady_clock;
    struct termT {
        clockT::time_point last;   // = {};
        bool active_at_this_frame; // = false; (Will cause trouble when building with gcc or clang...)
    };
    inline static termT terms[1 + (max_time / time_unit)]{};

public:
    static void begin_frame(frame_main_token) {
        const clockT::time_point now = clockT::now();
        for (int i = 0; termT & term : terms) {
            const int dur = time_unit * i++;
            if (term.last + std::chrono::milliseconds(dur) <= now) {
                term.last = now;
                term.active_at_this_frame = true;
            } else {
                term.active_at_this_frame = false;
            }
        }
    }

    // 0: will return true every frame.
    static constexpr int min_nonzero_interval = time_unit;

    class intervalT {
        int i; // terms[i].

    public:
        bool test() const { return terms[i].active_at_this_frame; }

        /*implicit*/ intervalT(int ms) {
            assert(min_time <= ms && ms <= max_time);
            assert(ms % time_unit == 0);
            i = std::clamp(ms, min_time, max_time) / time_unit;
        }

        void step_slide(const char* label, int min_ms, int max_ms) {
            assert(min_time <= min_ms && min_ms < max_ms && max_ms <= max_time);
            assert(min_ms % time_unit == 0);
            assert(max_ms % time_unit == 0);
            imgui_StepSliderInt::fn(label, &i, min_ms / time_unit, max_ms / time_unit, 1,
                                    [](int i) { return std::format("{} ms", i * time_unit); });
        }
    };

    static bool test(intervalT i) { return i.test(); }
};

// Preview rules.
class previewer : no_create {
public:
    class configT {
        friend class previewer;
        float zoom_ = 1;
        int width_ = 220;
        int height_ = 160;

        int step = 1;
        global_timer::intervalT interval = init_zero_interval ? 0 : global_timer::min_nonzero_interval;

        void _set();

    public:
        // TODO: temporarily preserved to avoid breaking existing calls.
        enum sizeE { _220_160 = 0 };
        configT(sizeE) {}

        int width() const { return width_; }
        int height() const { return height_; }
        ImVec2 size_imvec() const { return ImVec2(width_, height_); }

        void set(const char* label, bool small = false) {
            menu_like_popup::button(label, small);
            menu_like_popup::popup([&] { _set(); });
        }
    };

    static void dummy(const configT& config, const char* str = "--") {
        ImGui::Dummy(config.size_imvec());
        if (ImGui::IsItemVisible()) {
            imgui_ItemRectFilled(IM_COL32_BLACK);
            imgui_ItemRect(default_border_color());

            if (str && *str != '\0') {
                imgui_ItemStr(ImGui::GetColorU32(ImGuiCol_TextDisabled), str);
            }
        }
    }

    // (Note: type-erasure doesn't apply here.)
    // (`const ruleT&()` cannot adapt `ruleT()` calls, while `ruleT()` is unnecessarily costly for `const ruleT&()` calls.)
    static void preview(const uint32_t id, const configT& config, const auto& rule_or_get_rule) {
        ImGui::PushID(id);
        ImGui::InvisibleButton("Preview", config.size_imvec());
        ImGui::PopID();
        if (ImGui::IsItemVisible()) {
            const uint64_t id2 = (uint64_t(ImGui::GetItemID()) << 32) | id;
            if constexpr (implicitly_convertible_to<decltype(rule_or_get_rule), const aniso::ruleT&>) {
                _preview(id2, config, rule_or_get_rule);
            } else if constexpr (requires {
                                     { rule_or_get_rule() } -> implicitly_convertible_to<const aniso::ruleT&>;
                                 }) {
                _preview(id2, config, rule_or_get_rule());
            } else {
                static_assert(always_false_v<decltype(rule_or_get_rule)>);
            }
        }
    }

    static void preview_or_dummy(const uint32_t id, const configT& config, const auto* rule) {
        if (rule) {
            preview(id, config, *rule);
        } else {
            dummy(config);
        }
    }

    static ImU32 default_border_color() {
        return ImGui::GetColorU32(ImGuiCol_TableBorderStrong); // Instead of `ImGuiCol_Border`
    }

    static void begin_frame(frame_main_token);

private:
    static void _preview(uint64_t id, const configT& config, const aniso::ruleT& rule);

    static void _show_belongs(const aniso::ruleT& rule);
};

// TODO: support highlighting rule sources?
// TODO: add scope? some source->dest may be meaningless.
class pass_rule : no_create {
    inline static ImGuiID active = 0;
    inline static bool keep_active = false;
    inline static aniso::ruleT rule{};

    static void render_rect(const bool bright) {
        ImGui::PushStyleColor(ImGuiCol_DragDropTarget, IM_COL32(0, 128, 255, bright ? 255 : 64));
        ImGui::RenderDragDropTargetRect(imgui_GetItemRect(), GImGui->CurrentWindow->ClipRect);
        ImGui::PopStyleColor();
    }

public:
    static void begin_frame(frame_main_token) {
        if (!std::exchange(keep_active, false)) {
            active = 0;
        }
    }

    static const aniso::ruleT* peek() { return active ? &rule : nullptr; }

    static bool source(const aniso::ruleT& r) {
        const ImGuiID id = ImGui::GetItemID();
        assert(id != 0);

        if ((active == id) || (ImGui::IsItemActive() && !ImGui::IsItemHovered())) {
            if (ImGui::BeginDragDropSource(ImGuiDragDropFlags_SourceNoHoldToOpenOthers |
                                           ImGuiDragDropFlags_SourceNoPreviewTooltip)) {
                static char dummy = 0;
                ImGui::SetDragDropPayload("#Rule", &dummy, sizeof(dummy));
                ImGui::EndDragDropSource();
                render_rect(true);

                lock_scroll();
                active = id;
                keep_active = true;
                rule = r;
                return true;
            }
        }
        return false;
    }

    struct passT {
        const aniso::ruleT* rule = nullptr;
        bool hov = false, deliv = false;

        const aniso::ruleT* get_hov() const { return hov ? rule : nullptr; }
        const aniso::ruleT* get_deliv() const { return deliv ? rule : nullptr; }

        // (Using _ForTooltip for stable visual.)
        bool hov_for_tooltip() const {
            assert_implies(hov, rule);
            return hov && imgui_IsItemHoveredForTooltip(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem);
        }

        void tooltip_or_message(const std::string_view str) const {
            if (hov_for_tooltip() && ImGui::BeginTooltip()) {
                ImGui::PushTextWrapPos(wrap_len());
                imgui_Str(str);
                ImGui::PopTextWrapPos();
                ImGui::EndTooltip();
            } else if (deliv) {
                messenger::set_msg(std::string(str));
            }
        }
    };

    [[nodiscard]] static passT dest(const ImGuiKey /*shortcut*/ = ImGuiKey_None, const char /*label*/ = '\0') {
        if (active && ImGui::IsItemVisible()) {
            render_rect(false);
#if 0
            static item_timer timer{};
            render_rect(timer.test());
            if (label) { // TODO: should render at the foreground of individual windows...
                const char str[]{'^', ' ', label, '\0'};
                const ImVec2 pos = imgui_GetItemRect().GetBL() + ImVec2(-4, 5);
                const ImVec2 padding = ImGui::GetStyle().FramePadding;
                const ImVec2 size = imgui_CalcTextSize(str) + padding * 2;
                const float alpha = 1;
                ImDrawList* const drawlist = ImGui::GetForegroundDrawList();
                drawlist->AddRectFilled(pos, pos + size, ImGui::GetColorU32(ImGuiCol_PopupBg, alpha));
                drawlist->AddText(pos + padding, ImGui::GetColorU32(ImGuiCol_Text, alpha), str);
                drawlist->AddRect(pos, pos + size, ImGui::GetColorU32(ImGuiCol_Border, alpha));
            }
#endif
            if (ImGui::BeginDragDropTarget()) {
                const bool deliv = ImGui::AcceptDragDropPayload(
                    "#Rule", ImGuiDragDropFlags_AcceptNoPreviewTooltip | ImGuiDragDropFlags_AcceptNoDrawDefaultRect);
                ImGui::EndDragDropTarget();
                render_rect(true);
                if (deliv) {
                    active = false;
                }
                return {.rule = &rule, .hov = true, .deliv = deliv};
            }
#if 0
            if (shortcut != ImGuiKey_None && shortcuts::no_ctrl() &&
                GImGui->DragDropPayload.SourceId != ImGui::GetItemID() && shortcuts::test_pressed(shortcut)) {
                timer.bind();
                render_rect(true);
                return {.rule = &rule, .hov = false, .deliv = true};
            }
#endif
        }
        return {.rule = nullptr, .hov = false, .deliv = false};
    }
};

inline void set_clipboard_and_notify(const std::string& str) {
    if (str.empty()) {
        // Ignore silently...
        // messenger::set_msg("Ignored empty str.");
    } else if (str.find('\0') == str.npos) {
        ImGui::SetClipboardText(str.c_str());
        // messenger::dot();
        messenger::set_msg("Copied.");
    } else {
        // This can happen when the user tries to copy lines in a data file.
        // If copied, the result will be incomplete, and nothing in worst case (if starts with '\0').
        messenger::set_msg("The text contains null characters.");
    }
}

inline std::string_view read_clipboard() {
    // It's not obvious whether `ImGui::GetClipboardText` can return nullptr...
    const char* str = ImGui::GetClipboardText();
    if (!str || *str == '\0') {
        // As tested, the reasons can be:
        // The clipboard is actually empty.
        // The clipboard contains non-text content, so the read fails.
        // The clipboard contains a real empty string.
        messenger::set_msg("Nothing to paste.");
        return {};
    }
    return str;
}

inline bool input_text(const char* label, std::span<char> buf, const char* hint = nullptr, ImGuiInputFlags flags = 0,
                       ImGuiInputTextCallback callback = nullptr) {
    const bool ret = hint ? ImGui::InputTextWithHint(label, hint, buf.data(), buf.size(), flags, callback)
                          : ImGui::InputText(label, buf.data(), buf.size(), flags, callback);
    if (ImGui::IsItemActive() && !ImGui::IsMouseDown(ImGuiMouseButton_Left) &&
        ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
        ImGui::ClearActiveID();
    }
    return ret;
}

// TODO: the name is too casual but I cannot think of a very suitable one...
template <int max_digit = 5>
class input_int {
    static_assert(1 <= max_digit && max_digit <= std::numeric_limits<int>::digits10);
    char buf[max_digit + 1 /*'\0'*/]{};

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

        if (input_text(label, buf, hint, input_flags, input_filter)) {
            return flush();
        }
        return std::nullopt;
    }
};

// TODO: whether to apply this? (rule_snapshot & rec_for_rule(_b))
#if 0
struct rule_and_hash {
    aniso::compressT rule;
    size_t hash;
    rule_and_hash(const auto& r) : rule(r), hash(rule.hash()) {}
    bool operator==(const rule_and_hash& other) const { //
        return hash == other.hash && rule == other.rule;
    }
};
static_assert(std::is_trivially_copyable_v<rule_and_hash>);
#endif

class rule_snapshot : no_copy {
    using dataT = std::vector<aniso::compressT>;
    previewer::configT m_settings{previewer::configT::_220_160};

    dataT m_data{};
    bool m_newly_updated{};
    bool m_outdated{};

public:
    bool empty() const { return m_data.empty(); }
    explicit operator bool() const { return !empty(); }
    void clear() { m_data.clear(); }

    void update(const dataT& data) {
        assert(!data.empty());
        m_data = data;
        m_newly_updated = true;
        m_outdated = false;
    }
    void test_outdated(const dataT& data) { m_outdated = data != m_data; }

    struct contextT {
        func_ref<aniso::compressT()> get;
        func_ref<void(const aniso::compressT&)> set;
    };
    // TODO: support specifying title?
    open_state display(const dataT& data, const std::optional<contextT>& context) {
        assert(!m_data.empty());
        const bool updated = std::exchange(m_newly_updated, false);
        bool open = true;
        bool to_top = updated;

        const ImVec2 min_size = [&] {
            const auto& style = ImGui::GetStyle();
            const int min_size_x = m_settings.width() +
                                   (context ? (style.ItemSpacing.x + ImGui::GetFrameHeight()) /*radio*/ : 0) +
                                   (m_data.size() > 1 ? style.ScrollbarSize : 0);
            const int min_size_y =
                ImGui::GetFrameHeight() + ImGui::GetTextLineHeight() + style.ItemSpacing.y * 2 + m_settings.height();
            return ImVec2(min_size_x, min_size_y) + style.WindowPadding * 2;
        }();

        ImGui::SetNextWindowSize(min_size, updated ? ImGuiCond_Always : ImGuiCond_Appearing);
        ImGui::SetNextWindowSizeConstraints(min_size, {min_size.x + 120, 500});
        if (updated) {
            ImGui::SetNextWindowCollapsed(false);
            ImGui::SetNextWindowFocus();
            if (ImGui::IsMousePosValid()) {
                const float h = ImGui::GetFrameHeight();
                ImGui::SetNextWindowPos(clamp_window_pos(ImGui::GetMousePos() - ImVec2{h * 2, floor(h / 2)}, min_size),
                                        ImGuiCond_Always);
            }
        }

        // !!TODO: redesign title; ideally should not rely on `this`...
        // ("###Label" is different than "Label"...)
        // assert(ImGui::GetID("123###123") != ImGui::GetID("123"));
        const std::string title =
            std::string(m_outdated ? "Record *" : "Record") + "###Snapshot" + std::to_string(uintptr_t(this));

        imgui_Window::next_window_titlebar_tooltip =
            "This is a snapshot of the actual record. When it's outdated, the window title will be marked with '*', and you can update with 'Update'.";
        if (auto window = imgui_Window(title.c_str(), &open, ImGuiWindowFlags_NoSavedSettings)) {
            if (ImGui::SmallButton("Update") && messenger::dot()) {
                assert(!data.empty()); // (As there is currently no clear method.)
                m_data = data;
                m_outdated = false;
                to_top = true;
            }
            ImGui::SameLine();
            if (ImGui::SmallButton("Top") && messenger::dot()) {
                to_top = true;
            }
            ImGui::SameLine();
            m_settings.set("Settings", true /*small*/);
            ImGui::SameLine();
            const int total = m_data.size();
            ImGui::Text("Total:%d", total);

            if (to_top) {
                ImGui::SetNextWindowScroll({0, 0});
            }
            // TODO: ?`imgui_FillAvailRect(IM_COL32_GREY(24, 255));`
            ImGui::PushStyleColor(ImGuiCol_ChildBg, IM_COL32_GREY(24, 255));
            if (auto child = imgui_ChildWindow("Page")) {
                const auto current = context ? std::optional{context->get()} : std::nullopt;
                static_assert(std::is_same_v<decltype(*current), const aniso::compressT&>);
                std::optional<aniso::compressT> select = std::nullopt;

                for (int i = 0; i < total; ++i) {
                    ImGui::Separator();
                    previewer::preview(i, m_settings, m_data[i]);
                    if (context) {
                        ImGui::SameLine();
                        ImGui::PushID(i);
                        if (const bool eq = current == m_data[i]; //
                            ImGui::RadioButton("##Sel", eq) && !eq) {
                            select = m_data[i];
                        }
                        ImGui::PopID();
                    }
                }

                assert_implies(select, context);
                if (select) {
                    context->set(*select);
                    // m_outdated = true; // (Will be set back by the context.)
                }
            }
            ImGui::PopStyleColor();
        }
        return {open};
    }
};

class rec_for_rule : no_copy {
    using dataT = std::vector<aniso::compressT>;
    int m_capacity;
    dataT m_data;

    mutable bool m_written = false; // (Workaround to notify snapshot.)

public:
    explicit rec_for_rule(/*const int cap = 40*/) : m_capacity(40) {
        // assert(cap > 0 && cap < 100);
        m_data.reserve(m_capacity);
    }

    const dataT& data() const { return m_data; }
    bool empty() const { return m_data.empty(); }
    int size() const { return m_data.size(); }
    auto find(const aniso::compressT& rule) const { return std::ranges::find(m_data, rule); }
    bool contains(const aniso::compressT& rule) const { return find(rule) != m_data.end(); }

    // LRU; inefficient but no problem as `m_capacity` is small enough.
    void add(const aniso::compressT& rule) {
        if (const auto found = find(rule); found != m_data.end()) {
            m_data.erase(found);
        } else if (m_data.size() == m_capacity) {
            m_data.pop_back();
        }
        m_data.insert(m_data.begin(), rule);
        m_written = true;
    }

    bool written_since_last_check() const { return std::exchange(m_written, false); }

    // TODO: whether to support clearing? (Never necessary as the buffer is small enough...)
    // void clear() { m_data.clear(); }
};

// (Used by `rclick_popup`.)
inline void selectable_to_take_snapshot(const char* label, const rec_for_rule& rec, rule_snapshot& snapshot) {
    const bool empty = rec.empty();
    ImGui::BeginDisabled(empty);
    if (ImGui::Selectable(std::format("{} ({})###{}", label, rec.size(), label).c_str())) {
        snapshot.update(rec.data());
    }
    ImGui::EndDisabled();
    if (empty) {
        imgui_ItemTooltip("No rules.");
    }
}

inline void display_snapshot_if_present(rule_snapshot& snapshot, const rec_for_rule& rec,
                                        const std::optional<rule_snapshot::contextT>& context = std::nullopt) {
    if (snapshot) {
        if (rec.written_since_last_check()) {
            snapshot.test_outdated(rec.data());
        }
        if (snapshot.display(rec.data(), context).closed()) {
            snapshot.clear();
        }
    }
}

class rule_with_rec : no_copy {
    aniso::ruleT m_rule;
    rec_for_rule m_rec;

public:
    /*implicit*/ rule_with_rec(const aniso::ruleT& r) : m_rec{} {
        m_rule = r;
        m_rec.add(r);
    }

    operator const aniso::ruleT&() const { return m_rule; }
    const aniso::ruleT& get() const { return m_rule; }
    void set(const aniso::ruleT& r) {
        m_rule = r;
        m_rec.add(r);
    }

    const rec_for_rule& rec() const { return m_rec; }
};

class copy_rule : no_create {
    inline static rec_for_rule rec{};

    static void save(const aniso::ruleT& rule);

public:
    static void copy(const aniso::ruleT& rule) {
        set_clipboard_and_notify(aniso::to_MAP_str(rule));
        rec.add(rule);
        save(rule);
    }

    static const rec_for_rule& get_rec(frame_main_token) { return rec; }
};
