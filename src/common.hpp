#pragma once

#include <chrono>
#include <ctime>
#include <format>

#include "rule.hpp"

#include "dear_imgui.hpp"

/* Not inline */ static const bool check_version = IMGUI_CHECKVERSION();

// Enforce that ordinary string literals are encoded with utf-8.
// This requires certain compiler flags to be set (e.g. `/utf-8` in MSVC).
// (Currently not necessary, as the program is using only ascii characters.)
// (u8"..." is not usable in this project, as it becomes `char8_t[]` in C++20.)
// (This doesn't imply main's argv / fs::path.string() / exception.what() are encoded as utf-8...)
static_assert(std::ranges::equal("中文日本語한국어", u8"中文日本語한국어", [](auto l, auto r) {
    return static_cast<unsigned char>(l) == static_cast<unsigned char>(r);
}));

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

open_state load_file(frame_main_token);
open_state load_clipboard(frame_main_token);
open_state load_doc(frame_main_token);

void edit_rule(frame_main_token);
void edit_pattern(frame_main_token);

// Require the current window to be shown above its source. (Only valid for the current frame.)
void set_above(const ImGuiWindow* source);

// + ImGuiWindowFlags_NoFocusOnAppearing -> bring to front without taking focus.
// (_NoFocusOnAppearing alone disables bringing to front.)
inline void bring_to_front_on_appearing() {
    assert(!(GImGui->CurrentWindow->Flags & ImGuiWindowFlags_NoBringToFrontOnFocus));
    if (ImGui::IsWindowAppearing()) {
        ImGui::BringWindowToDisplayFront(GImGui->CurrentWindow);
    }
}

class rand_source : no_create {
    static uint32_t seed() { return std::time(0); }

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
    void code_image(aniso::codeT code, int zoom);
    bool code_button(aniso::codeT code, int zoom);

    std::string home_path_utf8(); // "." ~ current path, empty ~ unavailable.

    void set_frame_rate(); // Shown in popup.
} // namespace backend_fn

// The same as the size of checkbox square.
inline ImVec2 square_size(/*const float r = ImGui::GetFrameHeight()*/) {
    const float r = ImGui::GetFrameHeight();
    return ImVec2(r, r);
}

// TODO: too general to be global names...
// The same as the default item width in tooltip windows (search `16.0f` in "imgui.cpp").
inline float item_width() { return ImGui::GetFontSize() * 16.0f; }
// The same as the wrap width in `HelpMarker` in "imgui_demo.cpp".
inline float wrap_len() { return ImGui::GetFontSize() * 35.0f; }

inline ImVec4 to_opaque(ImVec4 col) {
    const float w = std::exchange(col.w, 1.0f);
    col.x *= w;
    col.y *= w;
    col.z *= w;
    return col;
}

inline ImVec2 clamp_window_pos(const ImVec2 pos, const ImVec2 size) {
    const ImVec2 padding = ImGui::GetStyle().WindowPadding;
    const ImVec2 min = padding;
    const ImVec2 max = ImGui::GetMainViewport()->Size - padding - size;
    return (min.x < max.x && min.y < max.y) ? ImClamp(pos, min, max) : pos;
}

// !!TODO: support configs.
inline constexpr bool init_maximize_window = false;
inline constexpr bool init_enable_vsync = true;
inline constexpr bool init_zero_interval = false;
inline constexpr bool init_show_intro = true;
inline constexpr bool init_extra_tooltips = init_show_intro;
inline constexpr bool init_compact_mode = false;
inline constexpr bool init_selectables_use_button_color = false;
inline constexpr bool init_set_scroll_with_up_down = true;

// (Return true for && chaining.)
inline bool highlight_item() {
    ImGui::NavHighlightActivated(ImGui::GetItemID());
    return true;
}
inline void highlight_item(ImGuiID id) { ImGui::NavHighlightActivated(id); }

// TODO: consider using ImGui::Shortcut?
// Some features cannot easily be satisfied with `ImGui::Shortcut` and `ImGui::SetNextItemShortcut`.
class shortcuts : no_create {
public:
    static bool no_input() { return !GImGui->IO.WantTextInput; }
    static bool global_flag(ImGuiKey key) { return no_input() && ImGui::IsKeyDown(key); }

    // (`IO.WantCaptureKeyboard` is for notifying non-imgui parts.)
    static bool no_active() { return !GImGui->ActiveId && no_input(); }

private:
    inline static ImGuiKey occupied = ImGuiKey_None;

public:
    static void begin_frame(frame_main_token) { occupied = ImGuiKey_None; }

    // Resolve shortcut competition when multiple keys are pressed.
    static bool test_down(ImGuiKey key) {
        assert(key != ImGuiKey_None);
        if (occupied == ImGuiKey_None && ImGui::IsKeyDown(key)) {
            occupied = key;
            return true;
        }
        return occupied == key;
    }

    static bool test_pressed(ImGuiKey key, bool repeat = false) { //
        return test_down(key) && ImGui::IsKeyPressed(key, repeat);
    }
};

// TODO: improve...
class guide_mode : no_create {
    inline static bool enable_tooltip = init_extra_tooltips;

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

template <auto>
inline bool first_of_this_window() {
    static const ImGuiWindow* prev = nullptr;
    static int frame = -1;
    if (compare_update(frame, ImGui::GetFrameCount())) {
        prev = nullptr;
    }
    return compare_update(prev, GImGui->CurrentWindow->RootWindow);
}

// TODO: cannot decide test_down vs test_pressed / dy...
// There can be at most one call in each window.
inline void set_scroll_with_up_down() {
    assert(first_of_this_window<set_scroll_with_up_down>());
    if constexpr (init_set_scroll_with_up_down) {
        if (imgui_IsWindowFocused() && may_scroll() && shortcuts::no_active() && imgui_IsWindowHoverable()) {
#if 1
            // (Works well in 60 fps.)
            const int dy = shortcuts::test_down(ImGuiKey_UpArrow)     ? -2
                           : shortcuts::test_down(ImGuiKey_DownArrow) ? 2
                                                                      : 0;
            if (dy != 0) {
                ImGui::SetScrollY(ImGui::GetScrollY() + dy);
                highlight_item(ImGui::GetWindowScrollbarID(GImGui->CurrentWindow, ImGuiAxis_Y));
            }
#else
            // Cannot guarantee similar speed in different fps - rounding does happen, but too early (cannot apply to the accumulation).
            // (See `CalcNextScrollFromScrollTargetAndClamp()`; so it's (round(dy)+...) instead of round((dy+...)).)
            // Related: https://github.com/ocornut/imgui/issues/6677
            const int dir = shortcuts::test_down(ImGuiKey_UpArrow)     ? -1
                            : shortcuts::test_down(ImGuiKey_DownArrow) ? 1
                                                                       : 0;
            if (dir != 0) {
                ImGui::SetScrollY(ImGui::GetScrollY() + dir * 120.0f / std::max(1.0f, ImGui::GetIO().Framerate));
                highlight_item(ImGui::GetWindowScrollbarID(GImGui->CurrentWindow, ImGuiAxis_Y));
            }
#endif
        }
    }
}

// It's enough to focus the source window by calling `SetWindowFocus` before `OpenPopup`.
// However, the parent window will be brought to foreground immediately, while the popup will appear at next frame due to auto-resize...
class popup_with_focus : no_create {
public:
    // TODO: should apply in release mode...
    static constexpr bool appear_at_same_frame = debug_mode;

    static void open_popup(const ImGuiID popup_id, const ImGuiPopupFlags popup_flags) {
        if constexpr (!appear_at_same_frame) {
            ImGui::SetWindowFocus();
        }
        ImGui::OpenPopupEx(popup_id, popup_flags);
        assert(imgui_IsPopupOpen(popup_id));
        if constexpr (appear_at_same_frame) {
            auto& popup_ref = GImGui->OpenPopupStack.back();
            assert(popup_ref.PopupId == popup_id);
            popup_ref.RestoreNavWindow = GImGui->CurrentWindow->RootWindow;
        }
    }

    // Must be called inside popup.
    static void set_focus([[maybe_unused]] const ImGuiID popup_id, [[maybe_unused]] const ImGuiWindow* source) {
        if constexpr (appear_at_same_frame) {
            const auto& popup_ref = GImGui->BeginPopupStack.back();
            // (BeginPopup should be called after OpenPopup.)
            if (popup_ref.OpenFrameCount + 1 == GImGui->FrameCount) { // Visually appearing.
                assert(popup_ref.PopupId == popup_id);
                assert(popup_ref.RestoreNavWindow == source->RootWindow);
                assert(popup_ref.Window == GImGui->CurrentWindow); // The popup itself.
                if (!(popup_ref.RestoreNavWindow->Flags & ImGuiWindowFlags_NoBringToFrontOnFocus)) {
                    ImGui::BringWindowToDisplayFront(popup_ref.RestoreNavWindow);
                }
                ImGui::BringWindowToDisplayFront(popup_ref.Window);
            }
        }
    }
};

// Workaround to prevent flicker (to let popups & windows disappear at the same frame).
// (`CloseCurrentPopup()` seems to hide popups immediately, while regular windows will remain open for one extra frame when closed.)
inline bool source_window_has_no_close_button() { //
    return !GImGui->CurrentWindow->RootWindowPopupTree->HasCloseButton;
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
        const bool is_open = imgui_IsPopupOpen(expected_id = ImGui::GetID(label));
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

#if 0
    // Not working well...
    static void selectable(const char* label) {
        ImGui::PushStyleColor(ImGuiCol_HeaderActive, ImGui::GetStyleColorVec4(ImGuiCol_HeaderHovered));
        const bool is_open = imgui_IsPopupOpen(expected_id = ImGui::GetID(label));
        ImGui::Selectable(label, false,
                          ImGuiSelectableFlags_NoAutoClosePopups /*necessary*/ |
                              (is_open ? ImGuiSelectableFlags_Highlight : ImGuiSelectableFlags_None));
        ImGui::PopStyleColor();
    }
#endif

    static void popup(const func_ref<void()> fn) {
        const ImRect item_rect = imgui_GetItemRect();
        const ImGuiID item_id = ImGui::GetItemID();
        assert(item_id != 0 && item_id == expected_id); // Must follow `(small_)button()`.
        const ImGuiID popup_id = item_id;

        const ImGuiWindow* source_window = GImGui->CurrentWindow;
        // (Using two hover test to prevent disabled case (without having to play with flags).)
        // (`ImGuiHoveredFlags_ForTooltip` imply `ImGuiHoveredFlags_AllowWhenDisabled` by default.)
        if (!imgui_IsPopupOpen(popup_id) && imgui_IsItemOrNoneActive() && ImGui::IsItemHovered() &&
            imgui_IsItemHoveredForTooltip()) {
            // TODO: whether to focus automatically?
            // (Or require the source window to be focused?)
            popup_with_focus::open_popup(popup_id, ImGuiPopupFlags_NoReopen);
            ImGui::SetNextWindowPos(item_rect.GetTR(), ImGuiCond_Appearing); // Like a menu.
        }

        if (imgui_BeginPopupRecycled(popup_id)) {
            // Possible only if `fn()` disables the outer scope (should not happen).
            assert(!imgui_TestItemFlag(ImGuiItemFlags_Disabled));
            if (imgui_IsWindowHoverable()) { // Topmost popup.
                popup_with_focus::set_focus(popup_id, source_window);
                const ImVec2 mouse_pos = ImGui::GetMousePos(); // Needn't be valid.
                const auto window_rect = imgui_GetWindowRect();
                if (!window_rect.Contains(mouse_pos)) {
                    // Disable mouse scrolling in other windows.
                    lock_scroll();
                }

                // Used to be `!window_rect.Contains(mouse_pos)`, but if the item overlaps with another window's title bar
                // and it's double-clicked, the window will be collapsed without closing the popup...
                if (GImGui->HoveredWindow == source_window && item_rect.Contains(mouse_pos)) {
                    // Avoid closing the popup when the item is clicked; relying on the impl details of this function:
                    (void)&ImGui::UpdateMouseMovingWindowEndFrame;
                    // Initially I tried to use modal popup to avoid the closing behavior, but that caused much more
                    // trouble than it solved :|

                    GImGui->IO.MouseClicked[0] = GImGui->IO.MouseClicked[1] = false;
                } else if (const ImVec2 pad = square_size(); !ImGui::IsAnyItemActive() &&
                                                             !item_rect.ContainsWithPad(mouse_pos, pad * 1.5) &&
                                                             !window_rect.ContainsWithPad(mouse_pos, pad * 2.5)) {
                    ImGui::CloseCurrentPopup();
                } else if constexpr (init_double_esc_to_close) {
                    if (want_close_windows && source_window_has_no_close_button()) {
                        // (Not sharing closing logic in `imgui_BeginPopupRecycled` due to this check...)
                        if (!item_rect.Contains(mouse_pos)) {
                            ImGui::CloseCurrentPopup();
                        }
                    }
                }
            }

            fn();
            ImGui::EndPopup();
        }
    }
};

class rclick_popup : no_create {
public:
    // Popups are hidden at the first frame due to auto-resize, but will still block items.
    enum class hoverE { None, Hovered, PopupHidden, PopupVisible };
    using enum hoverE;

    // (Not meant to be called recursively.)
    [[nodiscard]] static hoverE popup_no_highlight(const ImGuiID id, const func_ref<void()> fn) {
        assert(id != 0);
        const ImGuiID popup_id = id;
        const bool hovered = ImGui::IsItemHovered(); // !hovered when disabled.
        bool opened = imgui_IsPopupOpen(popup_id);
        if (!hovered && !opened) {
            // To respect ImGui::SetNextWindowXX calls.
            GImGui->NextWindowData.ClearFlags();
            return None;
        }

        const ImGuiWindow* source_window = GImGui->CurrentWindow;
        if (!opened && hovered && !ImGui::IsAnyItemActive() && ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
            popup_with_focus::open_popup(popup_id, ImGuiPopupFlags_NoReopen);
            opened = true;
        }

        hoverE hov = Hovered;
        if (opened && imgui_BeginPopupRecycled(popup_id)) {
            assert(!imgui_TestItemFlag(ImGuiItemFlags_Disabled));
            popup_with_focus::set_focus(popup_id, source_window);
            hov = GImGui->CurrentWindow->Hidden ? PopupHidden : PopupVisible;

            lock_scroll();
            fn();
            if constexpr (init_double_esc_to_close) {
                if (want_close_windows && source_window_has_no_close_button()) {
                    ImGui::CloseCurrentPopup();
                }
            }
            ImGui::EndPopup();
        } else {
            GImGui->NextWindowData.ClearFlags();
        }
        return hov;
    }

    static ImU32 highlight_col(const bool bright) {
        return ImGui::GetColorU32(bright ? ImGuiCol_Text : ImGuiCol_TextDisabled);
    }

    static hoverE for_text(const func_ref<void()> fn) {
        const hoverE hov = popup_no_highlight(imgui_GetItemIDNonZero(), fn);
        if (hov != None) {
            imgui_ItemUnderline(highlight_col(hov == PopupVisible));
        }
        return hov;
    }

    static hoverE for_button(const func_ref<void()> fn) {
        const ImGuiID id = ImGui::GetItemID();
        assert(id);
        const hoverE hov = popup_no_highlight(id, fn);
        if (hov == PopupHidden || hov == PopupVisible) {
            highlight_item(id);
        }
        return hov;
    }
};

#if 0
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
#endif

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
    GImGui->CurrentWindow->WriteAccessed = true;
    if (GImGui->CurrentWindow->SkipItems) {
        return false;
    }

    // (`ImGui::Selectable()` uses `ImGuiCol_HeaderXXX`.)
    ImGui::PushStyleColor(ImGuiCol_Button, selected ? ImGui::GetStyleColorVec4(ImGuiCol_Header) : ImVec4());
    ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_HeaderHovered));
    ImGui::PushStyleColor(ImGuiCol_ButtonActive, ImGui::GetStyleColorVec4(ImGuiCol_HeaderActive));

    static ImGuiID prev_id = 0;
    static int frame = -1;
    if (compare_update(frame, ImGui::GetFrameCount())) {
        prev_id = 0;
    }
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
    ImGui::PopStyleColor(3);

    return ret;
}

// The actual item-id is id/##Sel, irrelevant to 'label'.
inline bool imgui_SelectableStyledButtonEx_Clipped = false; // Label is not fully visible.
inline bool imgui_SelectableStyledButtonEx(const int id, const std::string_view label, const bool selected = false) {
    assert(!GImGui->CurrentWindow->DC.IsSameLine);
    GImGui->CurrentWindow->WriteAccessed = true;
    if (GImGui->CurrentWindow->SkipItems) {
        imgui_SelectableStyledButtonEx_Clipped = false;
        return false;
    }

    // (`ImGui::Selectable()` uses `ImGuiCol_HeaderXXX`.)
    ImGui::PushStyleColor(ImGuiCol_Button, selected ? ImGui::GetStyleColorVec4(ImGuiCol_Header) : ImVec4());
    ImGui::PushStyleColor(ImGuiCol_ButtonHovered, ImGui::GetStyleColorVec4(ImGuiCol_HeaderHovered));
    ImGui::PushStyleColor(ImGuiCol_ButtonActive, ImGui::GetStyleColorVec4(ImGuiCol_HeaderActive));

    static ImGuiID prev_id = 0;
    static int frame = -1;
    if (compare_update(frame, ImGui::GetFrameCount())) {
        prev_id = 0;
    }
    if (prev_id != 0 && prev_id == ImGui::GetItemID()) {
        // As if the last call used `ImGui::PushStyleVarY(ImGuiStyleVar_ItemSpacing, 0)`.
        // (PushStyleVar-ItemSpacing affects the spacing to the next item. See `ImGui::ItemSize` for details.)
        imgui_AddCursorPosY(-ImGui::GetStyle().ItemSpacing.y);
    }

    constexpr float frame_padding_y = 2;
    const ImVec2 label_size = imgui_CalcTextSize(label); // (Not trying to hide double-hash.)
    const float avail_size_x = ImGui::GetContentRegionAvail().x;
    const ImVec2 button_size = {std::max(avail_size_x, label_size.x), label_size.y + 2 * frame_padding_y};
    imgui_SelectableStyledButtonEx_Clipped = label_size.x > avail_size_x + 2;

    ImGui::PushID(id);
    const bool ret = ImGui::Button("##Sel", button_size);
    ImGui::PopID();
    if (ImGui::IsItemVisible()) {
        const ImRect rect = imgui_GetItemRect();
        ImGui::RenderTextClipped(rect.Min + ImVec2(0, frame_padding_y), rect.Max - ImVec2(0, frame_padding_y),
                                 label.data(), label.data() + label.size(), &label_size, {0, 0} /*align*/, &rect);
    }

    prev_id = ImGui::GetItemID();
    ImGui::PopStyleColor(3);

    return ret;
}

inline int imgui_CalcSelectableStyledButtonHeight() { return ImGui::GetFontSize() + 4; }

// (No longer need to be a class.)
class sequence : no_create {
public:
    // There can be at most one call in each window.
    // 0:first, 1:prev, 2:next, 3:last
    static int seq(const char* label_first, const char* label_prev, const char* label_next, const char* label_last) {
        assert(first_of_this_window<seq>());
        enum tagE : int { None = -1, First, Prev, Next, Last };
        tagE tag = None;

        const bool not_disabled = !imgui_TestItemFlag(ImGuiItemFlags_Disabled);
        // TODO: does focused imply hoverable?
        // (Used to require `may_scroll` to avoid previewed rule being changed by shortcut; perhaps no longer needed.)
        const bool shortcut_avail = not_disabled && imgui_IsWindowFocused() // Not including popup hierarchy.
                                    && may_scroll() && shortcuts::no_active() && imgui_IsWindowHoverable();

        // TODO: use `shortcut_avail` directly?
        // (Ideally should highlight iff the window's title bar uses `ImGuiCol_TitleBgActive` (see `RenderWindowDecorations`).)
        // (This can't always produce exact match, but very close and the condition is not easy to emulate...)
        // const bool shortcut_visible =
        //     not_disabled && ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows); // Including popup.
        // assert_implies(shortcut_avail, shortcut_visible);
        const auto item_shortcut = [shortcut_avail](ImGuiKey key) {
            return shortcut_avail && shortcuts::test_pressed(key) && highlight_item();
        };

        if (ImGui::Button(label_first)) {
            tag = First;
        }
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        const ImVec2 sample_size = imgui_CalcButtonSize(">");
        const float extra_padding = std::max(0.0f, std::floor((sample_size.y - sample_size.x) / 2));
        ImGui::GetStyle().FramePadding.x += extra_padding;
        if (ImGui::Button(label_prev) || item_shortcut(ImGuiKey_LeftArrow)) {
            tag = Prev;
        }
        ImGui::GetStyle().FramePadding.x -= extra_padding;
        // if (shortcut_visible) {
        //     imgui_ItemRect(ImGui::GetColorU32(ImGuiCol_ButtonActive /*, shortcut_avail ? 1.0f : 0.7f*/));
        // }
        ImGui::SameLine(0, 0), imgui_Str("/"), ImGui::SameLine(0, 0);
        ImGui::GetStyle().FramePadding.x += extra_padding;
        if (ImGui::Button(label_next) || item_shortcut(ImGuiKey_RightArrow)) {
            tag = Next;
        }
        ImGui::GetStyle().FramePadding.x -= extra_padding;
        // if (shortcut_visible) {
        //     imgui_ItemRect(ImGui::GetColorU32(ImGuiCol_ButtonActive /*, shortcut_avail ? 1.0f : 0.7f*/));
        // }
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
        constexpr float grab_padding = 2; // (In `SliderBehaviorT`: "FIXME: Should be part of style.")
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
        assert(!imgui_TestItemFlag(ImGuiItemFlags_Disabled)); // Not considering disabled case.
        const auto [minus, plus] = std::exchange(next_shortcuts, {ImGuiKey_None, ImGuiKey_None});
        GImGui->CurrentWindow->WriteAccessed = true;
        if (GImGui->CurrentWindow->SkipItems) {
            return false;
        }

        // TODO: whether to begin-disabled when `v_min == v_max`?
        assert(v_min <= v_max && v_step > 0 && ((v_max - v_min) % v_step) == 0);
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
            const ImRect rect = imgui_GetItemRect();
            const std::string str = to_str(to_v(u));
            ImGui::RenderTextClipped(rect.Min, rect.Max, str.data(), str.data() + str.size(), nullptr,
                                     ImVec2(0.5f, 0.5f));
            if (ImGui::IsMousePosValid() && ImGui::BeginItemTooltip()) {
                imgui_Str(to_str(to_v(value_if_clicked(rect.GetWidth(), u_max, ImGui::GetMousePos().x - rect.Min.x))));
                ImGui::EndTooltip();
            }
        }

        ImGui::PushItemFlag(ImGuiItemFlags_ButtonRepeat, true);
        ImGui::SameLine(0, s);
        // (`InputScalar` makes .FramePadding.x = y for these buttons, not added here.)
        if (ImGui::Button("-", ImVec2(r, r)) ||
            (minus != ImGuiKey_None && shortcuts::test_pressed(minus, true) && highlight_item())) {
            --u;
        }
        // imgui_ItemTooltip([&] { imgui_Str(to_str(to_v(u_init - 1))); }); // (too noisy)
        ImGui::SameLine(0, s);
        if (ImGui::Button("+", ImVec2(r, r)) ||
            (plus != ImGuiKey_None && shortcuts::test_pressed(plus, true) && highlight_item())) {
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

// !!TODO: when to display dot? (Whether to support dot feedback at all?)
class messenger : no_create {
    class messageT {
        using clockT = std::chrono::steady_clock;

        bool m_dot{};
        std::string m_str{};
        std::optional<ImVec2> m_min{};

        enum lifeE { Once, Auto, User };
        lifeE m_life{};

        int m_count{};
        clockT::time_point m_time{};

    public:
        // (Defined as a workaround for gcc & clang building.)
        // (Related: https://stackoverflow.com/questions/53408962)
        // messageT() = default; // Doesn't work...
        messageT() : m_dot{}, m_str{}, m_min{}, m_life{}, m_count{}, m_time{} {}

        void clear() {
            m_dot = false;
            m_str.clear();
            m_min.reset();
        }

        void set_str(std::string&& str) {
            clear();
            m_life = User;

            // (Ideally should take account of wrap-len; need `CalcWordWrapPosition()` & skip blanks and '\n'.)
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

        void set_dot() {
            clear();
            m_life = Auto;
            m_dot = true;
        }

        void set_auto_disappear() { m_life = Auto; }
        void set_once() { m_life = Once; }

        // Won't interfere with normal tooltips or popups.
        void display_if_present() {
            if (!m_dot && m_str.empty()) {
                return;
            }
            const auto now = clockT::now();
            if (m_min) {
                if (const ImVec2 delta = ImGui::GetIO().MouseDelta; delta.x || delta.y) {
                    --m_count;
                }
                const bool t_expired = now > m_time;
                const bool c_expired = m_count < 0;
                if (m_life == Once || (m_life == Auto ? (c_expired || t_expired) : (c_expired && t_expired))) {
                    clear();
                    return;
                }
            }

            assert(m_dot || !m_str.empty());
            if (m_dot) {
                assert(m_life != User);
                if (!m_min) {
                    m_count = 12;
                    m_time = now + std::chrono::milliseconds(400);
                    // TODO: `GetMousePos` may remain valid (with unclamped values) when mouse is moved outside the program...
                    m_min = ImGui::IsMousePosValid() ? ImGui::GetMousePos() : ImGui::GetStyle().WindowPadding;
                }
                assert(m_time >= now);
                const float radius =
                    4.0f * std::chrono::floor<std::chrono::milliseconds>(m_time - now).count() / 400.0f;
                ImGui::GetForegroundDrawList()->AddCircleFilled(*m_min - ImVec2(1, 1), radius,
                                                                IM_COL32(0, 255, 0, 255) /*light green*/);
            } else {
                const float text_wrap = wrap_len();
                const char *const text_beg = m_str.c_str(), *const text_end = text_beg + m_str.size();
                const ImVec2 window_padding = ImGui::GetStyle().WindowPadding;
                const ImVec2 window_size =
                    ImGui::CalcTextSize(text_beg, text_end, false, text_wrap) + window_padding * 2;
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
                ImDrawList& drawlist = *ImGui::GetForegroundDrawList();
                drawlist.AddRectFilled(window_min, window_max, ImGui::GetColorU32(ImGuiCol_PopupBg));
                drawlist.AddRect(window_min, window_max, ImGui::GetColorU32(ImGuiCol_Border));
                drawlist.AddText(nullptr, 0.0f, window_min + window_padding, ImGui::GetColorU32(ImGuiCol_Text),
                                 text_beg, text_end, text_wrap);
            }
        }
    };

    inline static messageT m_msg;

public:
    static void set_auto_disappear() { m_msg.set_auto_disappear(); }
    static void set_once() { m_msg.set_once(); } // To emulate tooltips.

    static void set_msg(std::string str) { m_msg.set_str(std::move(str)); }

    template <class... U>
    static void set_msg(std::format_string<const U&...> fmt, const U&... args) {
        m_msg.set_str(std::format(fmt, args...));
    }

    // TODO: looks strange when triggered by shortcuts... (Should use item pos instead of mouse pos...)
    static void dot() { m_msg.set_dot(); }
    static void dot_if(bool d) {
        if (d) {
            m_msg.set_dot();
        }
    }

    static void display_if_present(frame_main_token) { m_msg.display_if_present(); }
};

// TODO: interval -> frequency (fps) instead?
class global_timer : no_create {
    static constexpr int unit = 25;                             // ms.
    static constexpr int min_interval = 0, max_interval = 1000; // ms.
    static_assert(max_interval % unit == 0);

    using clockT = std::chrono::steady_clock;
    struct termT {
        clockT::time_point next;   // = {};
        bool active_at_this_frame; // = false; (Will cause trouble when building with gcc or clang...)
    };
    inline static termT terms[1 + (max_interval / unit)]{};

public:
    static void begin_frame(frame_main_token) {
        const clockT::time_point now = clockT::now();
        for (int i = 0; termT & term : terms) {
            const int this_i = i++;
            if (now >= term.next) {
                term.next = now + std::chrono::milliseconds(unit * this_i);
                term.active_at_this_frame = true;
            } else {
                term.active_at_this_frame = false;
            }
        }
    }

    // 0: will return true every frame.
    static constexpr int default_interval = init_zero_interval ? 0 : unit;

    static void step_slide(const char* label, int& interval /*ms*/, int min, int max) {
        assert(min_interval <= min && min < max && max <= max_interval);
        assert(min % unit == 0 && max % unit == 0);
        imgui_StepSliderInt::fn(label, &interval, min, max, unit, [](int i) { return std::format("{} ms", i); });
    }

    // TODO: (how to) translate time-interval to frame-interval?
    // (Frame interval is more stable but less user-friendly...)
    static constexpr bool debug_frame_interval = false;
#if 1
    // Unstable (in terms of frame interval) when the time interval is close to a multiple of frame duration...
    static bool test(int interval /*ms*/) {
        assert(min_interval <= interval && interval <= max_interval);
        assert(interval % unit == 0);
        return terms[std::clamp(interval, min_interval, max_interval) / unit].active_at_this_frame;
    }
#else
    // Relying on the stability of `Framerate`, which can be unstable when:
    // 1. Frame-capping fails to work, or the required calc time exceeds frame duration limit.
    // 2. The program's title bar is held for some time (when released, `Framerate` will need many frames to re-stabilize).
    // (Related: https://github.com/libsdl-org/SDL/issues/1059)
    static bool test(int interval /*ms*/) {
        const int frame = ImGui::GetFrameCount();
        const int fps = frame < 60 ? 60 : std::round(ImGui::GetIO().Framerate);
        const int i = (interval * fps + 999) / 1000; // ~ ceil(interval / (1000 / fps))
        return i <= 1 ? true : (frame % i) == 0;
    }
#endif
};

// Preview rules.
// TODO: support pausing groups/globally?
// TODO: support dumping all rules in display?
class previewer : no_create {
public:
    enum { default_settings };

    class configT {
        friend class previewer;
        float zoom_ = 1;
        int width_ = 220;
        int height_ = 160;

        int step = 1;
        int interval = global_timer::default_interval;

        void _set();

    public:
        /*implicit*/ configT(decltype(default_settings)) {}

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
            // imgui_ItemRectFilled(IM_COL32_BLACK);
            imgui_ItemRect(default_border_color());

            if (str && *str != '\0') {
                imgui_ItemStr(ImGui::GetColorU32(ImGuiCol_TextDisabled), str);
            }
        }
    }

    // (Note: type-erasure doesn't apply here.)
    // (`const ruleT&()` cannot adapt `ruleT()` calls, while `ruleT()` is unnecessarily costly for `const ruleT&()` calls.)
    // const auto& rule ~ implicitly convertible to const ruleT&.
    // true ~ called `_preview()`.
    static bool preview(const uint32_t id, const configT& config, const auto& rule) {
        ImGui::PushID(id);
        ImGui::InvisibleButton("Preview", config.size_imvec());
        ImGui::PopID();
        if (!ImGui::IsItemVisible()) {
            return false;
        } else if (!imgui_IsItemPartiallyVisible(0.15f)) {
            // imgui_ItemRectFilled(IM_COL32_BLACK);
            imgui_ItemRect(default_border_color());
            return false;
        } else {
            const uint64_t id2 = (uint64_t(ImGui::GetItemID()) << 32) | id;
            _preview(id2, config, rule);
            return true;
        }
    }

    // true ~ called `_preview()`.
    static bool preview_or_dummy(const uint32_t id, const configT& config, const auto* rule) {
        if (rule) {
            return preview(id, config, *rule);
        } else {
            dummy(config);
            return false;
        }
    }

    static ImU32 default_border_color() {
        return ImGui::GetColorU32(ImGuiCol_TableBorderStrong); // Instead of `ImGuiCol_Border`
    }

    static void begin_frame(frame_main_token);

private:
    struct _global_data;

    static void _preview(uint64_t id, const configT& config, const aniso::ruleT& rule);

    static void _show_belongs(const aniso::ruleT& rule);
};

// TODO: support highlighting rule sources?
class pass_rule : no_create {
    inline static aniso::ruleT rule{};

    static void render_rect(const bool bright) {
        // TODO: whether to set (or require) `WriteAccessed`?
        ImGui::PushStyleColor(ImGuiCol_DragDropTarget, IM_COL32(0, 128, 255, bright ? 255 : 64));
        ImGui::RenderDragDropTargetRect(imgui_GetItemRect(), GImGui->CurrentWindow->ClipRect);
        ImGui::PopStyleColor();
    }

    static constexpr const char* type = "rule";
    static bool active() {
        const auto* payload = ImGui::GetDragDropPayload();
        return payload && payload->IsDataType(type);
    }

public:
    static constexpr bool right_click_to_cancel = true;

    // Cannot be begin-frame (otherwise will interfere with r-click operations of other items e.g. popups).
    static void end_frame(frame_main_token) {
        if constexpr (right_click_to_cancel) {
            if (active() && ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
                // According to: https://github.com/ocornut/imgui/issues/9071
                ImGui::ClearActiveID();
                ImGui::ClearDragDrop();
            }
        }
    }

    static const aniso::ruleT* peek() { return active() ? &rule : nullptr; }

    static bool source(const aniso::ruleT& r) {
        const ImGuiID id = ImGui::GetItemID();
        assert(id != 0);

        const auto* payload = ImGui::GetDragDropPayload();
        if ((payload && payload->SourceId == id) || (!payload && ImGui::IsItemActive() && !ImGui::IsItemHovered())) {
            if (ImGui::BeginDragDropSource(ImGuiDragDropFlags_SourceNoHoldToOpenOthers |
                                           ImGuiDragDropFlags_SourceNoPreviewTooltip)) {
                char dummy = 0;
                ImGui::SetDragDropPayload(type, &dummy, sizeof(dummy));
                ImGui::EndDragDropSource();
                render_rect(true);

                lock_scroll();
                rule = r;
                assert(!ImGui::IsItemHovered()); // Blocked by drag-source.
                return true;
            }
        }
        return false;
    }

private:
    // TODO: experimental; should finally be enum-based...
    inline static int extra_rule_id{};
    inline static aniso::ruleT extra_rule{};

public:
    struct passT {
        const aniso::ruleT* hov = nullptr;
        const aniso::ruleT* deliv = nullptr;

        passT() = default;
        passT(const aniso::ruleT* r, bool hov, bool deliv) : hov{hov ? r : nullptr}, deliv{deliv ? r : nullptr} {}
        passT(const aniso::ruleT&) = delete; // Too easy to dangle.
        passT(const aniso::ruleT* r) : hov{nullptr}, deliv{r} {}

        const aniso::ruleT* rule() const {
            assert_implies(hov && deliv, hov == deliv);
            return hov ? hov : deliv;
        }

        // (Using _ForTooltip for stable visual.)
        bool hov_for_tooltip() const { //
            return hov && imgui_IsItemHoveredForTooltip(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem);
        }
    };

    static void set_extra(const aniso::ruleT& r, const int rule_id) {
        assert(rule_id != 0);
        extra_rule = r;
        extra_rule_id = rule_id;
    }

    static passT get_extra(const int rule_id) {
        assert(rule_id != 0);
        if (rule_id == extra_rule_id) {
            extra_rule_id = 0;
            return &extra_rule;
        }
        return {};
    }

    [[nodiscard]] static passT dest(const bool accept_drop = true, const int rule_id = 0) {
        assert(accept_drop || rule_id != 0);
        if (rule_id != 0 && rule_id == extra_rule_id) {
            extra_rule_id = 0;
            return &extra_rule;
        } else if (accept_drop && ImGui::IsItemVisible() && active()) {
            if (ImGui::BeginDragDropTarget()) {
                const bool deliv = ImGui::AcceptDragDropPayload(type, ImGuiDragDropFlags_AcceptNoPreviewTooltip |
                                                                          ImGuiDragDropFlags_AcceptNoDrawDefaultRect);
                ImGui::EndDragDropTarget();
                render_rect(true);
                if constexpr (0) {
                    if (deliv) {
                        ImGui::SetWindowFocus();
                    }
                }
                return {&rule, /*hov=*/true, deliv};
            } else {
                render_rect(false);
            }
        }
        return {};
    }
};

inline bool check_diff_no_msg = false; // (Workaround to suppress msg in one place.)
inline bool check_diff(const pass_rule::passT& pass, const aniso::ruleT& cmp) {
    if (const auto* rule = pass.rule(); rule && *rule == cmp) {
        if (!check_diff_no_msg) {
            const bool hov = pass.hov_for_tooltip();
            if (hov || pass.deliv) {
                messenger::set_msg("Same rule.");
                if (hov) {
                    messenger::set_once();
                }
                // else { messenger::set_auto_disappear(); }
            }
        }
        return false;
    }
    return true;
}

inline bool set_clipboard(const std::string& str) {
    if (str.empty()) {
        messenger::set_msg("Empty.");
        // messenger::set_auto_disappear();
        return false;
    } else if (str.find('\0') != str.npos) {
        // This can happen when the user tries to copy lines in a data file.
        // If copied, the result will be incomplete, and nothing in worst case (if starts with '\0').
        messenger::set_msg("Cannot copy. (The text contains null characters.)");
        return false;
    } else {
        ImGui::SetClipboardText(str.c_str());
        return true;
    }
}

inline void set_clipboard_and_notify(const std::string& str) {
    if (set_clipboard(str)) {
        messenger::set_msg("Copied.");
        messenger::set_auto_disappear();
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

class input_int : no_copy {
    static constexpr int max_digit = std::numeric_limits<int>::digits10;
    char m_buf[max_digit + 1 /*'\0'*/]{};

public:
    void clear() { m_buf[0] = '\0'; }

    std::optional<int> flush() {
        if (m_buf[0] != '\0') {
            int v = 0;
            const bool has_val = std::from_chars(m_buf, std::end(m_buf), v).ec == std::errc{};
            m_buf[0] = '\0';
            if (has_val) {
                return v;
            }
        }
        return std::nullopt;
    }

    std::optional<int> input(int digit, const char* label, const char* hint = nullptr) {
        assert(1 <= digit && digit <= max_digit);
        constexpr auto input_flags = ImGuiInputTextFlags_CallbackCharFilter | ImGuiInputTextFlags_EnterReturnsTrue;
        constexpr auto input_filter = [](ImGuiInputTextCallbackData* data) -> int {
            return (data->EventChar >= '0' && data->EventChar <= '9') ? 0 : 1;
        };

        if (input_text(label, {m_buf, m_buf + digit + 1 /*'\0'*/}, hint, input_flags, input_filter)) {
            return flush();
        }
        return std::nullopt;
    }
};

// TODO: whether to apply this to rec_for_rule(_b)?
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
    previewer::configT m_settings{previewer::default_settings};

    dataT m_data{};
    int m_pos{}; // !empty() -> ∈ [0, m_data.size())
    bool m_updated{};
    // std::string m_source{}; // !!TODO: show source...

public:
    explicit rule_snapshot() = default;

    bool empty() const { return m_data.empty(); }
    explicit operator bool() const { return !empty(); }
    void clear() { m_data.clear(); }

    void update(dataT data, const previewer::configT& settings) {
        assert(!data.empty());
        if (!data.empty()) { // Defensive.
            m_settings = settings;
            m_data = std::move(data);
            m_pos = m_data.size() - 1;
            m_updated = true;
        }
    }

    void display_if_present(const char* title) {
        if (m_data.empty()) {
            return;
        }

        // TODO: the effect should be more obvious...
        if (std::exchange(m_updated, false)) {
            ImGui::SetNextWindowCollapsed(false);
            ImGui::SetNextWindowFocus();
        }
        imgui_CenterNextWindow(ImGuiCond_FirstUseEver);

        bool open = true;
        if (auto window =
                imgui_Window(title, &open, ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_AlwaysAutoResize)) {
            const int total = m_data.size();
            assert(0 <= m_pos && m_pos < total);
            const auto set_pos = [&](const int pos) { m_pos = std::clamp(pos, 0, total - 1); };
            switch (sequence::seq("<|", "<##Prev", ">##Next", "|>")) {
                case 0: set_pos(0); break;
                case 1: set_pos(m_pos - 1); break;
                case 2: set_pos(m_pos + 1); break;
                case 3: set_pos(INT_MAX); break;
            }

            ImGui::SameLine();
            m_settings.set("Settings");
            ImGui::SameLine();
            ImGui::Text("Rules:%d At:%d", total, m_pos + 1);

            previewer::preview(0, m_settings, m_data[m_pos]);
        }
        if (!open) {
            clear();
        }
    }
};

class rec_for_rule : no_copy {
    using dataT = std::vector<aniso::compressT>;
    int m_capacity;
    dataT m_data;

public:
    explicit rec_for_rule(const int cap = 20) : m_capacity(cap) {
        assert(cap > 0 && cap < 100);
        m_data.reserve(m_capacity);
    }

    const dataT& data() const { return m_data; }
    bool empty() const { return m_data.empty(); }
    int size() const { return m_data.size(); }
    auto find(const aniso::compressT& rule) const { return std::ranges::find(m_data, rule); }
    bool contains(const aniso::compressT& rule) const { return find(rule) != m_data.end(); }

    // LRU; inefficient but no problem as `m_capacity` is small enough.
    // back() ~ most recent.
    void add(const aniso::compressT& rule) {
        if (!m_data.empty() /*asked by gcc*/) {
            if (const auto pos = find(rule); pos != m_data.end()) {
                m_data.erase(pos);
            } else if (m_data.size() == m_capacity) {
                m_data.erase(m_data.begin());
            }
        }
        m_data.push_back(rule);
    }

    static constexpr const char* about_dump = "Recheck recent rules.";

    // (Workaround; defined here for convenience.)
    void dump(const previewer::configT& settings) const {
        assert(!empty());
        snapshot.update(data(), settings);
    }

    static void display_if_present(frame_main_token, const char* label) { //
        snapshot.display_if_present(label);
    }

private:
    inline static rule_snapshot snapshot{}; // (Only) gcc requires {} here due to explicit default ctor (likely a bug)
};

// !!TODO: support cursor (undoing/redoing)? (But how to update the record when adding new rules?)
class rule_with_rec : no_copy {
    aniso::ruleT m_rule{};
    rec_for_rule m_rec{};

public:
    explicit rule_with_rec() = default;
    /*implicit*/ rule_with_rec(const aniso::ruleT& r) : m_rule{r} { m_rec.add(r); }

    bool assigned() const { return !m_rec.empty(); }

    operator const aniso::ruleT&() const = delete; // -> get()
    const aniso::ruleT& get() const {
        assert(assigned()); // Otherwise, it's all-0 rule and is likely a bug.
        return m_rule;
    }
    void set(const aniso::ruleT& r) {
        m_rule = r;
        m_rec.add(r);
    }

    const rec_for_rule& rec() const { return m_rec; }
};

// TODO: support recording copied rules? (The problem is, where to expose in UI...)
class copy_rule : no_create {
    static void save(const aniso::ruleT& rule);
    // inline static rec_for_rule record;

public:
    static void copy(const aniso::ruleT& rule) {
        set_clipboard_and_notify(aniso::to_MAP_str(rule));
        // record.add(rule);
        if constexpr (debug_mode) { // !!TODO: support in release mode...
            save(rule);
        }
    }
};

class test_active {
    int m_frame = -2;

public:
    void update() { m_frame = ImGui::GetFrameCount(); }

    explicit operator bool() const {
        const int frame = ImGui::GetFrameCount();
        return m_frame == frame || m_frame + 1 == frame;
    }
};

// `ImGui::IsWindowAppearing()` is usually not enough. (For example, appearing when the window is uncollapsed.)
class test_appearing {
    int m_frame = -2;
    bool m_appearing = false;

public:
    bool update() {
        const int frame = ImGui::GetFrameCount();
        m_appearing = m_frame + 1 != frame;
        m_frame = frame;
        return m_appearing;
    }

    explicit operator bool() const { return m_appearing; }
    void reset_if_appearing(bool& flag) const {
        if (m_appearing) {
            flag = false;
        }
    }
};

// (Horrible workaround for send-to operation...)
// (Should be plain class... temporarily template to avoid breaking code...)
// TODO: should refactor away...
template <int id>
class item_status : no_create {
    inline static test_active active;
    inline static bool disabled = false;

public:
    static void update() { active.update(); }
    static void begin_disabled() { disabled = true; }
    static void end_disabled() { disabled = false; }

    static bool available() { return active && !disabled; }
    static constexpr int rule_id = id; // For pass_rule::set_extra
};

using random_access_status = item_status<1>; // TODO: -> rule_editor_status.
using pattern_editor_status = item_status<2>;
bool has_pattern(std::string_view text);
void load_pattern(std::string_view text);

void load_capture(const aniso::ruleT& r, const aniso::lockT& l);
