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

    static_assert(std::ranges::equal(
        a, b, [](auto l, auto r) { return static_cast<unsigned char>(l) == static_cast<unsigned char>(r); }));
}

// Experience in MSVC
// It turns out that there are still a lot of messy encoding problems even if `/utf-8` is specified.
// For example, how is main's argv / fs::path.string() / exception.what() / ... encoded?
// (Well, are there even guarantees that we can get a valid fs::path from main's argv?)
// To make things easy the program does not try to deal with these strings.
#endif

struct no_create {
    no_create() = delete;
};

template <class T, class U>
inline bool compare_update(T& t, const U& u) {
    if (t == u) {
        return false;
    } else {
        t = u; // -> assert(t == u);
        return true;
    }
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

class rand_source : no_create {
    static uint32_t seed() { return time(0); }

public:
    static std::mt19937 create() { return std::mt19937{seed()}; }
    static void perturb(std::mt19937& rand) { rand.discard(1 + (seed() % 16)); }
};

class rule_recorder : no_create {
public:
    enum typeE : int { Current, Copied, RandomAccess, Ignore };

    static void record(typeE type, const aniso::ruleT& rule, const aniso::ruleT* from = nullptr);
    static void load_record(sync_point&);
};

class rule_algo : no_create {
public:
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
[[nodiscard]] ImTextureID make_screen(aniso::_misc::tile_ref_<const aniso::cellT> tile, scaleE scale);

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

// TODO: should finally be configurable in the program.
// inline const bool init_maximize_window = false;
inline const bool init_zero_interval = false;
inline const bool init_random_access_preview_mode = false;
inline const bool compact_mode = false;

// TODO: consider using ImGui::Shortcut?
// Some features cannot easily be satisfied with `ImGui::Shortcut` and `ImGui::SetNextItemShortcut`.
class shortcuts : no_create {
public:
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

    static bool test_down(ImGuiKey key) { //
        return filter(key) && ImGui::IsKeyDown(key);
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

class guide_mode : no_create {
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
#if 0
    // For use in combination with other tooltips.
    static bool enabled() { return enable_tooltip; }

    // It's getting unclear what should really be highlighted...
    static void highlight() {
        if (enable_tooltip) {
            _highlight();
        }
    }
#endif

    static bool item_tooltip(const std::string_view tooltip) {
        if (enable_tooltip) {
            _highlight();
            return imgui_ItemTooltip([tooltip] {
                if (ImGui::GetCurrentWindowRead()->BeginCount > 1) {
                    ImGui::Separator();
                }
                imgui_Str(tooltip);
            });
        } else {
            imgui_ItemTooltip_StrID = nullptr;
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

// There is intended to be at most one call to this function in each window hierarchy.
inline void set_scroll_by_up_down(float dy) {
    if (may_scroll() && shortcuts::keys_avail() && shortcuts::window_focused()) {
        if (shortcuts::test(ImGuiKey_DownArrow, true)) {
            ImGui::SetScrollY(ImGui::GetScrollY() + dy);
            shortcuts::highlight(ImGui::GetWindowScrollbarID(ImGui::GetCurrentWindowRead(), ImGuiAxis_Y));
        } else if (shortcuts::test(ImGuiKey_UpArrow, true)) {
            ImGui::SetScrollY(ImGui::GetScrollY() - dy);
            shortcuts::highlight(ImGui::GetWindowScrollbarID(ImGui::GetCurrentWindowRead(), ImGuiAxis_Y));
        }
    }
}

inline void global_tooltip(const bool highlight, const std::invocable<> auto& func) {
    // TODO: are there simpler ways to prevent inheriting styles?
    // const ImGuiStyle old_style = GImGui->Style;
    // auto old_stack = GImGui->StyleVarStack;
    // ImGui::PopStyleVar(GImGui->StyleVarStack.size()); // Restore style vars.

    if (highlight) {
        ImGui::PushStyleColor(ImGuiCol_Border, IM_COL32(0, 128, 255, 255));
    }
    // (The position will be finally rounded in `SetWindowPos`.)
    ImGui::SetNextWindowPos(ImGui::GetMainViewport()->GetCenter(), ImGuiCond_Always, {0.5, 0.5});
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

// Looks like a common popup, and will appear like a menu (but with more consistent closing behavior).
// (Can be called recursively.)
// (This could be made into an RAII class, but I find that would be even harder to name properly...)
inline bool begin_popup_for_item() {
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
        if (!ImGui::IsWindowContentHoverable(ImGui::GetCurrentWindowRead())) {
            return true;
        }

        // Topmost popup.
        ImGui::NavHighlightActivated(item_id);

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
        }
        return true;
    }
    return false;
}

class rclick_popup {
    inline static ImGuiID bound_id = 0, bound_id_next = 0;

    friend void frame_main();
    static void begin_frame() { bound_id = std::exchange(bound_id_next, 0); }

public:
    struct idT {
        ImGuiID id;
        /*implicit*/ operator ImGuiID() const { return id; }

        // idT() : id{ImGui::GetItemID()} { assert(id); }
        /*implicit*/ idT(ImGuiID id) : id{id} { assert(id); }
        /*implicit*/ idT(const char* str_id) : id{ImGui::GetID(str_id)} { assert(id); }
    };

    using highlight_fn = void (*)(bool popup);
    static void default_highlight(bool popup) { //
        imgui_ItemUnderline(ImGui::GetColorU32(popup ? ImGuiCol_Text : ImGuiCol_TextDisabled));
    }

    // TODO: improve highlighting logic; should this be callback or RAII class? (& `begin_popup_for_item`)
    // Not meant to be used recursively.
    template <highlight_fn highlight = default_highlight>
    static void popup(const idT id, const std::invocable<> auto& fn) {
        const bool hovered = ImGui::IsItemHovered();
        if (!hovered && id != bound_id) {
            return;
        }

        highlight(bound_id == id);

        // (Avoid creating windows for one-off usage.)
        constexpr const char* shared_popup = "Shared-popup";
        assert_implies(!bound_id, !ImGui::IsPopupOpen(shared_popup));
        // (This doesn't break assertion, which means skipping `BeginPopup` is able to close the popup as well.)
        // if (ImGui::IsKeyDown(ImGuiKey_Q)) {
        //     return;
        // }

        // TODO: will be blocked by input field (not ideal, but not a big problem either...)
        if (!bound_id && hovered && !ImGui::IsAnyItemActive() && ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
            ImGui::OpenPopup(shared_popup);
            bound_id = id;
        }

        if (bound_id == id) {
            if (ImGui::BeginPopup(shared_popup)) {
                bound_id_next = id;

                lock_scroll();
                fn();
                ImGui::EndPopup();
            }
            // else: `bound_id` will become 0 next frame.
        }
    }
};

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

class sequence : no_create {
    // Workaround to avoid previewed rule being changed by shortcut.
    static bool extra_cond() { return may_scroll(); }

    inline static ImGuiID bound_id = 0;
    inline static ImGuiID bound_id_next = 0;

    friend void frame_main();
    static void begin_frame() { bound_id = std::exchange(bound_id_next, 0); }

public:
    // (`disable_prev_next` is a workaround for a sequence in `edit_rule`.)
    // 0:first, 1:prev, 2:next, 3:last
    static int seq(const char* label_first, const char* label_prev, const char* label_next, const char* label_last,
                   const char* const disable_prev_next = nullptr) {
        enum tagE : int { None = -1, First, Prev, Next, Last };
        tagE tag = None;

        if (ImGui::Button(label_first)) {
            tag = First;
        }
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());

        if (disable_prev_next) {
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
            bound_id_next = bound_id;
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
        if (disable_prev_next) {
            ImGui::EndGroup();
            ImGui::EndDisabled();
            if (!imgui_TestItemFlag(ImGuiItemFlags_Disabled)) {
                imgui_ItemTooltip(disable_prev_next);
            }
        }

        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        if (ImGui::Button(label_last)) {
            tag = Last;
        }

        if ((tag != None) ||
            (bound_id == 0 && window_focused && !pair_disabled && shortcuts::keys_avail() && extra_cond() &&
             (shortcuts::test(ImGuiKey_LeftArrow) || shortcuts::test(ImGuiKey_RightArrow)))) {
            bound_id_next = id_prev;
        }

        return tag;
    }
};

class imgui_StepSliderShortcuts : no_create {
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

    return compare_update(*v, std::clamp(v2, v_min, v_max));
}

// `v_str` should be the direct string for `v`, instead of a format str.
inline bool imgui_StepSliderIntEx(const char* label, int* v, int v_min, int v_max, int v_step, const char* v_str) {
    assert(v_min < v_max && v_step > 0 && ((v_max - v_min) % v_step) == 0);
    const int u_max = (v_max - v_min) / v_step;
    int u = std::clamp((*v - v_min) / v_step, 0, u_max);
    imgui_StepSliderInt(label, &u, 0, u_max, v_str);

    return compare_update(*v, u * v_step + v_min);
}

class messenger : no_create {
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
                if (m_str.size() < 15 ? (c_expired || t_expired) : (c_expired && t_expired)) {
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
    static void set_msg(std::string str) { m_msg.set(std::move(str)); }

    template <class... U>
    static void set_msg(std::format_string<const U&...> fmt, const U&... args) {
        m_msg.set(std::format(fmt, args...));
    }

    // Managed by `frame_main`.
    static void display() { m_msg.display(); }
};

// Preview rules.
class previewer : no_create {
public:
    class configT {
        friend previewer;
        float zoom_ = 1;
        int width_ = 220;
        int height_ = 160;

        int seed = 0;
        int step = 1;

        void _set();
        void _reset_size_zoom() {
            zoom_ = 1;
            width_ = 220;
            height_ = 160;
            // seed = 0;
            // step = 1;
        }

    public:
        // TODO: temporarily preserved to avoid breaking existing calls.
        enum sizeE { _220_160 = 0 };
        configT(sizeE) {}

        int width() const { return width_; }
        int height() const { return height_; }
        ImVec2 size_imvec() const { return ImVec2(width_, height_); }

        void set(const char* label) {
            ImGui::Button(label);
            if (begin_popup_for_item()) {
                _set();
                ImGui::EndPopup();
            }
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

    static void preview(uint32_t id, const configT& config, const aniso::ruleT& rule) {
        ImGui::Dummy(config.size_imvec());
        if (ImGui::IsItemVisible()) {
            _preview(_get_id(id), config, rule);
        }
    }

    static void preview(uint32_t id, const configT& config, const std::invocable<> auto& get_rule) {
        ImGui::Dummy(config.size_imvec());
        if (ImGui::IsItemVisible()) {
            _preview(_get_id(id), config, get_rule());
        }
    }

    static ImU32 default_border_color() {
        return ImGui::GetColorU32(ImGuiCol_TableBorderStrong); // Instead of `ImGuiCol_Border`
    }

private:
    static uint64_t _get_id(uint32_t id) {
        return (uint64_t(ImGui::GetID("")) << 32) | id;
        // TODO: consider using window id?
        // return (uint64_t(ImGui::GetCurrentWindowRead()->ID) << 32) | id;
    }

    friend void frame_main();
    static void begin_frame();

    static void _preview(uint64_t id, const configT& config, const aniso::ruleT& rule);

    // TODO: declared here for minimal exposure, but looks strange...
    static void _identify_rule(const aniso::ruleT& rule);
};

class sync_point {
    friend void frame_main();

    std::optional<aniso::ruleT> out_rule = std::nullopt;
    rule_recorder::typeE rec_type = rule_recorder::Ignore;

    sync_point(const aniso::ruleT& rule) : rule{rule} {}

public:
    sync_point(const sync_point&) = delete;
    sync_point& operator=(const sync_point&) = delete;

    const aniso::ruleT rule;

    void set(const aniso::ruleT& rule, rule_recorder::typeE type = rule_recorder::Ignore) {
        out_rule.emplace(rule);
        rec_type = type;
    }
};

inline void set_clipboard_and_notify(const std::string& str) {
    if (str.empty() || str.find('\0') == str.npos) {
        // TODO: whether to copy when str is empty?
        ImGui::SetClipboardText(str.c_str());
        messenger::set_msg("Copied.");
    } else {
        // This can happen when the user opens a data file and tries to copy lines.
        // If copied, the result will be incomplete, and nothing in worst case (if starts with '\0').
        messenger::set_msg("The text contains null characters.");
    }
}

inline void set_msg_cleared(bool /*has_effect*/) {
    // if (has_effect) {
    messenger::set_msg("Cleared.");
    // }
}

// It's not obvious whether `ImGui::GetClipboardText` can return nullptr, and
// in some cases the function returns empty string "" for errors...
inline std::string_view read_clipboard() {
    const char* str = ImGui::GetClipboardText();
    if (!str || *str == '\0') {
        // If the clipboard contains a real empty string, the result is also "".
        // messenger::set_msg("Failed to read from the clipboard.");
        return {};
    }
    return str;
}

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

        if (hint ? ImGui::InputTextWithHint(label, hint, buf, std::size(buf), input_flags, input_filter)
                 : ImGui::InputText(label, buf, std::size(buf), input_flags, input_filter)) {
            return flush();
        }
        return std::nullopt;
    }
};
