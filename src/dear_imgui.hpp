#pragma once

#include <algorithm>
#include <optional>
#include <string>

#include "imgui.h"
#include "imgui_internal.h"

#include "utils.hpp"

// Follows `IM_COL32_XX`; note that `constexpr` cannot guarantee `fn(100, 255)` be
// calculated at compile time, especially in debug mode.
consteval ImU32 IM_COL32_GREY(ImU8 v, ImU8 alpha) { return IM_COL32(v, v, v, alpha); }

inline ImRect imgui_GetItemRect() { return GImGui->LastItemData.Rect; }

inline ImRect imgui_GetWindowRect() {
    const ImVec2 window_min = ImGui::GetWindowPos(), window_max = window_min + ImGui::GetWindowSize();
    return {window_min, window_max};
}

// (Referring to `ImGui::GetContentRegionAvail`.)
// (Returning optional as `GetContentRegionAvail` may return negative values.)
inline std::optional<ImRect> imgui_GetAvailRect() {
    const auto& window = *GImGui->CurrentWindow;
    const ImVec2 pos_min = window.DC.CursorPos;
    const ImVec2 pos_max =
        (window.DC.CurrentColumns || GImGui->CurrentTable) ? window.WorkRect.Max : window.ContentRegionRect.Max;
    if (pos_min.x < pos_max.x && pos_min.y < pos_max.y) {
        return ImRect{pos_min, pos_max};
    } else {
        return std::nullopt;
    }
}

inline ImGuiID imgui_GetItemPosID() { //
    return GImGui->CurrentWindow->GetIDFromPos(ImGui::GetItemRectMin());
}

inline ImGuiID imgui_GetItemIDNonZero() {
    const ImGuiID id = ImGui::GetItemID();
    return id ? id : imgui_GetItemPosID();
}

inline void imgui_FillAvailRect(ImU32 col) {
    if (const auto rect = imgui_GetAvailRect()) {
        const auto [pos_min, pos_max] = *rect;
        ImGui::GetWindowDrawList()->AddRectFilled(pos_min, pos_max, col);
    }
}

// These names are somewhat misleading after the introduction of `imgui_GetItemRect`...
inline void imgui_ItemRect(ImU32 col, ImVec2 off_min = {0, 0}) {
    const auto [pos_min, pos_max] = GImGui->LastItemData.Rect;
    ImGui::GetWindowDrawList()->AddRect(pos_min + off_min, pos_max - off_min, col);
}

inline void imgui_ItemRectFilled(ImU32 col, ImVec2 off_min = {0, 0}) {
    const auto [pos_min, pos_max] = GImGui->LastItemData.Rect;
    ImGui::GetWindowDrawList()->AddRectFilled(pos_min + off_min, pos_max - off_min, col);
}

inline void imgui_ItemUnderline(ImU32 col) {
    const auto [pos_min, pos_max] = GImGui->LastItemData.Rect;
    ImGui::GetWindowDrawList()->AddLine({pos_min.x, pos_max.y - 1}, {pos_max.x, pos_max.y - 1}, col);
}

// (Referring to ImGui::IsRectVisible() and ImGui::GetItemRectMin().)
inline bool imgui_IsItemFullyVisible() { //
    return GImGui->CurrentWindow->ClipRect.Contains(GImGui->LastItemData.Rect);
}

inline bool imgui_IsItemPartiallyVisible(const float least) {
    ImRect rect = GImGui->LastItemData.Rect;
    const float full_area = rect.GetArea();
    rect.ClipWithFull(GImGui->CurrentWindow->ClipRect);
    return rect.GetArea() >= full_area * least;
}

// `!ImGui::IsAnyItemActive() || ImGui::IsItemActive()`
inline bool imgui_IsItemOrNoneActive() { //
    return GImGui->ActiveId == 0 || GImGui->ActiveId == GImGui->LastItemData.ID;
}

inline bool imgui_IsBgHeld() { //
    return !ImGui::GetHoveredID() && GImGui->ActiveId == GImGui->CurrentWindow->MoveId;
}

inline bool imgui_IsWindowHoverable(ImGuiHoveredFlags flags = 0) { //
    return ImGui::IsWindowContentHoverable(GImGui->CurrentWindow, flags);
}

// ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows | ImGuiFocusedFlags_NoPopupHierarchy)
inline bool imgui_IsWindowFocused() {
    if (const ImGuiWindow* focused = GImGui->NavWindow) {
        if (const ImGuiWindow* current = GImGui->CurrentWindow) {
            return current->RootWindow == focused->RootWindow;
        }
    }
    return false;
}

// ImGui::IsPopupOpen(id, 0), i.e. open at the current BeginPopup() level.
inline bool imgui_IsPopupOpen(const ImGuiID id) {
    const auto& g = *GImGui;
    return g.OpenPopupStack.Size > g.BeginPopupStack.Size && g.OpenPopupStack[g.BeginPopupStack.Size].PopupId == id;
}

// (Referring to `ImGui::BeginPopupEx` and `ImGui::BeginPopup`.)
inline bool imgui_BeginPopupRecycled(const ImGuiID id, const ImGuiWindowFlags window_flags = 0) {
    if (!imgui_IsPopupOpen(id)) {
        GImGui->NextWindowData.ClearFlags();
        return false;
    }

    constexpr ImGuiWindowFlags extra_flags = ImGuiWindowFlags_Popup | ImGuiWindowFlags_AlwaysAutoResize |
                                             ImGuiWindowFlags_NoTitleBar | ImGuiWindowFlags_NoSavedSettings;
    const std::string name = "PopupEx_" + std::to_string(GImGui->BeginPopupStack.Size);
    if (ImGui::Begin(name.c_str(), nullptr, window_flags | extra_flags)) {
        assert(GImGui->CurrentWindow->BeginCount == 1); // Otherwise, likely due to id collision.
        if (GImGui->CurrentWindow->BeginCount == 1) [[likely]] {
            return true;
        }
    }
    ImGui::EndPopup();
    return false;
}

inline bool imgui_IsItemHoveredForTooltip(ImGuiHoveredFlags flags = 0) {
    return ImGui::IsItemHovered(ImGuiHoveredFlags_ForTooltip | flags);
}

// Workaround to provide stable hovering check for texts and groups.
// Related: https://github.com/ocornut/imgui/issues/7984 and 7945
[[deprecated]] inline bool imgui_IsItemHoveredForTooltipEx(const ImGuiID id) {
    // HoverFlagsForTooltipMouse ~ Stationary | DelayShort | AllowWhenDisabled
    if (!ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled)) {
        return false;
    }

    ImGuiContext& g = *GImGui;
    // if (g.HoverItemDelayIdPreviousFrame != id) { // ImGuiHoveredFlags_NoSharedDelay
    //     g.HoverItemDelayTimer = 0.0f;
    // }
    g.HoverItemDelayId = id;
    // ImGuiHoveredFlags_Stationary | ImGuiHoveredFlags_DelayShort
    return g.HoverItemUnlockedStationaryId == id && //
           g.HoverItemDelayTimer >= g.Style.HoverDelayShort;
}

inline bool imgui_ItemTooltip(const func_ref<void()> desc) {
    if (!GImGui->CurrentWindow->SkipItems && ImGui::BeginItemTooltip()) {
        // (Tooltips will be hidden for one extra frame before appearing.)
        const bool visible = !GImGui->CurrentWindow->Hidden;
        if (GImGui->CurrentWindow->BeginCount > 1) {
            ImGui::Separator();
        }
        // The same as the one in `HelpMarker` in "imgui_demo.cpp".
        ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
        desc();
        ImGui::PopTextWrapPos();
        ImGui::EndTooltip();
        return visible;
    }
    return false;
}

inline bool imgui_ItemTooltip(std::string_view desc) { //
    return imgui_ItemTooltip([desc] { ImGui::TextUnformatted(desc.data(), desc.data() + desc.size()); });
}

inline void imgui_ItemStr(ImU32 col, std::string_view str) {
    if (!str.empty()) {
        const char *begin = str.data(), *end = begin + str.size();
        const ImVec2 item_min = ImGui::GetItemRectMin();
        const ImVec2 item_size = ImGui::GetItemRectSize();
        const ImVec2 str_size = ImGui::CalcTextSize(begin, end);
        const ImVec2 pos(item_min.x + floor((item_size.x - str_size.x) / 2),
                         item_min.y + floor((item_size.y - str_size.y) / 2) - 1 /* -1 for better visual effect */);
        ImGui::GetWindowDrawList()->AddText(pos, col, begin, end);
    }
}

// Unlike ImGui::Text(Wrapped/...), these functions take unformatted string as the argument.
inline void imgui_Str(std::string_view str) { //
    ImGui::TextUnformatted(str.data(), str.data() + str.size());
}

inline void imgui_StrWrapped(std::string_view str, float min_len) {
    ImGui::PushTextWrapPos(ImGui::GetCursorPosX() + std::max(min_len, ImGui::GetContentRegionAvail().x));
    imgui_Str(str);
    ImGui::PopTextWrapPos();
}

inline void imgui_StrColored(std::string_view str, const ImVec4& col) {
    ImGui::PushStyleColor(ImGuiCol_Text, col);
    // const ImVec4 old_col = std::exchange(GImGui->Style.Colors[ImGuiCol_Text], col);
    imgui_Str(str);
    // GImGui->Style.Colors[ImGuiCol_Text] = old_col;
    ImGui::PopStyleColor();
}

// (Referring to `ImGui::TextLink`.)
inline void imgui_StrWithID(std::string_view str, const ImGuiID id) {
    assert(id != 0);
    // (All ImGui:: widgets set `WriteAccessed` (via `GetCurrentWindow`) before testing `SkipItems`...)
    GImGui->CurrentWindow->WriteAccessed = true;
    const auto& window = *GImGui->CurrentWindow;
    if (window.SkipItems) {
        return;
    }

    const char* const begin = str.data();
    const char* const end = begin + str.size();

    const ImVec2 pos = {window.DC.CursorPos.x, window.DC.CursorPos.y + window.DC.CurrLineTextBaseOffset};
    const ImVec2 size = ImGui::CalcTextSize(begin, end, false);
    const ImRect bb = {pos, pos + size};
    ImGui::ItemSize(size, 0.0f);
    if (!ImGui::ItemAdd(bb, id)) {
        return;
    }

    bool hovered, held;
    (void)ImGui::ButtonBehavior(bb, id, &hovered, &held);
    (void)hovered, (void)held;

    ImGui::RenderNavCursor(bb, id);
    ImGui::RenderText(bb.Min, begin, end, false);
    assert(ImGui::GetItemID() == id);
}

inline void imgui_StrWithID(std::string_view str) { //
    imgui_StrWithID(str, ImGui::GetID(str.data(), str.data() + str.size()));
}

inline void imgui_StrDisabled(std::string_view str) { //
    imgui_StrColored(str, ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled));
}

// Similar to `HelpMarker` in "imgui_demo.cpp".
inline bool imgui_StrTooltip(std::string_view str, const func_ref<void()> desc) {
    imgui_StrDisabled(str);
    return imgui_ItemTooltip(desc);
}

inline bool imgui_StrTooltip(std::string_view str, std::string_view desc) {
    imgui_StrDisabled(str);
    return imgui_ItemTooltip(desc);
}

// Related: https://github.com/ocornut/imgui/issues/5115
// `window->Name` may not be updated to Begin-name: https://github.com/ocornut/imgui/issues/8493
inline void imgui_StrTooltipForTitleBar(const std::string_view str, const std::string_view tooltip,
                                        const char* window_name) {
    ImGuiWindow& window = *ImGui::GetCurrentWindow();
    const bool old_skip = std::exchange(window.SkipItems, false); // Display regardless of whether collapsed.
    const auto [min, max] = window.TitleBarRect();
    ImGui::PushClipRect(min, max, false);
    {
        const bool has_collapse_button = !(window.Flags & ImGuiWindowFlags_NoCollapse);
        const ImVec2 old_pos = ImGui::GetCursorScreenPos();
        ImGui::SetCursorPos({ImGui::CalcTextSize(window_name, nullptr, true).x +
                                 ImGui::GetFrameHeight() * (has_collapse_button ? 2 : 1),
                             0});
        ImGui::AlignTextToFramePadding();
        imgui_StrColored(
            str, ImLerp(ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled), ImGui::GetStyleColorVec4(ImGuiCol_Text), 0.5));
        imgui_ItemTooltip(tooltip);
        ImGui::SetCursorScreenPos(old_pos);
    }
    ImGui::PopClipRect();
    window.SkipItems = old_skip;
}

// (Not general enough to add 'imgui' prefix...)
inline bool test_esc_single_hit() {
    static int frame = 0; // Avoid closing multiple windows within one frame.
    return !GImGui->IO.KeyCtrl && !GImGui->IO.WantCaptureKeyboard && imgui_IsWindowFocused() &&
           !ImGui::IsMouseDown(ImGuiMouseButton_Left) && imgui_IsWindowHoverable() &&
           ImGui::IsKeyPressed(ImGuiKey_Escape) && compare_update(frame, ImGui::GetFrameCount());
}

inline bool test_esc() { // Double-hit.
    if (test_esc_single_hit()) {
        static double last = 0;
        const double now = ImGui::GetTime();
        if (now < last + ImGui::GetIO().MouseDoubleClickTime) {
            last = 0;
            return true;
        }
        last = now;
    }
    return false;
}

class [[nodiscard]] imgui_Window : no_copy {
public:
    // (Without this, to show tooltip unconditionally, the window have to be declared outside of if scope.)
    inline static const char* next_window_titlebar_tooltip = nullptr;

    const bool visible;
    explicit imgui_Window(const char* name, bool* p_open = nullptr, ImGuiWindowFlags flags = {})
        : visible((assert_implies(p_open, *p_open), ImGui::Begin(name, p_open, flags))) {
        if (const char* tooltip = std::exchange(next_window_titlebar_tooltip, nullptr)) {
            imgui_StrTooltipForTitleBar("(?)", tooltip, name);
        }
        if (p_open && test_esc()) {
            *p_open = false;
        }
    }
    ~imgui_Window() {
        ImGui::End(); // Unconditional.
    }
    explicit operator bool() const { return visible; }
};

class [[nodiscard]] imgui_ChildWindow : no_copy {
public:
    const bool visible;
    explicit imgui_ChildWindow(const char* name, const ImVec2& size = {}, ImGuiChildFlags child_flags = {},
                               ImGuiWindowFlags window_flags = {})
        : visible(ImGui::BeginChild(name, size, child_flags, window_flags)) {}
    ~imgui_ChildWindow() {
        ImGui::EndChild(); // Unconditional.
    }
    explicit operator bool() const { return visible; }
};

inline void imgui_CenterNextWindow(ImGuiCond_ cond) {
    // (The position will be finally rounded in `SetWindowPos`.)
    ImGui::SetNextWindowPos(ImGui::GetMainViewport()->GetCenter(), cond, {0.5, 0.5});
}

template <class T>
    requires(!std::is_const_v<T>)
inline bool imgui_RadioButton(const char* label, T* v, std::type_identity_t<T> c) {
    assert(v != nullptr);
    if (ImGui::RadioButton(label, bool(*v == c))) {
        *v = c;
        return true;
    }
    return false;
}

inline bool imgui_CheckboxV(const char* label, bool v) { //
    return ImGui::Checkbox(label, &v);
}

// TODO: are there public ways to do this?
inline bool imgui_TestItemFlag(ImGuiItemFlags flag) { //
    return (GImGui->CurrentItemFlags & flag) != 0;
}

inline float imgui_ContentRegionMaxAbsX() {
    const auto& window = *GImGui->CurrentWindow;
    return (window.DC.CurrentColumns || GImGui->CurrentTable) ? window.WorkRect.Max.x : window.ContentRegionRect.Max.x;
}

inline float imgui_ItemSpacingX() { return ImGui::GetStyle().ItemSpacing.x; }

inline float imgui_ItemInnerSpacingX() { return ImGui::GetStyle().ItemInnerSpacing.x; }

inline bool imgui_MouseScrolling() { return ImGui::GetIO().MouseWheel != 0; }

inline bool imgui_MouseScrollingDown() { return ImGui::GetIO().MouseWheel < 0; }

inline bool imgui_MouseScrollingUp() { return ImGui::GetIO().MouseWheel > 0; }

inline float imgui_CalcCharWidth(unsigned char ch) { //
    return ceil(ImGui::GetFont()->GetCharAdvance(ch));
}

inline ImVec2 imgui_CalcTextSize(std::string_view text) { //
    return ImGui::CalcTextSize(text.data(), text.data() + text.size(), false);
}

inline ImVec2 imgui_CalcLabelSize(std::string_view label) { //
    return ImGui::CalcTextSize(label.data(), label.data() + label.size(), true);
}

inline ImVec2 imgui_CalcButtonSize(std::string_view label) { //
    return imgui_CalcLabelSize(label) + ImGui::GetStyle().FramePadding * 2;
}

inline ImVec2 imgui_CalcRequiredWindowSize() { //
    return GImGui->CurrentWindow->DC.CursorMaxPos + ImGui::GetStyle().WindowPadding - ImGui::GetWindowPos();
}

// (Referring to `ImGui::Get/SetCursorScreenPos(...)`.)
inline void imgui_AddCursorPosX(float dx) {
    ImGuiWindow& window = *ImGui::GetCurrentWindow();
    window.DC.CursorPos.x = floor(window.DC.CursorPos.x + dx);
    window.DC.IsSetPos = true;
}

inline void imgui_AddCursorPosY(float dy) {
    ImGuiWindow& window = *ImGui::GetCurrentWindow();
    window.DC.CursorPos.y = floor(window.DC.CursorPos.y + dy);
    window.DC.IsSetPos = true;
}

// TODO: workaround to set min column width for tables. Highly dependent on impl details.
// See: `ImGui::TableUpdateLayout`, `table->MinColumnWidth` and `ImGui::TableNextRow`.
// Are there public ways to do similar things?
inline void imgui_LockTableLayoutWithMinColumnWidth(const float min_column_width) {
    ImGuiTable* table = GImGui->CurrentTable;
    assert(table && !table->IsLayoutLocked);
    ImGui::PushStyleVarX(ImGuiStyleVar_FramePadding, min_column_width);
    ImGui::TableUpdateLayout(table);
    ImGui::PopStyleVar();
    assert(table->MinColumnWidth == min_column_width);
    table->IsLayoutLocked = true;
}
