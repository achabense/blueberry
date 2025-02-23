#pragma once

#include <algorithm>
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

inline ImGuiID imgui_GetItemPosID() { //
    return GImGui->CurrentWindow->GetIDFromPos(ImGui::GetItemRectMin());
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
inline bool imgui_ItemFullyVisible() { //
    return GImGui->CurrentWindow->ClipRect.Contains(GImGui->LastItemData.Rect);
}

// `!ImGui::IsAnyItemActive() || ImGui::IsItemActive()`
inline bool imgui_IsItemOrNoneActive() { //
    return GImGui->ActiveId == 0 || GImGui->ActiveId == GImGui->LastItemData.ID;
}

// Workaround to provide stable result for some cases.
// Related: https://github.com/ocornut/imgui/issues/7984 and 7945
inline bool imgui_ItemHoveredForTooltip(const char* str_id = nullptr) {
    if (!str_id) {
        return ImGui::IsItemHovered(ImGuiHoveredFlags_ForTooltip);
    } else {
        const ImGuiID using_id = ImGui::GetID(str_id);

        // HoverFlagsForTooltipMouse ~ Stationary | DelayShort | AllowWhenDisabled
        if (!ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled)) {
            return false;
        }

        ImGuiContext& g = *GImGui;
        // if (g.HoverItemDelayIdPreviousFrame != using_id) { // ImGuiHoveredFlags_NoSharedDelay
        //     g.HoverItemDelayTimer = 0.0f;
        // }
        g.HoverItemDelayId = using_id;
        if (g.HoverItemUnlockedStationaryId != using_id) { // ImGuiHoveredFlags_Stationary
            return false;
        }
        if (g.HoverItemDelayTimer < g.Style.HoverDelayShort) { // ImGuiHoveredFlags_DelayShort
            return false;
        }
        return true;
    }
}

inline const char* imgui_ItemTooltip_StrID = nullptr;

inline bool imgui_ItemTooltip(const func_ref<void()> desc) {
    const char* str_id = std::exchange(imgui_ItemTooltip_StrID, nullptr);
    if (GImGui->CurrentWindow->SkipItems) {
        return false;
    }
    if (imgui_ItemHoveredForTooltip(str_id)) {
        if (ImGui::BeginTooltip()) {
            // The same as the one in `HelpMarker` in "imgui_demo.cpp".
            ImGui::PushTextWrapPos(ImGui::GetFontSize() * 35.0f);
            desc();
            ImGui::PopTextWrapPos();
            ImGui::EndTooltip();
            return true;
        }
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

inline void imgui_StrColored(const ImVec4& col, std::string_view str) {
    ImGui::PushStyleColor(ImGuiCol_Text, col);
    imgui_Str(str);
    ImGui::PopStyleColor();
}

inline void imgui_StrDisabled(std::string_view str) {
    imgui_StrColored(ImGui::GetStyleColorVec4(ImGuiCol_TextDisabled), str);
}

// Similar to `HelpMarker` in "imgui_demo.cpp".
inline bool imgui_StrTooltip(std::string_view str, const func_ref<void()> desc) {
    imgui_StrDisabled(str);
    return imgui_ItemTooltip(desc);
}

inline bool imgui_StrTooltip(std::string_view str, std::string_view desc) { //
    return imgui_StrTooltip(str, [desc] { imgui_Str(desc); });
}

class [[nodiscard]] imgui_Window : no_copy {
public:
    const bool visible;
    explicit imgui_Window(const char* name, bool* p_open = nullptr, ImGuiWindowFlags flags = {})
        : visible(ImGui::Begin(name, p_open, flags)) {}
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

// TODO: are there public ways to do this?
inline bool imgui_TestItemFlag(ImGuiItemFlags flag) { //
    return (GImGui->CurrentItemFlags & flag) != 0;
}

inline bool imgui_IsWindowHoverable(ImGuiHoveredFlags flags = 0) { //
    return ImGui::IsWindowContentHoverable(ImGui::GetCurrentWindowRead(), flags);
}

inline float imgui_ContentRegionMaxAbsX() {
    ImGuiContext& g = *GImGui;
    ImGuiWindow* window = g.CurrentWindow;
    return (window->DC.CurrentColumns || g.CurrentTable) ? window->WorkRect.Max.x : window->ContentRegionRect.Max.x;
}

inline float imgui_ItemSpacingX() { return ImGui::GetStyle().ItemSpacing.x; }

inline float imgui_ItemInnerSpacingX() { return ImGui::GetStyle().ItemInnerSpacing.x; }

inline bool imgui_MouseScrolling() { return ImGui::GetIO().MouseWheel != 0; }

inline bool imgui_MouseScrollingDown() { return ImGui::GetIO().MouseWheel < 0; }

inline bool imgui_MouseScrollingUp() { return ImGui::GetIO().MouseWheel > 0; }

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
    return ImGui::GetCurrentWindowRead()->DC.CursorMaxPos + ImGui::GetStyle().WindowPadding - ImGui::GetWindowPos();
}

// (Referring to `ImGui::Get/SetCursorScreenPos(...)`.)
inline void imgui_AddCursorPosX(float dx) {
    ImGuiWindow* window = ImGui::GetCurrentWindow();
    window->DC.CursorPos.x = floor(window->DC.CursorPos.x + dx);
    window->DC.IsSetPos = true;
}

inline void imgui_AddCursorPosY(float dy) {
    ImGuiWindow* window = ImGui::GetCurrentWindow();
    window->DC.CursorPos.y = floor(window->DC.CursorPos.y + dy);
    window->DC.IsSetPos = true;
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
