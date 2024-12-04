#include "common.hpp"

#ifndef NDEBUG
#define SET_FRAME_RATE
#endif

#ifdef SET_FRAME_RATE
#include <thread>

class timerT {
    // (Typically the framerate is limited by VSync in this case, like 60 fps.)
    static constexpr int max_fps = 100;
    int fps = max_fps;

    using clockT = std::chrono::steady_clock;
    clockT::time_point last{};

public:
    void wait() {
        using namespace std::chrono_literals;
        const auto now = clockT::now();
        const auto until = last + 1000ms / fps;
        if (now < until) {
            std::this_thread::sleep_until(until);
            last = until; // Instead of another `clockT::now()` call.
        } else {
            last = now;
        }
    }

    void set_fps() {
        imgui_RadioButton("Auto", &fps, max_fps);
        ImGui::SameLine();
        imgui_StrTooltip("(?)", "Typically limited by VSync.");

        ImGui::SameLine();
        imgui_RadioButton("50 fps", &fps, 50);
        ImGui::SameLine();
        imgui_RadioButton("10 fps", &fps, 10);
    }
};
#endif

static void get_reversal_dual(const bool button_result, sync_point& sync) {
    if (button_result) {
        sync.set(rule_algo::trans_reverse(sync.rule));
    }
    imgui_ItemTooltip([&] {
        ImGui::PushTextWrapPos(400);
        imgui_Str(
            "Get the 0/1 reversal dual of the current rule.\n\n"
            "(That is, for any pattern, [apply the original rule] has the same effect as [flip all values -> apply the dual -> flip all values].)");
        ImGui::Separator();
        imgui_Str("Preview:");
        ImGui::SameLine();
        const aniso::ruleT rev = rule_algo::trans_reverse(sync.rule);
        previewer::preview(-1, previewer::configT::_220_160, rev, false);
        if (rev == sync.rule) {
            imgui_Str("(It's the same as the current rule, as the current rule is self-complementary.)");
        }
        ImGui::PopTextWrapPos();
    });
}

void frame_main() {
    // Make collapsed windows obvious to see.
    ImGui::PushStyleColor(ImGuiCol_TitleBgCollapsed, ImGui::GetColorU32(ImGuiCol_TitleBgActive, 0.8f));
    const bool compact_mode_this_frame = compact_mode;
    if (compact_mode_this_frame) {
        ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2{3, 2});
    }

#ifdef SET_FRAME_RATE
    static timerT timer;
    timer.wait();
#endif // SET_FRAME_RATE

    global_timer::begin_frame();
    guide_mode::begin_frame();
    shortcuts::begin_frame();
    sequence::begin_frame();
    previewer::begin_frame();

    static aniso::ruleT sync_rule = aniso::game_of_life();
    sync_point sync = sync_rule;

    rule_recorder::record(rule_recorder::Current, sync.rule);
    sync_point_override::begin_frame();
    if ((sync_point_override::want_test_run || sync_point_override::want_test_set) &&
        sync_point_override::rule == sync.rule) {
        messenger::set_msg("This is the same as the current rule.");
    }

    messenger::display();

    // TODO: add a way to specify the contents for the left panel. These loading windows can fit well.
    static bool show_file = false;
    static bool show_clipboard = false;
    static bool show_doc = false;
    static bool show_record = false;
    auto load_rule = [&](bool& open, const char* title, void (*load_fn)(sync_point&)) {
        if (ImGui::Checkbox(title, &open) && open) {
            ImGui::SetNextWindowCollapsed(false, ImGuiCond_Always);
        }

        // This is a workaround to support shortcut for clipboard-reading.
        // TODO: using 'W' to avoid conflicts with pattern-pasting; not quite conventional...
        if (&open == &show_clipboard) {
            if (shortcuts::keys_avail_and_window_hoverable() && shortcuts::test(ImGuiKey_W)) {
                open = true;
                ImGui::SetNextWindowCollapsed(false, ImGuiCond_Always);
                ImGui::SetNextWindowFocus();
            }
        }

        if (open) {
            ImGui::SetNextWindowPos(ImGui::GetItemRectMin() + ImVec2(0, ImGui::GetFrameHeight() + 4),
                                    ImGuiCond_FirstUseEver);
            ImGui::SetNextWindowSize({600, 400}, ImGuiCond_FirstUseEver);
            ImGui::SetNextWindowSizeConstraints(ImVec2(450, 300), ImVec2(FLT_MAX, FLT_MAX));
            if (auto window = imgui_Window(title, &open, ImGuiWindowFlags_NoSavedSettings)) {
                load_fn(sync);
            }
        }
    };

    const ImGuiWindowFlags flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove |
                                   ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoSavedSettings;
    const ImGuiViewport* viewport = ImGui::GetMainViewport();

    ImGui::SetNextWindowPos(viewport->WorkPos);
    ImGui::SetNextWindowSize(viewport->WorkSize);
    if (auto window = imgui_Window("Main", nullptr, flags)) {
        load_rule(show_file, "Files", load_file);
        guide_mode::item_tooltip("Load rules from files.");
        ImGui::SameLine();
        load_rule(show_clipboard, "Clipboard", load_clipboard);
        guide_mode::item_tooltip("Load rules from the clipboard. Shortcut: 'W'.\n\n"
                                 "('V' is for pasting patterns in the right panel.)");
        ImGui::SameLine();
        load_rule(show_doc, "Documents", load_doc);
        guide_mode::item_tooltip("Concepts, example rules, etc.");

        const int wide_spacing = ImGui::CalcTextSize(" ").x * 3;
        ImGui::SameLine(0, wide_spacing);
        load_rule(show_record, "Record", rule_recorder::load_record);
        guide_mode::item_tooltip("Record for current rule, copied rules, etc.");

        ImGui::SameLine(0, wide_spacing);
        ImGui::Text("(%d FPS)", (int)round(ImGui::GetIO().Framerate));
#ifdef SET_FRAME_RATE
        ImGui::SameLine();
        ImGui::SmallButton("Set");
        if (begin_popup_for_item()) {
            timer.set_fps();
            ImGui::EndPopup();
        }
#endif // SET_FRAME_RATE

#ifndef NDEBUG
        ImGui::SameLine(0, wide_spacing);
        imgui_Str("(Debug mode)");
        ImGui::SameLine();
        static bool show_demo = false;
        ImGui::Checkbox("Demo window", &show_demo);
        if (show_demo) {
            ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
            ImGui::ShowDemoWindow(&show_demo);
        }
        ImGui::SameLine(0, wide_spacing);
        ImGui::Text("Frame:%d", ImGui::GetFrameCount());
#endif // !NDEBUG

        ImGui::Separator();

        // !!TODO: rewrite...
        ImGui::AlignTextToFramePadding();
        imgui_StrTooltip(
            "(...)",
            "(Press 'H' to enter help mode for more tooltips; press 'H' again to quit the mode.)\n\n"
            "'<| Prev/Next |>' represents the record for the current rule (as shown in the right panel). You can switch to previously tested rules with it.\n\n"
            "Right-click 'Total:.. At:..' to clear the record.\n\n"
            "Right-click the MAP-string (for the current rule) to copy to the clipboard.");

        // !!TODO: find a new place for this...
        // When a button is clicked, or when the window is focused and you press the left/right arrow key, the
        // left/right arrow key will begin to serve as the shortcuts for 'Prev/Next'. This also applies to sequences
        // in other windows.

        ImGui::SameLine();
        imgui_Str("Current rule ~");
        ImGui::SameLine();
        if (imgui_StrCopyable(aniso::to_MAP_str(sync.rule), imgui_Str, set_clipboard_and_notify)) {
            rule_recorder::record(rule_recorder::Copied, sync.rule);
        }
        guide_mode::item_tooltip("Right-click to copy.");
        ImGui::SameLine();
        get_reversal_dual(ImGui::Button("0/1 rev"), sync);

        ImGui::Separator();

        if (ImGui::BeginTable("Layout", 2, ImGuiTableFlags_Resizable)) {
            auto try_hide = [](const float width) {
                if (const ImVec2 avail = ImGui::GetContentRegionAvail(); avail.x <= width) {
                    ImGui::Dummy(avail);
                    imgui_ItemRectFilled(ImGui::GetColorU32(ImGuiCol_FrameBg, ImGui::GetStyle().DisabledAlpha));
                    imgui_ItemTooltip("Hidden.");
                    return true;
                }
                return false;
            };

            static bool right_was_hidden = false;
            const float min_w = 6;
            {
                // TODO: what can be skipped when the program is minimized? Is this check reliable for all backends?
                const bool minimized = viewport->WorkSize.x <= 0 || viewport->WorkSize.y <= 0;
                if (!minimized && right_was_hidden) {
                    // TODO: working, but this looks very fragile...
                    // So when the program window is resized (e.g. maximized), the right panel will remain hidden.
                    // (As tested this does not affect manual resizing (using table's resize bar) within the program.)
                    ImGui::TableSetupColumn("", ImGuiTableColumnFlags_WidthStretch);
                    ImGui::TableSetupColumn("", ImGuiTableColumnFlags_WidthFixed, min_w);
                } else {
                    // (No need to check whether the left panel was hidden.)
                    ImGui::TableSetupColumn("", ImGuiTableColumnFlags_WidthFixed, 510);
                    ImGui::TableSetupColumn("", ImGuiTableColumnFlags_WidthStretch);
                }
            }

            imgui_LockTableLayoutWithMinColumnWidth(min_w);

            ImGui::TableNextRow();
            ImGui::TableNextColumn();
            // The child window is required here (for stable scrolling).
            if (auto child = imgui_ChildWindow("Edit", {}, 0, ImGuiWindowFlags_NoScrollbar)) {
                if (!try_hide(min_w)) {
                    edit_rule(sync);
                }
            }
            ImGui::TableNextColumn();
            if (auto child = imgui_ChildWindow("Apply", {}, 0, ImGuiWindowFlags_NoScrollbar)) {
                if (!(right_was_hidden = try_hide(min_w))) {
                    apply_rule(sync);
                }
            }
            ImGui::EndTable();
        }
    }

    if (sync.out_rule) {
        sync_rule = *sync.out_rule;
        rule_recorder::record(sync.rec_type, *sync.out_rule, &sync.rule);
    }

    if (compact_mode_this_frame) {
        ImGui::PopStyleVar();
    }
    ImGui::PopStyleColor();

#ifndef NDEBUG
    if (shortcuts::keys_avail() && shortcuts::test(ImGuiKey_7)) {
        compact_mode = !compact_mode;
    }
#endif
}
