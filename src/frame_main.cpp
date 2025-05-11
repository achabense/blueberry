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

static open_state intro_window(frame_main_token) {
    bool open = true;
    imgui_CenterNextWindow(ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);

    imgui_Window::next_window_titlebar_tooltip =
        "Click collapse button, or double click title bar to collapse/uncollapse.";
    if (auto window =
            imgui_Window("Introduction", &open, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
        ImGui::PushTextWrapPos(wrap_len());

        {
            ImGui::Bullet();
            // ImGui::SameLine(); // Not needed.
            imgui_Str("Double-press 'Esc' to close the focused window (or popup).");

            ImGui::Bullet();
            imgui_Str("Press 'H' to toggle on/off additional tooltips.");
            ImGui::SameLine();
            imgui_StrTooltip(
                "(?)", // (The shortcuts in input fields also require ctrl...)
                "Only 'Ctrl+V' uses 'Ctrl'; all the other shortcuts require 'Ctrl' not to be pressed.\n\n"
                "(There are two kinds of pasting in the program - 'Ctrl+V' to load text and extract rules, and 'V' to paste patterns.)");

            ImGui::Bullet();
            imgui_Str("Right-click underlined text (like this) to open menu.");
            rclick_popup::popup(imgui_GetItemPosID(), [] {
                if (ImGui::Selectable("...")) {
                    messenger::set_msg("...");
                }
            });

            ImGui::Bullet();
            imgui_Str("Buttons like ");
            ImGui::SameLine(0, 0);
            if (double_click_button_small("this")) {
                messenger::set_msg("this");
            }
            ImGui::SameLine(0, 0);
            imgui_Str(" require double-clicking.");
        }
        ImGui::Separator();
        {
            // (Selected from "Documents/Rules in different sets")
            // !!TODO: better examples.
            static const auto rules = aniso::extract_all_rules(
                "MAPAAAAAQABARcAAQEXARcXfwABARcBFxd/ARcXfxd/f/8AAQEXARcXfwEXF38Xf3//ARcXfxd/f/8Xf3//f////w "
                "MAPIIAAAYABARcAAQEXARcXf4ABARcBFxd/ARcXfxd/f/8AAQEXARcXfwEXF38Xf3//ARcXfxd/f/8Xf3//f////w "
                "MAPAAEAAAEBABcAEQEHARcXfwABARcBFxd/AVcXPxd/f/8AAQEXAxcVfwEXF38Xf3//ARcXfx9/d/8X/39///9//w "
                "MAPAAAAEQAREXcAAAARABERdwAREXcRd3f/ABERdxF3d/8AERF3EXd3/wAREXcRd3f/EXd3/3f///8Rd3f/d////w ");
            static int at = 0;
            static previewer::configT config{previewer::configT::_220_160};
            const int total = rules.size();
            assert(total == 4);

            ImGui::Bullet();
            imgui_Str("Right-click \"preview window\" (like below) to open menu.");

            ImGui::Bullet();
            imgui_Str("Press '</>' to control 'Prev/Next' in the focused window.");

            ImGui::Bullet();
            imgui_Str("Make sure you can save discoveries.");
            ImGui::SameLine();
            imgui_StrTooltip(
                "(?)",
                "The program relies on clipboard to save rules and patterns, so make sure the clipboard works.\n\n"
                "Take preview windows for example, there are two equivalent ways to copy rule to the clipboard: open menu -> 'Copy rule', or send the rule to '[C]' (after 'Clipboard').\n\n"
                "Some functions will record recently accepted rules. For example, the copied rules are accessible from the menu for '[C]'.\n\n"
                "For more details, see !!TODO...");

            ImGui::Bullet();
            ImGui::BeginGroup();
            switch (sequence::seq("<|", "Prev", "Next", "|>")) {
                case 0: at = 0; break;
                case 1: at = std::max(0, at - 1); break;
                case 2: at = std::min(total - 1, at + 1); break;
                case 3: at = total - 1; break;
            }
            ImGui::SameLine();
            ImGui::Text("Total:%d At:%d", total, at + 1);
            ImGui::SameLine();
            config.set("Settings");

            previewer::preview(at, config, rules[at]);
            ImGui::EndGroup();
        }

        ImGui::PopTextWrapPos();
    }
    return {open};
}

void frame_main() {
    // Make collapsed windows obvious to see.
    ImGui::PushStyleColor(ImGuiCol_TitleBgCollapsed, ImGui::GetColorU32(ImGuiCol_TitleBgActive, 0.8f));
    // ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, {8, 6});
    if constexpr (init_compact_mode) {
        ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2{3, 2});
    }

#ifdef SET_FRAME_RATE
    static timerT timer;
    timer.wait();
#endif // SET_FRAME_RATE

    global_timer::begin_frame({});
    shortcuts::begin_frame({});
    rclick_popup::begin_frame({});
    previewer::begin_frame({});
    pass_rule::begin_frame({});

    messenger::display_msg_if_present({});
    if (shortcuts::keys_avail_and_no_ctrl() && shortcuts::test_pressed(ImGuiKey_H)) {
        guide_mode::flip_enable({});
    }

    const ImGuiWindowFlags flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove |
                                   ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoSavedSettings;
    const ImGuiViewport* viewport = ImGui::GetMainViewport();

    ImGui::SetNextWindowPos(viewport->WorkPos);
    ImGui::SetNextWindowSize(viewport->WorkSize);
    if (auto window = imgui_Window("Main", nullptr, flags)) {
        const int wide_spacing = imgui_ItemSpacingX() * 3; // imgui_CalcCharWidth(' ') * 3;
        {
            static bool show_intro = init_show_intro;
            ImGui::Checkbox("Intro", &show_intro);
            if (show_intro) {
                intro_window({}).reset_if_closed(show_intro);
            }
        }
        ImGui::SameLine(0, wide_spacing);
        {
            static bool show_file = false;
            ImGui::Checkbox("Files", &show_file);
            guide_mode::item_tooltip("Load rules from files.");
            if (show_file) {
                const ImVec2 init_pos = ImGui::GetItemRectMin() + ImVec2(0, ImGui::GetFrameHeight() + 4);
                load_file(init_pos, {}).reset_if_closed(show_file);
            }
        }
        ImGui::SameLine();
        {
            static bool show_clipboard = false;
            ImGui::Checkbox("Clipboard", &show_clipboard);
            guide_mode::item_tooltip("Load rules from the clipboard.");
            const bool paste = shortcuts::ctrl() && shortcuts::keys_avail_and_window_hoverable() &&
                               shortcuts::test_pressed(ImGuiKey_V);
            if (paste) {
                show_clipboard = true;
            }
            if (show_clipboard) {
                const ImVec2 init_pos = ImGui::GetItemRectMin() + ImVec2(0, ImGui::GetFrameHeight() + 4);
                load_clipboard(init_pos, paste, {}).reset_if_closed(show_clipboard);
            }
        }
        ImGui::SameLine();
        {
            imgui_Str("[C]");
            if (const auto pass = pass_rule::dest(ImGuiKey_C, 'C'); pass.deliv) {
                copy_rule::copy(*pass.deliv);
            }
            rclick_popup::popup(imgui_GetItemPosID(), [] {
                copy_rule::get_rec({}).selectable_to_take_snapshot("Copied rules");
                guide_mode::item_tooltip("Recently copied rules, including those copied via 'Copy rule'.");
            });
            guide_mode::item_tooltip("Drag a rule here to copy it (as MAP-string) to the clipboard.\n\n"
                                     "(This is useful as some rule sources have no 'Copy rule' option.)");
            copy_rule::get_rec({}).display_snapshot_if_present();
        }
        ImGui::SameLine();
        {
            static bool show_doc = false;
            ImGui::Checkbox("Documents", &show_doc);
            guide_mode::item_tooltip("Concepts, example rules, etc.");
            if (show_doc) {
                const ImVec2 init_pos = ImGui::GetItemRectMin() + ImVec2(0, ImGui::GetFrameHeight() + 4);
                load_doc(init_pos, {}).reset_if_closed(show_doc);
            }
        }

        ImGui::SameLine(0, wide_spacing);
        ImGui::Text("(%d FPS)", (int)round(ImGui::GetIO().Framerate));
#ifdef SET_FRAME_RATE
        ImGui::SameLine();
        menu_like_popup::small_button("Set");
        menu_like_popup::popup([] { timer.set_fps(); });
#endif // SET_FRAME_RATE

        if constexpr (debug_mode) {
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
        }

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
                    edit_rule({});
                }
            }
            ImGui::TableNextColumn();
            if (auto child = imgui_ChildWindow("Apply", {}, 0, ImGuiWindowFlags_NoScrollbar)) {
                if (!(right_was_hidden = try_hide(min_w))) {
                    apply_rule({});
                }
            }
            ImGui::EndTable();
        }
    }

    if constexpr (init_compact_mode) {
        ImGui::PopStyleVar();
    }
    // ImGui::PopStyleVar();
    ImGui::PopStyleColor();
}
