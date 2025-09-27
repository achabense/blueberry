#include "common.hpp"

// TODO: when to use '? ('Ctrl' vs Ctrl, 'Clipboard' vs Clipboard, '0' vs 0 etc.)
// TODO: whether to require no-ctrl for shortcuts?

// !!TODO: unfinished...
static open_state intro_window(frame_main_token) {
    bool open = true;
    imgui_CenterNextWindow(ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);

    imgui_Window::next_window_titlebar_tooltip =
        "Click the collapse button, or double click the title bar to collapse/uncollapse.";
    if (auto window =
            imgui_Window("Introduction", &open, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
        static int page = 0;
        imgui_RadioButton("Overview", &page, 0);
        ImGui::SameLine(0, imgui_ItemInnerSpacingX());
        imgui_RadioButton("Preview windows", &page, 1);
        ImGui::Separator();

        ImGui::PushTextWrapPos(wrap_len());
        if (page == 0) {
            ImGui::Bullet();
            imgui_Str("Use 'Open' and 'Paste' to load rules.");
            ImGui::Bullet();
            imgui_Str("Use the left panel to generate rules.");
            ImGui::SameLine();
            imgui_StrTooltip("(?)",
                             "It's recommended to try the 'Random' window first (press '>>>' to generate rules).");
            ImGui::Bullet();
            imgui_Str("Use the right panel to operate on patterns.");
            ImGui::Bullet();
            imgui_Str("See 'Documents' for more information.");

            ImGui::SeparatorText("Misc controls");

            if constexpr (debug_mode_double_esc_to_close) {
                ImGui::Bullet();
                // ImGui::SameLine(); // Not needed.
                imgui_Str("Double-press 'Esc' to close the focused window (or popup).");
            }

            ImGui::Bullet();
            imgui_Str("Press 'H' to toggle on/off additional tooltips.");
            guide_mode::item_tooltip("...");
            ImGui::SameLine();
            imgui_StrTooltip("(?)", "Only input fields use Ctrl for shortcuts (Ctrl+C/X/V etc.).\n\n"
                                    "All the other shortcuts (like this) require Ctrl not to be pressed.");
            // & Ctrl+C to copy tooltip (debug mode)

            ImGui::Bullet();
            imgui_Str("Right-click underlined text (like this) to open menu.");
            rclick_popup::for_text([] {
                if (ImGui::Selectable("...")) {
                    messenger::dot();
                }
            });

            ImGui::Bullet();
            imgui_Str("Buttons like ");
            ImGui::SameLine(0, 0);
            if (double_click_button_small("this")) {
                messenger::dot();
            }
            ImGui::SameLine(0, 0);
            imgui_Str(" require double-clicking.");
        } else if (page == 1) {
            ImGui::Bullet();
            imgui_Str("Right-click to open menu.");
            ImGui::Bullet();
            imgui_Str("Drag to send the rule elsewhere (right-click to cancel).");
            ImGui::SameLine();
            imgui_StrTooltip("(?)", "The following can serve as rule sources:\n"
                                    "1. Preview windows.\n"
                                    "2. The MAP-string for (the rule shown in) the main window.\n"
                                    "3. Other items that display a preview window in tooltip.");
            ImGui::Bullet();
            imgui_Str("Press left/right arrow keys to control 'Prev/Next' in the focused window.");

            ImGui::Bullet();
            imgui_Str("Make sure you can save discoveries (rules and patterns).");
            ImGui::SameLine();
            imgui_StrTooltip( // !!TODO: about auto-saving...
                "(?)",
                "The program relies on the clipboard to save rules and patterns.\n\n"
                "To copy rule to the clipboard: open menu -> 'Copy rule', or equivalently, send the rule to '[C]' (after 'Clipboard').\n\n"
                "The method will:\n"
                "1. Copy rule to the clipboard.\n"
                "2. Save rule to the \"autosaved\" folder (if possible). However, it's recommended you deal with the copied rules yourselves.\n\n"
                "For more details, see !!TODO...");

            // (Selected from "Documents/Rules in different sets")
            // !!TODO: better examples.
            static const auto rules = aniso::extract_all_rules(
                "MAPAAAAAQABARcAAQEXARcXfwABARcBFxd/ARcXfxd/f/8AAQEXARcXfwEXF38Xf3//ARcXfxd/f/8Xf3//f////w "
                "MAPIIAAAYABARcAAQEXARcXf4ABARcBFxd/ARcXfxd/f/8AAQEXARcXfwEXF38Xf3//ARcXfxd/f/8Xf3//f////w "
                "MAPAAEAAAEBABcAEQEHARcXfwABARcBFxd/AVcXPxd/f/8AAQEXAxcVfwEXF38Xf3//ARcXfx9/d/8X/39///9//w "
                "MAPAAAAEQAREXcAAAARABERdwAREXcRd3f/ABERdxF3d/8AERF3EXd3/wAREXcRd3f/EXd3/3f///8Rd3f/d////w ");
            static int at = 0;
            static previewer::configT config{previewer::default_settings};
            const int total = rules.size();
            assert(total == 4);

            ImGui::AlignTextToFramePadding();
            ImGui::Bullet();
            ImGui::BeginGroup();
            switch (sequence::seq("<|", "Prev", "Next", "|>")) {
                case 0: at = 0; break;
                case 1: --at; break;
                case 2: ++at; break;
                case 3: at = INT_MAX; break;
            }
            at = std::clamp(at, 0, total - 1);

            ImGui::SameLine();
            ImGui::Text("Total:%d At:%d", total, at + 1);
            ImGui::SameLine();
            config.set("Settings");

            previewer::preview(0, config, rules[at]);
            ImGui::EndGroup();
        }
        ImGui::PopTextWrapPos();
    }
    return {open};
}

void frame_main() {
    // Make collapsed windows obvious to see.
    ImGui::PushStyleColor(ImGuiCol_TitleBgCollapsed, ImGui::GetColorU32(ImGuiCol_TitleBgActive, 0.8f));
    // By default header colors are less saturated than buttons.
    if constexpr (init_selectables_use_button_color) {
        ImGui::PushStyleColor(ImGuiCol_Header, ImGui::GetStyleColorVec4(ImGuiCol_Button));
        ImGui::PushStyleColor(ImGuiCol_HeaderHovered, ImGui::GetStyleColorVec4(ImGuiCol_ButtonHovered));
        ImGui::PushStyleColor(ImGuiCol_HeaderActive, ImGui::GetStyleColorVec4(ImGuiCol_ButtonActive));
    }
    // ImGui::PushStyleVar(ImGuiStyleVar_WindowPadding, {8, 6});
    if constexpr (init_compact_mode) {
        ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImVec2{3, 2});
    }

    global_timer::begin_frame({});
    shortcuts::begin_frame({});
    previewer::begin_frame({});
    pass_rule::begin_frame({});

    if (shortcuts::no_active_and_no_ctrl() && shortcuts::test_pressed(ImGuiKey_H)) {
        guide_mode::flip_enable({});
    }

    constexpr ImGuiWindowFlags flags = ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoMove |
                                       ImGuiWindowFlags_NoBringToFrontOnFocus | ImGuiWindowFlags_NoSavedSettings;
    const auto& viewport = *ImGui::GetMainViewport();

    ImGui::SetNextWindowPos(viewport.WorkPos);
    ImGui::SetNextWindowSize(viewport.WorkSize);
    if (auto window = imgui_Window("Main", nullptr, flags)) {
        // static test_appearing appearing{}; // Not needed.
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
            // appearing.reset_if_appearing(show_file);
            ImGui::Checkbox("Open##Files", &show_file);
            guide_mode::item_tooltip("Load text/rules from files.");
            if (show_file) {
                const ImVec2 init_pos = ImGui::GetItemRectMin() + ImVec2(0, ImGui::GetFrameHeight() + 4);
                load_file(init_pos, {}).reset_if_closed(show_file);
            }
        }
        ImGui::SameLine();
        {
            static bool show_clipboard = false;
            ImGui::Checkbox("Paste##Clipboard", &show_clipboard);
            guide_mode::item_tooltip("Load text/rules from the clipboard.");
            if (show_clipboard) {
                const ImVec2 init_pos = ImGui::GetItemRectMin() + ImVec2(0, ImGui::GetFrameHeight() + 4);
                load_clipboard(init_pos, {}).reset_if_closed(show_clipboard);
            }
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

        static bool show_edit_rule = true;
        static bool show_edit_pattern = true;
        bool reset_table = false;
        if (!show_edit_rule && !show_edit_pattern) [[unlikely]] { // Defensive.
            show_edit_pattern = true;
        }
        // (Undocumented.)
        if (!show_edit_rule || !show_edit_pattern) {
            ImGui::SameLine(0, wide_spacing);
            if (ImGui::Button("Restore")) {
                show_edit_rule = true;
                show_edit_pattern = true;
                reset_table = true;
            }
            guide_mode::item_tooltip("Display both panels.");
        }

        ImGui::SameLine(0, wide_spacing);
        ImGui::Text("(%d FPS)", (int)std::round(ImGui::GetIO().Framerate));
        if constexpr (debug_mode) {
            ImGui::SameLine();
            menu_like_popup::small_button("Set");
            menu_like_popup::popup(set_frame_rate);

            ImGui::SameLine(0, wide_spacing);
            imgui_Str("(Debug mode)");
            ImGui::SameLine();
            static bool show_demo = false;
            ImGui::Checkbox("Demo window", &show_demo);
            if (show_demo) {
                ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
                ImGui::ShowDemoWindow(&show_demo);
            }

            using clockT = std::chrono::steady_clock;
            static const auto started = clockT::now();
            const std::chrono::hh_mm_ss hms(clockT::now() - started);
            ImGui::SameLine(0, wide_spacing);
            ImGui::Text("Time:%02d:%02d:%02d", (int)hms.hours().count(), (int)hms.minutes().count(),
                        (int)hms.seconds().count());

            ImGui::SameLine(0, wide_spacing);
            ImGui::Text("Frame:%d", ImGui::GetFrameCount());
        }

        ImGui::Separator();

        assert(show_edit_rule || show_edit_pattern);
        if (ImGui::BeginTable("Layout", show_edit_rule + show_edit_pattern, ImGuiTableFlags_Resizable)) {
            if (show_edit_rule && show_edit_pattern) {
                if (reset_table) {
                    ImGui::TableResetSettings(ImGui::GetCurrentTable());
                }
                ImGui::TableSetupColumn("", ImGuiTableColumnFlags_WidthStretch, 0.65f);
                ImGui::TableSetupColumn("", ImGuiTableColumnFlags_WidthStretch);
            }

            ImGui::TableNextRow();
            constexpr float min_w = 8;
            if (show_edit_rule) {
                ImGui::TableNextColumn();
                // The child window is required here (for stable scrolling).
                if (auto child = imgui_ChildWindow("Rule", {}, 0, ImGuiWindowFlags_NoScrollbar)) {
                    edit_rule({});
                    if (ImGui::GetContentRegionAvail().x < min_w) {
                        show_edit_rule = false;
                    }
                }
            }
            if (show_edit_pattern) {
                ImGui::TableNextColumn();
                if (auto child = imgui_ChildWindow("Pattern", {}, 0, ImGuiWindowFlags_NoScrollbar)) {
                    edit_pattern({});
                    if (ImGui::GetContentRegionAvail().x < min_w) {
                        show_edit_pattern = false;
                    }
                }
            }
            ImGui::EndTable();
        }
    }

    messenger::display_if_present({});

    if constexpr (init_compact_mode) {
        ImGui::PopStyleVar();
    }
    // ImGui::PopStyleVar();
    if constexpr (init_selectables_use_button_color) {
        ImGui::PopStyleColor(3);
    }
    ImGui::PopStyleColor();
}
