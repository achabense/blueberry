#include "common.hpp"

// TODO: when to use '? ('Ctrl' vs Ctrl, 'Clipboard' vs Clipboard, '0' vs 0 etc.)
// TODO: whether to require no-ctrl for shortcuts?

static open_state intro_window(frame_main_token) {
    bool open = true;
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
    imgui_CenterNextWindow(ImGuiCond_FirstUseEver); // TODO: or _Appearing?

    imgui_Window::next_window_titlebar_tooltip =
        "Click the collapse button, or double-click the title bar to collapse/uncollapse.";
    if (auto window =
            imgui_Window("Get started", &open, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
        ImGui::PushTextWrapPos(wrap_len());
        // ImGui::SeparatorText("...");
        {
            if constexpr (init_double_esc_to_close) {
                ImGui::Bullet();
                imgui_Str("Double-press Esc to close all windows."); // & popups if possible.
            }

            ImGui::Bullet();
            imgui_Str("Press H to toggle on/off additional tooltips.");
            guide_mode::item_tooltip("Only input fields use Ctrl for shortcuts (Ctrl+C/X/V etc.).\n\n"
                                     "All the other shortcuts (including these) don't require Ctrl to be pressed.");

            // !!TODO: undocumented in release mode; uncertain about the design.
            // (& requires example in this window.)
            static_assert(init_set_scroll_with_up_down);
            if constexpr (debug_mode) {
                ImGui::Bullet();
                imgui_Str("Press Up/Down to set scroll in the focused window.");
            }

            ImGui::Bullet();
            imgui_Str("Press Left/Right to control '</>' in the focused window.");

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
        }
        ImGui::SeparatorText("Preview windows");
        {
            ImGui::Bullet();
            imgui_Str("Right-click to open menu.");
            ImGui::Bullet();
            imgui_Str("Drag to send the rule elsewhere (right-click to cancel).");
            ImGui::SameLine();
            imgui_StrTooltip("(?)", "The following can serve as rule sources:\n"
                                    "1. Preview windows.\n"
                                    "2. The MAP-string for (the rule shown in) the pattern editor.\n"
                                    "3. Other items that display a preview window in tooltip.");

            ImGui::Bullet();
            imgui_Str("Make sure you can save discoveries.");
            guide_mode::item_tooltip(
                "The program relies on the clipboard to save rules and patterns.\n\n"
                "To verify, copy the following rule (open menu -> 'Copy rule'), and see whether you can paste the rule elsewhere.");

            // ImGui::Separator();

            // TODO: better examples... (Currently: voting rule / gol / the 3 examples in readme)
            static const auto rules = aniso::extract_all_rules(
                "MAPAAAAAQABARcAAQEXARcXfwABARcBFxd/ARcXfxd/f/8AAQEXARcXfwEXF38Xf3//ARcXfxd/f/8Xf3//f////w "
                "MAPARYXfhZofugWaH7oaIDogBZofuhogOiAaIDogIAAgAAWaH7oaIDogGiA6ICAAIAAaIDogIAAgACAAIAAAAAAAA "
                "MAPBSEBKiGAcMxBVCdvQAH//ySAf8+AAd1aAEE/DwAT728JCDX/DgF9/6VEf34MAX7bAAB3/QkTVX3Mkf57g397Xw "
                "MAPFAgghg2AwFFSkCAAgQIRFGAAikEABAcACABkMhQMgkEoQAKEQAAgkoWWBjEUI8EMiBcBCBwABskEiREsEQUkkw "
                "MAPBQgEAEASAKBuS0EEikACIkkQICICJDDoCCgAChiCIgh80AkQWQgEMMwAhEEAIAkAQEIAMGCQUECgQEgECAAEAA ");
            static previewer::configT settings{previewer::default_settings};
            static int at = 0;
            const int total = rules.size();
            assert(total == 5 && 0 <= at && at < total);
            const auto set_pos = [&](const int pos) { at = std::clamp(pos, 0, total - 1); };

            ImGui::AlignTextToFramePadding();
            ImGui::Bullet();
            ImGui::BeginGroup();
            switch (sequence::seq("<|", "<##Prev", ">##Next", "|>")) {
                case 0: set_pos(0); break;
                case 1: set_pos(at - 1); break;
                case 2: set_pos(at + 1); break;
                case 3: set_pos(INT_MAX); break;
            }
            ImGui::SameLine();
            settings.set("Settings");
            ImGui::SameLine();
            ImGui::Text("Rules:%d At:%d", total, at + 1);

            previewer::preview(0, settings, rules[at]);
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
        ImGui::PushStyleVar(ImGuiStyleVar_FramePadding, ImMin(ImVec2{3, 2}, ImGui::GetStyle().FramePadding));
    }

    global_timer::begin_frame({});
    shortcuts::begin_frame({});
    previewer::begin_frame({});

    if (shortcuts::no_active()) {
        if constexpr (init_double_esc_to_close) {
            want_close_windows = false;
            if (shortcuts::test_pressed(ImGuiKey_Escape)) {
                static double last = 0;
                const double now = ImGui::GetTime();
                if (now < last + ImGui::GetIO().MouseDoubleClickTime) { // Double-pressed.
                    last = 0;
                    want_close_windows = true;
                } else {
                    last = now;
                }
            }
        } else {
            assert(!want_close_windows);
        }

        if (shortcuts::test_pressed(ImGuiKey_H)) {
            guide_mode::flip_enable({});
        }
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
                load_file({}).reset_if_closed(show_file);
            }
        }
        ImGui::SameLine();
        {
            static bool show_clipboard = false;
            ImGui::Checkbox("Paste##Clipboard", &show_clipboard);
            guide_mode::item_tooltip("Load text/rules from the clipboard.");
            if (show_clipboard) {
                load_clipboard({}).reset_if_closed(show_clipboard);
            }
        }
        ImGui::SameLine();
        {
            static bool show_doc = false;
            // ImGui::Checkbox("Documents", &show_doc);
            ImGui::Checkbox("About", &show_doc);
            guide_mode::item_tooltip("Concepts, example rules, etc.");
            if (show_doc) {
                load_doc({}).reset_if_closed(show_doc);
            }
        }

        static bool show_edit_rule = true;
        static bool show_edit_pattern = true;
        bool reset_table = false;
        if (!show_edit_rule && !show_edit_pattern) [[unlikely]] { // Defensive.
            assert(false);
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
        if constexpr (debug_mode) { // TODO: support in release mode?
            ImGui::SameLine();
            menu_like_popup::small_button("Set##FPS");
            menu_like_popup::popup(set_frame_rate);
        }
        if constexpr (debug_mode) {
            ImGui::SameLine(0, wide_spacing);
            imgui_Str("(Debug mode)");
            ImGui::SameLine();
            static bool show_demo = false;
            ImGui::Checkbox("Demo window", &show_demo);
            if (show_demo) {
                ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
                ImGui::ShowDemoWindow(&show_demo);
                if constexpr (init_double_esc_to_close) {
                    if (want_close_windows) {
                        show_demo = false;
                    }
                }
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
                // (Child window seems no longer necessary (except that it prevents auto-fitting).)
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

    rec_for_rule::display_if_present({}, "Recent rules (snapshot)");
    messenger::display_if_present({});

    pass_rule::end_frame({});

    if constexpr (init_compact_mode) {
        ImGui::PopStyleVar();
    }
    // ImGui::PopStyleVar();
    if constexpr (init_selectables_use_button_color) {
        ImGui::PopStyleColor(3);
    }
    ImGui::PopStyleColor();
}
