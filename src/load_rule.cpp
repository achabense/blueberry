#include <deque>
#include <filesystem>
#include <fstream>
#include <ranges>

#include "common.hpp"

// By default the project does not care about exceptions (taking as if they don't exist), but std::filesystem is an exception to this...
// (& `bad_alloc` is always considered impossible to happen.)

// The program is not going to support write access to files. However, there should be an easy way to open text file with default text editor...
// TODO: how to do that? `SDL_OpenUrl` is not suitable as it will open path in the "preferred way"...

using pathT = std::filesystem::path;

// (wontfix) After wasting so much time, I'd rather afford the extra copy than bothering with "more efficient"
// implementations any more.

// It's unclear whether these functions can fail due to transcoding...
static std::string cpp17_u8string(const pathT& path) noexcept {
    try {
        const auto u8string = path.u8string();
        return std::string(u8string.begin(), u8string.end());
    } catch (...) {
        assert(false);
        return "?";
    }
}

// As to why not using `filesystem::u8path`:
// There is no standard way to shut the compiler up for a [[deprecated]] warning.
// As to making an `std::u8string` first, see:
// https://stackoverflow.com/questions/57603013/how-to-safely-convert-const-char-to-const-char8-t-in-c20
// In short, in C++20 it's impossible to get `char8_t*` from `char*` without copy and in a well-defined way.
static pathT cpp17_u8path(const std::string_view path) noexcept {
    try {
        return pathT(std::u8string(path.begin(), path.end()));
    } catch (...) {
        assert(false);
        return {};
    }
}

// I hate this part so much...
// (This is horribly inefficient, but there are not going to be too many calls in each frame, so let it go.)
[[nodiscard]] static std::string clip_path(const pathT& p, const float avail_w, bool* clipped = nullptr) {
    auto set_clipped = [&clipped](bool b) {
        if (clipped) {
            *clipped = b;
        }
    };
    if (p.empty()) {
        set_clipped(false);
        return "";
    }

    std::string full_str = cpp17_u8string(p);
    const float full_w = imgui_CalcTextSize(full_str).x;
    if (full_w <= avail_w) {
        set_clipped(false);
        return full_str;
    } else {
        set_clipped(true);
        if (!p.has_relative_path()) {
            return full_str;
        }

        // Try to make a shorter string in the form of:
        // .../longest suffix in the relative_path within `avail_w`, always including the last element.
        std::vector<pathT> segs;
        for (pathT seg : p.relative_path()) {
            segs.push_back(std::move(seg));
        }
        assert(!segs.empty());

        // ~ `&sep + 1` is valid, see:
        // https://stackoverflow.com/questions/14505851/is-the-one-past-the-end-pointer-of-a-non-array-type-a-valid-concept-in-c
        const char sep = pathT::preferred_separator;
        const float sep_w = imgui_CalcTextSize({&sep, 1}).x;

        std::vector<std::string> vec;
        vec.push_back(cpp17_u8string(segs.back()));
        float suffix_w = imgui_CalcTextSize(vec.back()).x + imgui_CalcTextSize("...").x + sep_w;
        for (auto pos = segs.rbegin() + 1; pos != segs.rend(); ++pos) {
            std::string seg_str = cpp17_u8string(*pos);
            const float seg_w = imgui_CalcTextSize(seg_str).x;
            if (suffix_w + (seg_w + sep_w) <= avail_w) {
                suffix_w += (seg_w + sep_w);
                vec.push_back(std::move(seg_str));
            } else {
                break;
            }
        }

        if (suffix_w > full_w) {
            // This may happen in rare cases like `C:/very-long-name`. ('C:' ~> '...' makes the result longer.)
            return full_str;
        } else {
            std::string str = "...";
            for (auto pos = vec.rbegin(); pos != vec.rend(); ++pos) {
                str += sep;
                str += *pos;
            }
            return str;
        }
    }
}

// TODO: whether to support opening folder with `Platform_OpenInShellFn` (SDL_OpenUrl)?
static void display_path(const pathT& p, float avail_w) {
    bool clipped = false;
    imgui_Str(clip_path(p, avail_w, &clipped));
    rclick_popup::popup(imgui_GetItemPosID(), [&] {
        if (ImGui::Selectable("Copy path")) {
            set_clipboard_and_notify(cpp17_u8string(p));
        }
        if constexpr (debug_mode) {
            ImGuiContext& g = *ImGui::GetCurrentContext();
            if (g.PlatformIO.Platform_OpenInShellFn) {
                if (ImGui::Selectable("Open locally")) {
                    g.PlatformIO.Platform_OpenInShellFn(&g, cpp17_u8string(p).c_str());
                }
            }
        }
    });
    if (clipped) {
        imgui_ItemTooltip([&] { imgui_Str(cpp17_u8string(p)); });
    }
}
static void display_filename(const pathT& p) {
    const char prefix[]{'.', '.', '.', char(pathT::preferred_separator), '\0'};
    imgui_Str(prefix + cpp17_u8string(p.filename()));
    rclick_popup::popup(imgui_GetItemPosID(), [&] {
        if (ImGui::Selectable("Copy path")) {
            set_clipboard_and_notify(cpp17_u8string(p));
        }
    });
    imgui_ItemTooltip([&] { imgui_Str(cpp17_u8string(p)); });
}

class folderT {
    struct entryT {
        pathT name;
        std::string str;
        entryT(pathT&& n) noexcept : name(std::move(n)), str(cpp17_u8string(name)) {}
    };

    pathT m_path{};
    std::vector<entryT> m_dirs{}, m_files{};

    static void collect(const pathT& path, std::vector<entryT>& dirs, std::vector<entryT>& files) noexcept(false) {
        assert(dirs.empty() && files.empty());
        for (const auto& entry :
             std::filesystem::directory_iterator(path, std::filesystem::directory_options::skip_permission_denied)) {
            std::error_code ec{};
            if (const auto status = entry.status(ec); !ec) {
                const bool is_dir = std::filesystem::is_directory(status);
                const bool is_file = !is_dir && std::filesystem::is_regular_file(status);
                if (is_dir || is_file) {
                    std::vector<entryT>& dest = is_dir ? dirs : files;
                    if (pathT name = entry.path().filename(); !name.empty()) {
                        dest.emplace_back(std::move(name));
                    } else [[unlikely]] {
                        assert(false);
                        // 2024/12/25
                        // Not seen in my environment (windows), but this will happen if entry.path() ends with a separator.
                        // 1. The standard doesn't say whether that's possible.
                        // 2. For a path that ends with separators, the standard doesn't provide a way to extract the filename directly.
                        // 3. They don't even bother to provide a method to remove trailing separators.
                        // https://www.reddit.com/r/cpp/comments/1bioa6x/why_is_there_no_remove_trailing_separator_and_has/
                        // I wasted almost one hour on this and realized there is again no "efficient" way to deal with it, just like when I was messing with those fancy-neo-cpp20-styled utf8 strings.
                        // If it's not for avoiding dragging in an extra library, I'd never want to work with these craps...
                        pathT nAm_E = entry.path().parent_path().filename();
                        dest.emplace_back(!nAm_E.empty() ? std::move(nAm_E) : "why??");
                    }
                }
            }
        }
    }

    void swap(folderT& other) noexcept {
        m_path.swap(other.m_path);
        m_dirs.swap(other.m_dirs);
        m_files.swap(other.m_files);
    }

public:
    folderT() noexcept = default;

    bool valid() const noexcept {
        assert_implies(m_path.empty(), m_dirs.empty() && m_files.empty());
        return !m_path.empty();
    }

    // Canonical.
    const auto& path() const noexcept { return m_path; }

    // Will be empty when !valid().
    const auto& dirs() const noexcept { return m_dirs; }
    const auto& files() const noexcept { return m_files; }

    // TODO: ideally should only accept entry in m_files/m_dirs...
    pathT operator/(const pathT& path) const noexcept {
        // ~ `operator/` uses preferred-sep so the result should be all-preferred too.
        return m_path / path;
    }

    void clear() noexcept {
        m_path.clear();
        m_dirs.clear();
        m_files.clear();
        assert(!valid());
    }

    bool assign_dir(const pathT& path) noexcept {
        if (path.empty()) {
            return false;
        }

        try {
            pathT cp = std::filesystem::canonical(m_path / path);
            std::vector<entryT> dirs, files;
            collect(cp, dirs, files);
            m_path.swap(cp);
            m_dirs.swap(dirs);
            m_files.swap(files);
            return true;
        } catch (...) {
            return false;
        }
    }

    bool refresh() noexcept {
        if (!valid()) {
            return false;
        }

        try {
            std::vector<entryT> dirs, files;
            collect(m_path, dirs, files);
            m_dirs.swap(dirs);
            m_files.swap(files);
            return true;
        } catch (...) {
            return false;
        }
    }

    bool assign_dir_or_file(const pathT& path, std::optional<pathT>& out_file) noexcept {
        if (path.empty()) {
            return false;
        }

        std::error_code ec{};
        const pathT p = m_path / path;
        const auto status = std::filesystem::status(p, ec);
        if (ec) {
            return false;
        }

        if (std::filesystem::is_directory(status)) {
            return assign_dir(p);
        } else if (std::filesystem::is_regular_file(status)) {
            folderT temp;
            if (!temp.assign_dir(p / "..")) { // 'p' may contain trailing sep, so parent_path doesn't apply here.
                return false;
            }

            // Convert 'path' to the format so the equivalence can be checked by pure string comparision.
            // Have to resort to this horribly inefficient one-by-one test, as the format of filenames are unclear in both sides...
            // (Especially, it's unclear whether directory-entry.path() has "canonical" filename...)
            for (const entryT& file : temp.m_files) {
                if (std::filesystem::equivalent(temp / file.name, p, ec)) {
                    out_file.emplace(temp / file.name);
                    swap(temp);
                    return true;
                }
            }
        }
        return false;
    }
};

class file_nav : no_copy {
    char buf_path[200]{};
    char buf_filter[20]{}; // For files.

    pathT m_home{};
    folderT m_current{};

public:
    bool valid() const { return m_current.valid(); }

    void refresh_if_valid() {
        if (m_current.valid()) {
            if (!m_current.refresh()) {
                messenger::set_msg("Cannot refresh the current folder.");
                // TODO: whether to clear m_current?
            } else {
                messenger::set_msg("Done.");
            }
        }
    }

    explicit file_nav(const std::string& u8path) {
        if (!u8path.empty()) {
            std::error_code ec{};
            const pathT p = cpp17_u8path(u8path);
            if (!p.empty() && std::filesystem::is_directory(p, ec)) {
                if (pathT cp = std::filesystem::canonical(p, ec); !ec) {
                    if (m_current.assign_dir(cp)) {
                        m_home.swap(cp);
                    }
                }
            }
        }
    }

    void input_path(std::optional<pathT>& target) {
        if (input_text("Open", buf_path, "Folder or file path", ImGuiInputTextFlags_EnterReturnsTrue) &&
            buf_path[0] != '\0') {
            // It's impressive that path has implicit c-str ctor... why?
            if (!m_current.assign_dir_or_file(cpp17_u8path(buf_path), target)) {
                messenger::set_msg("Cannot open this path.");
            }

            buf_path[0] = '\0';
        }
    }

    void input_filter() { input_text("Filter", buf_filter); }

    void select_file(std::optional<pathT>& target, const pathT* current_file /*name*/ = nullptr, int* pid = nullptr) {
        if (auto child = imgui_ChildWindow("Files")) {
            int id = pid ? *pid : 0;
            bool any = false;
            for (const auto& [file, str] : m_current.files()) {
                if (!buf_filter[0] || str.find(buf_filter) != str.npos) {
                    any = true;
                    const bool selected = current_file && file == *current_file;
                    if (imgui_SelectableStyledButtonEx(id++, str, selected)) {
                        target = m_current / file;
                    }
                    if (selected && ImGui::IsWindowAppearing()) {
                        ImGui::SetScrollHereY();
                    }
                }
            }
            if (!any) {
                imgui_StrDisabled("None");
            }
            if (pid) {
                *pid = id;
            }
        }
    }

#if 0
    // TODO: redesign, and consider supporting recently-opened files as well.
    void select_history() {
        // ImGui::SetNextItemWidth(item_width);
        // imgui_StepSliderInt("Limit", &max_record, 5, 15);
        imgui_Str("Recently opened folders");
        if (m_record.size() > max_record) {
            m_record.resize(max_record);
        }
        ImGui::Separator();
        if (auto child = imgui_ChildWindow("Records")) {
            if (m_record.empty()) {
                imgui_StrDisabled("None");
            }
            const pathT* sel = nullptr;
            for (bool f = true; const pathT& p : m_record) {
                const bool is_current = std::exchange(f, false) && p == m_current;
                // if (ImGui::Selectable(clip_path(p, ImGui::GetContentRegionAvail().x).c_str(), is_current)) {
                if (imgui_SelectableStyledButton(clip_path(p, ImGui::GetContentRegionAvail().x).c_str(), is_current)) {
                    sel = &p;
                }
            }
            if (sel) {
                set_current(*sel);
            }
        }
    }
#endif

    void show_current() {
        if (m_current.valid()) {
            display_path(m_current.path(), ImGui::GetContentRegionAvail().x);
        } else {
            imgui_StrDisabled("N/A");
        }
    }

    // Return one of file path in `m_current`.
    std::optional<pathT> display() {
        std::optional<pathT> target = std::nullopt;
        auto set_dir = [&](const pathT& path) {
            if (!m_current.assign_dir(path)) {
                messenger::set_msg("Cannot open this folder.");
            }
        };

        if (ImGui::BeginTable("##Table", 2, ImGuiTableFlags_Resizable)) {
            imgui_LockTableLayoutWithMinColumnWidth(140); // TODO: improve...
            ImGui::TableNextRow();
            ImGui::TableNextColumn();
            int id = 0; // For selectables.
            {
                ImGui::SetNextItemWidth(std::min(ImGui::CalcItemWidth(), (float)item_width));
                input_path(target);
                ImGui::Separator();

                // TODO: reconsider disabled vs hiding...
                // ImGui::BeginDisabled(m_home.empty());
                if (!m_home.empty()) {
                    if (imgui_SelectableStyledButtonEx(id++, "Home")) {
                        set_dir(m_home);
                    }
                }
                // ImGui::EndDisabled();
                // if (m_home.empty()) {
                //     imgui_ItemTooltip("Not available.");
                // }

                // ImGui::BeginDisabled(!m_current.valid());
                if (imgui_SelectableStyledButtonEx(id++, "..")) {
                    set_dir(".."); // (Both ".." and m_current.path().parent_path() work here.)
                }
                // ImGui::EndDisabled();

                ImGui::Separator();
                if (auto child = imgui_ChildWindow("Folders")) {
                    if (m_current.dirs().empty()) {
                        imgui_StrDisabled("None");
                    }
                    const pathT* sel = nullptr;
                    for (const auto& [dir, str] : m_current.dirs()) {
                        if (imgui_SelectableStyledButtonEx(id++, str)) {
                            sel = &dir;
                        }
                    }
                    if (sel) {
                        set_dir(m_current / (*sel));
                    }
                }
            }
            ImGui::TableNextColumn();
            ImGui::SetNextItemWidth(std::min(ImGui::CalcItemWidth(), (float)item_width));
            input_filter();
            ImGui::Separator();
            select_file(target, nullptr, &id);
            ImGui::EndTable();
        }

        return target;
    }
};

struct preview_setting {
    bool enabled = true;
    previewer::configT config = previewer::configT::_220_160;
};

// !!TODO: (v0.9.9?v0.9.8) support loading pattern from text page directly.

// It is easy to locate all rules in the text via `extract_MAP_str`.
// However there are no easy ways to locate or highlight (only) the rule across the lines.
// See: https://github.com/ocornut/imgui/issues/2313
// So, currently `textT` is line-based, and only recognizes the first rule for each line, and will
// highlight the whole line if the line contains a rule.
class textT : no_copy {
    std::string m_text{};
    std::vector<aniso::compressT> m_rules{};

    // Won't be invalidated by reallocation.
    struct str_ref {
        int begin = 0, size = 0;
        std::string_view get(const decltype(m_text)& text) const { //
            return {text.data() + begin, (size_t)size};
        }
    };
    struct rule_ref {
        int pos = -1;
        bool has_value() const { return pos != -1; }
        const aniso::compressT& get(const decltype(m_rules)& rules) const {
            assert(pos >= 0 && pos < (int)rules.size());
            return rules[pos];
        }
    };
    struct line_ref {
        str_ref str = {};   // -> `m_text`
        rule_ref rule = {}; // -> `m_rules`
        bool highlight = false;
        bool eq_last = false;
    };

    std::vector<line_ref> m_lines{};
    std::vector<int> m_highlighted{}; // -> `m_lines`

    line_ref& _append_line(const std::string_view line) {
        const str_ref ref = {(int)m_text.size(), (int)line.size()};
        if (!line.empty()) {
            m_text += line;
        }
        return m_lines.emplace_back(ref);
    }
    void _attach_rule(line_ref& line, const aniso::ruleT& rule) {
        m_rules.emplace_back(rule);
        const int pos = m_rules.size() - 1;
        line.rule.pos = pos;
        line.eq_last = pos > 0 ? m_rules[pos] == m_rules[pos - 1] : false;
    }

    std::optional<int> m_pos = std::nullopt; // `display` iterated to `m_pos` last time.

    struct selT {
        int beg = 0, end = 0; // []
        bool contains(int l) const {
            if (beg < end) {
                return beg <= l && l <= end;
            } else {
                return end <= l && l <= beg;
            }
        }
        std::pair<int, int> minmax() const { return std::minmax(beg, end); }
    };
    std::optional<selT> m_sel = std::nullopt;

    preview_setting m_preview{}; // TODO: move out of class?

    bool do_rewind = false;
    int go_line = -1;

public:
    textT() = default;
    // textT(std::string_view str) { append(str); }

    bool empty() const { return m_lines.empty(); }

    // (No need to be accurate.)
    bool roughly_check_larger(int size, int line, int sec) const { //
        return (m_text.size() + m_lines.size()) > size || m_lines.size() > line || m_highlighted.size() > sec;
    }

    void clear() {
        m_lines.clear();
        m_rules.clear();
        m_text.clear();
        m_highlighted.clear();

        m_pos.reset();
        m_sel.reset();

        do_rewind = false;
        go_line = -1;
    }

    // `str` is assumed to be utf8-encoded. (If not, the rules are still extractable.)
    void append(std::string str, const std::string_view prefix = {}) {
        std::erase(str, '\r'); // So there won't exist "empty" lines with single invisible '\r'.
        for (const auto& l : std::views::split(str, '\n')) {
            std::string_view sv{l.data(), l.size()};
            const bool highlight = !prefix.empty() && sv.starts_with(prefix);
            if (highlight) {
                sv.remove_prefix(prefix.size());
            }

            line_ref& line = _append_line(sv);
            line.highlight = highlight;
            if (highlight) {
                m_highlighted.push_back(m_lines.size() - 1);
            }
            if (const auto extr = aniso::extract_MAP_str(l); extr.has_rule()) {
                _attach_rule(line, extr.get_rule());
            }
        }
    }

    void reset_scroll() { do_rewind = true; }

    void set_last_sec() {
        if (!m_highlighted.empty()) {
            go_line = m_highlighted.back();
        }
    }

    void select_line() {
        const bool window_appearing = ImGui::IsWindowAppearing();
        static input_int<6> input_line;
        if (window_appearing) {
            (void)input_line.flush();
        }

        ImGui::AlignTextToFramePadding();
        imgui_Str("Go to line ~ ");
        ImGui::SameLine(0, 0); // TODO: show "Max:N/A" if m_lines.empty?
        ImGui::SetNextItemWidth(imgui_CalcButtonSize("MAX:000000").x);
        if (auto l = input_line.input("##Line", std::format("Max:{}", m_lines.size()).c_str()); l && !m_lines.empty()) {
            go_line = std::clamp(*l - 1, 0, (int)m_lines.size() - 1);
        }
        if (!m_highlighted.empty()) {
            ImGui::Separator();

            // TODO: are there easy ways to introduce vertical scrollbar, without messing with width?
            const int limit = 10;
            const float h = std::min((int)m_highlighted.size(), limit) *
                            (ImGui::GetFontSize() + 4 /*imgui_SelectableStyledButton*/);
            float w = 0;
            if (window_appearing) { // (As tested, it's ok to specify width only when appearing.)
                for (const int l : m_highlighted) {
                    w = std::max(w, imgui_CalcLabelSize(m_lines[l].str.get(m_text)).x);
                }
                if (m_highlighted.size() > limit) {
                    w += ImGui::GetStyle().ScrollbarSize;
                }
            }
            if (auto child = imgui_ChildWindow("Sections", {w, h})) {
                for (int id = 0; const int l : m_highlighted) {
                    if (imgui_SelectableStyledButtonEx(id++, m_lines[l].str.get(m_text))) {
                        go_line = l;
                    }
                }
            }
        }
    }

    void display() {
        assert_implies(m_lines.empty(), m_text.empty() && m_rules.empty());

        if (m_sel) {
            if (ImGui::IsWindowAppearing()) {
                // This should not happen, as the interaction to the parent window will be blocked
                // when there are selected lines.
                assert(false);
                m_sel.reset(); // Defensive.
            } else if (!ImGui::IsMouseDown(ImGuiMouseButton_Right) /* From anywhere */) {
                // Note: `IsMouseReleased` may fail to catch release event in rare cases. For example:
                // [right-down] -> left-click the program-window's title bar [both-down] ->
                // release right mouse [left-down], then a menu will appear -> minimize and restore the program.
                m_sel.reset();
            } else if (ImGui::IsMouseClicked(ImGuiMouseButton_Left) /* From anywhere */ ||
                       shortcuts::test_pressed(ImGuiKey_C) /*Raw test; the interaction will be locked by `m_sel`*/) {
                const auto [min, max] = m_sel->minmax();
                std::string str;
                for (int i = min; i <= max; ++i) {
                    if (i != min) {
                        str += '\n';
                    }
                    str += m_lines[i].str.get(m_text);
                }
                // (wontfix) Won't copy if `str` contains '\0' (not expected to appear in utf8 text files).
                set_clipboard_and_notify(str);
            }
        }

        {
            // Precedence:
            // Line-selecting > iterating > (starting line-selection) > left-click setting
            std::optional<int> iter_pos = display_header(m_rules.size(), m_pos);
            if (!m_rules.empty()) {
                // ImGui::SameLine();
                // ImGui::Checkbox("Preview", &m_preview.enabled); // TODO: whether to support hiding preview windows?
                assert(m_preview.enabled);
                if (m_preview.enabled) {
                    ImGui::SameLine();
                    m_preview.config.set("Settings");
                }
            }
            ImGui::Separator();

            if (m_sel) {
                iter_pos.reset();
                do_rewind = false;
                go_line = -1;
            }

            if (std::exchange(do_rewind, false)) {
                ImGui::SetNextWindowScroll({0, 0});
            }
            const auto [click_pos, sel] = display_page(iter_pos.value_or(-1), std::exchange(go_line, -1));
            if (m_sel) {
                if (sel) {
                    m_sel = *sel;
                }
            } else if (iter_pos) {
                m_pos = *iter_pos;
            } else if (sel) {
                m_sel = sel;
            } else if (click_pos) {
                assert(*click_pos >= 0 && *click_pos < int(m_rules.size()));
                // out.set(m_rules[*click_pos]); // !!TODO: redesign...
                m_pos = *click_pos;
            }
        }

        // Prevent interaction with other widgets, so that for example, the parent window cannot be
        // closed when there are selected lines.
        if (m_sel) {
            const ImGuiID claim = ImGui::GetID("Claim");
            ImGuiWindow* const window = ImGui::GetCurrentWindow();

            // (Idk which are actually necessary/preferable, but the following combination works as intended.)

            // ImGui::SetHoveredID(claim);
            ImGui::SetActiveID(claim, window);
            // ImGui::SetFocusID(claim, window);
            ImGui::FocusWindow(window);

            // Otherwise, for some reason the parent window will still be collapsed if its
            // title bar is double-clicked.
            // Related: https://github.com/ocornut/imgui/issues/7841
            ImGui::SetKeyOwner(ImGuiKey_MouseLeft, claim);
            // ImGui::SetKeyOwner(ImGuiKey_MouseRight, claim);
        }
    }

private:
    static std::optional<int> display_header(const int total, const std::optional<int> m_pos) {
        std::optional<int> pos = std::nullopt;

        if (total != 0) {
            switch (sequence::seq("<|", "Prev", "Next", "|>")) {
                case 0: pos = 0; break;
                case 1: pos = std::max(0, m_pos.value_or(-1) - 1); break;
                case 2: pos = std::min(total - 1, m_pos.value_or(-1) + 1); break;
                case 3: pos = total - 1; break;
            }

            ImGui::SameLine();
            if (m_pos.has_value()) {
                ImGui::Text("Total:%d At:%d", total, *m_pos + 1);
            } else {
                ImGui::Text("Total:%d At:N/A", total);
            }
        } else {
            imgui_Str("(No rules)");
        }

        return pos;
    }

    struct passT {
        std::optional<int> pos = std::nullopt;
        std::optional<selT> sel = std::nullopt;
    };

    [[nodiscard]] passT display_page(const int locate_rule, const int locate_line) const {
        const bool locating = locate_rule >= 0 || locate_line >= 0;
        assert_implies(m_sel, !locating);
        passT pass{};

        // TODO: ?`imgui_FillAvailRect(IM_COL32_GREY(24, 255));`
        ImGui::PushStyleColor(ImGuiCol_ChildBg, IM_COL32_GREY(24, 255));
        if (auto child = imgui_ChildWindow("Content")) {
            // set_scroll_by_up_down(ImGui::GetTextLineHeight() * 2);

            const bool test_hover = (ImGui::IsWindowHovered() || m_sel) && ImGui::IsMousePosValid();
            const ImVec2 mouse_pos = ImGui::GetMousePos();
            const float region_max_x = imgui_ContentRegionMaxAbsX();
            ImDrawList* const drawlist = ImGui::GetWindowDrawList();

            // (Not trying to align with larger numbers (>=1000) at the beginning.)
            const int digit_width = m_lines.size() < 100 ? 2 : 3;
            ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
            for (int l = 0; const auto& [str, rule, highlight, eq_last] : m_lines) {
                const int this_l = l++;
                ImGui::TextDisabled("%*d ", digit_width, this_l + 1);
                if (locate_line == this_l) {
                    ImGui::SetScrollHereY(0);
                }
                ImGui::SameLine();
                if (m_preview.enabled && rule.has_value()) {
                    ImGui::BeginGroup();
                }

                if (highlight) {
                    ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 255, 160, 255));
                }
                // (`ImGui::TextWrapped` has no problem rendering long single-lines now.)
                // (Related: https://github.com/ocornut/imgui/issues/7496)
                imgui_StrWrapped(str.get(m_text), item_width);
                if (highlight) {
                    ImGui::PopStyleColor();
                }

                const auto [str_min, str_max] = imgui_GetItemRect();
                const bool line_hovered = test_hover && mouse_pos.y >= str_min.y && mouse_pos.y < str_max.y;
                // `line_hovered` may become true for two adjacent lines if using `mouse_pos.y <= str_max.y`.

                if (!locating && line_hovered) {
                    if (ImGui::IsMouseClicked(ImGuiMouseButton_Right)) {
                        pass.sel = {this_l, this_l};
                    } else if (m_sel && ImGui::IsMouseDown(ImGuiMouseButton_Right)) {
                        pass.sel = {m_sel->beg, this_l};
                    }
                }
                if (m_sel && m_sel->contains(this_l)) {
                    drawlist->AddRectFilled(str_min, {region_max_x, str_max.y}, IM_COL32_GREY(255, 16));
                    drawlist->AddRectFilled(str_min, str_max, IM_COL32_GREY(255, 40));
                }

                if (rule.has_value()) {
                    // TODO: temporarily preserved.
                    constexpr bool has_lock = false;

                    if (rule.pos == m_pos) {
                        drawlist->AddRectFilled(str_min, str_max, IM_COL32(has_lock ? 196 : 0, 255, 0, 60));
                    }
                    if (!m_sel &&
                        (line_hovered && mouse_pos.x >= str_min.x && mouse_pos.x < str_max.x /*str-hovered*/)) {
                        drawlist->AddRectFilled(str_min, str_max, IM_COL32(has_lock ? 196 : 0, 255, 0, 30));
                        if (!locating && ImGui::IsMouseClicked(ImGuiMouseButton_Left)) {
                            pass.pos = rule.pos;
                        }
                    }

                    if (m_preview.enabled) {
                        imgui_StrDisabled("-: ");
                        ImGui::SameLine();
                        if (eq_last) {
                            imgui_StrDisabled("The same as the last rule.");
                        } else {
                            // Workaround to avoid affecting popup & tooltip.
                            ImGui::PopStyleVar();
                            previewer::preview(rule.pos, m_preview.config, rule.get(m_rules) /*cheap call*/);
                            ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
                        }
                        ImGui::EndGroup();
                    }

                    // It's ok to test fully-visible even if the region is not large enough.
                    if (rule.pos == locate_rule && !imgui_ItemFullyVisible()) {
                        ImGui::SetScrollHereY();
                    }
                }
            }
            ImGui::PopStyleVar();
        }
        ImGui::PopStyleColor();

        assert_implies(locating, !pass.pos && !pass.sel);
        return pass;
    }
};

// These limits are not inherent to textT's functions, but just arbitrary numbers
// small enough to guarantee performance and large enough for normal use cases.
static const int max_size = 1024 * 256;
static const int max_line = 20000;

// For error message.
static std::string to_size(uintmax_t size) {
    const bool use_mb = size >= 1024 * 1024;
    return std::format("{:.2f}{}", size / (use_mb ? (1024 * 1024.0) : 1024.0), use_mb ? "MB" : "KB");
}

[[nodiscard]] static bool load_binary(const pathT& path, std::string& str) /*noexcept*/ {
    std::error_code ec{};
    const auto size = std::filesystem::file_size(path, ec);
    if (!ec && size <= max_size) {
        std::ifstream file(path, std::ios::in | std::ios::binary);
        if (file) {
            std::string data(size, '\0');
            file.read(data.data(), size);
            if (file && file.gcount() == size) {
                str.swap(data);
                return true;
            }
        }
    }

    if (!ec && size > max_size) {
        messenger::set_msg("File too large: {} > {}", to_size(size), to_size(max_size));
    } else {
        messenger::set_msg("Failed to load file.");
    }
    return false;
}

static int count_line(const std::string_view str) { //
    return std::ranges::count(str, '\n');
}

// TODO: support opening multiple files?
// TODO: add a mode to avoid opening files without rules?
void load_file() {
    static file_nav nav(home_path_utf8());

    static textT text;
    static std::optional<pathT> path;
    static auto try_load = [](const pathT& p) -> bool {
        if (std::string str; load_binary(p, str)) {
            if (const int l = count_line(str); l > max_line) {
                messenger::set_msg("The file contains too many lines: {} > {}", l, max_line);
                return false;
            }
            text.clear();
            if constexpr (debug_mode) {
                text.append(std::move(str), "@@");
            } else {
                text.append(std::move(str));
            }
            return true;
        }
        return false;
    };

    if (!path) {
        // ImGui::BeginDisabled(!nav.valid());
        if (ImGui::SmallButton("Refresh")) {
            nav.refresh_if_valid();
        }
        // ImGui::EndDisabled();
#if 0
        ImGui::SameLine();
        // `BeginPopup` will consume the settings, even if not opened.
        ImGui::SetNextWindowSize({300, 200}, ImGuiCond_Always);
        menu_like_popup::small_button("Recent");
        menu_like_popup::popup([] { nav.selet_history(); });
#endif
        ImGui::SameLine();
        nav.show_current();

        ImGui::Separator();
        if (auto sel = nav.display(); sel && try_load(*sel)) {
            text.reset_scroll();
            path = std::move(*sel);
        }
    } else {
        const bool close = ImGui::SmallButton("Close");
        ImGui::SameLine();
        if (ImGui::SmallButton("Reload")) {
            if (try_load(*path)) {
                messenger::set_msg("Done.");
            }
            // Won't reset scroll.
        }
        guide_mode::item_tooltip("Reload from disk.");
        ImGui::SameLine();
        ImGui::SetNextWindowSize({300, 200}, ImGuiCond_Always);
        menu_like_popup::small_button("Select");
        menu_like_popup::popup([] {
            std::optional<pathT> sel = std::nullopt;
            {
                if (ImGui::Button("Refresh")) {
                    nav.refresh_if_valid();
                }
                ImGui::SameLine();
                ImGui::SetNextItemWidth(floor(item_width * 0.75));
                nav.input_filter();
                ImGui::Separator();
                const pathT name = path->filename();
                nav.select_file(sel, &name);
            }
            if (sel && try_load(*sel)) {
                text.reset_scroll(); // Even if the new path is the same as the old one.
                path = std::move(*sel);
            }
        });
        ImGui::SameLine();
        display_filename(*path);
        ImGui::SameLine();
        menu_like_popup::small_button(">");
        menu_like_popup::popup([] { text.select_line(); });

        ImGui::Separator();
        text.display();
        if (close) {
            path.reset();
            text.clear();
        }
    }
}

// TODO: 'Read' -> create a temp window that will be destroyed when closed?
void load_clipboard() {
    static textT text;
    static std::string last_str;
    static int last_index = 0;

    // (The page will hold roughly at most 1.5*max_size/line.)
    const bool too_much_content = text.roughly_check_larger(max_size, max_line, 100);
    ImGui::BeginDisabled(too_much_content);
    if (ImGui::SmallButton("Read") || shortcuts::item_shortcut(ImGuiKey_W)) {
        const std::string_view str = read_clipboard();
        if (str.size() > max_size / 2) {
            messenger::set_msg("Text too long: {} > {}", to_size(str.size()), to_size(max_size / 2));
        } else if (const int l = count_line(str); l > max_line / 2) {
            messenger::set_msg("The text contains too many lines: {} > {}", l, max_line / 2);
        } else if (!str.empty() && compare_update(last_str, str)) {
            if (!text.empty()) {
                text.append("\n"); // (Will append two lines.)
            }
            text.append(std::format("@@[{}]", ++last_index), "@@");
            text.append(std::string(str));
            text.set_last_sec();
        }
    }
    ImGui::EndDisabled();
    if (too_much_content) {
        imgui_ItemTooltip("Too much content.");
    }

    ImGui::SameLine();
    if (double_click_button_small("Clear")) {
        set_msg_cleared();
        text.clear();
        last_str.clear();
        last_index = 0;
    }
    ImGui::SameLine();
    imgui_Str("Clipboard");
    ImGui::SameLine();
    menu_like_popup::small_button(">");
    menu_like_popup::popup([] { text.select_line(); });

    ImGui::Separator();
    text.display();
}

// Defined in "docs.cpp". [0]:title [1]:contents, null-terminated.
extern const char* const docs[][2];

void load_doc() {
    static textT text;
    static std::optional<int> doc_id = std::nullopt;
    static auto select = []() {
        for (int i = 0; docs[i][0] != nullptr; ++i) {
            const auto [title, contents] = docs[i];
            // if (ImGui::Selectable(title, doc_id == i, ImGuiSelectableFlags_NoAutoClosePopups) && doc_id != i) {
            if (imgui_SelectableStyledButtonEx(i, title, doc_id == i) && compare_update(doc_id, i)) {
                text.clear();
                text.append(std::string(contents), "@@");
                text.reset_scroll();
            }
        }
    };

    if (!doc_id) {
        imgui_Str("A toy for exploring MAP rules, by GitHub user 'achabense'.");
        imgui_Str("The latest version is available at: ");
        ImGui::SameLine(0, 0);
        constexpr const char* url = "https://github.com/achabense/blueberry";
        // ImGui::TextLinkOpenURL(url);
        imgui_Str(url);
        rclick_popup::popup(imgui_GetItemPosID(), [] {
            if (ImGui::Selectable("Copy link")) {
                set_clipboard_and_notify(url);
            }

            ImGuiContext& g = *ImGui::GetCurrentContext();
            if (g.PlatformIO.Platform_OpenInShellFn) {
                if (ImGui::Selectable("Open in browser")) {
                    g.PlatformIO.Platform_OpenInShellFn(&g, url);
                }
            }
        });

        ImGui::Separator();
        select();
    } else {
        const bool close = ImGui::SmallButton("Close");
        ImGui::SameLine();
        menu_like_popup::small_button("Select");
        menu_like_popup::popup(select);
        ImGui::SameLine();
        imgui_Str(docs[*doc_id][0]);
        ImGui::SameLine();
        menu_like_popup::small_button(">");
        menu_like_popup::popup([] { text.select_line(); });

        ImGui::Separator();
        text.display();
        if (close) {
            text.clear();
            doc_id.reset();
        }
    }
}
