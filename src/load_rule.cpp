#define _CRT_SECURE_NO_WARNINGS // For `localtime`
#include <filesystem>
#include <fstream>
#include <ranges>

#include "common.hpp"

// By default the project does not care about exceptions (taking as if they don't exist), but std::filesystem is an exception to this...
// (& `bad_alloc` is always considered impossible to happen.)

using pathT = std::filesystem::path;

// It's unclear whether these functions can fail due to transcoding...

// The direct conversion between path and utf8-encoded std::string was broken by this paper.
// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0482r6.html
static std::string cpp17_u8string_maythrow(const pathT& path) {
    const auto u8string = path.u8string();
    return std::string(u8string.begin(), u8string.end());
}
static pathT cpp17_u8path_maythrow(const std::string_view path) {
    // There is no well-defined way to get a u8string_view from a string_view.
    // https://stackoverflow.com/questions/57603013/how-to-safely-convert-const-char-to-const-char8-t-in-c20
    return pathT(std::u8string(path.begin(), path.end()));
}

static std::optional<std::string> cpp17_u8string(const pathT& path) noexcept {
    try {
        return cpp17_u8string_maythrow(path);
    } catch (...) {
        assert(false);
        return std::nullopt;
    }
}
static std::string cpp17_u8string_b(const pathT& path) noexcept {
    try {
        return cpp17_u8string_maythrow(path);
    } catch (...) {
        assert(false);
        return "?";
    }
}
static std::optional<pathT> cpp17_u8path(const std::string_view path) noexcept {
    try {
        return cpp17_u8path_maythrow(path);
    } catch (...) {
        assert(false);
        return std::nullopt;
    }
}

static const pathT& get_home_path() /*noexcept*/ {
    static pathT p = []() -> pathT {
        try {
            if (const auto p = home_path_utf8(); !p.empty()) {
                return std::filesystem::canonical(cpp17_u8path_maythrow(p));
            }
        } catch (...) {
        }
        return {};
    }();
    return p;
}

// I hate this part so much...
// (This is horribly inefficient, but there are not going to be too many calls in each frame, so let it go.)
[[nodiscard]] static std::string clip_path(const pathT& p, const float avail_w, bool& clipped) {
    if (p.empty()) {
        clipped = false;
        return {};
    }

    std::string full_str = cpp17_u8string_b(p);
    const float full_w = imgui_CalcTextSize(full_str).x;
    if (full_w <= avail_w) {
        clipped = false;
        return full_str;
    } else {
        clipped = true;
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

        // (Btw, `imgui_CalcTextSize({&sep, 1})` is valid.)
        // https://stackoverflow.com/questions/14505851/is-the-one-past-the-end-pointer-of-a-non-array-type-a-valid-concept-in-c
        const char sep = pathT::preferred_separator;
        const float sep_w = imgui_CalcCharWidth(sep);

        std::vector<std::string> vec;
        vec.push_back(cpp17_u8string_b(segs.back()));
        float suffix_w = imgui_CalcTextSize(vec.back()).x + imgui_CalcTextSize("...").x + sep_w;
        for (auto pos = segs.rbegin() + 1; pos != segs.rend(); ++pos) {
            std::string seg_str = cpp17_u8string_b(*pos);
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

static void copy_path(const pathT& p) {
    if (const auto str = cpp17_u8string(p)) {
        set_clipboard_and_notify(*str);
    } else {
        messenger::set_msg("Cannot convert to utf-8 text.");
    }
}
static bool has_open_url_fn() { return GImGui->PlatformIO.Platform_OpenInShellFn; }
static bool open_url(const char* url) {
    if (GImGui->PlatformIO.Platform_OpenInShellFn) {
        return GImGui->PlatformIO.Platform_OpenInShellFn(GImGui, url);
    }
    return false;
}
static bool open_folder(const pathT& p) {
    std::error_code ec{};
    if (!std::filesystem::is_directory(p, ec)) {
        messenger::set_msg("Not a folder.");
        return false;
    } else if (const auto str = cpp17_u8string(p)) {
        return open_url(str->c_str());
    } else {
        messenger::set_msg("Cannot convert to utf-8 text.");
        return false;
    }
}

static void display_path(const pathT& p, const float avail_w) {
    bool clipped = false;
    imgui_Str(clip_path(p, avail_w, clipped));
    rclick_popup::popup(imgui_GetItemPosID(), [&] {
        if (ImGui::Selectable("Copy full path")) {
            copy_path(p);
        }
        if (ImGui::Selectable("Copy name")) {
            copy_path(p.filename());
        }
        if (has_open_url_fn()) {
            if (ImGui::Selectable("Open in system")) {
                open_folder(p);
            }
        }
    });
    if (clipped) {
        imgui_ItemTooltip([&] { imgui_Str(cpp17_u8string_b(p)); });
    }
}
static void display_filename(const pathT& p) {
    constexpr char prefix[]{'.', '.', '.', char(pathT::preferred_separator), '\0'};
    imgui_Str(prefix + cpp17_u8string_b(p.filename()));
    rclick_popup::popup(imgui_GetItemPosID(), [&] {
        if (ImGui::Selectable("Copy full path")) {
            copy_path(p);
        }
        if (ImGui::Selectable("Copy name")) {
            copy_path(p.filename());
        }
        if constexpr (debug_mode) {
            // TODO: useful but it's hard to decide the label...
            if (has_open_url_fn()) {
                if (ImGui::Selectable("Open parent path")) {
                    open_folder(p.parent_path());
                }
            }
        }
    });
    imgui_ItemTooltip([&] { imgui_Str(cpp17_u8string_b(p)); });
}

class natural_compare : no_copy {
    static bool is_digit(const char ch) { return ch >= '0' && ch <= '9'; }

    static std::string_view extract_num(const char*& pos, const char* const end) {
        assert(pos < end && is_digit(*pos));
        const char* const beg = pos++;
        while (pos != end && is_digit(*pos)) {
            ++pos;
        }
        std::string_view digits{beg, pos}; // (v So finally weak-ordering.)
        digits.remove_prefix(std::min(digits.size(), digits.find_first_not_of('0')));
        // if (digits.empty()) { digits = "0"; } // Empty str compares fine.
        return digits;
    }

    static std::strong_ordering compare_num(const std::string_view a, const std::string_view b) {
        const auto comp = a.size() <=> b.size();
        return comp != 0 ? comp : a <=> b;
    }

    // (So finally weak-ordering.)
    static char to_lower(const char ch) { return ch >= 'A' && ch <= 'Z' ? 'a' + (ch - 'A') : ch; }

public:
    static std::weak_ordering compare(const std::string_view a, const std::string_view b) {
        const char *a_pos = a.data(), *const a_end = a_pos + a.size();
        const char *b_pos = b.data(), *const b_end = b_pos + b.size();
        while (a_pos != a_end && b_pos != b_end) {
            if (is_digit(*a_pos) && is_digit(*b_pos)) {
                const auto comp = compare_num(extract_num(a_pos, a_end), extract_num(b_pos, b_end));
                if (comp != 0) {
                    return comp;
                }
            } else if (const char a_ch = to_lower(*a_pos++), b_ch = to_lower(*b_pos++); a_ch != b_ch) {
                // (std::string_view compares by unsigned char, not affected by char's signedness.)
                return (unsigned char)a_ch <=> (unsigned char)b_ch;
            }
        }
        return (a_end - a_pos) <=> (b_end - b_pos);
    }

    static bool less(const std::string_view a, const std::string_view b) { //
        return compare(a, b) < 0;
    }
};

class folderT {
public:
    struct entryT {
        bool is_file;
        pathT name;
        std::string str;
        entryT(bool f, pathT&& n) noexcept : is_file(f), name(std::move(n)), str(cpp17_u8string_b(name)) {}
    };

private:
    pathT m_path{};
    std::vector<entryT> m_dirs{}, m_files{};

    static void collect_maythrow(const pathT& path, std::vector<entryT>& dirs, std::vector<entryT>& files) {
        assert(dirs.empty() && files.empty());
        int max_entry = 3000;
        for (const auto& entry :
             std::filesystem::directory_iterator(path, std::filesystem::directory_options::skip_permission_denied)) {
            if (--max_entry < 0) [[unlikely]] { // (Undocumented, but should be rare enough.)
                messenger::set_msg("Too many entries.");
                break; // Instead of throwing.
            }

            // About symlinks:
            // 1. Real symlinks seems to be resolved automatically (directory_iterator & fstream)...
            // 2. Windows shortcuts are not symlinks (they are regular files) and cannot be resolved using filesystem functions...
            std::error_code ec{};
            if (const auto status = entry.status(ec); !ec) {
                const bool is_file = std::filesystem::is_regular_file(status);
                if (is_file || std::filesystem::is_directory(status)) {
                    std::vector<entryT>& dest = is_file ? files : dirs;
                    if (pathT name = entry.path().filename(); !name.empty()) {
                        dest.emplace_back(is_file, std::move(name));
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
                        dest.emplace_back(is_file, !nAm_E.empty() ? std::move(nAm_E) : "why??");
                    }
                }
            }
        }
        if (!dirs.empty()) {
            std::ranges::stable_sort(dirs, natural_compare::less, &entryT::str);
        }
        if (!files.empty()) {
            std::ranges::stable_sort(files, natural_compare::less, &entryT::str);
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
        // Let's assume no real implementation will ever throw from `is_absolute()`...
        if (path.empty() || (m_path.empty() && !path.is_absolute())) {
            return false;
        }

        try {
            pathT cp = std::filesystem::canonical(m_path / path);
            std::vector<entryT> dirs, files;
            collect_maythrow(cp, dirs, files);
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
            collect_maythrow(m_path, dirs, files);
            m_dirs.swap(dirs);
            m_files.swap(files);
            return true;
        } catch (...) {
            return false;
        }
    }

    bool assign_dir_or_file(const pathT& path, std::optional<pathT>& out_file) noexcept {
        if (path.empty() || (m_path.empty() && !path.is_absolute())) {
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

            // Convert 'path' to the format so the equivalence can be checked by pure string comparison.
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

// TODO: support filtering folder names as well?
// TODO: support recording recently-opened folders/files?
class file_nav : no_copy {
    char buf_path[200]{};
    char buf_filter[20]{}; // For files and folders.

    pathT m_home{};
    folderT m_current{};

public:
    bool valid() const { return m_current.valid(); }
    const auto& current_path() const {
        assert(m_current.valid());
        return m_current.path();
    }

    void refresh_if_valid() {
        if (m_current.valid()) {
            if (!m_current.refresh()) {
                messenger::set_msg("Cannot refresh.");
            } else {
                messenger::dot();
            }
        }
    }

    explicit file_nav(const pathT& base) {
        if (!base.empty()) {
            assert(base.is_absolute());
            if (m_current.assign_dir(base)) {
                m_home = m_current.path();
            }
        }
    }

    void input_path(std::optional<pathT>& target) {
        if (input_text("Open", buf_path, "Folder or file path", ImGuiInputTextFlags_EnterReturnsTrue) &&
            buf_path[0] != '\0') {
            // It's impressive that path has implicit c-str ctor... why?
            const auto p = cpp17_u8path(buf_path);
            if (!(p && m_current.assign_dir_or_file(*p, target))) {
                std::error_code ec{};
                messenger::set_msg(p && !std::filesystem::exists(*p, ec) ? "Path doesn't exist." : "Cannot open.");
            }

            buf_path[0] = '\0';
        }
    }

    void input_filter() { input_text("Filter", buf_filter, ".txt"); }

private:
    enum typeE { File, Folder, Both };
    const folderT::entryT* select_entry(const typeE type, int& id, const pathT* current /*name*/ = nullptr) const {
        assert_implies(current, type == File); // (Due to `assign_dir_or_file`.)
        const folderT::entryT* sel = nullptr;
        if (auto child = imgui_ChildWindow("Entries")) {
            bool any = false;

            // To distinguish files and folders.
            const float indent_spacing = imgui_ItemSpacingX() * 2;
            const float inner_spacing = imgui_ItemInnerSpacingX();
            const ImU32 text_disabled_col = ImGui::GetColorU32(ImGuiCol_TextDisabled);
            ImDrawList& drawlist = *ImGui::GetWindowDrawList();
            for (const auto* list :
                 {type != File ? &m_current.dirs() : nullptr, type != Folder ? &m_current.files() : nullptr}) {
                if (!list) {
                    continue;
                }
                for (const folderT::entryT& entry : *list) {
                    if (buf_filter[0] && entry.str.find(buf_filter) == std::string::npos) {
                        continue;
                    }
                    any = true;
                    const bool selected = current && entry.name == *current;
                    const bool indent = type == Both && !entry.is_file;
                    if (indent) {
                        imgui_AddCursorPosX(indent_spacing);
                    }
                    if (imgui_SelectableStyledButtonEx(id++, entry.str, selected)) {
                        sel = &entry;
                    }
                    if (indent) {
                        const ImVec2 item_min = ImGui::GetItemRectMin();
                        const float h = floor(ImGui::GetItemRectSize().y / 2);
                        drawlist.AddLine(item_min + ImVec2(-indent_spacing, h), item_min + ImVec2(-inner_spacing, h),
                                         text_disabled_col);
                    }
                    if (selected && ImGui::IsWindowAppearing()) {
                        ImGui::SetScrollHereY();
                    }
                    rclick_popup::popup2([&] { // (Undocumented.)
                        if (ImGui::Selectable("Copy full path")) {
                            copy_path(m_current / entry.name);
                        }
                        if (ImGui::Selectable("Copy name")) {
                            copy_path(entry.name);
                        }
                    });
                }
            }
            if (!any) {
                imgui_StrDisabled("None");
            }
        }
        return sel;
    }

public:
    std::optional<pathT> select_file(const pathT* current /*name*/ = nullptr, int* p_id = nullptr) {
        int id2 = 0;
        if (const folderT::entryT* sel = select_entry(File, p_id ? *p_id : id2, current)) {
            assert(sel->is_file);
            return m_current / sel->name;
        }
        return std::nullopt;
    }

    // Return one of file path in `m_current`.
    std::optional<pathT> display() {
        std::optional<pathT> target = std::nullopt;
        const auto set_dir = [&](const pathT& path) {
            if (!m_current.assign_dir(path)) {
                messenger::set_msg("Cannot open.");
            }
        };

        ImGui::SetNextItemWidth(floor(item_width * 0.8));
        input_path(target);
        ImGui::SameLine(0, imgui_ItemSpacingX() * 3);
        ImGui::SetNextItemWidth(floor(item_width * 0.6));
        input_filter();

        ImGui::Separator();
        int id = 0; // For selectables.
        {
            // TODO: reconsider disabled vs hiding...
            bool any = false;
            if (!m_home.empty()) {
                any = true;
                if (imgui_SelectableStyledButtonEx(id++, "Home") && messenger::dot()) {
                    set_dir(m_home);
                }
                rclick_popup::popup2([&] { // (Undocumented.)
                    if (ImGui::Selectable("Copy full path")) {
                        copy_path(m_home);
                    }
                    if (ImGui::Selectable("Copy name")) {
                        // m_home ~ canonical ~ won't have trailing sep unless it's root path (nearly impossible).
                        copy_path(m_home.filename());
                    }
                });
            }
            if (m_current.valid()) {
                any = true;
                if (imgui_SelectableStyledButtonEx(id++, "..")) {
                    set_dir(".."); // (Both ".." and m_current.path().parent_path() work here.)
                }
            }
            if (any) {
                ImGui::Separator();
            }
        }

        if (const folderT::entryT* sel = select_entry(Both, id)) {
            if (sel->is_file) {
                target = m_current / sel->name;
            } else {
                set_dir(m_current / sel->name);
            }
        }
        return target;
    }
};

struct preview_setting {
    bool enabled = true;
    previewer::configT config = previewer::default_settings;
};

// It is easy to locate all rules in the text via `extract_MAP_str`.
// However there are no easy ways to locate or highlight (only) the rule across the lines.
// See: https://github.com/ocornut/imgui/issues/2313
// So, currently `textT` is line-based, and only recognizes the first rule for each line, and will
// highlight the whole line if the line contains a rule.
class textT : no_copy {
    std::vector<char> m_text{};
    std::vector<aniso::compressT> m_rules{};

    // Won't be invalidated by reallocation.
    struct str_ref {
        int begin = 0, size = 0;
        std::string_view get(const decltype(m_text)& text) const {
            // (It's safe to return `{nullptr + 0, 0}`.)
            // https://stackoverflow.com/questions/59409034/is-it-allowed-to-add-a-zero-to-a-null-pointer
            // https://stackoverflow.com/questions/79676156/is-string-view-nullptr-0-valid
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
        bool seg_start = false;
    };

    std::vector<line_ref> m_lines{};
    std::vector<int> m_highlighted{}; // -> `m_lines`

    line_ref& _append_line(const std::string_view line) {
        // Intentionally not:
        // if (line.ends_with('\r')) { line.remove_suffix(1); }
        const int old_size = m_text.size();
        for (const char ch : line) {
            // So there won't exist "empty" lines with single invisible '\r'.
            if (ch != '\r') {
                m_text.push_back(ch);
            }
        }
        const int new_size = m_text.size();
        return m_lines.emplace_back(str_ref{.begin = old_size, .size = new_size - old_size});
    }
    void _attach_rule(line_ref& line, const aniso::ruleT& rule) {
        m_rules.emplace_back(rule);
        const int pos = m_rules.size() - 1;
        line.rule.pos = pos;
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
    bool menu_opened = false;

    preview_setting m_preview{}; // TODO: move out of class?

    bool do_rewind = false;
    int go_line = -1;

public:
    textT() = default;
    // textT(std::string_view str) { append(str); }

    bool empty() const { return m_lines.empty(); }
    int lines() const { return m_lines.size(); }
    int length() const { return m_lines.empty() ? 0 : m_text.size() + m_lines.size() - 1; }

    void clear(const bool rewind = true) {
        m_lines.clear();
        m_rules.clear();
        m_text.clear();
        m_highlighted.clear();

        m_pos.reset();
        m_sel.reset();
        menu_opened = false;

        do_rewind = rewind;
        go_line = -1;
    }

    // `str` is assumed to be utf8-encoded. (If not, the rules are still extractable.)
    // (A single "\n" will append two lines as {}{}; however, length() is still 1 for this case, and copying {}{} still results in a single "\n".)
    void append(const std::string_view str, const std::string_view prefix = {}) {
        bool seg_start = true;
        for (const auto& l : std::views::split(str, '\n')) {
            std::string_view sv{l.data(), l.size()};
            const bool has_prefix = !prefix.empty() && sv.starts_with(prefix);
            if (has_prefix) {
                sv.remove_prefix(prefix.size());
            }

            line_ref& line = _append_line(sv);
            line.seg_start = std::exchange(seg_start, false);
            if (has_prefix) {
                line.highlight = true;
                m_highlighted.push_back(m_lines.size() - 1);
            }
            // TODO: support extracting all rules in a line?
            if (const auto extr = aniso::extract_MAP_str(sv); extr.has_rule()) {
                _attach_rule(line, extr.get_rule());
            }
        }
    }

    void assign(const bool rewind, const std::string_view str, const std::string_view prefix = {}) {
        clear(rewind);
        append(str, prefix);
    }

    void reset_scroll() { do_rewind = true; }
    void to_line(int l /*starting from 0*/) {
        if (!m_lines.empty()) {
            go_line = std::clamp(l, 0, (int)m_lines.size() - 1);
        }
    }

    void select_line() {
        const bool window_appearing = ImGui::IsWindowAppearing();
        static input_int input_line{};
        if (window_appearing) {
            (void)input_line.flush();
        }

        ImGui::AlignTextToFramePadding();
        imgui_Str("Go to line ~ ");
        ImGui::SameLine(0, 0); // TODO: show "Max:N/A" if m_lines.empty?
        ImGui::SetNextItemWidth(imgui_CalcButtonSize("MAX:000000").x);
        if (auto l = input_line.input(6, "##Line", std::format("Max:{}", m_lines.size()).c_str())) {
            to_line(*l - 1);
        }
        if (!m_highlighted.empty()) {
            ImGui::Separator();

            // TODO: are there easy ways to introduce vertical scrollbar, without messing with width?
            constexpr int limit = 10;
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

    static constexpr const char* about_selection = "Drag with right button to select lines.";

    void display() {
        assert_implies(m_lines.empty(), m_text.empty() && m_rules.empty());

        if (m_sel) {
            if (ImGui::IsWindowAppearing()) {
                // This should not happen, as the interaction to the parent window will be blocked
                // when there are selected lines.
                assert(false);
                m_sel.reset(); // Defensive.
                menu_opened = false;
            } else {
                const ImGuiID popup_id = ImGui::GetID("Menu");
                if (!menu_opened && !ImGui::IsMouseDown(ImGuiMouseButton_Right) /* Released from anywhere */) {
                    // Note: `IsMouseReleased` may fail to catch release event in rare cases. For example:
                    // [right-down] -> left-click the program-window's title bar [both-down] ->
                    // release right mouse [left-down], then a menu will appear -> minimize and restore the program.
                    ImGui::OpenPopup(popup_id);
                    menu_opened = true;
                }
                if (menu_opened) {
                    if (imgui_BeginPopupRecycled(popup_id)) {
                        lock_scroll();
                        const auto get_str = [&] {
                            const auto [min, max] = m_sel->minmax();
                            std::string str;
                            for (int i = min; i <= max; ++i) {
                                if (i != min) {
                                    str += '\n';
                                }
                                str += m_lines[i].str.get(m_text);
                            }
                            return str;
                        };
                        if (ImGui::Selectable("Copy text")) {
                            // (wontfix) Won't copy if `str` contains '\0' (not expected to appear in utf8 text files).
                            set_clipboard_and_notify(get_str());
                        }
                        // !!TODO: (v0.9.9) support in release mode (currently not well designed)...
                        if constexpr (debug_mode) {
                            static bool can_load_pattern = false;
                            if (ImGui::IsWindowAppearing()) {
                                can_load_pattern = pattern_editor_status::available() && has_pattern(get_str());
                            }
                            if (can_load_pattern) {
                                if (ImGui::Selectable("Load pattern")) {
                                    load_pattern(get_str());
                                }
                            }
                        }
                        if constexpr (debug_mode_double_esc_to_close) {
                            if (test_esc()) {
                                ImGui::CloseCurrentPopup();
                            }
                        }
                        ImGui::EndPopup();
                    } else {
                        menu_opened = false;
                        m_sel.reset();
                    }
                }
            }
        }

        {
            // Precedence:
            // Line-selecting > iterating > (starting line-selection) > left-click setting
            std::optional<int> iter_pos = std::nullopt;
            if (!m_rules.empty()) {
                iter_pos = display_seq(m_rules.size(), m_pos);
                if constexpr (0) {
                    // TODO: whether to support hiding preview windows?
                    ImGui::SameLine();
                    ImGui::Checkbox("Preview", &m_preview.enabled);
                } else {
                    assert(m_preview.enabled);
                }
                if (m_preview.enabled) {
                    ImGui::SameLine();
                    m_preview.config.set("Settings");
                }
            } else {
                imgui_Str("(No rules)");
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
                // set_apply_rule_target(m_rules[*click_pos]); // TODO: whether to support this?
                m_pos = *click_pos;
            }
        }

        // Prevent interaction with other widgets, so that for example, the parent window cannot be
        // closed when there are selected lines.
        if (!menu_opened && m_sel) {
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
    static std::optional<int> display_seq(const int total, std::optional<int>& m_pos) {
        std::optional<int> pos = std::nullopt;
        assert(total > 0);
        switch (sequence::seq("<|", "Prev", "Next", "|>")) {
            case 0: pos = 0; break;
            case 1: pos = std::max(0, m_pos ? *m_pos - 1 : 0); break;
            case 2: pos = std::min(total - 1, m_pos ? *m_pos + 1 : 0); break;
            case 3: pos = total - 1; break;
        }

        ImGui::SameLine();
        if (m_pos.has_value()) {
            ImGui::Text("Total:%d At:%d", total, *m_pos + 1);
        } else {
            ImGui::Text("Total:%d At:N/A", total);
        }
        rclick_popup::popup(imgui_GetItemPosID(), [&] {
            if (ImGui::Selectable("Reset cursor") && messenger::dot()) {
                m_pos.reset();
            }
        });
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

            const bool test_hover = !menu_opened && (ImGui::IsWindowHovered() || m_sel) && ImGui::IsMousePosValid();
            const ImVec2 mouse_pos = ImGui::GetMousePos();
            const float region_max_x = imgui_GetContentRegionMaxAbsX();
            ImDrawList& drawlist = *ImGui::GetWindowDrawList();

            // (Inefficient, but not worth bothering.)
            const int digit_width = m_lines.size() < 100 ? 2 : std::to_string(m_lines.size()).size();
            ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));
            for (int l = 0; const auto& line : m_lines) {
                const int this_l = l++;
                const auto& rule = line.rule;
                if (this_l != 0 && line.seg_start) {
                    ImGui::SeparatorText("");
                }

                ImGui::TextDisabled("%*d ", digit_width, this_l + 1);
                if (locate_line == this_l) {
                    ImGui::SetScrollHereY(0);
                }
                ImGui::SameLine();
                if (m_preview.enabled && rule.has_value()) {
                    ImGui::BeginGroup();
                }

                if (line.highlight) {
                    ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 255, 160, 255));
                }
                // (`ImGui::TextWrapped` has no problem rendering long single-lines now.)
                // (Related: https://github.com/ocornut/imgui/issues/7496)
                imgui_StrWrapped(line.str.get(m_text), item_width);
                if (line.highlight) {
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
                    drawlist.AddRectFilled(str_min, {region_max_x, str_max.y}, IM_COL32_GREY(255, 16));
                    drawlist.AddRectFilled(str_min, str_max, IM_COL32_GREY(255, 40));
                }

                if (rule.has_value()) {
                    constexpr bool has_lock = false; // TODO: temporarily preserved.

                    if (rule.pos == m_pos) {
                        drawlist.AddRectFilled(str_min, str_max, IM_COL32(has_lock ? 196 : 0, 255, 0, 60));
                    }
                    if (!m_sel &&
                        (line_hovered && mouse_pos.x >= str_min.x && mouse_pos.x < str_max.x /*str-hovered*/)) {
                        drawlist.AddRectFilled(str_min, str_max, IM_COL32(has_lock ? 196 : 0, 255, 0, 30));
                        if (!locating && ImGui::IsMouseClicked(ImGuiMouseButton_Left)) {
                            pass.pos = rule.pos;
                        }
                    }

                    // TODO: ideally should not split RLE blob from header....
                    if (m_preview.enabled) {
                        imgui_StrDisabled("-: ");
                        ImGui::SameLine();

                        ImGui::PopStyleVar(); // (Workaround to avoid affecting popup & tooltip.)
                        previewer::preview(rule.pos, m_preview.config, rule.get(m_rules) /*cheap call*/);
                        ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, ImVec2(0, 0));

                        ImGui::EndGroup();
                    }

                    // It's ok to test fully-visible even if the region is not large enough.
                    if (rule.pos == locate_rule && !imgui_IsItemFullyVisible()) {
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

// For error message.
static std::string to_size(uintmax_t size) {
    const bool use_mb = size >= 1024 * 1024;
    return std::format("{:.2f}{}", size / (use_mb ? (1024 * 1024.0) : 1024.0), use_mb ? "MB" : "KB");
}

// <= max_size ~ successful; > ~ too large; -1 ~ other errors.
[[nodiscard]] static std::uintmax_t load_binary(const pathT& path, std::string& str, const int max_size) /*noexcept*/ {
    std::error_code ec{};
    const auto size = std::filesystem::file_size(path, ec);
    assert_implies(ec, size == uintmax_t(-1) && size > max_size);
    if (size > max_size) {
        return size;
    } else {
        if (std::ifstream file(path, std::ios::in | std::ios::binary); file) {
            str.resize(size);
            if (file.read(str.data(), size) && file.gcount() == size) {
                return size;
            }
        }
        return uintmax_t(-1);
    }
}

static int count_line(const std::string_view str) { //
    return str.empty() ? 0 : std::ranges::count(str, '\n') + 1;
}

// These limits are not inherent to textT's functions, but just arbitrary numbers
// small enough to guarantee performance and large enough for normal use cases.
static constexpr int max_size = 1024 * 256;
static constexpr int max_line = 20000;

// TODO: -> m_.
// TODO: support opening multiple files?
// TODO: add a mode to avoid opening files without rules?
class load_file_impl : no_copy {
    file_nav nav;
    textT text;
    std::optional<pathT> file_path = std::nullopt;

    bool try_load(const pathT& p, const bool reset_scroll) {
        std::string str;
        if (const uintmax_t size = load_binary(p, str, max_size); size > max_size) {
            if (size != uintmax_t(-1)) {
                messenger::set_msg("File too large: {} > {}", to_size(size), to_size(max_size));
            } else {
                messenger::set_msg("Cannot open.");
            }
            return false;
        } else if (const int l = count_line(str); l > max_line) {
            messenger::set_msg("Too many lines: {} > {}", l, max_line);
            return false;
        } else {
            if constexpr (debug_mode) {
                text.assign(reset_scroll, str, "@@");
            } else {
                text.assign(reset_scroll, str);
            }
            return true;
        }
    };

public:
    load_file_impl(const pathT& base) : nav(base) {}

    void display() {
        if (!file_path) {
            if (nav.valid()) {
                if (ImGui::SmallButton("Refresh")) {
                    nav.refresh_if_valid();
                }
                // guide_mode::item_tooltip("Reload entry list.");
                ImGui::SameLine();
                display_path(nav.current_path(), ImGui::GetContentRegionAvail().x);
            } else {
                imgui_StrDisabled("N/A");
            }

            ImGui::Separator();
            if (auto sel = nav.display(); sel && try_load(*sel, /*reset-scroll*/ true)) {
                file_path = std::move(*sel);
            }
        } else {
            const bool close = ImGui::SmallButton("Close");
            ImGui::SameLine();
            if (ImGui::SmallButton("Reload")) {
                if (try_load(*file_path, /*reset-scroll*/ false)) {
                    messenger::dot();
                }
            }
            // guide_mode::item_tooltip("Reload from disk.");
            ImGui::SameLine();
            ImGui::SetNextWindowSize({300, 200}, ImGuiCond_Always);
            menu_like_popup::small_button("Select");
            menu_like_popup::popup([&] {
                if (ImGui::Button("Refresh")) {
                    nav.refresh_if_valid();
                }
                // guide_mode::item_tooltip("Reload entry list.");
                ImGui::SameLine();
                ImGui::SetNextItemWidth(floor(item_width * 0.6));
                nav.input_filter();
                ImGui::Separator();

                const pathT name = file_path->filename();
                std::optional<pathT> sel = nav.select_file(&name);
                if (sel && try_load(*sel, /*reset-scroll*/ true)) {
                    file_path = std::move(*sel);
                }
            });
            ImGui::SameLine();
            display_filename(*file_path);
            ImGui::SameLine();
            menu_like_popup::small_button(">");
            menu_like_popup::popup([&] { text.select_line(); });
            ImGui::SameLine();
            if (ImGui::SmallButton("Top") && messenger::dot()) {
                text.reset_scroll();
            }

            ImGui::Separator();
            text.display();
            if (close) {
                file_path.reset();
                text.clear();
            }
        }
    }

    const char* window_tooltip() const { //
        return file_path.has_value() && !text.empty() ? textT::about_selection : nullptr;
    }
};

class load_clipboard_impl : no_copy {
    textT text;
    std::string last_str;
    bool dedup = true;

public:
    void display() {
        // (The page will hold roughly at most 1.5*max_size/line.)
        const bool too_much_content = text.lines() > max_line || text.length() > max_size;
        ImGui::BeginDisabled(too_much_content);
        if (ImGui::SmallButton("Paste")) {
            if (const std::string_view str = read_clipboard(); !str.empty()) {
                if (str.size() > max_size / 2) {
                    messenger::set_msg("Too much content: {} > {}", to_size(str.size()), to_size(max_size / 2));
                } else if (const int l = count_line(str); l > max_line / 2) {
                    messenger::set_msg("Too many lines: {} > {}", l, max_line / 2);
                } else if (!compare_update(last_str, str) && dedup) {
                    messenger::set_msg("Ignored. (Identical text.)");
                } else {
                    const int to = text.lines();
                    text.append(str);
                    text.to_line(to);
                }
            }
        }
        ImGui::EndDisabled();
        if (too_much_content) {
            imgui_ItemTooltip("Too much content.");
        }

        ImGui::SameLine();
        if (double_click_button_small("Clear") && messenger::dot()) {
            text.clear();
            last_str.clear();
            // last_str.shrink_to_fit(); // TODO: whether to release memory?
        }
        if constexpr (debug_mode) {
            ImGui::SameLine();
            ImGui::PushStyleVarY(ImGuiStyleVar_FramePadding, 0);
            ImGui::Checkbox("Dedup", &dedup);
            ImGui::PopStyleVar();
        }
        // ImGui::SameLine();
        // imgui_Str("Clipboard");
        ImGui::SameLine();
        menu_like_popup::small_button(">");
        menu_like_popup::popup([&] { text.select_line(); });
        ImGui::SameLine();
        if (ImGui::SmallButton("Top") && messenger::dot()) {
            text.reset_scroll();
        }

        ImGui::Separator();
        text.display();
    }

    const char* window_tooltip() const { return nullptr; }
};

// Defined in "docs.cpp". [0]:title [1]:contents, null-terminated.
extern const char* const docs[][2];

class load_doc_impl : no_copy {
    textT text;
    std::optional<int> doc_id = std::nullopt;

    void select() {
        for (int i = 0; docs[i][0] != nullptr; ++i) {
            const auto [title, contents] = docs[i];
            // if (ImGui::Selectable(title, doc_id == i, ImGuiSelectableFlags_NoAutoClosePopups) && doc_id != i) {
            if (imgui_SelectableStyledButtonEx(i, title, doc_id == i) && compare_update(doc_id, i)) {
                text.assign(/*reset-scroll*/ true, contents, "@@");
            }
        }
    };

public:
    void display() {
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
                if (has_open_url_fn() && ImGui::Selectable("Open in browser")) {
                    open_url(url);
                }
            });

            ImGui::Separator();
            select();
        } else {
            const bool close = ImGui::SmallButton("Close");
            ImGui::SameLine();
            menu_like_popup::small_button("Select");
            menu_like_popup::popup([&] { select(); });
            ImGui::SameLine();
            imgui_Str(docs[*doc_id][0]);
            ImGui::SameLine();
            menu_like_popup::small_button(">");
            menu_like_popup::popup([&] { text.select_line(); });
            ImGui::SameLine();
            if (ImGui::SmallButton("Top") && messenger::dot()) {
                text.reset_scroll();
            }

            ImGui::Separator();
            text.display();
            if (close) {
                text.clear();
                doc_id.reset();
            }
        }
    }

    const char* window_tooltip() const { //
        return doc_id.has_value() ? textT::about_selection : nullptr;
    }
};

static imgui_Window prepare_window(const char* title, bool& open, const ImVec2& init_pos, const char* tooltip) {
    // assert(open);
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
    ImGui::SetNextWindowPos(init_pos, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowSize({600, 400}, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowSizeConstraints(ImVec2(450, 300), ImVec2(FLT_MAX, FLT_MAX));
    if constexpr (debug_mode) {
        // TODO: whether to document this way?
        imgui_Window::next_window_titlebar_tooltip = tooltip;
    }
    return imgui_Window(title, &open, ImGuiWindowFlags_NoSavedSettings);
}

open_state load_file(const ImVec2 init_pos, frame_main_token) {
    static load_file_impl loader(get_home_path());
    bool open = true;
    if (auto window = prepare_window("Files", open, init_pos, loader.window_tooltip())) {
        loader.display();
    }
    return {open};
}

open_state load_clipboard(const ImVec2 init_pos, frame_main_token) {
    static load_clipboard_impl loader;
    bool open = true;
    if (auto window = prepare_window("Clipboard", open, init_pos, loader.window_tooltip())) {
        loader.display();
    }
    return {open};
}

open_state load_doc(const ImVec2 init_pos, frame_main_token) {
    static load_doc_impl loader;
    bool open = true;
    if (auto window = prepare_window("Documents", open, init_pos, loader.window_tooltip())) {
        loader.display();
    }
    return {open};
}

static std::array<int, 3> get_year_month_day() {
    const time_t now = time(0);
    if (const tm* local = localtime(&now)) {
        return {local->tm_year + 1900, local->tm_mon + 1, local->tm_mday};
    } else { // UTC; won't bother with `chrono::current_zone()`.
        const std::chrono::year_month_day ymd(
            std::chrono::floor<std::chrono::days>(std::chrono::system_clock::from_time_t(now)));
        // Why do they define only unsigned explicit casts for month and day...
        return {
            ymd.year().operator int(),
            int(ymd.month().operator unsigned int()),
            int(ymd.day().operator unsigned int()),
        };
    }
}

class rec_for_rule_b : no_copy {
    std::vector<aniso::compressT> m_rules;
    int m_capacity;
    int m_pos = 0;

public:
    explicit rec_for_rule_b(/*const int cap = 100*/) : m_capacity(100) { //
        m_rules.reserve(m_capacity);
    }

    // Ring buffer.
    bool add(const aniso::compressT& rule) {
        const bool contains = std::ranges::find(m_rules, rule) != m_rules.end();
        if (!contains) {
            if (m_rules.size() < m_capacity) {
                m_rules.push_back(rule);
            } else {
                m_rules[m_pos] = rule;
                if (++m_pos == m_capacity) {
                    m_pos = 0;
                }
            }
            return true;
        }
        return false;
    }
};

class rule_saver : no_copy {
    std::fstream m_file{};
    rec_for_rule_b m_rec{};

public:
    explicit rule_saver(const pathT& base) {
        if (base.empty()) {
            return;
        }
        try {
            if (!std::filesystem::is_directory(base)) {
                return;
            }
            const pathT folder = base / "autosaved"; // !!TODO: decide the name before v0.9.8 release...
            const auto status = std::filesystem::status(folder);
            if (std::filesystem::is_directory(status) ||
                (!std::filesystem::exists(status) && std::filesystem::create_directory(folder))) {
                const auto [y, m, d] = get_year_month_day();
                const std::string ymd_str = std::format("{}-{:02}-{:02}", y, m, d);
                // `app` does imply `out`, see https://eel.is/c++draft/filebuf.members
                m_file.open(folder / (ymd_str + ".txt"),
                            std::ios_base::in | std::ios_base::app | std::ios_base::binary);
                if (m_file) {
                    bool empty = true;
                    std::string line;
                    int max_line = 3000;
                    while (--max_line >= 0 && std::getline(m_file, line)) {
                        empty = false;
                        if (const auto rule = aniso::extract_one_rule(line)) {
                            m_rec.add(*rule);
                        }
                    }
                    m_file.clear(); // Necessary for writing.
                    if (empty) {
                        m_file << (ymd_str + '\n') << std::flush;
                    }
                }
            }
        } catch (...) {
            return;
        }
    }

    bool valid() const { return m_file.is_open() && m_file; }

    void save_if_valid(const aniso::ruleT& rule) {
        if (valid() && m_rec.add(rule)) {
            m_file << ('\n' + aniso::to_MAP_str(rule)) << std::flush;
        }
    }
};

void copy_rule::save(const aniso::ruleT& rule) {
    static rule_saver saver(get_home_path());
    saver.save_if_valid(rule);
}
