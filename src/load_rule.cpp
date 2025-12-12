#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS // For `localtime`
#endif

#include <cstring> // For `std::strchr` (`::strchr` doesn't need this...)
#include <filesystem>
#include <fstream>
#include <ranges>

#include "common.hpp"

// By default the project does not care about exceptions (taking as if they don't exist), but std::filesystem is an exception to this...
// (& `bad_alloc` is always considered impossible to happen.)

using pathT = std::filesystem::path;

// Direct path comparison (a == b) is expensive.
static bool native_equal(const pathT& a, const pathT& b) /*noexcept*/ { //
    return a.native() == b.native();
}

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
// (The clipped path may still exceed avail_w in rare cases.)
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
            // Workaround: -1 for the assertion.
            if (suffix_w + (seg_w + sep_w) <= avail_w - 1) {
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
            // assert(suffix_w >= imgui_CalcTextSize(str).x); // X due to strange rounding in `CalcTextSize()`.
            assert_implies(suffix_w <= avail_w, imgui_CalcTextSize(str).x <= avail_w);
            return str;
        }
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

static void copy_path(const pathT& p) {
    if (const auto str = cpp17_u8string(p)) {
        set_clipboard_and_notify(*str);
    } else {
        messenger::set_msg("Cannot convert to utf-8 text.");
    }
}
static void path_options(const pathT& p) {
    if (ImGui::Selectable("Copy path")) {
        copy_path(p);
    }
    if (ImGui::Selectable("Copy name")) {
        copy_path(p.filename()); // (`p` shouldn't end with separator.)
    }
}

// (Won't mess with auto-fitting.)
static void display_path(const pathT& p, const float avail_w) {
    bool clipped = false;
    imgui_StrClipped(clip_path(p, avail_w, clipped), avail_w);
    rclick_popup::for_text([&] { path_options(p); });
    if (clipped) {
        imgui_ItemTooltip([&] { imgui_Str(cpp17_u8string_b(p)); });
    }
}
static void display_filename(const pathT& p, const float avail_w) {
    constexpr char prefix[]{'.', '.', '.', char(pathT::preferred_separator), '\0'};
    imgui_StrClipped(prefix + cpp17_u8string_b(p.filename()), avail_w);
    rclick_popup::for_text([&] { path_options(p); });
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

    bool assign_dir(const pathT& path, bool* same_dir = nullptr) noexcept {
        // Let's assume no real implementation will ever throw from `is_absolute()`...
        if (path.empty() || (m_path.empty() && !path.is_absolute())) {
            return false;
        }

        try {
            pathT cp = std::filesystem::canonical(m_path / path);
            std::vector<entryT> dirs, files;
            collect_maythrow(cp, dirs, files);
            if (same_dir) {
                // (Compare str directly as both are canonical.)
                *same_dir = native_equal(cp, m_path);
            }
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

    bool assign_dir_or_file(const pathT& path, const entryT*& file_pos, bool* same_dir = nullptr) noexcept {
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
            return assign_dir(p, same_dir);
        } else if (std::filesystem::is_regular_file(status)) {
#if 1
            if (!assign_dir(p / ".." /*, nullptr*/)) {
                return false;
            }

            for (const pathT name = p.filename(); const entryT& file : m_files) {
                if (native_equal(name, file.name)) {
                    file_pos = &file;
                    return true;
                }
            }

            // The file exists, but cannot resolve with string comparison.
            // Used to resort to fs::equivalent (below), but that's too expensive...
            messenger::set_msg("Cannot resolve the name.");
            return true; // Still successful.
#else
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
#endif
        }
        return false;
    }
};

// TODO: support filtering folder names as well?
// TODO: support recording recently-opened folders/files?
class file_nav : no_copy {
    char buf_path[260]{};
    char buf_filter[20]{}; // For files and folders.

    pathT m_home{};
    folderT m_current{};
    test_appearing m_appearing{};

    bool reset_scroll = true;

public:
    bool valid() const { return m_current.valid(); }
    const auto& current_path() const {
        assert(m_current.valid());
        return m_current.path();
    }

    void refresh_if_valid() {
        if (m_current.valid()) {
            constexpr bool same_dir = true;
            if (m_current.refresh()) {
                messenger::dot_if(same_dir);
                reset_scroll = !same_dir;
            } else {
                messenger::set_msg("Cannot refresh.");
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

    void input_filter() { input_text("Filter", buf_filter, ".txt"); }

private:
    const folderT::entryT* input_path() {
        const folderT::entryT* pos = nullptr;
        if (input_text("Open", buf_path, "Folder or file path", ImGuiInputTextFlags_EnterReturnsTrue) &&
            buf_path[0] != '\0') {
            // It's impressive that path has implicit c-str ctor... why?
            // Related: https://github.com/microsoft/STL/issues/909
            auto p = cpp17_u8path(buf_path);
            if (p) {
                // (Prevent some strange '.'-related behaviors on Windows...)
                // (Not defined in `assign_dir_or_file`, for use by the `exists` check.)
                // Related: https://github.com/microsoft/STL/issues/5748
                const auto filename = p->filename();
                if (!filename.empty() && std::ranges::all_of(filename.native(), [](auto c) { return c == '.'; })) {
                    *p /= ""; // Guaranteed to end with sep.
                    assert(!p->has_filename());
                }
            }
            bool same_dir = false;
            if (p && m_current.assign_dir_or_file(*p, pos, &same_dir)) {
                messenger::dot_if(same_dir);
                reset_scroll = !same_dir;
            } else {
                std::error_code ec{};
                messenger::set_msg(p && !std::filesystem::exists(*p, ec) ? "Path doesn't exist." : "Cannot open.");
            }

            buf_path[0] = '\0';
        }
        return pos;
    }

    enum typeE { File, Folder, Both };
    const folderT::entryT* select_entry(const typeE type, int& id, const pathT* current /*name*/ = nullptr) const {
        assert_implies(current, type == File); // (Due to `assign_dir_or_file`.)
        const folderT::entryT* sel = nullptr;
        if (auto child = imgui_ChildWindow("Entries")) {
            set_scroll_with_up_down();
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
                    if (buf_filter[0] && entry.str.find(buf_filter) == entry.str.npos) {
                        continue;
                    }
                    any = true;
                    const bool selected = current && native_equal(entry.name, *current);
                    const bool indent = type == Both && !entry.is_file;
                    if (indent) {
                        imgui_AddCursorPosX(indent_spacing);
                    }
                    if (imgui_SelectableStyledButtonEx(id++, entry.str, selected)) {
                        sel = &entry;
                    }
                    if (imgui_SelectableStyledButtonEx_Clipped) {
                        // imgui_ItemTooltip(entry.str); // (To avoid wrapping...)
                        ImGui::SetNextWindowSizeConstraints({}, {ImGui::GetFontSize() * 80, FLT_MAX});
                        if (ImGui::BeginItemTooltip()) {
                            imgui_Str(entry.str);
                            ImGui::EndTooltip();
                        }
                    }
                    if (indent && ImGui::IsItemVisible()) {
                        const ImVec2 item_min = ImGui::GetItemRectMin();
                        const float h = std::floor(ImGui::GetItemRectSize().y / 2);
                        drawlist.AddLine(item_min + ImVec2(-indent_spacing, h), item_min + ImVec2(-inner_spacing, h),
                                         text_disabled_col);
                    }
                    if (selected && ImGui::IsWindowAppearing()) {
                        ImGui::SetScrollHereY();
                    }
                    rclick_popup::for_button([&] { // (Undocumented.)
                        path_options(m_current / entry.name);
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
        if (m_appearing.update()) {
            buf_path[0] = '\0';
            // buf_filter[0] = '\0';
        }

        std::optional<pathT> target = std::nullopt;
        const auto set_dir = [&](const pathT& path) {
            bool same_dir = false;
            if (m_current.assign_dir(path, &same_dir)) {
                messenger::dot_if(same_dir);
                reset_scroll = !same_dir;
            } else {
                messenger::set_msg("Cannot open.");
            }
        };

        const float default_w = item_width();
        ImGui::SetNextItemWidth(std::floor(default_w * 0.8));
        if (const folderT::entryT* pos = input_path()) {
            assert(pos->is_file);
            target = m_current / pos->name;
        }
        ImGui::SameLine(0, imgui_ItemSpacingX() * 3);
        ImGui::SetNextItemWidth(std::floor(default_w * 0.6));
        input_filter();

        ImGui::Separator();
        int id = 0; // For selectables.
        {
            // TODO: reconsider disabled vs hiding...
            bool any = false;
            if (!m_home.empty()) {
                any = true;
                if (imgui_SelectableStyledButtonEx(id++, "Home")) {
                    set_dir(m_home);
                }
                rclick_popup::for_button([&] { // (Undocumented.)
                    // m_home ~ canonical ~ won't have trailing sep unless it's root path (nearly impossible).
                    path_options(m_home);
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

        if (std::exchange(reset_scroll, false)) {
            ImGui::SetNextWindowScroll({0, 0});
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

struct preview_settings {
    // !!TODO: support in release mode...
    static constexpr bool support_window_mode = debug_mode;

    bool window_mode = false; // false -> inline-mode
    previewer::configT settings = previewer::default_settings;
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

    preview_settings m_preview{};
    test_appearing m_appearing{};

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
    void to_line(int l /*starting from 0*/, bool dot = false) {
        if (!m_lines.empty()) {
            dot_if_no_effect = dot;
            go_line = std::clamp(l, 0, (int)m_lines.size() - 1);
        } else {
            messenger::dot_if(dot);
        }
    }

    void select_line_in_popup() {
        static input_int input_line{};
        if (ImGui::IsWindowAppearing()) {
            input_line.clear();
            ImGui::ActivateItemByID(ImGui::GetID("##Line"));
            if (!m_highlighted.empty()) {
                // Workaround to guarantee correct size when visually appearing.
                // Related: https://github.com/ocornut/imgui/issues/8959
                ImGui::GetCurrentWindow()->HiddenFramesForRenderOnly = 2;
            }
        }

        ImGui::AlignTextToFramePadding();
        imgui_Str("Line ~ ");
        ImGui::SameLine(0, 0);
        ImGui::SetNextItemWidth(imgui_CalcButtonSize("MAX:000000").x);
        if (auto l = input_line.input(6, "##Line",
                                      ("Max:" + (m_lines.empty() ? "N/A" : std::to_string(m_lines.size()))).c_str())) {
            to_line(*l - 1, true);
        }

        // TODO: whether to support this? Currently lacking highlight for "current" section...
        if constexpr (0) {
            if (!m_highlighted.empty()) {
                ImGui::Separator();

                constexpr int limit = 10;
                const float h = std::min((int)m_highlighted.size(), limit) * imgui_CalcSelectableStyledButtonHeight();
                if (auto child = imgui_ChildWindow("Sections", {0, h}, ImGuiChildFlags_AutoResizeX)) {
                    set_scroll_with_up_down();
                    for (int id = 0; const int l : m_highlighted) {
                        if (imgui_SelectableStyledButtonEx(id++, m_lines[l].str.get(m_text))) {
                            to_line(l, true);
                        }
                    }
                }
            }
        }
    }

    static constexpr const char* about_selection = "Right-click and drag to select lines (in the text page).";

    // (Not inherent to impl; just arbitrary values small enough to guarantee perf and large enough for normal use cases.)
    static constexpr int max_size = 1024 * 256;
    static constexpr int max_line = 20000;

    void display() {
        // TODO: whether to set !m_preview.window_mode on appearing?
        if (m_appearing.update()) {
            // This may happen if the parent window is closed with double-esc.
            m_sel.reset();
            menu_opened = false;
        }
        if constexpr (preview_settings::support_window_mode) {
            if (m_rules.empty()) {
                m_preview.window_mode = false;
            }
        }

        assert_implies(m_lines.empty(), m_text.empty() && m_rules.empty());
        assert_implies(m_rules.empty(), !m_pos.has_value() && !m_preview.window_mode);
        assert_implies(menu_opened, m_sel);
        {
            // Precedence:
            // Line-selecting > iterating > (starting line-selection) > left-click setting
            std::optional<int> iter_pos = std::nullopt;
            if (!m_rules.empty()) {
                // TODO: hide or disable? (disable ~ slightly more stable visual, but need to explain (tooltip)...)
                const int total = m_rules.size();
                if constexpr (preview_settings::support_window_mode) {
                    ImGui::Checkbox("Window", &m_preview.window_mode);
                    ImGui::SameLine();
                }
                switch (sequence::seq("<|", "<##Prev", ">##Next", "|>")) {
                    case 0: iter_pos = 0; break;
                    case 1: iter_pos = m_pos ? *m_pos - 1 : 0; break;
                    case 2: iter_pos = m_pos ? *m_pos + 1 : 0; break;
                    case 3: iter_pos = INT_MAX; break;
                }
                if (iter_pos) {
                    iter_pos = std::clamp(*iter_pos, 0, total - 1);
                }
                ImGui::SameLine();
                m_preview.settings.set("Settings");
                ImGui::SameLine();
                if (m_pos.has_value()) {
                    ImGui::Text("Rules:%d At:%d", total, *m_pos + 1);
                } else {
                    ImGui::Text("Rules:%d At:N/A", total);
                }
                rclick_popup::for_text([&] {
                    ImGui::BeginDisabled(!m_pos.has_value());
                    if (ImGui::Selectable("Reset cursor")) {
                        m_pos.reset();
                    }
                    ImGui::EndDisabled();
                });

                ImGui::Separator();
            }

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
                m_pos = *click_pos;
            }

            if constexpr (preview_settings::support_window_mode) {
                // TODO: whether to enable?
                constexpr bool close_from_window = false;
                if (m_preview.window_mode) {
                    // Currently should not set window-mode automatically when !m_rules.empty().
                    // (If the text-page is updated from popup and this window appears [for the first time], it will be assigned at the back of `g.Window` (above the popup).)
                    // TODO: unresolved:
                    // Whether to set `ImGuiWindowFlags_NoCollapse`?
                    // Whether to preserve mode when closed with double-Esc?
                    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
                    imgui_CenterNextWindow(ImGuiCond_FirstUseEver);

                    const ImGuiWindow* source = GImGui->CurrentWindow;
                    assert(!std::strchr(source->Name, '#')); // (Relying on stable name & no "##".)
                    const std::string title = std::format("For '{}'", source->Name);
                    if (auto window =
                            imgui_Window(title.c_str(), close_from_window ? &m_preview.window_mode : nullptr,
                                         ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings |
                                             ImGuiWindowFlags_NoFocusOnAppearing)) {
                        set_above(source);
                        bring_to_front_on_appearing();
                        previewer::preview_or_dummy(0, m_preview.settings, m_pos ? &m_rules[*m_pos] : nullptr);
                    } else { // Collapsed (title bar only).
                        set_above(source);
                    }
                }
            }
        }

        if (m_sel) {
            const ImGuiID popup_id = ImGui::GetID("Menu");
            if (!menu_opened) {
                // Prevent interaction with other widgets.
                // (Idk which are actually necessary/preferable, but the following combination works as intended.)
                if (GImGui->ActiveId != popup_id) {
                    // TODO: whether to set `WriteAccessed`?
                    ImGui::FocusWindow(GImGui->CurrentWindow);
                    ImGui::SetActiveID(popup_id, GImGui->CurrentWindow);
                }
                ImGui::KeepAliveID(popup_id);

                // (Already fixed) for some reason the parent window will still be collapsed if its title bar is double-clicked.
                // Related: https://github.com/ocornut/imgui/issues/7841
                // ImGui::SetKeyOwner(ImGuiKey_MouseLeft, popup_id);

                if (!ImGui::IsMouseDown(ImGuiMouseButton_Right) /* Released from anywhere */) {
                    // Note: `IsMouseReleased` may fail to catch release event in rare cases. For example:
                    // [right-down] -> left-click the program-window's title bar [both-down] ->
                    // release right mouse [left-down], then a menu will appear -> minimize and restore the program.
                    ImGui::OpenPopup(popup_id);
                    menu_opened = true;
                }
            }
            if (menu_opened) {
                lock_scroll();
                if (imgui_BeginPopupRecycled(popup_id)) {
                    // Workaround: when appearing, the popup seems to have reset active id, but doesn't disable hovering...
                    // (So if released from other items, will cause spurious hovering effect...)
                    if (ImGui::IsWindowAppearing()) {
                        ImGui::SetActiveID(popup_id, GImGui->CurrentWindow);
                    }
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
                        // TODO: disable directly?
                        // (Won't copy if `str` contains '\0' (should't appear in regular utf8 text files).)
                        set_clipboard_and_notify(get_str());
                    }
                    // !!TODO: support in release mode (currently not well designed)...
                    if constexpr (debug_mode) {
                        // TODO: also support load-rule?
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
                    if constexpr (init_double_esc_to_close) {
                        if (want_close_windows && source_window_has_no_close_button()) {
                            ImGui::CloseCurrentPopup();
                        }
                    }
                    ImGui::EndPopup();
                } else {
                    m_sel.reset();
                    menu_opened = false;
                }
            }
        }
    }

private:
    struct passT {
        std::optional<int> pos = std::nullopt;
        std::optional<selT> sel = std::nullopt;
    };

    // !!TODO: simplify (currently too convoluted...) & remove mutable...
    // Workaround for dot-feedback. (ImGui::SetScrollHereY doesn't change scroll value immediately.)
    mutable std::optional<int> old_scroll = std::nullopt;
    bool dot_if_no_effect = false;
    static int window_scroll() { return std::round(GImGui->CurrentWindow->Scroll.y); }

    [[nodiscard]] passT display_page(const int locate_rule, const int locate_line) const {
        const bool locating = locate_rule >= 0 || locate_line >= 0;
        assert_implies(m_sel, !locating);
        passT pass{};

        imgui_FillAvailRect(IM_COL32_GREY(24, 255)); // Child bg.
        if (auto child = imgui_ChildWindow("Content")) {
            set_scroll_with_up_down();
            if (old_scroll) {
                messenger::dot_if(*old_scroll == window_scroll());
                old_scroll.reset();
            }
            const bool inline_mode = !m_preview.window_mode;
            const bool test_hover = !menu_opened && (ImGui::IsWindowHovered() || m_sel) && ImGui::IsMousePosValid();
            const ImVec2 mouse_pos = ImGui::GetMousePos(); // Needn't be valid.
            const float region_max_x = imgui_GetContentRegionMaxAbsX();
            // Avail size is shared by two columns here...
            // const float wrap_width = std::max(item_width(), ImGui::GetContentRegionAvail().x);
            const float min_wrap = item_width();
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
                    if (dot_if_no_effect) {
                        old_scroll = window_scroll();
                    }
                    ImGui::SetScrollHereY(0);
                }
                ImGui::SameLine();
                if (inline_mode && rule.has_value()) {
                    ImGui::BeginGroup();
                }

                if (line.highlight) {
                    ImGui::PushStyleColor(ImGuiCol_Text, IM_COL32(255, 255, 160, 255));
                }
                // (`ImGui::TextWrapped` has no problem rendering long single-lines now.)
                // (Related: https://github.com/ocornut/imgui/issues/7496)
                imgui_StrWrapped(line.str.get(m_text), std::max(min_wrap, ImGui::GetContentRegionAvail().x));
                if (line.highlight) {
                    ImGui::PopStyleColor();
                }

                if (ImGui::IsItemVisible()) {
                    const auto [str_min, str_max] = imgui_GetItemRect();
                    const bool line_hovered = test_hover && mouse_pos.y >= str_min.y && mouse_pos.y < str_max.y;
                    // `line_hovered` may become true for two adjacent lines if using `mouse_pos.y <= str_max.y`.

                    if (!locating && line_hovered && !imgui_IsBgHeld()) {
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
                    }
                }

                if (rule.has_value()) {
                    // TODO: ideally should not split RLE blob from header....
                    if (inline_mode) {
                        imgui_StrDisabled("-: ");
                        ImGui::SameLine();

                        ImGui::PopStyleVar(); // (Workaround to avoid affecting popup & tooltip.)
                        previewer::preview(rule.pos, m_preview.settings, rule.get(m_rules));
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

// TODO: -> m_.
// TODO: support opening multiple files?
// TODO: add a mode to avoid opening files without rules?
class load_file_impl : no_copy {
    file_nav nav;
    textT text;
    std::optional<pathT> file_path = std::nullopt;

    bool try_load(const pathT& p, const bool reset_scroll) {
        std::string str;
        if (const uintmax_t size = load_binary(p, str, textT::max_size); size > textT::max_size) {
            if (size != uintmax_t(-1)) {
                messenger::set_msg("File too large: {} > {}", to_size(size), to_size(textT::max_size));
            } else {
                messenger::set_msg("Cannot open.");
            }
            return false;
        } else if (const int l = count_line(str); l > textT::max_line) {
            messenger::set_msg("Too many lines: {} > {}", l, textT::max_line);
            return false;
        } else {
            if constexpr (0) {
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
                if (has_open_url_fn()) {
                    ImGui::SameLine();
                    if (double_click_button_small("Local")) {
                        open_folder(nav.current_path());
                    }
                    guide_mode::item_tooltip("Open current path in the file manager.");
                }
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
                constexpr bool same_file = true;
                if (try_load(*file_path, /*reset-scroll*/ !same_file)) {
                    messenger::dot_if(same_file);
                }
            }
            // guide_mode::item_tooltip("Reload from disk.");
            ImGui::SameLine();
            menu_like_popup::small_button("Select");
            menu_like_popup::popup([&] {
                if (ImGui::Button("Refresh")) {
                    nav.refresh_if_valid();
                }
                // guide_mode::item_tooltip("Reload entry list.");
                ImGui::SameLine();
                ImGui::SetNextItemWidth(std::floor(item_width() * 0.6));
                nav.input_filter();

                ImGui::Separator();

                ImGui::SetNextWindowSize(ImVec2(imgui_CalcContentTotalWidth() + ImGui::GetStyle().ScrollbarSize,
                                                8 * imgui_CalcSelectableStyledButtonHeight()));
                const pathT name = file_path->filename();
                if (auto sel = nav.select_file(&name)) { // imgui_ChildWindow
                    const bool same_file = native_equal(*file_path, *sel);
                    if (try_load(*sel, /*reset-scroll*/ !same_file)) {
                        messenger::dot_if(same_file);
                        file_path = std::move(*sel);
                    }
                }
            });
            ImGui::SameLine();
            display_filename(*file_path,
                             ImGui::GetContentRegionAvail().x - imgui_ItemSpacingX() - imgui_CalcButtonSize("To").x);
            ImGui::SameLine();
            ImGui::BeginDisabled(text.empty());
            menu_like_popup::small_button("To");
            menu_like_popup::popup([&] { text.select_line_in_popup(); });
            ImGui::EndDisabled();

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
        const bool too_much_content = text.lines() > textT::max_line || text.length() > textT::max_size;
        ImGui::BeginDisabled(too_much_content);
        if (ImGui::SmallButton("Paste")) {
            if (const auto str = read_clipboard(); !str.empty()) {
                if (str.size() > textT::max_size / 2) {
                    messenger::set_msg("Too much content: {} > {}", to_size(str.size()), to_size(textT::max_size / 2));
                } else if (const int l = count_line(str); l > textT::max_line / 2) {
                    messenger::set_msg("Too many lines: {} > {}", l, textT::max_line / 2);
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
        ImGui::BeginDisabled(text.empty());
        if (double_click_button_small("Clear")) {
            text.clear();
            last_str.clear();
            last_str.shrink_to_fit();
        }
        ImGui::EndDisabled();
        if constexpr (debug_mode) {
            ImGui::SameLine();
            ImGui::PushStyleVarY(ImGuiStyleVar_FramePadding, 0);
            ImGui::Checkbox("Dedup", &dedup);
            ImGui::PopStyleVar();
        }
        ImGui::SameLine();
        ImGui::BeginDisabled(text.empty()); // TODO: hide or disable?
        menu_like_popup::small_button("To");
        menu_like_popup::popup([&] { text.select_line_in_popup(); });
        ImGui::EndDisabled();

        ImGui::Separator();
        text.display();
    }

    const char* window_tooltip() const { return nullptr; }
};

// Defined in "docs.cpp". [0]:title [1]:contents, null-terminated.
extern const char* const docs[][2];

class load_doc_impl : no_copy {
    textT text;
    const char* doc_title = nullptr;

    void select() {
        for (int i = 0; docs[i][0] != nullptr; ++i) {
            const auto [title, contents] = docs[i];
            if (imgui_SelectableStyledButtonEx(i, title, doc_title == title)) {
                messenger::dot_if(doc_title == title);
                if (compare_update(doc_title, title)) {
                    text.assign(/*reset-scroll*/ true, contents, "@@");
                }
            }
        }
    };

public:
    void display() {
        if (!doc_title) {
            imgui_Str("A toy for exploring MAP rules, by GitHub user 'achabense'.");
            imgui_Str("The latest version is available at: ");
            ImGui::SameLine(0, 0);
            constexpr const char* url = "https://github.com/achabense/blueberry";
            // ImGui::TextLinkOpenURL(url);
            imgui_Str(url);
            rclick_popup::for_text([] {
                if (ImGui::Selectable("Copy link")) {
                    set_clipboard_and_notify(url);
                }
                if (has_open_url_fn() && ImGui::Selectable("Open in browser")) {
                    open_url(url);
                }
            });

            ImGui::Separator();
            if (auto child = imgui_ChildWindow("List")) { // Workaround to disable auto-fitting.
                select();
            }
        } else {
            const bool close = ImGui::SmallButton("Close");
            ImGui::SameLine();
            menu_like_popup::small_button("Select");
            menu_like_popup::popup([&] { select(); });
            ImGui::SameLine();
            imgui_Str(doc_title);
            ImGui::SameLine();
            ImGui::BeginDisabled(text.empty());
            menu_like_popup::small_button("To");
            menu_like_popup::popup([&] { text.select_line_in_popup(); });
            ImGui::EndDisabled();

            ImGui::Separator();
            text.display();
            if (close) {
                text.clear();
                doc_title = nullptr;
            }
        }
    }

    const char* window_tooltip() const { //
        return doc_title ? textT::about_selection : nullptr;
    }
};

static imgui_Window prepare_window(const char* title, bool& open, const char* tooltip) {
    // assert(open);
    ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
    imgui_CenterNextWindow(ImGuiCond_FirstUseEver);
    const float h = ImGui::GetFontSize();
    ImGui::SetNextWindowSize(ImVec2(h * 48, h * 32), ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowSizeConstraints(ImVec2(h * 36, h * 24), ImVec2(FLT_MAX, FLT_MAX));
    imgui_Window::next_window_titlebar_tooltip = tooltip;
    return imgui_Window(title, &open, ImGuiWindowFlags_NoSavedSettings);
}

open_state load_file(frame_main_token) {
    static load_file_impl loader(get_home_path());
    bool open = true;
    if (auto window = prepare_window("Files", open, loader.window_tooltip())) {
        loader.display();
    }
    return {open};
}

open_state load_clipboard(frame_main_token) {
    static load_clipboard_impl loader;
    bool open = true;
    if (auto window = prepare_window("Clipboard", open, loader.window_tooltip())) {
        loader.display();
    }
    return {open};
}

open_state load_doc(frame_main_token) {
    if constexpr (0) {
        static load_doc_impl loader;
        bool open = true;
        if (auto window = prepare_window("Documents", open, loader.window_tooltip())) {
            loader.display();
        }
        return {open};
    } else {
        // !!TODO: temporary; I've no time to rewrite the documents in this version...
        // TODO: ideally should always appear on top (but below popup windows)...
        bool open = true;
        ImGui::SetNextWindowCollapsed(false, ImGuiCond_Appearing);
        imgui_CenterNextWindow(ImGuiCond_FirstUseEver);

        // Doesn't affect popup style (ImGuiCol_PopupBg).
        ImGui::PushStyleColor(ImGuiCol_TitleBg, to_opaque(ImGui::GetStyleColorVec4(ImGuiCol_FrameBg)));
        ImGui::PushStyleColor(ImGuiCol_WindowBg, to_opaque(ImGui::GetStyleColorVec4(ImGuiCol_FrameBg)));
        if (auto window = imgui_Window("About this program", &open,
                                       ImGuiWindowFlags_NoSavedSettings | ImGuiWindowFlags_AlwaysAutoResize)) {
            // TODO: not using ImGui::TextLink() due to style (unconditional underline etc).
            // Whether to use a similar color / support click-to-open behavior?
            const auto item_link = [](const char* url) {
                imgui_ItemTooltip(url);
                rclick_popup::for_text([url] {
                    if (ImGui::Selectable("Copy link")) {
                        set_clipboard_and_notify(url);
                    }
                    if (has_open_url_fn() && ImGui::Selectable("Open in browser")) {
                        open_url(url);
                    }
                });
            };

            ImGui::PushStyleVarY(ImGuiStyleVar_ItemSpacing, 0);
            imgui_Str("Blueberry v0.10.2 WIP");
            imgui_Str("This is a toy for exploring arbitrary MAP rules.");
            imgui_Str("");
            imgui_Str("(c) 2023-2025 achabense (GitHub username)");
            // ImGui::TextLinkOpenURL("Repository", "https://github.com/achabense/blueberry");
            imgui_Str("> ");
            ImGui::SameLine(0, 0);
            imgui_Str("Repository");
            ImGui::PopStyleVar(); // Otherwise affects ImGui::Selectable().
            item_link("https://github.com/achabense/blueberry");
            ImGui::PushStyleVarY(ImGuiStyleVar_ItemSpacing, 0);
            ImGui::SameLine(0, imgui_ItemSpacingX() * 3);
            imgui_Str("> ");
            ImGui::SameLine(0, 0);
            imgui_Str("Discoveries (rules)");
            ImGui::PopStyleVar();
            item_link("https://github.com/achabense/rules");
            // ImGui::PushStyleVarY(ImGuiStyleVar_ItemSpacing, 0);
            // ...
            // ImGui::PopStyleVar();
        }
        ImGui::PopStyleColor(2);
        return {open};
    }
}

static std::array<int, 3> get_year_month_day() {
    const time_t now = std::time(0);
    if (const tm* local = std::localtime(&now)) {
        return {local->tm_year + 1900, local->tm_mon + 1, local->tm_mday};
    } else [[unlikely]] { // UTC; won't bother with `chrono::current_zone()`.
        assert(false);
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

// !!TODO: should redesign...
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
            const pathT folder = base / "autosaved"; // TODO: -> "rules/auto"?
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
