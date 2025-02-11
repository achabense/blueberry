#include <unordered_map>

#include "common.hpp"

// (Identical to `textT::display_header`.)
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

class recordT {
    using keyT = aniso::compressT;
    using mapT = std::unordered_map<keyT, int /*-> m_vec[i]*/, keyT::hashT>;
    using vecT = std::vector<const keyT* /*-> m_map's keys*/>;

    mapT m_map;
    vecT m_vec;

public:
    int size() const {
        assert(m_map.size() == m_vec.size());
        return m_vec.size();
    }

    const keyT& at(int i) const {
        assert(0 <= i && i < m_vec.size());
        return *m_vec[i];
    }

    std::optional<int> find(const aniso::ruleT& rule) const {
        const auto iter = m_map.find(rule);
        return iter != m_map.end() ? std::optional<int>(iter->second) : std::nullopt;
    }

    std::pair<int /*-> m_vec[i]*/, bool> emplace(const aniso::ruleT& rule) {
        assert(m_map.size() == m_vec.size());
        const auto [iter, newly_emplaced] = m_map.try_emplace(rule, m_vec.size());
        if (newly_emplaced) {
            m_vec.push_back({&iter->first});
        }
        return {iter->second, newly_emplaced};
    }

    void clear() {
        m_vec.clear(); // TODO: whether to release memory?
        m_map.clear();
    }
};

static std::optional<int> display_page(const recordT& record, const previewer::configT& config,
                                       const std::optional<int> highlight, const std::optional<int> locate) {
    std::optional<int> sel_pos = std::nullopt;

    ImGui::PushStyleColor(ImGuiCol_ChildBg, IM_COL32_GREY(24, 255));
    if (auto child = imgui_ChildWindow("Page")) {
        // set_scroll_by_up_down(floor(config.height() * 0.5));

        const int total = record.size();
        for (int l = 0; l < total; ++l) {
            if (l != 0) {
                ImGui::Separator();
            }

            if (l == highlight) {
                const ImVec2 min = ImGui::GetCursorScreenPos();
                const ImVec2 max = {min.x + ImGui::GetContentRegionAvail().x, min.y + config.height()};
                const ImVec2 pad = {0, ImGui::GetStyle().ItemSpacing.y};
                ImGui::GetWindowDrawList()->AddRectFilled(min - pad + ImVec2(0, 1) /*to avoid hiding the separator*/,
                                                          max + pad, IM_COL32_GREY(36, 255));
            }

            ImGui::TextDisabled("%2d -", l + 1);
            ImGui::SameLine();
            previewer::preview(l, config, [&]() -> decltype(auto) { return record.at(l); } /*()*/);
            if (l == locate && !imgui_ItemFullyVisible()) {
                ImGui::SetScrollHereY();
            }
            ImGui::SameLine();
            ImGui::PushID(l);
            if (ImGui::Button(">> Cur") && !locate) {
                sel_pos = l;
            }
            ImGui::PopID();
        }
    }
    ImGui::PopStyleColor();

    return sel_pos;
}

static recordT record_current;
static recordT record_copied;
static recordT record_random_access;

void rule_recorder::record(typeE type, const aniso::ruleT& rule, const aniso::ruleT* from) {
    if (type == typeE::Current) {
        record_current.emplace(rule);
    } else if (type == typeE::Copied) {
        record_copied.emplace(rule);
    } else if (type == typeE::RandomAccess) {
        assert(from);
        record_random_access.emplace(*from);
        record_random_access.emplace(rule);
    } else {
        assert(type == typeE::Ignore);
    }
}

// !!TODO: tooltips (especially explanations for each record).
// TODO: ideally there should be special support for random-access editing (e.g. preceding-rule).
// TODO: support sorting by recentness (especially useful for random-access).
// TODO: support exporting as text (-> clipboard).
void rule_recorder::load_record(sync_point& sync) {
    struct termT {
        const char* label;
        recordT* record;
    };
    // TODO: find more suitable labels...
    static constexpr termT record_terms[]{
        {"Current rule", &record_current}, {"Copied rules", &record_copied}, {"Random access", &record_random_access}};
    static const termT* active_term = &record_terms[0];

    static previewer::configT config{previewer::configT::_220_160};
    static std::optional<int> iter_pos = std::nullopt;
    bool reset_scroll = false;

    ImGui::PushStyleVarY(ImGuiStyleVar_FramePadding, 0);
    ImGui::SetNextItemWidth(imgui_CalcTextSize("Random access    ").x);
    if (ImGui::BeginCombo("##Select", active_term->label)) {
        lock_scroll();
        for (int id = 0; const termT& term : record_terms) {
            if (imgui_SelectableStyledButtonEx(id++, term.label, active_term == &term) &&
                compare_update(active_term, &term)) {
                iter_pos.reset();
                reset_scroll = true;
            }
        }
        ImGui::EndCombo();
    }
    ImGui::PopStyleVar();
    ImGui::SameLine();
    if (double_click_button_small("Clear")) {
        set_msg_cleared();
        iter_pos.reset();
        reset_scroll = true;
        active_term->record->clear();
        if (active_term == &record_terms[0]) {
            active_term->record->emplace(sync.rule);
        }
    }

    recordT& active_record = *active_term->record;
    const int record_size = active_record.size();
    const std::optional<int> found = active_record.find(sync.rule);
    std::optional<int> locate = std::nullopt;

    ImGui::SameLine();
    ImGui::BeginDisabled(!found);
    if (ImGui::SmallButton("Locate")) {
        locate = found;
    }
    ImGui::EndDisabled();
    if (!found) {
        // TODO: improve...
        imgui_ItemTooltip("The current rule is not in this list.");
    }

    ImGui::Separator();
    if (const auto pos = display_header(record_size, iter_pos)) {
        if (!locate) {
            locate = *pos;
        }
    }
    if (record_size != 0) {
        ImGui::SameLine();
        config.set("Settings");
    }
    ImGui::Separator();

    if (reset_scroll) {
        ImGui::SetNextWindowScroll({0, 0});
    }
    // (`iter_pos` should be updated after `display_page`, as locate -> ImGui::SetScrollHereY will have one-frame delay.)
    if (const auto sel = display_page(active_record, config, iter_pos, locate)) {
        assert(!locate);
        sync.set(active_record.at(*sel));
        locate = *sel;
    }
    if (locate) {
        iter_pos = locate;
    }
}
