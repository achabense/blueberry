#include <unordered_map>

#include "common.hpp"

// (Sharing the same style with textT::display.)
static std::optional<int> display_header(const int total, const std::optional<int> m_pos) {
    std::optional<int> n_pos = std::nullopt;

    if (total == 0) {
        imgui_Str("(No rules)");
    } else {
        switch (sequence::seq("<|", "Prev", "Next", "|>")) {
            case 0: n_pos = 0; break;
            case 1: n_pos = std::max(0, m_pos.value_or(-1) - 1); break;
            case 2: n_pos = std::min(total - 1, m_pos.value_or(-1) + 1); break;
            case 3: n_pos = total - 1; break;
        }
        ImGui::SameLine();
        if (m_pos.has_value()) {
            ImGui::Text("Total:%d At:%d", total, *m_pos + 1);
        } else {
            ImGui::Text("Total:%d At:N/A", total);
        }

        // TODO: whether to support -> At like textT?
        // TODO: tooltips?
    }

    return n_pos;
}

static std::optional<int> display_page(const int total, const std::function<aniso::ruleT(int)> access,
                                       const previewer::configT& config, const std::optional<int> highlight,
                                       const std::optional<int> locate) {
    std::optional<int> n_pos = std::nullopt;

    ImGui::PushStyleColor(ImGuiCol_ChildBg, IM_COL32_GREY(24, 255));
    if (auto child = imgui_ChildWindow("Page")) {
        set_scroll_by_up_down(floor(config.height() * 0.5)); // TODO: document the behavior.

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
            previewer::preview(l, config, [&] { return access(l); } /*()*/);
            if (l == locate && !imgui_ItemFullyVisible()) {
                ImGui::SetScrollHereY();
            }
            ImGui::SameLine();
            // TODO: improve style...
            ImGui::PushID(l);
            if (ImGui::Button(">> Cur")) {
                n_pos = l;
            }
            ImGui::PopID();
        }
    }
    ImGui::PopStyleColor();

    return n_pos;
}

struct recordT {
    using keyT = aniso::compressT;
    using mapT = std::unordered_map<keyT, int /*-> m_vec[i]*/, keyT::hashT>;
    using vecT = std::vector<const keyT* /*-> m_map's keys*/>;

    mapT m_map;
    vecT m_vec;

    int size() const {
        assert(m_map.size() == m_vec.size());
        return m_vec.size();
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
        m_vec.clear();
        m_map.clear();
    }
};

static recordT record_current;
static recordT record_copied;
static recordT record_traverse_or_random;
static recordT record_random_access;

void rule_recorder::record(typeE type, const aniso::ruleT& rule, const aniso::ruleT* from) {
    if (type == typeE::Current) {
        record_current.emplace(rule);
    } else if (type == typeE::Copied) {
        record_copied.emplace(rule);
    } else if (type == typeE::TraverseOrRandom) {
        record_traverse_or_random.emplace(rule);
    } else if (type == typeE::RandomAccess) {
        assert(from);
        record_random_access.emplace(*from);
        record_random_access.emplace(rule);
    } else {
        assert(type == typeE::Ignore);
    }
}

// !!TODO: tooltips (especially explanations for each record).
// !!TODO: support clearing.
// !!TODO: for random-access record, support prev/next.
// !!TODO: support sorting by recentness (especially useful for random-access).
// TODO: support more records (custom masking-rule etc.)?
// TODO: support exporting as text (-> clipboard).
void rule_recorder::load_record(sync_point& sync) {
    struct termT {
        const char* label;
        recordT* record;
    };
    // TODO: find more suitable labels...
    static constexpr termT record_terms[]{{"Current rule", &record_current},
                                          {"Copied rules", &record_copied},
                                          {"'Traverse' or 'Random'", &record_traverse_or_random},
                                          {"Random access", &record_random_access}};
    static const termT* active_term = &record_terms[0];

    static previewer::configT config{previewer::configT::_220_160};
    static std::optional<int> last_returned = std::nullopt;
    static bool auto_locate = false;
    bool reset_scroll = false;

    ImGui::SmallButton("Select");
    if (begin_popup_for_item()) {
        for (int id = 0; const termT& term : record_terms) {
            if (imgui_SelectableStyledButtonEx(id++, term.label, active_term == &term) &&
                compare_update(active_term, &term)) {
                last_returned.reset();
                auto_locate = false;
                reset_scroll = true;
            }
        }
        ImGui::EndPopup();
    }
    ImGui::SameLine();
    imgui_Str(active_term->label);

    recordT& active_record = *active_term->record;
    const int record_size = active_record.size();
    const std::optional<int> found = active_record.find(sync.rule);
    if (auto_locate && !found) {
        auto_locate = false;
    }
    // (!found -> !auto <=> auto -> found)
    assert_implies(auto_locate, found);
    std::optional<int> locate = (auto_locate && found != last_returned) ? found : std::nullopt;

    ImGui::SameLine();
    ImGui::BeginDisabled(!found);
    ImGui::BeginGroup();
    if (ImGui::SmallButton("Locate")) {
        locate = found;
    }
    ImGui::SameLine();
    ImGui::PushStyleVarY(ImGuiStyleVar_FramePadding, 0);
    ImGui::Checkbox("Auto", &auto_locate);
    ImGui::PopStyleVar();
    ImGui::EndGroup();
    ImGui::EndDisabled();
    if (!found) {
        // TODO: improve...
        imgui_ItemTooltip("The current rule is not in this list.");
    }
    assert_implies(auto_locate, found);

    ImGui::Separator();
    const auto n_pos = display_header(record_size, last_returned);
    if (record_size != 0) {
        ImGui::SameLine();
        config.set("Preview settings");
    }
    ImGui::Separator();

    if (reset_scroll) {
        ImGui::SetNextWindowScroll({0, 0});
    }
    // (Not eagerly updating highlight line, as locate -> ImGui::SetScrollHereY will have one-frame delay.)
    const auto o_pos = display_page(
        record_size, [&](int i) { return *active_record.m_vec[i]; }, config, last_returned, locate ? locate : n_pos);

    if (locate) {
        last_returned = locate;
    } else if (n_pos) { // (Both n_pos and o_pos work well in auto_locate mode.)
        sync.set(*active_record.m_vec[*n_pos]);
        last_returned = n_pos; // (Will not trigger auto-locate next frame.)
    } else if (o_pos) {
        sync.set(*active_record.m_vec[*o_pos]);
        last_returned = o_pos; // (Will not trigger auto-locate next frame.)
    }
}
