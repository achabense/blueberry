// For reference see "example_sdl3_sdlrenderer3/main.cpp":
// https://github.com/ocornut/imgui/blob/master/examples/example_sdl3_sdlrenderer3/main.cpp

// Unfortunately, SDL2-renderer backend doesn't support docking features...
// https://github.com/ocornut/imgui/issues/5835
// TODO: what about SDL3?

#include <SDL3/SDL.h>
#include <SDL3/SDL_main.h>
#include <unordered_set>

#include "imgui_impl_sdl3.h"
#include "imgui_impl_sdlrenderer3.h"

#include "common.hpp"
#include "tile_base.hpp"

[[noreturn]] static void resource_failure() {
    assert(false);
    SDL_Log("Error: %s", SDL_GetError());
    std::exit(EXIT_FAILURE);
}

static SDL_Window* window = nullptr;
static SDL_Renderer* renderer = nullptr;

// (Using macro in case the function is not inlined in debug mode.)
#define color_for(c) ((c) ? IM_COL32_WHITE : IM_COL32_BLACK)

// (Debug mode; screenshot) if using `IM_COL32_BLACK_TRANS`, the saved bmp file will look different. I've no clue what's going on...
// #define color_for(c) ((c) ? IM_COL32_WHITE : IM_COL32_BLACK_TRANS)

static SDL_Texture* create_texture(SDL_TextureAccess access, int w, int h) {
    assert(window && renderer);

    // Using `SDL_PIXELFORMAT_RGBA32` (also used in "imgui_impl_sdlrenderer3.cpp") to be compatible with `IM_COL32(...)`.
    // static_assert(IM_COL32_A_SHIFT == 24 && IM_COL32_B_SHIFT == 16 && IM_COL32_G_SHIFT == 8 && IM_COL32_R_SHIFT == 0);
    // assert(SDL_GetWindowPixelFormat(window) == SDL_PIXELFORMAT_RGBA32); // X in my environment...
    SDL_Texture* texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA32, access, w, h);
    if (!texture) {
        resource_failure();
    }
    return texture;
}

// Manage textures for `to_texture`.
class texture_pool : no_create {
    struct blobT {
        bool used;
        int w, h;
        SDL_Texture* texture;
    };
    inline static std::vector<blobT> blobs;

public:
    static void begin() { assert(window && renderer && blobs.empty()); }
    static void end() {
        assert(window && renderer);

        for (blobT& blob : blobs) {
            SDL_DestroyTexture(blob.texture);
        }
        blobs.clear();
    }

    // There are not going to be too many textures, so for-loop is efficient enough.
    static SDL_Texture* get(int w, int h) {
        assert(window && renderer);

        for (blobT& blob : blobs) {
            if (!blob.used && blob.w == w && blob.h == h) {
                blob.used = true;
                return blob.texture;
            }
        }
        SDL_Texture* texture = create_texture(SDL_TEXTUREACCESS_STREAMING, w, h);
        blobs.push_back({.used = true, .w = w, .h = h, .texture = texture});
        return texture;
    }

    static void begin_frame() {
        assert(window && renderer);

        // According to https://en.cppreference.com/w/cpp/container/vector/erase2
        // std::erase_if doesn't apply, as for vector the predicate is required not to modify the values.
        auto pos = blobs.begin();
        for (blobT& blob : blobs) {
            if (!std::exchange(blob.used, false)) { // Not used in the last frame.
                SDL_DestroyTexture(blob.texture);
            } else {
                *pos++ = blob;
            }
        }
        blobs.erase(pos, blobs.end());
    }
};

ImTextureID backend_fn::to_texture(const aniso::_misc::tile_ref_<const aniso::cellT> tile, const scaleE scale) {
    assert(scale == scaleE::Nearest || scale == scaleE::Linear);

    SDL_Texture* texture = texture_pool::get(tile.size.x, tile.size.y);
    SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_NONE);
    SDL_SetTextureScaleMode(texture, scale == scaleE::Nearest ? SDL_SCALEMODE_NEAREST : SDL_SCALEMODE_LINEAR);
    void* pixels = nullptr;
    int pitch = 0;
    if (!SDL_LockTexture(texture, nullptr, &pixels, &pitch)) {
        resource_failure();
    }

    if (const int pixel_size = sizeof(Uint32); pitch % pixel_size != 0) [[unlikely]] {
        assert(false); // Is this really possible?
        tile.for_each_line([&](int y, std::span<const aniso::cellT> line) {
            Uint32* p = (Uint32*)((char*)pixels + pitch * y);
            for (const aniso::cellT v : line) {
                *p++ = color_for(v);
            }
        });
    } else {
        const aniso::_misc::tile_ref_<Uint32> texture_data{(Uint32*)pixels, tile.size, pitch / pixel_size};
        aniso::for_all_data(tile, texture_data, [](const aniso::cellT* s, Uint32* p, int len) {
            for (int i = 0; i < len; ++i) {
                p[i] = color_for(s[i]);
            }
        });
    }
    SDL_UnlockTexture(texture);
    return (ImTextureID)(intptr_t)texture;
}

// Manage the texture for `code_image` and `code_button`.
class code_atlas : no_create {
    inline static SDL_Texture* texture = nullptr;

public:
    static void begin() {
        assert(window && renderer && !texture);

        constexpr int width = 3, height = 3 * 512;
        texture = create_texture(SDL_TEXTUREACCESS_STATIC, width, height);
        SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_NONE);
        SDL_SetTextureScaleMode(texture, SDL_SCALEMODE_NEAREST);

        // Using heap allocation to avoid "Function uses XXX bytes of stack" warning.
        std::unique_ptr<Uint32[]> pixels(new Uint32[512 * 9]);
        Uint32* p = pixels.get();
        for (const auto code : aniso::each_code) {
            for (const auto cell : aniso::decode(code).to_9()) {
                *p++ = color_for(cell);
            }
        }

        SDL_UpdateTexture(texture, nullptr, pixels.get(), width * sizeof(Uint32));
    }

    static void end() {
        assert(window && renderer && texture);
        SDL_DestroyTexture(texture);
        texture = nullptr;
    }

    static ImTextureID get() { return (ImTextureID)(intptr_t)texture; }
};

void backend_fn::code_image(aniso::codeT code, int zoom) {
    const ImVec2 size(3 * zoom, 3 * zoom);
    const ImVec2 uv0(0, code * (1.0f / 512));
    const ImVec2 uv1(1, (code + 1) * (1.0f / 512));
    ImGui::Image(code_atlas::get(), size, uv0, uv1);
}

bool backend_fn::code_button(aniso::codeT code, int zoom) {
    const ImVec2 size(3 * zoom, 3 * zoom);
    const ImVec2 uv0(0, code * (1.0f / 512));
    const ImVec2 uv1(1, (code + 1) * (1.0f / 512));
    ImGui::PushID(code);
    const bool hit = ImGui::ImageButton("Code", code_atlas::get(), size, uv0, uv1);
    ImGui::PopID();
    return hit;
}

// TODO: use `SDL_GetPrefPath`?
// (Related: https://github.com/libsdl-org/SDL/issues/13322)
std::string backend_fn::home_path_utf8() {
    const char* base_path = SDL_GetBasePath();
    return base_path ? base_path : ""; // No "." fallback.
}

static bool enable_vsync = init_enable_vsync;
static int frame_per_sec = 120;
void backend_fn::set_frame_rate() {
    assert(window && renderer);
    if (ImGui::Checkbox("VSync", &enable_vsync)) {
        SDL_SetRenderVSync(renderer, enable_vsync ? 1 : SDL_RENDERER_VSYNC_DISABLED);
    }
    ImGui::SameLine();
    ImGui::SetNextItemWidth(item_width());
    imgui_StepSliderInt::fn("FPS", &frame_per_sec, 4, 200);
}

// Not backend-specific, but have to sort between `EndFrame()` and `Render()`.
// (Cannot apply immediately; will flicker if the source window is clicked from below.)
// (Likely due to `UpdateMouseMovingWindowEndFrame`, which makes additional focus.)
#if 1
static std::vector<std::array<ImGuiID, 2>> below_above{};
void set_above(const ImGuiWindow* source) {
    const ImGuiWindow* current = GImGui->CurrentWindow;
    assert(source && current);
    source = source->RootWindow;
    current = current->RootWindow;
    assert(source->ID != current->ID);
    assert(!(current->Flags & ImGuiWindowFlags_NoBringToFrontOnFocus));
    if (!(source->Flags & ImGuiWindowFlags_NoBringToFrontOnFocus) /*perf*/) {
        below_above.push_back({source->ID, current->ID});
    }
}
static void sort_windows() {
    if (!below_above.empty()) {
        auto& windows = GImGui->Windows;
        const auto end = windows.end();
        for (const auto /*supposed*/ [below, above] : below_above) {
            const auto below_pos = std::ranges::find(windows, below, &ImGuiWindow::ID);
            const auto above_pos = std::ranges::find(windows, above, &ImGuiWindow::ID);
            if (below_pos != end && above_pos != end && above_pos < below_pos) {
                // Move `above` to after `below` (begin <- mid):
                std::rotate(above_pos, above_pos + 1, below_pos + 1);
            }
        }
        below_above.clear();
    }
}
#else
// Old set-front behavior:
static std::unordered_set<ImGuiID> fronts{};
// void set_front() {
void set_above(const ImGuiWindow*) {
    assert(!(GImGui->CurrentWindow->RootWindow->Flags & ImGuiWindowFlags_NoBringToFrontOnFocus));
    fronts.insert(GImGui->CurrentWindow->RootWindow->ID);
}
static void sort_windows() {
    if (!fronts.empty()) {
        std::ranges::stable_sort(GImGui->Windows, [](const ImGuiWindow* l, const ImGuiWindow* r) {
            const auto get_order = [](const ImGuiWindow* window) {
                constexpr ImGuiWindowFlags special =
                    ImGuiWindowFlags_Popup | ImGuiWindowFlags_Tooltip | ImGuiWindowFlags_ChildMenu;
                return (window->Flags & special) ? 2 : fronts.contains(window->ID) ? 1 : 0;
            };
            return get_order(l->RootWindow) < get_order(r->RootWindow);
        });
        fronts.clear();
    }
}
#endif

// The encoding of `argv` cannot be relied upon, see:
// https://stackoverflow.com/questions/5408730/what-is-the-encoding-of-argv
int main(int, char**) {
    assert(!window && !renderer);

    // 1. There seems no simple way to scale everything automatically within program... Some parts are still not suitably scaled / tested.
    // 2. The pattern textures (which requires nearest/rounded scale-mode) will appear broken if the scale factor is not integral (e.g. 1.5).
    // 3. Even if suitably scaled, the visual (e.g. fonts) appears less crispy than scaled by the system...
    constexpr bool scale_manually = false; // Not fully working...

    // Workaround to setup DPI unawareness (to let the system (Windows) do the scaling).
    // Without this the program will appear small by default, and users need to fix DPI settings manually (Compatibility/Change high DPI settings/Override high DPI scaling behavior->System).
    // (The `SDL_HINT_WINDOWS_DPI_AWARENESS` macro has been removed in the new SDL3 version, but the string still works.)
    // Related: https://github.com/libsdl-org/SDL/pull/7145
    if constexpr (!scale_manually) {
        SDL_SetHint("SDL_WINDOWS_DPI_AWARENESS", "unaware");
    }

    // Setup SDL
    if (!SDL_Init(SDL_INIT_VIDEO /*| SDL_INIT_GAMEPAD*/)) {
        resource_failure();
    }

    // IME is enabled by default in SDL3.
    // SDL_SetHint(SDL_HINT_IME_IMPLEMENTED_UI, "1");

    // Create window with SDL_Renderer graphics context
    const float main_scale = !scale_manually ? 1 : SDL_GetDisplayContentScale(SDL_GetPrimaryDisplay());
    {
        if constexpr (0) {
            // This can reduce memory & GPU usage, but has a lot of visual flaws (off-by-1-pixel etc).
            SDL_SetHint(SDL_HINT_RENDER_DRIVER, SDL_SOFTWARE_RENDERER);
        }

        constexpr const char* window_title = "Blueberry";
        constexpr SDL_WindowFlags window_flags =
            (SDL_WindowFlags)(SDL_WINDOW_RESIZABLE | SDL_WINDOW_HIGH_PIXEL_DENSITY | SDL_WINDOW_HIDDEN);
        if (!SDL_CreateWindowAndRenderer(window_title, (int)(1280 * main_scale), (int)(720 * main_scale), window_flags,
                                         &window, &renderer)) {
            resource_failure();
        }

        SDL_SetWindowPosition(window, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
        if constexpr (init_maximize_window) {
            SDL_MaximizeWindow(window);
        }

        SDL_SetRenderVSync(renderer, enable_vsync ? 1 : SDL_RENDERER_VSYNC_DISABLED);
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);
        SDL_RenderPresent(renderer);
        SDL_ShowWindow(window); // Guaranteed to be all-black.
    }

    // Setup Dear ImGui context
    ImGui::CreateContext();

    // Currently the controls of the program work poorly with navigation mode.
    assert(!(ImGui::GetIO().ConfigFlags & ImGuiConfigFlags_NavEnableKeyboard));
    assert(!(ImGui::GetIO().ConfigFlags & ImGuiConfigFlags_NavEnableGamepad));

    ImGui::GetIO().IniFilename = nullptr;
    ImGui::GetIO().LogFilename = nullptr;
    ImGui::GetIO().ConfigDebugHighlightIdConflicts = true;

    // Setup Dear ImGui style
    ImGui::StyleColorsDark();

    // Setup scaling
    if constexpr (scale_manually) {
        ImGuiStyle& style = ImGui::GetStyle();
        style.ScaleAllSizes(main_scale);
        style.FontScaleDpi = main_scale;
    }

    // Setup Platform/Renderer backends
    ImGui_ImplSDL3_InitForSDLRenderer(window, renderer);
    ImGui_ImplSDLRenderer3_Init(renderer);
    ImGui::GetPlatformIO().Platform_OpenInShellFn = [](ImGuiContext*, const char* u8path) {
        return SDL_OpenURL(u8path);
    };

    if constexpr (0) {
        // TODO: working, but how to support fonts in release mode...
        ImGui::GetIO().Fonts->AddFontDefault();
        ImFontConfig config{};
        config.MergeMode = true;
        ImGui::GetIO().Fonts->AddFontFromFileTTF(R"(C:\Windows\Fonts\Deng.ttf)", 0.0f, &config);
    }

    const auto begin_frame = [] {
        for (;;) {
            SDL_Event event;
            while (SDL_PollEvent(&event)) {
                // Disable tab-related controls (nav menu & cycling through input fields)
                // Related: https://github.com/ocornut/imgui/issues/8525 and 7987
                // (It's possible to disable using only imgui functions, but disabling at backend level is the most reliable way.)
                if ((event.type == SDL_EVENT_KEY_DOWN || event.type == SDL_EVENT_KEY_UP) && event.key.key == SDLK_TAB) {
                    continue;
                }

                ImGui_ImplSDL3_ProcessEvent(&event);
                if (event.type == SDL_EVENT_QUIT) {
                    return false;
                }
                // This appears not needed. (See the doc for `SDL_HINT_QUIT_ON_LAST_WINDOW_CLOSE`.)
                // if (event.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED &&
                //     event.window.windowID == SDL_GetWindowID(window)) {
                //     return false;
                // }
            }

            // Related: https://github.com/ocornut/imgui/issues/7844
            if (SDL_GetWindowFlags(window) & SDL_WINDOW_MINIMIZED) {
                SDL_Delay(10 /*ms*/);
            } else {
                break;
            }
        }

        ImGui_ImplSDLRenderer3_NewFrame();
        ImGui_ImplSDL3_NewFrame();
        ImGui::NewFrame();
        return true;
    };

    const auto end_frame = [] {
        const bool screenshot = // Undocumented. (shortcut = `/~)
            debug_mode && shortcuts::no_active() && shortcuts::test_pressed(ImGuiKey_GraveAccent);

        // Cannot rely on Render() calling EndFrame(). (EndFrame() modifies `GImGui->Windows` on focus, so have to sort after it.)
        ImGui::EndFrame();
        sort_windows();
        ImGui::Render();

        // Skip rendering in the first frame for better visual.
        // (The intro window is hidden in the first frame due to auto-resize.)
        if (ImGui::GetFrameCount() >= 2) {
            // TODO: how does this work?
            const auto& io = ImGui::GetIO();
            SDL_SetRenderScale(renderer, io.DisplayFramebufferScale.x, io.DisplayFramebufferScale.y);

            // `SDL_RenderClear` seems not necessary, as the program uses full-screen window.
            // (Kept as it does no harm.)
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
            SDL_RenderClear(renderer);

            ImGui_ImplSDLRenderer3_RenderDrawData(ImGui::GetDrawData(), renderer);
            if constexpr (debug_mode) {
                if (screenshot) {
                    bool saved = false;
                    // According to the doc, this should be called before SDL_RenderPresent().
                    if (SDL_Surface* s = SDL_RenderReadPixels(renderer, nullptr)) {
                        saved = SDL_SaveBMP(s, "screenshot.bmp");
                        SDL_DestroySurface(s);
                    }
                    messenger::set_msg(saved ? "Saved (screenshot)." : "Failed.");
                }
            }

            SDL_RenderPresent(renderer);
        }
    };

    texture_pool::begin();
    code_atlas::begin();
    while (begin_frame()) {
        texture_pool::begin_frame();

        frame_main();

        end_frame();

        // (May be further limited by vsync (like 60fps).)
        static Uint64 last = 0;
        const Uint64 now = SDL_GetTicksNS();
        const Uint64 until = last + (1000 * 1000 * 1000) /*ns*/ / frame_per_sec;
        if (now < until) {
            // SDL_DelayPrecise(until - now);
            SDL_DelayNS(until - now);
            last = until; // Instead of another `SDL_GetTicksNs()` call.
        } else {
            last = now;
        }
    }
    code_atlas::end();
    texture_pool::end();

    ImGui_ImplSDLRenderer3_Shutdown();
    ImGui_ImplSDL3_Shutdown();
    ImGui::DestroyContext();

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    window = nullptr;
    renderer = nullptr;

    return 0;
}
