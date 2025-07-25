// For reference see "example_sdl2_sdlrenderer2/main.cpp":
// https://github.com/ocornut/imgui/blob/master/examples/example_sdl2_sdlrenderer2/main.cpp

// Unfortunately, SDL2-renderer backend doesn't support docking features...
// https://github.com/ocornut/imgui/issues/5835

// TODO: (?v0.9.9) switch to SDL3...
#include <SDL.h>

#include "imgui_impl_sdl2.h"
#include "imgui_impl_sdlrenderer2.h"

#include "common.hpp"
#include "tile_base.hpp"

[[noreturn]] static void resource_failure() {
    SDL_Log("Error: %s", SDL_GetError());
    exit(EXIT_FAILURE);
}

static SDL_Window* window = nullptr;
static SDL_Renderer* renderer = nullptr;

// (Using macro in case the function is not inlined in debug mode.)
#define color_for(c) ((c) ? IM_COL32_WHITE : IM_COL32_BLACK_TRANS)

static SDL_Texture* create_texture(SDL_TextureAccess access, int w, int h) {
    assert(window && renderer);

    // Using `SDL_PIXELFORMAT_ABGR8888` (also in "imgui_impl_sdlrenderer2.cpp") to be compatible with `IM_COL32(...)`.
    // (Actually, for current color setting (IM_COL32_WHITE and IM_COL32_BLACK_TRANS), whichever _XXXX8888 will be ok.)
    static_assert(IM_COL32_A_SHIFT == 24 && IM_COL32_B_SHIFT == 16 && IM_COL32_G_SHIFT == 8 && IM_COL32_R_SHIFT == 0);
    SDL_Texture* texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ABGR8888, access, w, h);
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
    SDL_Texture* texture = texture_pool::get(tile.size.x, tile.size.y);
    SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_NONE);
    if (scale == scaleE::Nearest) {
        SDL_SetTextureScaleMode(texture, SDL_ScaleModeNearest);
    } else {
        assert(scale == scaleE::Linear);
        SDL_SetTextureScaleMode(texture, SDL_ScaleModeLinear);
    }

    void* pixels = nullptr;
    int pitch = 0;
    if (SDL_LockTexture(texture, nullptr, &pixels, &pitch) != 0) {
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
        SDL_SetTextureScaleMode(texture, SDL_ScaleModeNearest);

        // Using heap allocation to avoid "Function uses XXX bytes of stack" warning.
        std::unique_ptr<Uint32[][3][3]> pixels(new Uint32[512][3][3]);
        for (const auto code : aniso::each_code) {
            const auto fill = aniso::decode(code).to_3x3();
            for (int y = 0; y < 3; ++y) {
                for (int x = 0; x < 3; ++x) {
                    pixels[code][y][x] = color_for(fill[y][x]);
                }
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

void backend_fn::code_image(aniso::codeT code, int zoom, const ImVec4& tint_col, const ImVec4& border_col) {
    const ImVec2 size(3 * zoom, 3 * zoom);
    const ImVec2 uv0(0, code * (1.0f / 512));
    const ImVec2 uv1(1, (code + 1) * (1.0f / 512));
    ImGui::Image(code_atlas::get(), size, uv0, uv1, tint_col, border_col);
}

bool backend_fn::code_button(aniso::codeT code, int zoom, const ImVec4& bg_col, const ImVec4& tint_col) {
    const ImVec2 size(3 * zoom, 3 * zoom);
    const ImVec2 uv0(0, code * (1.0f / 512));
    const ImVec2 uv1(1, (code + 1) * (1.0f / 512));
    ImGui::PushID(code);
    const bool hit = ImGui::ImageButton("Code", code_atlas::get(), size, uv0, uv1, bg_col, tint_col);
    ImGui::PopID();
    return hit;
}

// TODO: use `SDL_GetPrefPath`?
// (Related: https://github.com/libsdl-org/SDL/issues/13322)
std::string backend_fn::home_path_utf8() {
    if (char* base_path = SDL_GetBasePath()) {
        std::string str = base_path;
        SDL_free(base_path);
        return str;
    } else {
        return {}; // Instead of ".".
    }
}

static int frame_per_sec = 100;
void backend_fn::set_frame_rate() { //
    imgui_StepSliderInt::fn("FPS", &frame_per_sec, 4, 100);
}

// The encoding of `argv` cannot be relied upon, see:
// https://stackoverflow.com/questions/5408730/what-is-the-encoding-of-argv
int main(int, char**) {
    assert(!window && !renderer);

    // Setup SDL
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER | SDL_INIT_GAMECONTROLLER) != 0) {
        resource_failure();
    }

    // IME: "Input Method Editor"
    // From 2.0.18: Enable native IME.
#ifdef SDL_HINT_IME_SHOW_UI
    SDL_SetHint(SDL_HINT_IME_SHOW_UI, "1");
#endif

    // Create window with SDL_Renderer graphics context
    {
        constexpr const char* window_title = "Blueberry v 0.9.8 (WIP)";

        constexpr SDL_WindowFlags window_flags =
            (SDL_WindowFlags)(SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI | SDL_WINDOW_HIDDEN);
        window =
            SDL_CreateWindow(window_title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1280, 720, window_flags);
        if (!window) {
            resource_failure();
        }
        if constexpr (1) {
            renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
        } else {
            // The high memory usage is mostly due to GPU rendering.
            // However, software rendering has a lot of visual flaws (off-by-1-pixel etc)...
            renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);
        }
        if (!renderer) {
            resource_failure();
        }

        if constexpr (init_maximize_window) {
            SDL_MaximizeWindow(window);
        }
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
    // if constexpr (!debug_mode) {
    //     ImGui::GetIO().ConfigDebugHighlightIdConflicts = false;
    // }

    // Setup Dear ImGui style
    ImGui::StyleColorsDark();

    // Setup Platform/Renderer backends
    ImGui_ImplSDL2_InitForSDLRenderer(window, renderer);
    ImGui_ImplSDLRenderer2_Init(renderer);
    ImGui::GetPlatformIO().Platform_OpenInShellFn = [](ImGuiContext*, const char* u8path) {
        return SDL_OpenURL(u8path) == 0;
    };

#if 0
    // Test-only.
    ImGui::GetIO().Fonts->AddFontDefault();
    static const ImWchar full_range[]{0x0001, 0xFFFD, 0};
    ImGui::GetIO().Fonts->AddFontFromFileTTF(R"(C:\Windows\Fonts\Deng.ttf)", 13, nullptr, full_range);
#endif

    const auto begin_frame = [] {
        for (;;) {
            SDL_Event event;
            while (SDL_PollEvent(&event)) {
                // Disable tab-related controls (nav menu & cycling through input fields)
                // Related: https://github.com/ocornut/imgui/issues/8525
                if ((event.type == SDL_KEYDOWN || event.type == SDL_KEYUP) && event.key.keysym.sym == SDLK_TAB) {
                    continue;
                }

                ImGui_ImplSDL2_ProcessEvent(&event);
                if (event.type == SDL_QUIT) {
                    return false;
                }
                // This appears not needed.
                // if (event.type == SDL_WINDOWEVENT && event.window.event == SDL_WINDOWEVENT_CLOSE &&
                //     event.window.windowID == SDL_GetWindowID(window)) {
                //     return false;
                // }
            }

            // Related: https://github.com/ocornut/imgui/issues/7844
            if (SDL_GetWindowFlags(window) & SDL_WINDOW_MINIMIZED) {
                SDL_Delay(10);
            } else {
                break;
            }
        }

        ImGui_ImplSDLRenderer2_NewFrame();
        ImGui_ImplSDL2_NewFrame();
        ImGui::NewFrame();
        return true;
    };

    const auto end_frame = [] {
        ImGui::Render();

        // Skip rendering in the first frame for better visual.
        // (The intro window is hidden in the first frame due to auto-resize.)
        if (ImGui::GetFrameCount() >= 2) {
            const ImGuiIO& io = ImGui::GetIO();
            SDL_RenderSetScale(renderer, io.DisplayFramebufferScale.x, io.DisplayFramebufferScale.y);

            // `SDL_RenderClear` seems not necessary, as the program uses full-screen window.
            // (Kept as it does no harm.)
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
            SDL_RenderClear(renderer);

            ImGui_ImplSDLRenderer2_RenderDrawData(ImGui::GetDrawData(), renderer);
            SDL_RenderPresent(renderer);
        }
    };

    texture_pool::begin();
    code_atlas::begin();
    while (begin_frame()) {
        texture_pool::begin_frame();

        frame_main();

        end_frame();

        // (Normally `SDL_RENDERER_PRESENTVSYNC` will further limit to a smaller framerate, like 60fps.)
        static Uint64 last = 0;
        const Uint64 now = SDL_GetTicks64();
        const Uint64 until = last + 1000 / frame_per_sec;
        if (now < until) {
            SDL_Delay(until - now);
            last = until; // Instead of another `SDL_GetTicks64()` call.
        } else {
            last = now;
        }
    }
    code_atlas::end();
    texture_pool::end();

    ImGui_ImplSDLRenderer2_Shutdown();
    ImGui_ImplSDL2_Shutdown();
    ImGui::DestroyContext();

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();

    window = nullptr;
    renderer = nullptr;

    return 0;
}
