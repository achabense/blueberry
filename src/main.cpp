// For reference see "example_sdl2_sdlrenderer2/main.cpp":
// https://github.com/ocornut/imgui/blob/master/examples/example_sdl2_sdlrenderer2/main.cpp

// Unfortunately, SDL2-renderer backend doesn't support docking features...
// https://github.com/ocornut/imgui/issues/5835

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

static SDL_Texture* create_texture(SDL_TextureAccess access, int w, int h) {
    assert(window && renderer);

    // Using the same pixel format as the one in "imgui_impl_sdlrenderer2.cpp".
    SDL_Texture* texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ABGR8888, access, w, h);
    if (!texture) {
        resource_failure();
    }
    return texture;
}

// (Using macro in case the function is not inlined in debug mode.)
#define color_for(b) Uint32(bool(b) ? -1 : 0)

// static Uint32 color_for(bool b) {
//     // Guaranteed to work under SDL_PIXELFORMAT_XXXX8888.
//     return b ? -1 /* White */ : 0 /* Black*/;
// }

// Manage textures for `make_screen`.
class screen_textures {
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

[[nodiscard]] ImTextureID make_screen(const aniso::_misc::tile_ref_<const bool> tile, const scaleE scale) {
    SDL_Texture* texture = screen_textures::get(tile.size.x, tile.size.y);
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
        tile.for_each_line([&](int y, std::span<const bool> line) {
            Uint32* p = (Uint32*)((char*)pixels + pitch * y);
            for (bool v : line) {
                *p++ = color_for(v);
            }
        });
    } else {
        const aniso::_misc::tile_ref_<Uint32> texture_data{
            .size = tile.size, .stride = pitch / pixel_size, .data = (Uint32*)pixels};
        tile.for_all_data_vs(texture_data, [](const bool* s, Uint32* p, int len) {
            for (int i = 0; i < len; ++i) {
                p[i] = color_for(s[i]);
            }
        });
    }
    SDL_UnlockTexture(texture);
    return texture;
}

// Manage the texture for `code_image` and `code_button`.
class code_atlas {
    inline static SDL_Texture* texture = nullptr;

public:
    static void begin() {
        assert(window && renderer && !texture);

        const int width = 3, height = 3 * 512;
        texture = create_texture(SDL_TEXTUREACCESS_STATIC, width, height);
        SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_NONE);
        SDL_SetTextureScaleMode(texture, SDL_ScaleModeNearest);

        // Using heap allocation to avoid "Function uses XXX bytes of stack" warning.
        std::unique_ptr<Uint32[][3][3]> pixels(new Uint32[512][3][3]);
        aniso::for_each_code([&](aniso::codeT code) {
            const aniso::situT situ = aniso::decode(code);
            const bool fill[3][3] = {{situ.q, situ.w, situ.e}, {situ.a, situ.s, situ.d}, {situ.z, situ.x, situ.c}};
            for (int y = 0; y < 3; ++y) {
                for (int x = 0; x < 3; ++x) {
                    pixels[code][y][x] = color_for(fill[y][x]);
                }
            }
        });

        SDL_UpdateTexture(texture, nullptr, pixels.get(), width * sizeof(Uint32));
    }

    static void end() {
        assert(window && renderer && texture);
        SDL_DestroyTexture(texture);
        texture = nullptr;
    }

    static SDL_Texture* get() { return texture; }
};

void code_image(aniso::codeT code, int zoom, const ImVec4& tint_col, const ImVec4& border_col) {
    const ImVec2 size(3 * zoom, 3 * zoom);
    const ImVec2 uv0(0, code * (1.0f / 512));
    const ImVec2 uv1(1, (code + 1) * (1.0f / 512));
    ImGui::Image(code_atlas::get(), size, uv0, uv1, tint_col, border_col);
}

bool code_button(aniso::codeT code, int zoom, const ImVec4& bg_col, const ImVec4& tint_col) {
    const ImVec2 size(3 * zoom, 3 * zoom);
    const ImVec2 uv0(0, code * (1.0f / 512));
    const ImVec2 uv1(1, (code + 1) * (1.0f / 512));
    ImGui::PushID(code);
    const bool hit = ImGui::ImageButton("Code", code_atlas::get(), size, uv0, uv1, bg_col, tint_col);
    ImGui::PopID();
    return hit;
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
        const char* const window_title = "Moody v 0.9.6 (WIP)";
        const SDL_WindowFlags window_flags = (SDL_WindowFlags)(SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
        window =
            SDL_CreateWindow(window_title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1280, 720, window_flags);
    }
    if (!window) {
        resource_failure();
    }

    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC | SDL_RENDERER_ACCELERATED);
    if (!renderer) {
        resource_failure();
    }

    // Setup Dear ImGui context
    ImGui::CreateContext();
    {
        char* const base_path = SDL_GetBasePath();
        if (!set_home(base_path)) {
            resource_failure();
        }
        if (base_path) {
            SDL_free(base_path);
        }
    }

    // Currently the controls of the program work poorly with navigation mode.
    assert(!(ImGui::GetIO().ConfigFlags & ImGuiConfigFlags_NavEnableKeyboard));
    assert(!(ImGui::GetIO().ConfigFlags & ImGuiConfigFlags_NavEnableGamepad));

    // Setup Dear ImGui style
    ImGui::StyleColorsDark();

    // Setup Platform/Renderer backends
    ImGui_ImplSDL2_InitForSDLRenderer(window, renderer);
    ImGui_ImplSDLRenderer2_Init(renderer);

#if 0
    // Test-only.
    ImGui::GetIO().Fonts->AddFontDefault();
    static const ImWchar full_range[]{0x0001, 0xFFFD, 0};
    ImGui::GetIO().Fonts->AddFontFromFileTTF(R"(C:\Windows\Fonts\Deng.ttf)", 13, nullptr, full_range);
#endif

    auto begin_frame = [] {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
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

        ImGui_ImplSDLRenderer2_NewFrame();
        ImGui_ImplSDL2_NewFrame();
        ImGui::NewFrame();
        return true;
    };

    auto end_frame = [] {
        ImGui::Render();
        const ImGuiIO& io = ImGui::GetIO();
        SDL_RenderSetScale(renderer, io.DisplayFramebufferScale.x, io.DisplayFramebufferScale.y);

        // `SDL_RenderClear` seems not necessary, as the program uses full-screen window.
        // (Kept as it does no harm.)
        SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
        SDL_RenderClear(renderer);

        ImGui_ImplSDLRenderer2_RenderDrawData(ImGui::GetDrawData(), renderer);
        SDL_RenderPresent(renderer);
    };

    screen_textures::begin();
    code_atlas::begin();
    while (begin_frame()) {
        screen_textures::begin_frame();

        // Make collapsed windows obvious to see. Set outside of `frame_main` for convenience.
        // TODO: the collapse and close buttons always take ImGuiCol_ButtonXXX (bright-blue) and
        // look strange in the reddish background.
        ImGui::PushStyleColor(ImGuiCol_TitleBgCollapsed, IM_COL32(180, 0, 120, 128));
        frame_main();
        ImGui::PopStyleColor();

        end_frame();

        // Limit the framerate to be at most 100 fps.
        // (Normally `SDL_RENDERER_PRESENTVSYNC` will further limit to a smaller framerate, like 60fps.)
        static Uint64 last = 0;
        const Uint64 now = SDL_GetTicks64();
        const Uint64 until = last + 10;
        if (now < until) {
            SDL_Delay(until - now);
            last = until; // Instead of another `SDL_GetTicks64()` call.
        } else {
            last = now;
        }
    }
    code_atlas::end();
    screen_textures::end();

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
