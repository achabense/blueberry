-- https://xmake.io/guide/quick-start.html
-- xmake config -m release
-- xmake
-- xmake run

-- set_allowedplats("windows")
set_allowedarchs("x64")
set_allowedmodes("debug", "release")
set_defaultmode("release")

add_rules("mode.debug", "mode.release")

-- release-3.2.20
add_requires("libsdl3 96292a5b464258a2b926e0a3d72f8b98c2a81aa6", {configs = {shared = false}})

-- TODO: setup build-dir & compiler-specific flags.
target("blueberry")
    set_kind("binary")
    if is_plat("windows") then
        add_rules("win.sdk.application")
    end
    set_languages("c++20")
    set_encodings("source:utf-8", "target:utf-8")
    add_includedirs("src", "imgui", "imgui/backends")
    add_files("src/*.cpp", "imgui/*.cpp", "imgui/backends/*.cpp")
    add_packages("libsdl3")
    if is_mode("release") then
        set_optimize("faster")
        set_strip("all")
    end
target_end()
