-- Warning: some 3rd-party libraries may use __FILE__ even in release mode, and by default, __FILE__ is absolute path.
-- To make things worse, afaik MSVC doesn't formally support banning absolute path (though undoced workaround exists).

-- https://xmake.io/guide/quick-start.html
-- xmake config -o build
-- xmake config -m release
-- xmake
-- xmake -r
-- xmake run

--[[
set_xmakever("3.0.0")
set_allowedplats("windows")
set_allowedarchs("x64")
set_allowedmodes("debug", "release")
set_defaultmode("release")

add_rules("mode.debug", "mode.release")

-- release-3.2.20
add_requires("libsdl3 96292a5b464258a2b926e0a3d72f8b98c2a81aa6", {configs = {shared = false}})

target("blueberry")
    set_kind("binary")
    if is_plat("windows") then
        set_values("windows.subsystem", "windows")
        add_rules("platform.windows.subsystem")
    end
    set_languages("c++20")
    set_encodings("source:utf-8", "target:utf-8")
    set_warnings("all", "pedantic") -- "extra"
    add_includedirs("src", "imgui", "imgui/backends")
    add_files("src/*.cpp", "imgui/*.cpp", "imgui/backends/*.cpp")
    add_packages("libsdl3")
    if is_mode("release") then
        set_optimize("faster")
        set_strip("all")
    end
target_end()
]]--
