cmake_minimum_required(VERSION 3.25)

# (wontfix) I wasted many hours trying to find a "reliable way" to avoid repeated cloning for different configurations. Now I'd rather let it go.
# Note to self: don't bother with this problem anymore.
include(FetchContent)
set(FETCHCONTENT_QUIET FALSE)
set(FETCHCONTENT_UPDATES_DISCONNECTED TRUE)
FetchContent_Declare(
    SDL2
    GIT_REPOSITORY "https://github.com/libsdl-org/SDL.git"
    GIT_TAG        9c821dc21ccbd69b2bda421fdb35cb4ae2da8f5e #release-2.30.10
    GIT_PROGRESS   TRUE
    GIT_SHALLOW    TRUE
)
FetchContent_MakeAvailable(SDL2)

project(Blueberry)

# Otherwise, force-inline doesn't work.
if (MSVC)
string(REPLACE "/Ob0" "/Ob1" CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")
endif()

file(GLOB imgui_src CONFIGURE_DEPENDS imgui/* imgui/backends/*)

add_executable(${PROJECT_NAME} WIN32
    ${imgui_src}

    src/rule.hpp
    src/tile_base.hpp
    src/tile.hpp
    src/rule_algo.hpp
    src/dear_imgui.hpp
    src/common.hpp

    src/apply_rule.cpp
    src/edit_rule.cpp
    src/record_rule.cpp
    src/docs.cpp
    src/load_rule.cpp
    src/frame_main.cpp
    src/main.cpp
)

# https://stackoverflow.com/questions/47690822/possible-to-force-cmake-msvc-to-use-utf-8-encoding-for-source-files-without-a-bo
target_compile_options(${PROJECT_NAME} PRIVATE "$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")

target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_20)
target_include_directories(${PROJECT_NAME} PRIVATE src imgui imgui/backends)
target_link_libraries(${PROJECT_NAME} PRIVATE SDL2::SDL2-static SDL2::SDL2main)