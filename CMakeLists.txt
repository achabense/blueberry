cmake_minimum_required(VERSION 3.25)

# (wontfix) I wasted many hours trying to find a "reliable way" to avoid repeated cloning for different configurations. Now I'd rather let it go.
# Note to self: don't bother with this problem anymore.
include(FetchContent)
set(FETCHCONTENT_QUIET FALSE)
set(FETCHCONTENT_UPDATES_DISCONNECTED TRUE)
FetchContent_Declare(
    SDL2
    GIT_REPOSITORY "https://github.com/libsdl-org/SDL.git"
    GIT_TAG        8236e01a9f758d15927624925c6043f84d8a261f #release-2.30.12
    GIT_PROGRESS   TRUE
    GIT_SHALLOW    TRUE
)
FetchContent_MakeAvailable(SDL2)

project(Blueberry LANGUAGES CXX)

if (MSVC)
    string(REPLACE "/Ob0" "/Ob1" CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}") # Otherwise, force-inline doesn't work.
#   string(REPLACE "/W3" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
endif()

file(GLOB SOURCE_LIST CONFIGURE_DEPENDS src/*.cpp imgui/*.cpp imgui/backends/*.cpp)

add_executable(${PROJECT_NAME} WIN32 ${SOURCE_LIST})

target_compile_options(${PROJECT_NAME} PRIVATE
    $<$<CXX_COMPILER_ID:MSVC>:/utf-8> # https://stackoverflow.com/questions/47690822
    $<$<CXX_COMPILER_ID:Clang>:-finput-charset=utf-8 -fexec-charset=utf-8>
    $<$<CXX_COMPILER_ID:GNU>:-finput-charset=utf-8 -fexec-charset=utf-8>

    $<$<CXX_COMPILER_ID:Clang>:-Wall -Wextra -Wpedantic -Wdeprecated -Wnewline-eof -Wshadow> # -Wconversion

#   $<$<CXX_COMPILER_ID:MSVC>:/WX>
#   $<$<CXX_COMPILER_ID:Clang>:-Werror>
#   $<$<CXX_COMPILER_ID:GNU>:-Werror>
)

target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_20)
target_include_directories(${PROJECT_NAME} PRIVATE src imgui imgui/backends)
target_link_libraries(${PROJECT_NAME} PRIVATE SDL2::SDL2-static SDL2::SDL2main)