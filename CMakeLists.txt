cmake_minimum_required(VERSION 3.25)

project(blueberry LANGUAGES CXX)

# Get rid of absolute paths (via __FILE__; most are introduced by `SDL_assert_release`, some are hard-coded...)
# MSVC has no __FILE_NAME__ / -ffile-prefix-map equivalence... /pathmap looks close but will invalidate breakpoints, so have to be excluded in debug mode...
# (Have to write this way as MSVC doesn't split ' '-separated list automatically.)
add_compile_options("$<$<AND:$<CXX_COMPILER_ID:MSVC>,$<NOT:$<CONFIG:Debug>>>:/experimental:deterministic;/pathmap:${CMAKE_SOURCE_DIR}=project>")
add_compile_options($<$<CXX_COMPILER_ID:Clang,GNU>:-ffile-prefix-map=${CMAKE_SOURCE_DIR}=project>)

# I wasted many hours trying to find a "reliable way" to avoid repeated cloning for different configurations. Now I'd rather let it go.
# Note to self: don't bother with this problem anymore.
include(FetchContent)
set(FETCHCONTENT_QUIET FALSE)
set(FETCHCONTENT_UPDATES_DISCONNECTED TRUE)
FetchContent_Declare(
    SDL3
    GIT_REPOSITORY "https://github.com/libsdl-org/SDL.git"
    GIT_TAG        7f3ae3d57459e59943a4ecfefc8f6277ec6bf540 #release-3.2.28
    GIT_PROGRESS   TRUE
    GIT_SHALLOW    TRUE
)
set(SDL_SHARED FALSE CACHE BOOL "" FORCE)
set(SDL_STATIC TRUE CACHE BOOL "" FORCE) # Necessary for SDL3-static.
FetchContent_MakeAvailable(SDL3)

target_compile_definitions(SDL3-static PRIVATE
    _CRT_SECURE_NO_WARNINGS _CRT_NONSTDC_NO_WARNINGS # To silence warnings for itoa() etc (in "SDL_string.c").
#   $<$<CONFIG:Release>:SDL_ASSERT_LEVEL=0>
)

if (MSVC)
    string(REPLACE "/Ob0" "/Ob1" CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}") # Otherwise, force-inline doesn't work.
#   string(REPLACE "/W3" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
endif()
# This works, but there seems no good way to silence /Obx overriding warning...
# target_compile_options(... $<$<AND:$<CXX_COMPILER_ID:MSVC>,$<CONFIG:Debug>>:/Ob1>)

file(GLOB SOURCE_LIST CONFIGURE_DEPENDS src/*.cpp imgui/*.cpp imgui/backends/*.cpp)

add_executable(${PROJECT_NAME} WIN32 ${SOURCE_LIST})

target_compile_options(${PROJECT_NAME} PRIVATE
    $<$<CXX_COMPILER_ID:MSVC>:/utf-8> # https://stackoverflow.com/questions/47690822
    $<$<CXX_COMPILER_ID:Clang,GNU>:-finput-charset=utf-8 -fexec-charset=utf-8>

    # -Wreturn-type is neither enabled by default nor implied by -Wall or -Wextra... That's ridiculous...
    $<$<CXX_COMPILER_ID:Clang>:-Wall -Wextra -Wpedantic -Wdeprecated -Wnewline-eof -Wshadow -Werror=return-type>
#   $<$<CXX_COMPILER_ID:Clang>:-Wconversion>
#   $<$<CXX_COMPILER_ID:MSVC>:/WX>
#   $<$<CXX_COMPILER_ID:Clang,GNU>:-Werror>
)

target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_20)
target_include_directories(${PROJECT_NAME} PRIVATE src imgui imgui/backends)
target_link_libraries(${PROJECT_NAME} PRIVATE SDL3::SDL3-static)
