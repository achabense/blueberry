### Blueberry (WIP)
<p><sub>
(Renamed from "Moody" in v0.9.8)
</sub></p>

https://github.com/achabense/blueberry

This program helps to explore a set of cellular automaton rules known as "[MAP rules](https://golly.sourceforge.io/Help/Algorithms/QuickLife.html#map)". In short, MAP rules are 2-state rules in the range-1 Moore neighborhood. They are a strict superset of isotropic rules and outer-totalistic (aka life-like) rules, the most famous one being [Conway's Game of Life](https://conwaylife.com/wiki/Conway%27s_Game_of_Life). Below are some discoveries found by this program.

<img width="200" alt="MAPBSEBKiGAcMxBVCdvQAH//ySAf8+AAd1aAEE/DwAT728JCDX/DgF9/6VEf34MAX7bAAB3/QkTVX3Mkf57g397Xw" src="https://github.com/user-attachments/assets/1242708a-65ca-4095-9b05-3d2b9b1362fa">
<img width="200" alt="MAPFAgghg2AwFFSkCAAgQIRFGAAikEABAcACABkMhQMgkEoQAKEQAAgkoWWBjEUI8EMiBcBCBwABskEiREsEQUkkw" src="https://github.com/user-attachments/assets/25085603-7d94-4537-8cec-33f0c7cc9fc2">
<img width="200" alt="MAPBQgEAEASAKBuS0EEikACIkkQICICJDDoCCgAChiCIgh80AkQWQgEMMwAhEEAIAkAQEIAMGCQUECgQEgECAAEAA" src="https://github.com/user-attachments/assets/5d2c914d-2c90-4f75-a330-6d21c39231f5">

#### Features
The program supports a series of subsets. You can easily identify which subsets a rule belongs to, and explore rules in the subsets or their intersections (e.g., self-complementary isotropic rules). You can get random rules in the `Random` window, or test all rules in the set with the `Traverse` window (practical if the set is reasonably small), or modify existing ones - with the help of preview windows (`Preview`), it's easy to discover interesting rules around a given rule.

<img width="300" alt="Screenshot" src="https://github.com/user-attachments/assets/5414db0a-a4a9-4930-86f6-d45ddaafbe36">
<img width="300" alt="Screenshot" src="https://github.com/user-attachments/assets/fa467202-d50d-4aa6-9030-10c0aae00e6d">

~~Also, the program is able to extract value constraints from patterns, and generate rules under the constraints (for example, rules allowing for gliders).~~ (v0.9.7: temporarily removed, as the old implementation was terrible; will re-support in the future.)

The program saves rules and patterns as common MAP-strings and RLE-strings, which can further be tested by other programs like Golly.

There are a lot of rules collected during the development of this program, available [here](https://github.com/achabense/blueberry/tree/main/rules). You may download and have a look at some of them using the `Files` or `Clipboard` window.

<img width="300" alt="Screenshot" src="https://github.com/user-attachments/assets/0d6c5336-5b37-4575-b19e-22c52d363a1a">

#### Getting started
The binary built for Windows 10 is available at the [latest-release](https://github.com/achabense/blueberry/releases/latest) page. (If you are using a different system, you may try building the project yourself; see the "building" section below.)

It's recommended to place the program in a separate folder, as it will create an "imgui.ini" file in the same directory, and for example, you can download those rule lists to a subdirectory, then it will be easy to find them in the `Files` window.

To get familiar with the program, in the program: press `H` to enter help mode; check the tooltips; check the `Documents` for the concepts and workflow. Below are some basic operations for saving/loading rules and patterns (listed for convenience; all of them are recorded within the program):
- Saving rules: right-click the MAP-string to save to the clipboard. (Also, you can right-click a preview window to copy the previewed rule.)
- Loading rules: open the `Clipboard` window and click `Read clipboard` to load rules from the clipboard. The equivalent shortcut is `W`.
- Saving patterns: select area in the space window with right mouse button, and press `C` to save the entire area, or `I` to identify and save the pattern (limited to oscillators and spaceships).
- Pasting patterns: hover over the space window and press `V` to paste.

#### Building
The project is written in C++20 using the [ImGui](https://github.com/ocornut/imgui) and [SDL2](https://github.com/libsdl-org/SDL) libraries, and uses CMake for building. ImGui is included in this project; SDL2 will be fetched automatically by CMake.

The project is developed on Windows 10. In my experience, it should be easy to open and build the project with Visual Studio (CMake tools required). Alternatively, to build the project directly with command line, assuming the project is to be built in the `Build` subdirectory, run:
```
cmake -B Build -S .

cmake --build Build --config Release
```

There is no explicit dependency on OS-specific features, so the project may also work on some other systems.