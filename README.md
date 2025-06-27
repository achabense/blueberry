### [Blueberry](https://github.com/achabense/blueberry) (v0.9.8 WIP)

<p><sub>
(Renamed from "Moody" in v0.9.8. The "latest-release" version (v0.9.7) has been heavily outdated. v0.9.8 is mostly finished, but still needs some time to stabilize (tooltips etc).)
</sub></p>

This program is useful for exploring [MAP rules](https://conwaylife.com/wiki/Non-isotropic_rule). In short, they are arbitrary 2-state rules in the range-1 Moore neighborhood - a superset of isotropic rules, life-like rules (e.g. [Conway's Game of Life](https://conwaylife.com/wiki/Conway%27s_Game_of_Life)) and so on.

Below are some discoveries found using this program. More discoveries are recorded [here](https://github.com/achabense/blueberry/tree/main/rules).

<img width="200" alt="MAPBSEBKiGAcMxBVCdvQAH//ySAf8+AAd1aAEE/DwAT728JCDX/DgF9/6VEf34MAX7bAAB3/QkTVX3Mkf57g397Xw" src="https://github.com/user-attachments/assets/1242708a-65ca-4095-9b05-3d2b9b1362fa">
<img width="200" alt="MAPFAgghg2AwFFSkCAAgQIRFGAAikEABAcACABkMhQMgkEoQAKEQAAgkoWWBjEUI8EMiBcBCBwABskEiREsEQUkkw" src="https://github.com/user-attachments/assets/25085603-7d94-4537-8cec-33f0c7cc9fc2">
<img width="200" alt="MAPBQgEAEASAKBuS0EEikACIkkQICICJDDoCCgAChiCIgh80AkQWQgEMMwAhEEAIAkAQEIAMGCQUECgQEgECAAEAA" src="https://github.com/user-attachments/assets/5d2c914d-2c90-4f75-a330-6d21c39231f5">

#### Building

<p><sub>
(The project is developed and tested on Windows 10, but there is no explicit dependency on OS-specific features, so hopefully it may also work on other systems.)
</sub></p>

The project uses CMake for building. The dependencies are:
- C++20
- [ImGui](https://github.com/ocornut/imgui) (included in this project)
- [SDL2](https://github.com/libsdl-org/SDL) (will be fetched by CMake)

On Windows 10, the project can be directly opened and built in Visual Studio (CMake tools required). Alternatively, to build from command line, run:

```
cd root-path-of-this-project-i.e.-the-path-containing-CMakeLists.txt

cmake -B out -S .

cmake --build out
```