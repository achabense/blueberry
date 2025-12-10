#### [Blueberry](https://github.com/achabense/blueberry)

This is a toy for exploring arbitrary [MAP rules](https://conwaylife.com/wiki/Non-isotropic_rule) (2-state rules in the range-1 Moore neighborhood). Below are some discoveries found using this program. More discoveries are recorded [here](https://github.com/achabense/rules).

<img width="200" alt="MAPBSEBKiGAcMxBVCdvQAH//ySAf8+AAd1aAEE/DwAT728JCDX/DgF9/6VEf34MAX7bAAB3/QkTVX3Mkf57g397Xw" src="https://github.com/user-attachments/assets/1242708a-65ca-4095-9b05-3d2b9b1362fa">
<img width="200" alt="MAPFAgghg2AwFFSkCAAgQIRFGAAikEABAcACABkMhQMgkEoQAKEQAAgkoWWBjEUI8EMiBcBCBwABskEiREsEQUkkw" src="https://github.com/user-attachments/assets/25085603-7d94-4537-8cec-33f0c7cc9fc2">
<img width="200" alt="MAPBQgEAEASAKBuS0EEikACIkkQICICJDDoCCgAChiCIgh80AkQWQgEMMwAhEEAIAkAQEIAMGCQUECgQEgECAAEAA" src="https://github.com/user-attachments/assets/5d2c914d-2c90-4f75-a330-6d21c39231f5">

#### Features

The program has support for many subsets (isotropic rules, life-like rules, hexagonal rules and so on), and can generate rules based on the sets. Rules/patterns can be saved and loaded in regular MAP/RLE format.

#### Building

The project uses CMake for building. The dependencies are:
- C++20
- [ImGui](https://github.com/ocornut/imgui) (included in this project)
- [SDL](https://github.com/libsdl-org/SDL) (will be fetched by CMake)

There is no explicit dependency on OS-specific APIs. However, the project is only tested on Windows 10.
