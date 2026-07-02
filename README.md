# Octoglow

Octoglow is a native Win32 screensaver written in Rust. The goal is to let the user choose folders from the Windows file tree, then show a gently animated random rotation of images from those folders. Video support is planned for later.

This repository currently contains:

- a Rust workspace with the `octoglow` screensaver binary;
- a `scrnsave` crate that implements the screensaver command-line protocol, fullscreen/preview window setup, message loop, timer, and input-to-dismiss behavior normally hidden by `Scrnsave.lib`;
- an embedded Tauri configuration UI initialized only for the `/c` configuration path;
- an `xtask` build command that emits `target/release/octoglow.scr`;
- Win32 screensaver mode routing for `/s`, `/c`, `/p`, and `/a`;
- a Win32-like configuration dialog with a checkbox tree view for selecting folders;
- configuration persisted as TOML under the local config directory, usually `%LOCALAPPDATA%\Octoglow\settings.toml` on Windows;
- recursive folder scanning through `FindFirstFileExW` with `FIND_FIRST_EX_LARGE_FETCH`;
- config file I/O through safe Rust standard library filesystem APIs;
- WIC probing for PNG, JPEG, and HEIC/HEIF containers.

## Requirements

- Windows
- Rust toolchain with Cargo
- The Windows HEIF Image Extensions package if HEIC files should decode on a machine that does not already have a HEIF WIC codec installed

## Build

Generate the screensaver:

```powershell
cargo run -p xtask -- release
```

The output is:

```text
target\release\octoglow.scr
```

During development, a normal executable build also works:

```powershell
cargo build -p octoglow
```

## Run Modes

Windows screensavers are ordinary executables renamed to `.scr`. The shell invokes them with conventional command-line switches:

```powershell
target\release\octoglow.scr /s
target\release\octoglow.scr /c
target\release\octoglow.scr /c:<parent-hwnd>
target\release\octoglow.scr /p <parent-hwnd>
target\release\octoglow.scr /p:<parent-hwnd>
target\release\octoglow.scr /a <parent-hwnd>
```

Current behavior:

- `/s` opens a borderless fullscreen Win32 window, scans configured folders, and renders images with Direct2D. Status lines are drawn by the same Direct2D renderer.
- `/c` initializes the embedded Tauri configuration UI.
- `/c:<hwnd>` also launches configuration, passing the parent HWND through the screensaver framework.
- `/p` creates a preview window skeleton hosted by the parent HWND when one is provided.
- `/p:<hwnd>` is accepted in addition to `/p <hwnd>`.
- `/a` and `/a:<hwnd>` are accepted as password-change modes and currently exit.

The `scrnsave` crate owns these mode semantics and exposes a Rust `ScreenSaver` trait. The `octoglow` crate supplies rendering, timer, and configuration callbacks through that trait.

## Configuration

The configuration UI is embedded in the screensaver binary and presents a Win32-like dialog with a filesystem tree and checkboxes. It saves the selected folders through `dirs::config_local_dir()`, which is usually:

```text
%LOCALAPPDATA%\Octoglow\settings.toml
```

The current format is:

```toml
memory_cap_mb = 1024
folders = [
  "D:\\Pictures",
  "E:\\Wallpapers",
]
```

The tree contents are supplied by Rust commands that enumerate drives and directories. Drive roots come from the Win32 `GetLogicalDrives` API; directory traversal uses `FindFirstFileExW` with `FIND_FIRST_EX_LARGE_FETCH` and avoids reparse-point recursion. The UI currently hides hidden and system directories, loads child folders on demand, and stores selected folders when Save is clicked.

Only the TOML file under the local config directory is supported. Earlier development formats are intentionally ignored.

Only `octoglow.scr` is needed for installation or manual testing.

## Image Support

Folder scanning accepts files with these extensions:

- `.png`
- `.jpg`
- `.jpeg`
- `.heic`
- `.heif`

Image selection is split from decoding. At screensaver startup, a background selector thread samples a small number of photo-bearing directories by walking randomly through the configured roots and applying randomness at each level of the hierarchy. It starts sending image candidates to the renderer as soon as the first sampled photo directory is found, so startup does not wait for a full recursive tree scan.

Each selected candidate file is then opened with Windows Imaging Component. PNG and JPEG should work on standard Windows installations. HEIC/HEIF depends on the installed WIC codec, typically supplied by Microsoft's HEIF Image Extensions.

Decoded image memory is bounded by `memory_cap_mb`, which defaults to 1024. Octoglow estimates decoded image cost as `width * height * 3` bytes and stops adding decoded images when the cap is reached. A decoder worker performs WIC decode into BGR CPU pixels off the render thread. The render thread only drains ready decoded images and incrementally uploads missing current/next Direct2D bitmaps, expanding one image at a time to BGRA for Direct2D upload and avoiding full cache rebuilds during playback. Direct2D owns both image drawing and status text rendering; there is no GDI paint fallback.

The file traversal and playlist pipeline live in `crates/octoglow/src/playlist.rs`. The screensaver owns rendering state and pulls from the playlist through `Playlist::poll()`, which yields `PlaylistEvent::Candidate`, `Decoded`, `DecodeFailed`, `Pending`, or `Finished`.

## Next Implementation Steps

Good follow-up work:

- Add gentle pan and scale animation.
- Teach the decoder cache to prioritize the current and next image before each crossfade.
- Track image metadata and cache scan results for large libraries.
- Add video support through Media Foundation.
- Add an installer or documented manual install step that copies `octoglow.scr` to the appropriate Windows screensaver location.

## Project Layout

```text
.
|-- Cargo.toml
|-- README.md
|-- crates
|   |-- octoglow
|   |   |-- Cargo.toml
|   |   |-- build.rs
|   |   |-- tauri.conf.json
|   |   |-- src
|   |   |   |-- app.rs
|   |   |   |-- config_ui.rs
|   |   |   `-- main.rs
|   |   `-- ui
|   |       |-- index.html
|   |       |-- main.js
|   |       `-- styles.css
|   `-- scrnsave
|       |-- Cargo.toml
|       `-- src
|           `-- lib.rs
`-- xtask
    |-- Cargo.toml
    `-- src
        `-- main.rs
```
