# Octoglow

Octoglow is a native Win32 screensaver written in Rust. The goal is to let the user choose folders from the Windows file tree, then show a gently animated random rotation of images from those folders. Video support is planned for later.

This repository currently contains the first skeleton:

- a Rust workspace with the `octoglow` screensaver binary;
- a Tauri-based `octoglow-config-ui` companion binary for the configuration dialog;
- an `xtask` build command that emits `target/release/octoglow.scr` and `target/release/octoglow-config-ui.exe`;
- Win32 screensaver mode routing for `/s`, `/c`, `/p`, and `/a`;
- a Win32-like configuration dialog with a checkbox tree view for selecting folders;
- configuration persisted as TOML under `%APPDATA%\Octoglow\settings.toml`;
- recursive folder scanning through Win32 file APIs (`FindFirstFileW`, `FindNextFileW`);
- config file I/O through Win32 file APIs (`CreateFileW`, `ReadFile`, `WriteFile`);
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
target\release\octoglow-config-ui.exe
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
target\release\octoglow.scr /p <parent-hwnd>
```

Current behavior:

- `/s` opens a borderless fullscreen Win32 window, scans configured folders, and paints a placeholder animated status line.
- `/c` launches `octoglow-config-ui.exe`, which should be next to the `.scr` file.
- `/p` creates a small preview window skeleton.
- `/a` is accepted as the password-change mode and currently exits.

## Configuration

The configuration UI is a Tauri companion app that presents a Win32-like dialog with a filesystem tree and checkboxes. It saves the selected folders to:

```text
%APPDATA%\Octoglow\settings.toml
```

The current format is:

```toml
folders = [
  "D:\\Pictures",
  "E:\\Wallpapers",
]
```

The tree contents are supplied by Rust commands that enumerate drives and directories with Win32 file APIs. The UI currently hides hidden and system directories, loads child folders on demand, and stores selected folders when Save is clicked.

For compatibility during early development, Octoglow can still read the previous `%APPDATA%\Octoglow\folders.txt` line-based format if `settings.toml` does not exist. Saving always writes `settings.toml`.

When installing or manually testing the screensaver configuration endpoint, keep these files together:

```text
octoglow.scr
octoglow-config-ui.exe
```

## Image Support

Folder scanning accepts files with these extensions:

- `.png`
- `.jpg`
- `.jpeg`
- `.heic`
- `.heif`

Each candidate file is then opened with Windows Imaging Component. PNG and JPEG should work on standard Windows installations. HEIC/HEIF depends on the installed WIC codec, typically supplied by Microsoft's HEIF Image Extensions.

## Next Implementation Steps

Good follow-up work:

- Replace placeholder painting with real WIC image decoding and GDI, Direct2D, or DirectComposition rendering.
- Add gentle pan, scale, fade-in, and fade-out animation.
- Build the real configuration tree view UI with checkboxes and persisted selection state.
- Track image metadata and cache scan results to avoid a full recursive scan on every launch.
- Add video support through Media Foundation.
- Improve `/p` preview sizing by querying the parent preview rectangle.
- Add an installer or documented manual install step that copies `octoglow.scr` to the appropriate Windows screensaver location.

## Project Layout

```text
.
├── Cargo.toml
├── README.md
├── crates
│   ├── octoglow-config-ui
│   │   ├── Cargo.toml
│   │   ├── build.rs
│   │   ├── tauri.conf.json
│   │   ├── icons
│   │   │   └── icon.ico
│   │   ├── src
│   │   │   └── main.rs
│   │   └── ui
│   │       ├── index.html
│   │       ├── main.js
│   │       └── styles.css
│   └── octoglow
│       ├── Cargo.toml
│       ├── build.rs
│       └── src
│           ├── app.rs
│           └── main.rs
└── xtask
    ├── Cargo.toml
    └── src
        └── main.rs
```
