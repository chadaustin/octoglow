# Octoglow

Octoglow is a native Win32 screensaver written in Rust. The goal is to let the user choose folders from the Windows file tree, then show a gently animated random rotation of images from those folders. Video support is planned for later.

This repository currently contains the first skeleton:

- a Rust workspace with the `octoglow` screensaver binary;
- an `xtask` build command that emits `target/release/octoglow.scr`;
- Win32 screensaver mode routing for `/s`, `/c`, `/p`, and `/a`;
- a native configuration flow using `IFileOpenDialog` in folder-picking mode;
- configuration persisted under `%APPDATA%\Octoglow\folders.txt`;
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
- `/c` opens the native folder picker and saves the selected folders.
- `/p` creates a small preview window skeleton.
- `/a` is accepted as the password-change mode and currently exits.

## Configuration

The configuration UI currently uses the native Windows folder picker with multi-select enabled. It saves one folder path per line to:

```text
%APPDATA%\Octoglow\folders.txt
```

The next session should replace or extend this with the intended custom tree view UI so users can browse the filesystem and toggle folders directly inside Octoglow.

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
