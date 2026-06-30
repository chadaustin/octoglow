#![cfg_attr(windows, windows_subsystem = "windows")]

#[cfg(windows)]
mod app;
#[cfg(windows)]
mod config_ui;

#[cfg(windows)]
fn main() -> windows::core::Result<()> {
    app::run()
}

#[cfg(not(windows))]
fn main() {
    eprintln!("Octoglow is a native Win32 screensaver and must be built on Windows.");
}
