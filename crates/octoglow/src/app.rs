#![allow(unsafe_op_in_unsafe_fn)]

use std::ffi::OsStr;
use std::iter::once;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use scrnsave::{RunMode, ScreenSaver, ScreenSaverConfig};
use serde::Deserialize;
use windows::core::{w, PCWSTR};
use windows::Win32::Foundation::{CloseHandle, COLORREF, GENERIC_ACCESS_RIGHTS, HWND, RECT};
use windows::Win32::Graphics::Gdi::{
    BeginPaint, CreateSolidBrush, DeleteObject, EndPaint, FillRect, SetBkMode, SetTextColor,
    TextOutW, HBRUSH, PAINTSTRUCT, TRANSPARENT,
};
use windows::Win32::Graphics::Imaging::{
    CLSID_WICImagingFactory, GUID_ContainerFormatHeif, GUID_ContainerFormatJpeg,
    GUID_ContainerFormatPng, IWICImagingFactory, WICDecodeMetadataCacheOnDemand,
};
use windows::Win32::Storage::FileSystem::{
    CreateDirectoryW, CreateFileW, FindClose, FindFirstFileW, FindNextFileW, GetFileAttributesW,
    ReadFile, FILE_ATTRIBUTE_DIRECTORY, FILE_ATTRIBUTE_NORMAL, FILE_GENERIC_READ, FILE_SHARE_READ,
    FILE_SHARE_WRITE, OPEN_EXISTING, WIN32_FIND_DATAW,
};
use windows::Win32::System::Com::{
    CoCreateInstance, CoInitializeEx, CoUninitialize, CLSCTX_INPROC_SERVER,
    COINIT_APARTMENTTHREADED,
};
use windows::Win32::UI::Shell::ShellExecuteW;
use windows::Win32::UI::WindowsAndMessaging::{
    GetClientRect, MessageBoxW, MB_ICONINFORMATION, MB_OK, SW_SHOWNORMAL,
};

#[derive(Deserialize, Default)]
struct Settings {
    folders: Vec<String>,
}

pub fn run() -> windows::core::Result<()> {
    unsafe {
        CoInitializeEx(None, COINIT_APARTMENTTHREADED).ok()?;
    }

    let result = scrnsave::run(OctoglowScreensaver, std::env::args().skip(1));

    unsafe {
        CoUninitialize();
    }
    result
}

struct OctoglowScreensaver;

struct AppState {
    started: Instant,
    image_roots: Vec<PathBuf>,
    images: Vec<PathBuf>,
    featured: Option<PathBuf>,
}

impl ScreenSaver for OctoglowScreensaver {
    type State = AppState;

    fn config(&self) -> ScreenSaverConfig {
        ScreenSaverConfig::new("OctoglowScreenSaver", "Octoglow")
    }

    fn initialize(&mut self, _mode: RunMode) -> windows::core::Result<Self::State> {
        let image_roots = load_config().unwrap_or_default();
        let images = collect_images(&image_roots);
        let featured = pick_random(&images).cloned();
        Ok(AppState {
            started: Instant::now(),
            image_roots,
            images,
            featured,
        })
    }

    fn configure(&mut self, parent: Option<HWND>) -> windows::core::Result<()> {
        launch_config_dialog(parent)
    }

    unsafe fn paint(&mut self, hwnd: HWND, state: &mut Self::State) {
        paint(hwnd, state);
    }

    unsafe fn timer(&mut self, hwnd: HWND, _state: &mut Self::State) {
        let _ = windows::Win32::Graphics::Gdi::InvalidateRect(Some(hwnd), None, false);
    }
}

unsafe fn paint(hwnd: HWND, state: &mut AppState) {
    let mut ps = PAINTSTRUCT::default();
    let hdc = BeginPaint(hwnd, &mut ps);
    let mut rect = RECT::default();
    let _ = GetClientRect(hwnd, &mut rect);

    let brush: HBRUSH = CreateSolidBrush(COLORREF(0x00101410));
    FillRect(hdc, &rect, brush);
    let _ = DeleteObject(brush.into());

    let elapsed = state.started.elapsed().as_secs_f32();
    let glow = ((elapsed.sin() + 1.0) * 70.0 + 90.0) as u8;
    SetBkMode(hdc, TRANSPARENT);
    SetTextColor(hdc, COLORREF((glow as u32) << 8 | 0x40));

    let lines = status_lines(state);
    for (index, line) in lines.iter().enumerate() {
        let text = wide(line);
        let _ = TextOutW(hdc, 48, 48 + (index as i32 * 24), &text[..text.len() - 1]);
    }

    let _ = EndPaint(hwnd, &ps);
}

fn launch_config_dialog(parent: Option<HWND>) -> windows::core::Result<()> {
    let config_exe = std::env::current_exe().ok().and_then(|path| {
        path.parent()
            .map(|parent| parent.join("octoglow-config-ui.exe"))
    });

    let Some(config_exe) = config_exe else {
        show_message("Unable to locate the Octoglow configuration executable.");
        return Ok(());
    };

    if !config_exe.exists() {
        show_message("octoglow-config-ui.exe was not found next to the screensaver.");
        return Ok(());
    }

    let config_exe = wide_path(&config_exe);
    let result = unsafe {
        ShellExecuteW(
            parent,
            w!("open"),
            PCWSTR(config_exe.as_ptr()),
            PCWSTR::null(),
            PCWSTR::null(),
            SW_SHOWNORMAL,
        )
    };

    if (result.0 as isize) <= 32 {
        show_message("Octoglow could not open the configuration dialog.");
    }

    Ok(())
}

fn collect_images(roots: &[PathBuf]) -> Vec<PathBuf> {
    let mut images = Vec::new();
    for root in roots {
        collect_images_from(root, &mut images);
    }

    images
        .into_iter()
        .filter(|path| can_decode_with_wic(path).unwrap_or(false))
        .collect()
}

fn collect_images_from(root: &Path, images: &mut Vec<PathBuf>) {
    let pattern = root.join("*");
    let mut find_data = WIN32_FIND_DATAW::default();
    let pattern = wide_path(&pattern);

    unsafe {
        let handle = match FindFirstFileW(
            PCWSTR(pattern.as_ptr()),
            &mut find_data as *mut WIN32_FIND_DATAW,
        ) {
            Ok(handle) => handle,
            Err(_) => return,
        };

        loop {
            if let Some(name) = filename_from_find_data(&find_data) {
                if name != "." && name != ".." {
                    let child = root.join(&name);
                    if find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY.0 != 0 {
                        collect_images_from(&child, images);
                    } else if has_supported_extension(&child) {
                        images.push(child);
                    }
                }
            }

            if FindNextFileW(handle, &mut find_data).is_err() {
                break;
            }
        }

        let _ = FindClose(handle);
    }
}

fn has_supported_extension(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| {
            matches!(
                ext.to_ascii_lowercase().as_str(),
                "png" | "jpg" | "jpeg" | "heic" | "heif"
            )
        })
        .unwrap_or(false)
}

fn status_lines(state: &AppState) -> Vec<String> {
    if let Some(path) = &state.featured {
        return vec![
            format!("Octoglow found {} image(s).", state.images.len()),
            format!("Next: {}", path.display()),
        ];
    }

    if state.image_roots.is_empty() {
        return vec!["Octoglow: configure folders to begin".to_string()];
    }

    let mut lines = vec![format!(
        "Octoglow loaded {} folder(s), but found no decodable images.",
        state.image_roots.len()
    )];
    lines.extend(
        state
            .image_roots
            .iter()
            .take(4)
            .map(|path| path.display().to_string()),
    );
    if state.image_roots.len() > 4 {
        lines.push(format!("...and {} more", state.image_roots.len() - 4));
    }
    lines
}

fn can_decode_with_wic(path: &Path) -> windows::core::Result<bool> {
    let factory: IWICImagingFactory =
        unsafe { CoCreateInstance(&CLSID_WICImagingFactory, None, CLSCTX_INPROC_SERVER)? };
    let path = wide_path(path);
    let decoder = unsafe {
        factory.CreateDecoderFromFilename(
            PCWSTR(path.as_ptr()),
            None,
            GENERIC_ACCESS_RIGHTS(FILE_GENERIC_READ.0),
            WICDecodeMetadataCacheOnDemand,
        )?
    };
    let format = unsafe { decoder.GetContainerFormat()? };

    Ok(format == GUID_ContainerFormatPng
        || format == GUID_ContainerFormatJpeg
        || format == GUID_ContainerFormatHeif)
}

fn config_file() -> windows::core::Result<PathBuf> {
    let base = std::env::var_os("APPDATA")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."));
    let dir = base.join("Octoglow");
    ensure_directory(&dir)?;
    Ok(dir.join("settings.toml"))
}

fn legacy_config_file() -> windows::core::Result<PathBuf> {
    let base = std::env::var_os("APPDATA")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."));
    let dir = base.join("Octoglow");
    ensure_directory(&dir)?;
    Ok(dir.join("folders.txt"))
}

fn load_config() -> windows::core::Result<Vec<PathBuf>> {
    let file = config_file()?;
    if file.exists() {
        let bytes = read_file(&file)?;
        let text = String::from_utf8_lossy(&bytes);
        let settings = toml::from_str::<Settings>(&text).unwrap_or_default();
        return Ok(settings.folders.into_iter().map(PathBuf::from).collect());
    }

    let legacy_file = legacy_config_file()?;
    if !legacy_file.exists() {
        return Ok(Vec::new());
    }

    let bytes = read_file(&legacy_file)?;
    let text = String::from_utf8_lossy(&bytes);
    Ok(text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(PathBuf::from)
        .collect())
}

fn ensure_directory(path: &Path) -> windows::core::Result<()> {
    let path_w = wide_path(path);
    unsafe {
        let attrs = GetFileAttributesW(PCWSTR(path_w.as_ptr()));
        if attrs != u32::MAX && attrs & FILE_ATTRIBUTE_DIRECTORY.0 != 0 {
            return Ok(());
        }
        if CreateDirectoryW(PCWSTR(path_w.as_ptr()), None).is_ok() {
            return Ok(());
        }
    }
    Ok(())
}

fn read_file(path: &Path) -> windows::core::Result<Vec<u8>> {
    let path_w = wide_path(path);
    unsafe {
        let handle = CreateFileW(
            PCWSTR(path_w.as_ptr()),
            FILE_GENERIC_READ.0,
            FILE_SHARE_READ | FILE_SHARE_WRITE,
            None,
            OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL,
            None,
        )?;
        let mut data = Vec::new();
        let mut buffer = [0u8; 4096];
        loop {
            let mut read = 0u32;
            ReadFile(handle, Some(&mut buffer), Some(&mut read), None)?;
            if read == 0 {
                break;
            }
            data.extend_from_slice(&buffer[..read as usize]);
        }
        CloseHandle(handle)?;
        Ok(data)
    }
}

fn filename_from_find_data(data: &WIN32_FIND_DATAW) -> Option<String> {
    let len = data
        .cFileName
        .iter()
        .position(|ch| *ch == 0)
        .unwrap_or(data.cFileName.len());
    if len == 0 {
        None
    } else {
        Some(String::from_utf16_lossy(&data.cFileName[..len]))
    }
}

fn pick_random<T>(items: &[T]) -> Option<&T> {
    if items.is_empty() {
        return None;
    }
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::ZERO)
        .subsec_nanos() as usize;
    items.get(nanos % items.len())
}

fn wide(value: &str) -> Vec<u16> {
    OsStr::new(value).encode_wide().chain(once(0)).collect()
}

fn wide_path(path: &Path) -> Vec<u16> {
    path.as_os_str().encode_wide().chain(once(0)).collect()
}

fn show_message(message: &str) {
    let message = wide(message);
    unsafe {
        MessageBoxW(
            None,
            PCWSTR(message.as_ptr()),
            w!("Octoglow"),
            MB_OK | MB_ICONINFORMATION,
        );
    }
}
