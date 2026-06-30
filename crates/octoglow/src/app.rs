#![allow(unsafe_op_in_unsafe_fn)]

use std::ffi::OsStr;
use std::iter::once;
use std::mem::size_of;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::time::Instant;

use scrnsave::{RunMode, ScreenSaver, ScreenSaverConfig};
use serde::Deserialize;
use windows::core::{w, PCWSTR};
use windows::Win32::Foundation::{CloseHandle, COLORREF, GENERIC_READ, HWND, RECT};
use windows::Win32::Graphics::Dwm::DwmFlush;
use windows::Win32::Graphics::Gdi::{
    BeginPaint, CreateSolidBrush, DeleteObject, EndPaint, FillRect, SetBkMode, SetTextColor,
    StretchDIBits, TextOutW, BITMAPINFO, BITMAPINFOHEADER, BI_RGB, DIB_RGB_COLORS, HBRUSH,
    PAINTSTRUCT, SRCCOPY, TRANSPARENT,
};
use windows::Win32::Graphics::Imaging::{
    CLSID_WICImagingFactory, GUID_ContainerFormatHeif, GUID_ContainerFormatJpeg,
    GUID_ContainerFormatPng, GUID_WICPixelFormat32bppBGRA, IWICImagingFactory,
    WICConvertBitmapSource, WICDecodeMetadataCacheOnDemand,
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
use windows::Win32::UI::WindowsAndMessaging::{
    GetClientRect, MessageBoxW, MB_ICONINFORMATION, MB_OK,
};

const MAX_RENDER_PIXELS: u32 = 1280 * 720;

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
    candidate_count: usize,
    decode_failure_count: usize,
    images: Vec<DecodedImage>,
    render_cache: Option<RenderCache>,
}

struct DecodedImage {
    path: PathBuf,
    width: u32,
    height: u32,
    pixels: Vec<u8>,
}

struct RenderCache {
    width: u32,
    height: u32,
    frames: Vec<Vec<u8>>,
}

impl ScreenSaver for OctoglowScreensaver {
    type State = AppState;

    fn config(&self) -> ScreenSaverConfig {
        let mut config = ScreenSaverConfig::new("OctoglowScreenSaver", "Octoglow");
        config.timer_ms = 16;
        config
    }

    fn initialize(&mut self, _mode: RunMode) -> windows::core::Result<Self::State> {
        let image_roots = load_config().unwrap_or_default();
        let scan = collect_images(&image_roots);
        Ok(AppState {
            started: Instant::now(),
            image_roots,
            candidate_count: scan.candidate_count,
            decode_failure_count: scan.decode_failure_count,
            images: scan.images,
            render_cache: None,
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

    let elapsed = state.started.elapsed().as_secs_f32();
    let glow = ((elapsed.sin() + 1.0) * 70.0 + 90.0) as u8;
    SetBkMode(hdc, TRANSPARENT);

    let lines = status_lines(state);

    if state.images.is_empty() {
        let brush: HBRUSH = CreateSolidBrush(COLORREF(0x00101410));
        FillRect(hdc, &rect, brush);
        let _ = DeleteObject(brush.into());
    } else {
        let width = (rect.right - rect.left).max(1) as u32;
        let height = (rect.bottom - rect.top).max(1) as u32;
        let (render_width, render_height) = render_size(width, height);
        let frame = render_frame(state, render_width, render_height);
        blit_frame(hdc, width, height, render_width, render_height, &frame);
    }

    let text_color = if state.images.is_empty() {
        COLORREF((glow as u32) << 8 | 0x40)
    } else {
        COLORREF(0x00f0f0f0)
    };
    SetTextColor(hdc, text_color);

    for (index, line) in lines.iter().enumerate() {
        let text = wide(line);
        let _ = TextOutW(hdc, 48, 48 + (index as i32 * 24), &text[..text.len() - 1]);
    }

    let _ = EndPaint(hwnd, &ps);
    let _ = DwmFlush();
}

fn launch_config_dialog(parent: Option<HWND>) -> windows::core::Result<()> {
    if let Err(error) = crate::config_ui::run(parent) {
        show_message("Octoglow could not open the configuration dialog.");
        return Err(error);
    }

    Ok(())
}

struct ImageScan {
    candidate_count: usize,
    decode_failure_count: usize,
    images: Vec<DecodedImage>,
}

fn collect_images(roots: &[PathBuf]) -> ImageScan {
    let mut candidates = Vec::new();
    for root in roots {
        collect_image_candidates(root, &mut candidates);
    }

    let candidate_count = candidates.len();
    let mut decode_failure_count = 0;
    let images = candidates
        .into_iter()
        .filter_map(|path| match decode_image_with_wic(&path) {
            Ok(image) => Some(image),
            Err(_) => {
                decode_failure_count += 1;
                None
            }
        })
        .collect();

    ImageScan {
        candidate_count,
        decode_failure_count,
        images,
    }
}

fn collect_image_candidates(root: &Path, images: &mut Vec<PathBuf>) {
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
                        collect_image_candidates(&child, images);
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
    if let Some(image) = current_image(state) {
        return vec![
            format!("Octoglow found {} image(s).", state.images.len()),
            format!("Showing: {}", image.path.display()),
        ];
    }

    if state.image_roots.is_empty() {
        return vec!["Octoglow: configure folders to begin".to_string()];
    }

    let mut lines = vec![format!(
        "Octoglow loaded {} folder(s), found {} supported file candidate(s), decoded 0 image(s).",
        state.image_roots.len(),
        state.candidate_count
    )];
    if state.decode_failure_count > 0 {
        lines.push(format!(
            "{} candidate(s) failed WIC decode validation.",
            state.decode_failure_count
        ));
    }
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

fn decode_image_with_wic(path: &Path) -> windows::core::Result<DecodedImage> {
    let factory: IWICImagingFactory =
        unsafe { CoCreateInstance(&CLSID_WICImagingFactory, None, CLSCTX_INPROC_SERVER)? };
    let path = wide_path(path);
    let decoder = unsafe {
        factory.CreateDecoderFromFilename(
            PCWSTR(path.as_ptr()),
            None,
            GENERIC_READ,
            WICDecodeMetadataCacheOnDemand,
        )?
    };
    let format = unsafe { decoder.GetContainerFormat()? };

    if !(format == GUID_ContainerFormatPng
        || format == GUID_ContainerFormatJpeg
        || format == GUID_ContainerFormatHeif)
    {
        return Err(windows::core::Error::from_hresult(windows::core::HRESULT(
            0x80004005u32 as i32,
        )));
    }

    let frame = unsafe { decoder.GetFrame(0)? };
    let source = unsafe { WICConvertBitmapSource(&GUID_WICPixelFormat32bppBGRA, &frame)? };
    let mut width = 0;
    let mut height = 0;
    unsafe {
        source.GetSize(&mut width, &mut height)?;
    }

    let stride = width.saturating_mul(4);
    let mut pixels = vec![0; stride.saturating_mul(height) as usize];
    unsafe {
        source.CopyPixels(std::ptr::null(), stride, &mut pixels)?;
    }

    Ok(DecodedImage {
        path: PathBuf::from(String::from_utf16_lossy(
            &path[..path.len().saturating_sub(1)],
        )),
        width,
        height,
        pixels,
    })
}

fn current_image(state: &AppState) -> Option<&DecodedImage> {
    let (current, _, _) = playback_indices(state, 1.0);
    state.images.get(current)
}

fn render_frame(state: &mut AppState, width: u32, height: u32) -> Vec<u8> {
    ensure_render_cache(state, width, height);
    let mut frame = black_frame(width, height);

    let Some(cache) = &state.render_cache else {
        return frame;
    };

    let elapsed = state.started.elapsed().as_secs_f32();
    let (current, next, alpha_next) = playback_indices(state, elapsed);

    if elapsed < 2.0 {
        let alpha = ((elapsed / 2.0).max(0.08) * 255.0).round() as u8;
        if let Some(source) = cache.frames.get(current) {
            fade_from_black(source, &mut frame, alpha);
        }
    } else if current == next || alpha_next <= 0.0 {
        if let Some(source) = cache.frames.get(current) {
            frame.copy_from_slice(source);
        }
    } else {
        if let (Some(from), Some(to)) = (cache.frames.get(current), cache.frames.get(next)) {
            let alpha = (alpha_next.clamp(0.0, 1.0) * 255.0).round() as u8;
            crossfade_frames(from, to, &mut frame, alpha);
        }
    }

    frame
}

fn ensure_render_cache(state: &mut AppState, width: u32, height: u32) {
    let cache_matches = state
        .render_cache
        .as_ref()
        .map(|cache| {
            cache.width == width
                && cache.height == height
                && cache.frames.len() == state.images.len()
        })
        .unwrap_or(false);
    if cache_matches {
        return;
    }

    let frames = state
        .images
        .iter()
        .map(|image| scaled_image_frame(image, width, height))
        .collect();
    state.render_cache = Some(RenderCache {
        width,
        height,
        frames,
    });
}

fn black_frame(width: u32, height: u32) -> Vec<u8> {
    let mut frame = vec![0; width.saturating_mul(height).saturating_mul(4) as usize];
    for pixel in frame.chunks_exact_mut(4) {
        pixel[3] = 255;
    }
    frame
}

fn render_size(width: u32, height: u32) -> (u32, u32) {
    let pixels = width.saturating_mul(height);
    if pixels <= MAX_RENDER_PIXELS {
        return (width.max(1), height.max(1));
    }

    let scale = (MAX_RENDER_PIXELS as f32 / pixels as f32).sqrt();
    (
        ((width as f32 * scale).round() as u32).max(1),
        ((height as f32 * scale).round() as u32).max(1),
    )
}

fn playback_indices(state: &AppState, elapsed: f32) -> (usize, usize, f32) {
    let len = state.images.len();
    if len <= 1 {
        return (0, 0, 0.0);
    }

    let elapsed = (elapsed - 2.0).max(0.0);
    let hold = 6.0;
    let fade = 2.0;
    let period = hold + fade;
    let cycle = (elapsed / period).floor() as usize;
    let local = elapsed % period;
    let current = cycle % len;
    let next = (current + 1) % len;
    let alpha_next = if local < hold {
        0.0
    } else {
        (local - hold) / fade
    };

    (current, next, alpha_next)
}

fn scaled_image_frame(image: &DecodedImage, width: u32, height: u32) -> Vec<u8> {
    let mut frame = black_frame(width, height);
    if image.width == 0 || image.height == 0 || width == 0 || height == 0 {
        return frame;
    }

    let scale = (width as f32 / image.width as f32).min(height as f32 / image.height as f32);
    let draw_width = ((image.width as f32 * scale).round() as u32)
        .max(1)
        .min(width);
    let draw_height = ((image.height as f32 * scale).round() as u32)
        .max(1)
        .min(height);
    let offset_x = (width - draw_width) / 2;
    let offset_y = (height - draw_height) / 2;

    for y in 0..draw_height {
        let src_y = ((y as u64 * image.height as u64) / draw_height as u64) as u32;
        for x in 0..draw_width {
            let src_x = ((x as u64 * image.width as u64) / draw_width as u64) as u32;
            let src_idx = ((src_y * image.width + src_x) * 4) as usize;
            let dst_idx = (((offset_y + y) * width + (offset_x + x)) * 4) as usize;
            let alpha = image.pixels[src_idx + 3] as u32;

            for channel in 0..3 {
                frame[dst_idx + channel] =
                    ((image.pixels[src_idx + channel] as u32 * alpha) / 255) as u8;
            }
            frame[dst_idx + 3] = 255;
        }
    }

    frame
}

fn fade_from_black(source: &[u8], destination: &mut [u8], alpha: u8) {
    let alpha = alpha as u32;
    for (src, dst) in source.chunks_exact(4).zip(destination.chunks_exact_mut(4)) {
        dst[0] = ((src[0] as u32 * alpha) / 255) as u8;
        dst[1] = ((src[1] as u32 * alpha) / 255) as u8;
        dst[2] = ((src[2] as u32 * alpha) / 255) as u8;
        dst[3] = 255;
    }
}

fn crossfade_frames(from: &[u8], to: &[u8], destination: &mut [u8], alpha: u8) {
    let alpha_to = alpha as u32;
    let alpha_from = 255 - alpha_to;
    for ((from, to), dst) in from
        .chunks_exact(4)
        .zip(to.chunks_exact(4))
        .zip(destination.chunks_exact_mut(4))
    {
        dst[0] = ((from[0] as u32 * alpha_from + to[0] as u32 * alpha_to) / 255) as u8;
        dst[1] = ((from[1] as u32 * alpha_from + to[1] as u32 * alpha_to) / 255) as u8;
        dst[2] = ((from[2] as u32 * alpha_from + to[2] as u32 * alpha_to) / 255) as u8;
        dst[3] = 255;
    }
}

unsafe fn blit_frame(
    hdc: windows::Win32::Graphics::Gdi::HDC,
    dest_width: u32,
    dest_height: u32,
    source_width: u32,
    source_height: u32,
    frame: &[u8],
) {
    let mut info = BITMAPINFO {
        bmiHeader: BITMAPINFOHEADER {
            biSize: size_of::<BITMAPINFOHEADER>() as u32,
            biWidth: source_width as i32,
            biHeight: -(source_height as i32),
            biPlanes: 1,
            biBitCount: 32,
            biCompression: BI_RGB.0,
            ..Default::default()
        },
        ..Default::default()
    };

    let _ = StretchDIBits(
        hdc,
        0,
        0,
        dest_width as i32,
        dest_height as i32,
        0,
        0,
        source_width as i32,
        source_height as i32,
        Some(frame.as_ptr().cast()),
        &mut info,
        DIB_RGB_COLORS,
        SRCCOPY,
    );
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
