use std::ffi::OsStr;
use std::fs;
use std::iter::once;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::playlist::{DecodedImage, Playlist, PlaylistEvent};
use scrnsave::{RunMode, ScreenSaver, ScreenSaverConfig};
use serde::Deserialize;
use windows::core::{w, Interface, PCWSTR};
use windows::Win32::Foundation::{HMODULE, HWND, RECT};
use windows::Win32::Graphics::Direct2D::Common::{
    D2D1_ALPHA_MODE_PREMULTIPLIED, D2D1_COLOR_F, D2D1_PIXEL_FORMAT, D2D_RECT_F, D2D_SIZE_U,
};
use windows::Win32::Graphics::Direct2D::{
    D2D1CreateFactory, ID2D1Bitmap1, ID2D1DeviceContext, ID2D1Factory1, ID2D1Image,
    ID2D1SolidColorBrush, D2D1_BITMAP_OPTIONS_CANNOT_DRAW, D2D1_BITMAP_OPTIONS_TARGET,
    D2D1_BITMAP_PROPERTIES1, D2D1_DEVICE_CONTEXT_OPTIONS_NONE, D2D1_DRAW_TEXT_OPTIONS_NONE,
    D2D1_FACTORY_TYPE_SINGLE_THREADED, D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC,
};
use windows::Win32::Graphics::Direct3D::{
    D3D_DRIVER_TYPE_HARDWARE, D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_11_1,
};
use windows::Win32::Graphics::Direct3D11::{
    D3D11CreateDevice, ID3D11Device, D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_SDK_VERSION,
};
use windows::Win32::Graphics::DirectWrite::{
    DWriteCreateFactory, IDWriteFactory, IDWriteFontCollection, IDWriteTextFormat,
    DWRITE_FACTORY_TYPE_SHARED, DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_NORMAL,
    DWRITE_FONT_WEIGHT_NORMAL, DWRITE_MEASURING_MODE_NATURAL, DWRITE_PARAGRAPH_ALIGNMENT_NEAR,
    DWRITE_TEXT_ALIGNMENT_LEADING,
};
use windows::Win32::Graphics::Dwm::DwmFlush;
use windows::Win32::Graphics::Dxgi::Common::{
    DXGI_ALPHA_MODE_IGNORE, DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_SAMPLE_DESC,
};
use windows::Win32::Graphics::Dxgi::{
    IDXGIDevice, IDXGIFactory2, IDXGISurface, IDXGISwapChain1, DXGI_SCALING_STRETCH,
    DXGI_SWAP_CHAIN_DESC1, DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL, DXGI_USAGE_RENDER_TARGET_OUTPUT,
};
use windows::Win32::Graphics::Gdi::ValidateRect;
use windows::Win32::System::Com::{CoInitializeEx, CoUninitialize, COINIT_APARTMENTTHREADED};
use windows::Win32::UI::WindowsAndMessaging::{
    GetClientRect, MessageBoxW, MB_ICONINFORMATION, MB_OK,
};

#[derive(Deserialize)]
struct Settings {
    #[serde(default)]
    folders: Vec<String>,
    #[serde(default = "default_memory_cap_mb")]
    memory_cap_mb: u64,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            folders: Vec::new(),
            memory_cap_mb: DEFAULT_MEMORY_CAP_MB,
        }
    }
}

fn default_memory_cap_mb() -> u64 {
    DEFAULT_MEMORY_CAP_MB
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
    playlist: Playlist,
    selection_done: bool,
    candidate_count: usize,
    decode_failure_count: usize,
    memory_cap_bytes: u64,
    decoded_bytes: u64,
    pending_decoded: Vec<DecodedImage>,
    images: Vec<DecodedImage>,
    d2d: Option<Direct2DState>,
}

const DEFAULT_MEMORY_CAP_MB: u64 = 1024;
const MAX_UPLOADS_PER_FRAME: usize = 1;

struct Direct2DState {
    width: u32,
    height: u32,
    d2d_context: ID2D1DeviceContext,
    swap_chain: IDXGISwapChain1,
    _target_bitmap: ID2D1Bitmap1,
    text_format: IDWriteTextFormat,
    text_brush: ID2D1SolidColorBrush,
    bitmaps: Vec<Option<ID2D1Bitmap1>>,
}

impl ScreenSaver for OctoglowScreensaver {
    type State = AppState;

    fn config(&self) -> ScreenSaverConfig {
        let mut config = ScreenSaverConfig::new("OctoglowScreenSaver", "Octoglow");
        config.timer_ms = 16;
        config
    }

    fn initialize(&mut self, _mode: RunMode) -> windows::core::Result<Self::State> {
        let settings = load_config().unwrap_or_default();
        let image_roots = settings
            .folders
            .iter()
            .map(PathBuf::from)
            .collect::<Vec<_>>();
        let memory_cap_bytes = settings.memory_cap_mb.saturating_mul(1024 * 1024);
        let playlist = Playlist::start(image_roots.clone());
        Ok(AppState {
            started: Instant::now(),
            image_roots,
            playlist,
            selection_done: false,
            candidate_count: 0,
            decode_failure_count: 0,
            memory_cap_bytes,
            decoded_bytes: 0,
            pending_decoded: Vec::new(),
            images: Vec::new(),
            d2d: None,
        })
    }

    fn configure(&mut self, parent: Option<HWND>) -> windows::core::Result<()> {
        launch_config_dialog(parent)
    }

    unsafe fn paint(&mut self, hwnd: HWND, state: &mut Self::State) {
        paint(hwnd, state);
    }

    unsafe fn timer(&mut self, hwnd: HWND, _state: &mut Self::State) {
        unsafe {
            let _ = windows::Win32::Graphics::Gdi::InvalidateRect(Some(hwnd), None, false);
        }
    }
}

fn paint(hwnd: HWND, state: &mut AppState) {
    let mut rect = RECT::default();
    unsafe {
        let _ = GetClientRect(hwnd, &mut rect);
    }
    let width = (rect.right - rect.left).max(1) as u32;
    let height = (rect.bottom - rect.top).max(1) as u32;
    service_decoded_queue(state);

    let _ = render_with_direct2d(hwnd, state, width, height);

    unsafe {
        let _ = ValidateRect(Some(hwnd), None);
        let _ = DwmFlush();
    }
}

fn launch_config_dialog(parent: Option<HWND>) -> windows::core::Result<()> {
    if let Err(error) = crate::config_ui::run(parent) {
        show_message("Octoglow could not open the configuration dialog.");
        return Err(error);
    }

    Ok(())
}

fn service_decoded_queue(state: &mut AppState) {
    if state.selection_done || state.decoded_bytes >= state.memory_cap_bytes {
        return;
    }

    loop {
        let message = match state.playlist.poll() {
            PlaylistEvent::Pending => break,
            PlaylistEvent::Finished => {
                state.selection_done = true;
                break;
            }
            event => event,
        };

        match message {
            PlaylistEvent::Candidate => {
                state.candidate_count += 1;
            }
            PlaylistEvent::Decoded(image) => {
                state.pending_decoded.push(image);
            }
            PlaylistEvent::DecodeFailed => {
                state.decode_failure_count += 1;
            }
            PlaylistEvent::Pending | PlaylistEvent::Finished => unreachable!(),
        }
    }

    let mut uploads_this_frame = 0;
    while uploads_this_frame < MAX_UPLOADS_PER_FRAME {
        let Some(image) = state.pending_decoded.pop() else {
            break;
        };

        let image_bytes = decoded_image_bytes(&image);
        if !state.images.is_empty()
            && state.decoded_bytes.saturating_add(image_bytes) > state.memory_cap_bytes
        {
            state.selection_done = true;
            break;
        }

        if state.images.is_empty() {
            state.started = Instant::now();
        }
        state.decoded_bytes = state.decoded_bytes.saturating_add(image_bytes);
        state.images.push(image);
        if let Some(d2d) = state.d2d.as_mut() {
            d2d.bitmaps.push(None);
        }
        uploads_this_frame += 1;

        if state.decoded_bytes >= state.memory_cap_bytes {
            state.selection_done = true;
            break;
        }
    }
}

fn status_lines(state: &AppState) -> Vec<String> {
    if let Some(image) = current_image(state) {
        return vec![
            format!(
                "Octoglow loaded {} of {} candidate(s), using {} MB of {} MB.",
                state.images.len(),
                state.candidate_count,
                state.decoded_bytes / (1024 * 1024),
                state.memory_cap_bytes / (1024 * 1024)
            ),
            format!("Showing: {}", image.path.display()),
        ];
    }

    if state.image_roots.is_empty() {
        return vec!["Octoglow: configure folders to begin".to_string()];
    }

    let mut lines = vec![format!(
        "Octoglow loaded {} folder(s), discovered {} candidate(s), decoded 0 image(s).",
        state.image_roots.len(),
        state.candidate_count
    )];
    if !state.selection_done {
        lines.push("Selecting images...".to_string());
    }
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

fn decoded_image_bytes(image: &DecodedImage) -> u64 {
    u64::from(image.width)
        .saturating_mul(u64::from(image.height))
        .saturating_mul(3)
}

fn current_image(state: &AppState) -> Option<&DecodedImage> {
    let (current, _, _) = playback_indices(state, 1.0);
    state.images.get(current)
}

fn render_with_direct2d(
    hwnd: HWND,
    state: &mut AppState,
    width: u32,
    height: u32,
) -> windows::core::Result<()> {
    let elapsed = state.started.elapsed().as_secs_f32();
    ensure_direct2d_state(hwnd, state, width, height)?;
    let black = D2D1_COLOR_F {
        r: 0.0,
        g: 0.0,
        b: 0.0,
        a: 1.0,
    };

    unsafe {
        state
            .d2d
            .as_ref()
            .expect("Direct2D state was initialized")
            .d2d_context
            .BeginDraw();
        state
            .d2d
            .as_ref()
            .expect("Direct2D state was initialized")
            .d2d_context
            .Clear(Some(&black));
    }

    if !state.images.is_empty() {
        let (current, next, alpha_next) = playback_indices(state, elapsed);
        ensure_direct2d_bitmap(state, current)?;
        if current != next && alpha_next == 0.0 {
            let _ = ensure_direct2d_bitmap(state, next);
        }
        let d2d = state.d2d.as_ref().expect("Direct2D state was initialized");
        let can_draw_next = current != next
            && alpha_next > 0.0
            && d2d.bitmaps.get(next).and_then(Option::as_ref).is_some();
        let image_rects = state
            .images
            .iter()
            .map(|image| fitted_rect(image.width, image.height, width, height))
            .collect::<Vec<_>>();

        if elapsed < 2.0 {
            let opacity = (elapsed / 2.0).max(0.08);
            draw_direct2d_bitmap(d2d, &image_rects, current, opacity);
        } else if !can_draw_next {
            draw_direct2d_bitmap(d2d, &image_rects, current, 1.0);
        } else {
            draw_direct2d_bitmap(d2d, &image_rects, current, 1.0 - alpha_next);
            draw_direct2d_bitmap(d2d, &image_rects, next, alpha_next);
        }
    }

    let d2d = state.d2d.as_ref().expect("Direct2D state was initialized");
    draw_status_text(d2d, state, width, height);

    unsafe {
        d2d.d2d_context.EndDraw(None, None)?;
        d2d.swap_chain.Present(1, Default::default()).ok()
    }
}

fn ensure_direct2d_state(
    hwnd: HWND,
    state: &mut AppState,
    width: u32,
    height: u32,
) -> windows::core::Result<&mut Direct2DState> {
    let state_matches = state
        .d2d
        .as_ref()
        .map(|d2d| {
            d2d.width == width && d2d.height == height && d2d.bitmaps.len() == state.images.len()
        })
        .unwrap_or(false);
    if !state_matches {
        let (d2d_context, swap_chain, target_bitmap) =
            create_direct2d_device_context(hwnd, width, height)?;
        let text_format = create_text_format()?;
        let text_color = D2D1_COLOR_F {
            r: 0.92,
            g: 0.96,
            b: 0.92,
            a: 0.88,
        };
        let text_brush = unsafe { d2d_context.CreateSolidColorBrush(&text_color, None)? };
        state.d2d = Some(Direct2DState {
            width,
            height,
            d2d_context,
            swap_chain,
            _target_bitmap: target_bitmap,
            text_format,
            text_brush,
            bitmaps: (0..state.images.len()).map(|_| None).collect(),
        });
    }

    Ok(state.d2d.as_mut().expect("Direct2D state was initialized"))
}

fn create_text_format() -> windows::core::Result<IDWriteTextFormat> {
    let factory: IDWriteFactory = unsafe { DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED)? };
    let format = unsafe {
        factory.CreateTextFormat(
            w!("Segoe UI"),
            None::<&IDWriteFontCollection>,
            DWRITE_FONT_WEIGHT_NORMAL,
            DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_STRETCH_NORMAL,
            18.0,
            w!("en-us"),
        )?
    };
    unsafe {
        format.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING)?;
        format.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR)?;
    }
    Ok(format)
}

fn draw_status_text(d2d: &Direct2DState, state: &AppState, width: u32, height: u32) {
    let lines = status_lines(state);
    let mut top = 44.0;
    for line in lines {
        let text = OsStr::new(&line).encode_wide().collect::<Vec<_>>();
        let rect = D2D_RECT_F {
            left: 48.0,
            top,
            right: (width as f32 - 48.0).max(48.0),
            bottom: (top + 28.0).min(height as f32),
        };
        unsafe {
            d2d.d2d_context.DrawText(
                &text,
                &d2d.text_format,
                &rect,
                &d2d.text_brush,
                D2D1_DRAW_TEXT_OPTIONS_NONE,
                DWRITE_MEASURING_MODE_NATURAL,
            );
        }
        top += 24.0;
        if top >= height as f32 {
            break;
        }
    }
}

fn ensure_direct2d_bitmap(state: &mut AppState, index: usize) -> windows::core::Result<()> {
    let Some(image) = state.images.get(index) else {
        return Ok(());
    };
    let d2d = state.d2d.as_mut().expect("Direct2D state was initialized");
    while d2d.bitmaps.len() < state.images.len() {
        d2d.bitmaps.push(None);
    }
    if d2d.bitmaps.get(index).and_then(Option::as_ref).is_some() {
        return Ok(());
    }

    let properties = D2D1_BITMAP_PROPERTIES1 {
        pixelFormat: D2D1_PIXEL_FORMAT {
            format: DXGI_FORMAT_B8G8R8A8_UNORM,
            alphaMode: D2D1_ALPHA_MODE_PREMULTIPLIED,
        },
        dpiX: 96.0,
        dpiY: 96.0,
        bitmapOptions: Default::default(),
        colorContext: Default::default(),
    };

    let size = D2D_SIZE_U {
        width: image.width,
        height: image.height,
    };
    let upload_pixels = bgr_to_bgra(&image.pixels);
    let pitch = image.width.saturating_mul(4);
    let bitmap = unsafe {
        d2d.d2d_context.CreateBitmap(
            size,
            Some(upload_pixels.as_ptr().cast()),
            pitch,
            &properties,
        )?
    };
    if let Some(slot) = d2d.bitmaps.get_mut(index) {
        *slot = Some(bitmap);
    }
    Ok(())
}

fn bgr_to_bgra(pixels: &[u8]) -> Vec<u8> {
    let mut expanded = Vec::with_capacity((pixels.len() / 3) * 4);
    for bgr in pixels.chunks_exact(3) {
        expanded.extend_from_slice(&[bgr[0], bgr[1], bgr[2], 0xff]);
    }
    expanded
}

fn create_direct2d_device_context(
    hwnd: HWND,
    width: u32,
    height: u32,
) -> windows::core::Result<(ID2D1DeviceContext, IDXGISwapChain1, ID2D1Bitmap1)> {
    let mut d3d_device = None;
    let feature_levels = [D3D_FEATURE_LEVEL_11_1, D3D_FEATURE_LEVEL_11_0];
    unsafe {
        D3D11CreateDevice(
            None,
            D3D_DRIVER_TYPE_HARDWARE,
            HMODULE::default(),
            D3D11_CREATE_DEVICE_BGRA_SUPPORT,
            Some(&feature_levels),
            D3D11_SDK_VERSION,
            Some(&mut d3d_device),
            None,
            None,
        )?;
    }
    let d3d_device: ID3D11Device = d3d_device.expect("D3D11CreateDevice returned no device");
    let dxgi_device: IDXGIDevice = d3d_device.cast()?;
    let adapter = unsafe { dxgi_device.GetAdapter()? };
    let dxgi_factory: IDXGIFactory2 = unsafe { adapter.GetParent()? };
    let d2d_factory: ID2D1Factory1 =
        unsafe { D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, None)? };
    let d2d_device = unsafe { d2d_factory.CreateDevice(&dxgi_device)? };
    let d2d_context = unsafe { d2d_device.CreateDeviceContext(D2D1_DEVICE_CONTEXT_OPTIONS_NONE)? };

    let swap_chain_desc = DXGI_SWAP_CHAIN_DESC1 {
        Width: width,
        Height: height,
        Format: DXGI_FORMAT_B8G8R8A8_UNORM,
        Stereo: false.into(),
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
        BufferCount: 2,
        Scaling: DXGI_SCALING_STRETCH,
        SwapEffect: DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL,
        AlphaMode: DXGI_ALPHA_MODE_IGNORE,
        Flags: 0,
    };
    let swap_chain = unsafe {
        dxgi_factory.CreateSwapChainForHwnd(&d3d_device, hwnd, &swap_chain_desc, None, None)?
    };
    let target_bitmap = create_swap_chain_target(&d2d_context, &swap_chain)?;
    unsafe {
        d2d_context.SetTarget(&target_bitmap.cast::<ID2D1Image>()?);
    }

    Ok((d2d_context, swap_chain, target_bitmap))
}

fn create_swap_chain_target(
    d2d_context: &ID2D1DeviceContext,
    swap_chain: &IDXGISwapChain1,
) -> windows::core::Result<ID2D1Bitmap1> {
    let surface: IDXGISurface = unsafe { swap_chain.GetBuffer(0)? };
    let properties = D2D1_BITMAP_PROPERTIES1 {
        pixelFormat: D2D1_PIXEL_FORMAT {
            format: DXGI_FORMAT_B8G8R8A8_UNORM,
            alphaMode: D2D1_ALPHA_MODE_PREMULTIPLIED,
        },
        dpiX: 96.0,
        dpiY: 96.0,
        bitmapOptions: D2D1_BITMAP_OPTIONS_TARGET | D2D1_BITMAP_OPTIONS_CANNOT_DRAW,
        colorContext: Default::default(),
    };

    unsafe { d2d_context.CreateBitmapFromDxgiSurface(&surface, Some(&properties)) }
}

fn playback_indices(state: &AppState, elapsed: f32) -> (usize, usize, f32) {
    let len = state.images.len();
    if len <= 1 {
        return (0, 0, 0.0);
    }

    let elapsed = (elapsed - 2.0).max(0.0);
    let hold = 6.0;
    let fade = 4.0;
    let period = hold + fade;
    let cycle = (elapsed / period).floor() as usize;
    let local = elapsed % period;
    let current = cycle % len;
    let next = (current + 1) % len;
    let alpha_next = if local < hold {
        0.0
    } else {
        logistic_crossfade((local - hold) / fade)
    };

    (current, next, alpha_next)
}

fn logistic_crossfade(progress: f32) -> f32 {
    let progress = progress.clamp(0.0, 1.0);
    let steepness = 10.0;
    let low = 1.0 / (1.0 + f32::exp(steepness * 0.5));
    let high = 1.0 / (1.0 + f32::exp(-steepness * 0.5));
    let value = 1.0 / (1.0 + f32::exp(-steepness * (progress - 0.5)));
    ((value - low) / (high - low)).clamp(0.0, 1.0)
}

fn fitted_rect(image_width: u32, image_height: u32, width: u32, height: u32) -> D2D_RECT_F {
    if image_width == 0 || image_height == 0 || width == 0 || height == 0 {
        return D2D_RECT_F {
            left: 0.0,
            top: 0.0,
            right: width as f32,
            bottom: height as f32,
        };
    }

    let scale = (width as f32 / image_width as f32).min(height as f32 / image_height as f32);
    let draw_width = image_width as f32 * scale;
    let draw_height = image_height as f32 * scale;
    let left = (width as f32 - draw_width) * 0.5;
    let top = (height as f32 - draw_height) * 0.5;

    D2D_RECT_F {
        left,
        top,
        right: left + draw_width,
        bottom: top + draw_height,
    }
}

fn draw_direct2d_bitmap(d2d: &Direct2DState, rects: &[D2D_RECT_F], index: usize, opacity: f32) {
    let Some(bitmap) = d2d.bitmaps.get(index).and_then(Option::as_ref) else {
        return;
    };
    let Some(rect) = rects.get(index) else {
        return;
    };

    unsafe {
        d2d.d2d_context.DrawBitmap(
            bitmap,
            Some(rect),
            opacity.clamp(0.0, 1.0),
            D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC,
            None,
            None,
        );
    }
}

fn config_file() -> windows::core::Result<PathBuf> {
    let base = dirs::config_local_dir().unwrap_or_else(|| PathBuf::from("."));
    let dir = base.join("Octoglow");
    ensure_directory(&dir)?;
    Ok(dir.join("settings.toml"))
}

fn load_config() -> windows::core::Result<Settings> {
    let file = config_file()?;
    if !file.exists() {
        return Ok(Settings::default());
    }

    let bytes = fs::read(&file).map_err(io_error)?;
    let text = String::from_utf8_lossy(&bytes);
    Ok(toml::from_str::<Settings>(&text).unwrap_or_default())
}

fn ensure_directory(path: &Path) -> windows::core::Result<()> {
    fs::create_dir_all(path).map_err(io_error)
}

fn wide(value: &str) -> Vec<u16> {
    OsStr::new(value).encode_wide().chain(once(0)).collect()
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

fn io_error(error: std::io::Error) -> windows::core::Error {
    windows::core::Error::new(
        windows::core::HRESULT(0x80004005u32 as i32),
        error.to_string(),
    )
}
