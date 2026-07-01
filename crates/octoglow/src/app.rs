use std::ffi::OsStr;
use std::fs;
use std::iter::once;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use scrnsave::{RunMode, ScreenSaver, ScreenSaverConfig};
use serde::Deserialize;
use windows::core::{w, Interface, PCWSTR};
use windows::Win32::Foundation::{COLORREF, GENERIC_READ, HMODULE, HWND, RECT};
use windows::Win32::Graphics::Direct2D::Common::{
    D2D1_ALPHA_MODE_PREMULTIPLIED, D2D1_COLOR_F, D2D1_PIXEL_FORMAT, D2D_RECT_F,
};
use windows::Win32::Graphics::Direct2D::{
    D2D1CreateFactory, ID2D1Bitmap1, ID2D1DeviceContext, ID2D1Factory1, ID2D1Image,
    D2D1_BITMAP_OPTIONS_CANNOT_DRAW, D2D1_BITMAP_OPTIONS_TARGET, D2D1_BITMAP_PROPERTIES1,
    D2D1_DEVICE_CONTEXT_OPTIONS_NONE, D2D1_FACTORY_TYPE_SINGLE_THREADED,
    D2D1_INTERPOLATION_MODE_HIGH_QUALITY_CUBIC,
};
use windows::Win32::Graphics::Direct3D::{
    D3D_DRIVER_TYPE_HARDWARE, D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_11_1,
};
use windows::Win32::Graphics::Direct3D11::{
    D3D11CreateDevice, ID3D11Device, D3D11_CREATE_DEVICE_BGRA_SUPPORT, D3D11_SDK_VERSION,
};
use windows::Win32::Graphics::Dwm::DwmFlush;
use windows::Win32::Graphics::Dxgi::Common::{
    DXGI_ALPHA_MODE_IGNORE, DXGI_FORMAT_B8G8R8A8_UNORM, DXGI_SAMPLE_DESC,
};
use windows::Win32::Graphics::Dxgi::{
    IDXGIDevice, IDXGIFactory2, IDXGISurface, IDXGISwapChain1, DXGI_SCALING_STRETCH,
    DXGI_SWAP_CHAIN_DESC1, DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL, DXGI_USAGE_RENDER_TARGET_OUTPUT,
};
use windows::Win32::Graphics::Gdi::{
    BeginPaint, CreateSolidBrush, DeleteObject, EndPaint, FillRect, SetBkMode, SetTextColor,
    TextOutW, ValidateRect, HBRUSH, PAINTSTRUCT, TRANSPARENT,
};
use windows::Win32::Graphics::Imaging::{
    CLSID_WICImagingFactory, GUID_ContainerFormatHeif, GUID_ContainerFormatJpeg,
    GUID_ContainerFormatPng, GUID_WICPixelFormat32bppPBGRA, IWICImagingFactory,
    WICConvertBitmapSource, WICDecodeMetadataCacheOnDemand,
};
use windows::Win32::System::Com::{
    CoCreateInstance, CoInitializeEx, CoUninitialize, CLSCTX_INPROC_SERVER,
    COINIT_APARTMENTTHREADED,
};
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
    candidate_count: usize,
    decode_failure_count: usize,
    memory_cap_bytes: u64,
    decoded_bytes: u64,
    images: Vec<DecodedImage>,
    d2d: Option<Direct2DState>,
}

struct DecodedImage {
    path: PathBuf,
    width: u32,
    height: u32,
    source: windows::Win32::Graphics::Imaging::IWICBitmapSource,
}

const DEFAULT_MEMORY_CAP_MB: u64 = 1024;
const PHOTO_DIRECTORY_SAMPLE_COUNT: usize = 5;
const MAX_SELECTION_ATTEMPTS: usize = 128;

struct Direct2DState {
    width: u32,
    height: u32,
    d2d_context: ID2D1DeviceContext,
    swap_chain: IDXGISwapChain1,
    _target_bitmap: ID2D1Bitmap1,
    bitmaps: Vec<ID2D1Bitmap1>,
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
        let scan = collect_images(&image_roots, memory_cap_bytes);
        Ok(AppState {
            started: Instant::now(),
            image_roots,
            candidate_count: scan.candidate_count,
            decode_failure_count: scan.decode_failure_count,
            memory_cap_bytes,
            decoded_bytes: scan.decoded_bytes,
            images: scan.images,
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

    if !state.images.is_empty() && render_with_direct2d(hwnd, state, width, height).is_ok() {
        unsafe {
            let _ = ValidateRect(Some(hwnd), None);
            let _ = DwmFlush();
        }
        return;
    }

    let mut ps = PAINTSTRUCT::default();
    let hdc = unsafe { BeginPaint(hwnd, &mut ps) };

    let elapsed = state.started.elapsed().as_secs_f32();
    let glow = ((elapsed.sin() + 1.0) * 70.0 + 90.0) as u8;
    unsafe {
        SetBkMode(hdc, TRANSPARENT);
    }

    let lines = status_lines(state);

    unsafe {
        let brush: HBRUSH = CreateSolidBrush(COLORREF(0x00101410));
        FillRect(hdc, &rect, brush);
        let _ = DeleteObject(brush.into());
    }

    let text_color = if state.images.is_empty() {
        COLORREF((glow as u32) << 8 | 0x40)
    } else {
        COLORREF(0x00f0f0f0)
    };
    unsafe {
        SetTextColor(hdc, text_color);
    }

    for (index, line) in lines.iter().enumerate() {
        let text = wide(line);
        unsafe {
            let _ = TextOutW(hdc, 48, 48 + (index as i32 * 24), &text[..text.len() - 1]);
        }
    }

    unsafe {
        let _ = EndPaint(hwnd, &ps);
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

struct ImageScan {
    candidate_count: usize,
    decode_failure_count: usize,
    decoded_bytes: u64,
    images: Vec<DecodedImage>,
}

fn collect_images(roots: &[PathBuf], memory_cap_bytes: u64) -> ImageScan {
    let mut rng = RandomState::new();
    let candidates = select_image_candidates(roots, &mut rng);
    let candidate_count = candidates.len();
    let mut decode_failure_count = 0;
    let mut decoded_bytes = 0u64;
    let mut images = Vec::new();

    for path in candidates {
        match decode_image_with_wic(&path) {
            Ok(image) => {
                let image_bytes = decoded_image_bytes(&image);
                if !images.is_empty()
                    && decoded_bytes.saturating_add(image_bytes) > memory_cap_bytes
                {
                    break;
                }
                decoded_bytes = decoded_bytes.saturating_add(image_bytes);
                images.push(image);
                if decoded_bytes >= memory_cap_bytes {
                    break;
                }
            }
            Err(_) => {
                decode_failure_count += 1;
            }
        }
    }

    ImageScan {
        candidate_count,
        decode_failure_count,
        decoded_bytes,
        images,
    }
}

fn select_image_candidates(roots: &[PathBuf], rng: &mut RandomState) -> Vec<PathBuf> {
    let mut photo_dirs = Vec::new();
    for _ in 0..MAX_SELECTION_ATTEMPTS {
        if photo_dirs.len() >= PHOTO_DIRECTORY_SAMPLE_COUNT {
            break;
        }
        let Some(root) = rng.choose(roots) else {
            break;
        };
        if let Some(dir) = choose_photo_directory(root, rng) {
            if !photo_dirs.iter().any(|existing| existing == &dir) {
                photo_dirs.push(dir);
            }
        }
    }

    let mut candidates = Vec::new();
    for dir in &photo_dirs {
        collect_directory_images(dir, &mut candidates);
    }
    rng.shuffle(&mut candidates);
    candidates
}

fn choose_photo_directory(root: &Path, rng: &mut RandomState) -> Option<PathBuf> {
    let mut current = root.to_path_buf();
    let mut best = None;

    for _ in 0..64 {
        let mut child_dirs = Vec::new();
        let mut image_count = 0usize;
        crate::traversal::for_each_child(&current, |entry| {
            if entry.is_reparse_point() || entry.is_hidden_or_system() {
                return true;
            }
            if entry.is_directory() {
                child_dirs.push(entry.path);
            } else if entry.is_file() && has_supported_extension(&entry.path) {
                image_count += 1;
            }
            true
        });

        if image_count > 0 {
            best = Some(current.clone());
            if child_dirs.is_empty() || rng.next_usize(3) == 0 {
                break;
            }
        }

        if child_dirs.is_empty() {
            break;
        }
        current = child_dirs.swap_remove(rng.next_usize(child_dirs.len()));
    }

    best
}

fn collect_directory_images(dir: &Path, images: &mut Vec<PathBuf>) {
    crate::traversal::for_each_child(dir, |entry| {
        if entry.is_reparse_point() || entry.is_hidden_or_system() {
            return true;
        }
        if entry.is_file() && has_supported_extension(&entry.path) {
            images.push(entry.path);
        }
        true
    });
}

struct RandomState {
    state: u64,
}

impl RandomState {
    fn new() -> Self {
        Self {
            state: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|duration| duration.as_nanos() as u64)
                .unwrap_or(0x9e3779b97f4a7c15),
        }
    }

    fn next_u64(&mut self) -> u64 {
        self.state = self
            .state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.state
    }

    fn next_usize(&mut self, upper: usize) -> usize {
        if upper == 0 {
            0
        } else {
            (self.next_u64() as usize) % upper
        }
    }

    fn choose<'a, T>(&mut self, values: &'a [T]) -> Option<&'a T> {
        values.get(self.next_usize(values.len()))
    }

    fn shuffle<T>(&mut self, values: &mut [T]) {
        for index in (1..values.len()).rev() {
            values.swap(index, self.next_usize(index + 1));
        }
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

fn decoded_image_bytes(image: &DecodedImage) -> u64 {
    u64::from(image.width)
        .saturating_mul(u64::from(image.height))
        .saturating_mul(4)
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
    let source = unsafe { WICConvertBitmapSource(&GUID_WICPixelFormat32bppPBGRA, &frame)? };
    let mut width = 0;
    let mut height = 0;
    unsafe {
        source.GetSize(&mut width, &mut height)?;
    }

    Ok(DecodedImage {
        path: PathBuf::from(String::from_utf16_lossy(
            &path[..path.len().saturating_sub(1)],
        )),
        width,
        height,
        source,
    })
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
    if state.images.is_empty() {
        return Err(windows::core::Error::from_hresult(windows::core::HRESULT(
            0x80004005u32 as i32,
        )));
    }

    let elapsed = state.started.elapsed().as_secs_f32();
    let (current, next, alpha_next) = playback_indices(state, elapsed);
    ensure_direct2d_state(hwnd, state, width, height)?;
    let d2d = state.d2d.as_ref().expect("Direct2D state was initialized");
    let image_rects = state
        .images
        .iter()
        .map(|image| fitted_rect(image.width, image.height, width, height))
        .collect::<Vec<_>>();
    let black = D2D1_COLOR_F {
        r: 0.0,
        g: 0.0,
        b: 0.0,
        a: 1.0,
    };

    unsafe {
        d2d.d2d_context.BeginDraw();
        d2d.d2d_context.Clear(Some(&black));
    }

    if elapsed < 2.0 {
        let opacity = (elapsed / 2.0).max(0.08);
        draw_direct2d_bitmap(d2d, &image_rects, current, opacity);
    } else if current == next || alpha_next <= 0.0 {
        draw_direct2d_bitmap(d2d, &image_rects, current, 1.0);
    } else {
        draw_direct2d_bitmap(d2d, &image_rects, current, 1.0 - alpha_next);
        draw_direct2d_bitmap(d2d, &image_rects, next, alpha_next);
    }

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
        let mut bitmaps = Vec::with_capacity(state.images.len());
        for image in &state.images {
            bitmaps.push(unsafe { d2d_context.CreateBitmapFromWicBitmap(&image.source, None)? });
        }
        state.d2d = Some(Direct2DState {
            width,
            height,
            d2d_context,
            swap_chain,
            _target_bitmap: target_bitmap,
            bitmaps,
        });
    }

    Ok(state.d2d.as_mut().expect("Direct2D state was initialized"))
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
    let Some(bitmap) = d2d.bitmaps.get(index) else {
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

fn io_error(error: std::io::Error) -> windows::core::Error {
    windows::core::Error::new(
        windows::core::HRESULT(0x80004005u32 as i32),
        error.to_string(),
    )
}
