use std::iter::once;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{self, Receiver, SyncSender};
use std::thread;
use std::time::{SystemTime, UNIX_EPOCH};

use windows::core::PCWSTR;
use windows::Win32::Foundation::GENERIC_READ;
use windows::Win32::Graphics::Imaging::{
    CLSID_WICImagingFactory, GUID_ContainerFormatHeif, GUID_ContainerFormatJpeg,
    GUID_ContainerFormatPng, GUID_WICPixelFormat24bppBGR, IWICBitmapSource, IWICImagingFactory,
    WICConvertBitmapSource, WICDecodeMetadataCacheOnDemand,
};
use windows::Win32::System::Com::{
    CoCreateInstance, CoInitializeEx, CoUninitialize, CLSCTX_INPROC_SERVER, COINIT_MULTITHREADED,
};

const PHOTO_DIRECTORY_SAMPLE_COUNT: usize = 5;
const MAX_SELECTION_ATTEMPTS: usize = 128;
const DECODE_QUEUE_CAPACITY: usize = 3;

pub struct Playlist {
    decoded_rx: Receiver<DecodeMessage>,
}

pub enum PlaylistEvent {
    Candidate,
    Decoded(DecodedImage),
    DecodeFailed,
    Pending,
    Finished,
}

pub struct DecodedImage {
    pub path: PathBuf,
    pub width: u32,
    pub height: u32,
    /// 24-bit BGR pixels, tightly packed.
    pub pixels: Vec<u8>,
}

enum DecodeMessage {
    Candidate,
    Decoded(DecodedImage),
    DecodeFailed,
}

impl Playlist {
    pub fn start(roots: Vec<PathBuf>) -> Self {
        let (candidate_tx, candidate_rx) = mpsc::sync_channel::<PathBuf>(DECODE_QUEUE_CAPACITY);
        let (decoded_tx, decoded_rx) = mpsc::sync_channel::<DecodeMessage>(DECODE_QUEUE_CAPACITY);

        let selector_tx = candidate_tx.clone();
        thread::spawn(move || {
            let mut rng = RandomState::new();
            stream_image_candidates(&roots, &mut rng, |path| selector_tx.send(path).is_ok());
        });

        thread::spawn(move || {
            run_decoder(candidate_rx, decoded_tx);
        });

        Self { decoded_rx }
    }

    pub fn poll(&self) -> PlaylistEvent {
        match self.decoded_rx.try_recv() {
            Ok(DecodeMessage::Candidate) => PlaylistEvent::Candidate,
            Ok(DecodeMessage::Decoded(image)) => PlaylistEvent::Decoded(image),
            Ok(DecodeMessage::DecodeFailed) => PlaylistEvent::DecodeFailed,
            Err(mpsc::TryRecvError::Empty) => PlaylistEvent::Pending,
            Err(mpsc::TryRecvError::Disconnected) => PlaylistEvent::Finished,
        }
    }
}

fn run_decoder(candidate_rx: Receiver<PathBuf>, decoded_tx: SyncSender<DecodeMessage>) {
    let com_initialized = unsafe { CoInitializeEx(None, COINIT_MULTITHREADED).is_ok() };
    while let Ok(path) = candidate_rx.recv() {
        if decoded_tx.send(DecodeMessage::Candidate).is_err() {
            break;
        }

        let message = match decode_image_with_wic(&path) {
            Ok(image) => DecodeMessage::Decoded(image),
            Err(_) => DecodeMessage::DecodeFailed,
        };
        if decoded_tx.send(message).is_err() {
            break;
        }
    }
    if com_initialized {
        unsafe {
            CoUninitialize();
        }
    }
}

fn stream_image_candidates<F>(roots: &[PathBuf], rng: &mut RandomState, mut emit: F)
where
    F: FnMut(PathBuf) -> bool,
{
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
                if !stream_directory_images(&dir, rng, &mut emit) {
                    break;
                }
                photo_dirs.push(dir);
            }
        }
    }
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

fn stream_directory_images<F>(dir: &Path, rng: &mut RandomState, emit: &mut F) -> bool
where
    F: FnMut(PathBuf) -> bool,
{
    let mut images = Vec::new();
    crate::traversal::for_each_child(dir, |entry| {
        if entry.is_reparse_point() || entry.is_hidden_or_system() {
            return true;
        }
        if entry.is_file() && has_supported_extension(&entry.path) {
            images.push(entry.path);
        }
        true
    });
    rng.shuffle(&mut images);
    for image in images {
        if !emit(image) {
            return false;
        }
    }
    true
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

fn decode_image_with_wic(path: &Path) -> windows::core::Result<DecodedImage> {
    let factory: IWICImagingFactory =
        unsafe { CoCreateInstance(&CLSID_WICImagingFactory, None, CLSCTX_INPROC_SERVER)? };
    let path_w = wide_path(path);
    let decoder = unsafe {
        factory.CreateDecoderFromFilename(
            PCWSTR(path_w.as_ptr()),
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
    let source = unsafe { WICConvertBitmapSource(&GUID_WICPixelFormat24bppBGR, &frame)? };
    let mut width = 0;
    let mut height = 0;
    unsafe {
        source.GetSize(&mut width, &mut height)?;
    }

    Ok(DecodedImage {
        path: PathBuf::from(String::from_utf16_lossy(
            &path_w[..path_w.len().saturating_sub(1)],
        )),
        width,
        height,
        pixels: copy_wic_pixels(&source, width, height)?,
    })
}

fn copy_wic_pixels(
    source: &IWICBitmapSource,
    width: u32,
    height: u32,
) -> windows::core::Result<Vec<u8>> {
    let stride = width.saturating_mul(3);
    let size = stride.saturating_mul(height) as usize;
    let mut pixels = vec![0u8; size];
    unsafe {
        source.CopyPixels(std::ptr::null(), stride, &mut pixels)?;
    }
    Ok(pixels)
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

fn wide_path(path: &Path) -> Vec<u16> {
    path.as_os_str().encode_wide().chain(once(0)).collect()
}
