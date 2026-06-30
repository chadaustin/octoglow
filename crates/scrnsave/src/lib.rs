//! Rust implementation of the core screensaver protocol and window framework
//! normally provided by Microsoft's `Scrnsave.lib`.

#![cfg_attr(windows, allow(unsafe_op_in_unsafe_fn))]

use std::ffi::OsStr;
use std::iter::once;
use std::marker::PhantomData;
use std::os::windows::ffi::OsStrExt;

use windows::core::PCWSTR;
use windows::Win32::Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM};
use windows::Win32::Graphics::Gdi::{InvalidateRect, UpdateWindow};
use windows::Win32::System::LibraryLoader::GetModuleHandleW;
use windows::Win32::UI::WindowsAndMessaging::{
    CreateWindowExW, DefWindowProcW, DestroyWindow, DispatchMessageW, GetCursorPos, GetMessageW,
    GetSystemMetrics, GetWindowLongPtrW, GetWindowRect, KillTimer, LoadCursorW, PostQuitMessage,
    RegisterClassW, SetTimer, SetWindowLongPtrW, ShowCursor, ShowWindow, TranslateMessage,
    CREATESTRUCTW, CS_HREDRAW, CS_VREDRAW, GWLP_USERDATA, IDC_ARROW, MSG, SM_CXSCREEN, SM_CYSCREEN,
    SW_SHOW, WINDOW_EX_STYLE, WM_CREATE, WM_DESTROY, WM_KEYDOWN, WM_LBUTTONDOWN, WM_MBUTTONDOWN,
    WM_MOUSEMOVE, WM_PAINT, WM_RBUTTONDOWN, WM_TIMER, WNDCLASSW, WS_CHILD, WS_OVERLAPPEDWINDOW,
    WS_POPUP, WS_VISIBLE,
};

const DEFAULT_TIMER_ID: usize = 0x5343_5253;
const DEFAULT_TIMER_MS: u32 = 33;
const MOUSE_DEADZONE: i32 = 4;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScreenSaverMode {
    ScreenSaver,
    Configure { parent: Option<HWND> },
    Preview { parent: Option<HWND> },
    ChangePassword { parent: Option<HWND> },
}

impl Default for ScreenSaverMode {
    fn default() -> Self {
        Self::Configure { parent: None }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RunMode {
    ScreenSaver,
    Preview { parent: Option<HWND> },
}

#[derive(Clone, Debug)]
pub struct ScreenSaverConfig {
    pub class_name: String,
    pub window_title: String,
    pub timer_id: usize,
    pub timer_ms: u32,
    pub close_on_input: bool,
    pub mouse_deadzone: i32,
}

impl ScreenSaverConfig {
    pub fn new(class_name: impl Into<String>, window_title: impl Into<String>) -> Self {
        Self {
            class_name: class_name.into(),
            window_title: window_title.into(),
            timer_id: DEFAULT_TIMER_ID,
            timer_ms: DEFAULT_TIMER_MS,
            close_on_input: true,
            mouse_deadzone: MOUSE_DEADZONE,
        }
    }
}

pub trait ScreenSaver {
    type State;

    fn config(&self) -> ScreenSaverConfig;
    fn initialize(&mut self, mode: RunMode) -> windows::core::Result<Self::State>;
    fn configure(&mut self, parent: Option<HWND>) -> windows::core::Result<()>;

    fn change_password(&mut self, _parent: Option<HWND>) -> windows::core::Result<()> {
        Ok(())
    }

    unsafe fn paint(&mut self, _hwnd: HWND, _state: &mut Self::State) {}

    unsafe fn timer(&mut self, _hwnd: HWND, _state: &mut Self::State) {}

    unsafe fn destroy(&mut self, _state: &mut Self::State) {}
}

pub fn run<A, I, S>(app: A, args: I) -> windows::core::Result<()>
where
    A: ScreenSaver + 'static,
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    match parse_args(args) {
        ScreenSaverMode::ScreenSaver => unsafe { run_window(app, RunMode::ScreenSaver) },
        ScreenSaverMode::Preview { parent } => unsafe {
            run_window(app, RunMode::Preview { parent })
        },
        ScreenSaverMode::Configure { parent } => {
            let mut app = app;
            app.configure(parent)
        }
        ScreenSaverMode::ChangePassword { parent } => {
            let mut app = app;
            app.change_password(parent)
        }
    }
}

pub fn parse_args<I, S>(args: I) -> ScreenSaverMode
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let args = args
        .into_iter()
        .map(|arg| arg.as_ref().trim().to_string())
        .filter(|arg| !arg.is_empty())
        .collect::<Vec<_>>();

    let Some(first) = args.first() else {
        return ScreenSaverMode::default();
    };

    let switch = first.trim_start_matches(['/', '-']);
    let command = switch
        .chars()
        .next()
        .map(|command| command.to_ascii_lowercase())
        .unwrap_or('c');
    let attached = switch.split_once(':').map(|(_, value)| value);
    let following = args.get(1).map(String::as_str);

    match command {
        's' => ScreenSaverMode::ScreenSaver,
        'p' => ScreenSaverMode::Preview {
            parent: parse_optional_hwnd(attached, following),
        },
        'a' => ScreenSaverMode::ChangePassword {
            parent: parse_optional_hwnd(attached, following),
        },
        'c' => ScreenSaverMode::Configure {
            parent: parse_optional_hwnd(attached, following),
        },
        _ => ScreenSaverMode::default(),
    }
}

unsafe fn run_window<A>(mut app: A, mode: RunMode) -> windows::core::Result<()>
where
    A: ScreenSaver + 'static,
{
    let config = app.config();
    let class_name = wide(&config.class_name);
    let title = wide(&config.window_title);
    let hinstance = HINSTANCE(GetModuleHandleW(None)?.0);
    let cursor = LoadCursorW(None, IDC_ARROW)?;
    let wc = WNDCLASSW {
        hCursor: cursor,
        hInstance: hinstance,
        lpszClassName: PCWSTR(class_name.as_ptr()),
        style: CS_HREDRAW | CS_VREDRAW,
        lpfnWndProc: Some(window_proc::<A>),
        ..Default::default()
    };
    RegisterClassW(&wc);

    let state = app.initialize(mode)?;
    let pending = Box::new(PendingWindow {
        app,
        state,
        config,
        mode,
    });
    let pending_ptr = Box::into_raw(pending);

    let placement = WindowPlacement::for_mode(mode);
    let hwnd = match CreateWindowExW(
        WINDOW_EX_STYLE::default(),
        PCWSTR(class_name.as_ptr()),
        PCWSTR(title.as_ptr()),
        placement.style,
        placement.x,
        placement.y,
        placement.width,
        placement.height,
        placement.parent,
        None,
        Some(hinstance),
        Some(pending_ptr.cast()),
    ) {
        Ok(hwnd) => hwnd,
        Err(error) => {
            drop(Box::from_raw(pending_ptr));
            return Err(error);
        }
    };

    if mode == RunMode::ScreenSaver {
        let _ = ShowCursor(false);
    }
    let _ = ShowWindow(hwnd, SW_SHOW);
    let _ = InvalidateRect(Some(hwnd), None, false);
    let _ = UpdateWindow(hwnd);

    let mut msg = MSG::default();
    while GetMessageW(&mut msg, None, 0, 0).into() {
        let _ = TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }

    if mode == RunMode::ScreenSaver {
        let _ = ShowCursor(true);
    }
    Ok(())
}

unsafe extern "system" fn window_proc<A>(
    hwnd: HWND,
    msg: u32,
    wparam: WPARAM,
    lparam: LPARAM,
) -> LRESULT
where
    A: ScreenSaver + 'static,
{
    match msg {
        WM_CREATE => {
            let create = lparam.0 as *const CREATESTRUCTW;
            let pending = Box::from_raw((*create).lpCreateParams as *mut PendingWindow<A>);
            let mut cursor = POINT::default();
            let _ = GetCursorPos(&mut cursor);

            let timer_id = pending.config.timer_id;
            let timer_ms = pending.config.timer_ms;
            let data = Box::new(WindowData {
                pending,
                initial_cursor: Some(cursor),
                _marker: PhantomData::<A>,
            });

            SetWindowLongPtrW(hwnd, GWLP_USERDATA, Box::into_raw(data) as isize);
            SetTimer(Some(hwnd), timer_id, timer_ms, None);
            LRESULT(0)
        }
        WM_TIMER => {
            if let Some(data) = window_data::<A>(hwnd) {
                data.pending.app.timer(hwnd, &mut data.pending.state);
            }
            LRESULT(0)
        }
        WM_MOUSEMOVE => {
            if should_close_on_mouse_move::<A>(hwnd) {
                let _ = DestroyWindow(hwnd);
            }
            LRESULT(0)
        }
        WM_KEYDOWN | WM_LBUTTONDOWN | WM_MBUTTONDOWN | WM_RBUTTONDOWN => {
            if let Some(data) = window_data::<A>(hwnd) {
                if data.pending.config.close_on_input && data.pending.mode == RunMode::ScreenSaver {
                    let _ = DestroyWindow(hwnd);
                }
            }
            LRESULT(0)
        }
        WM_PAINT => {
            if let Some(data) = window_data::<A>(hwnd) {
                data.pending.app.paint(hwnd, &mut data.pending.state);
            }
            LRESULT(0)
        }
        WM_DESTROY => {
            let raw = GetWindowLongPtrW(hwnd, GWLP_USERDATA);
            if raw != 0 {
                SetWindowLongPtrW(hwnd, GWLP_USERDATA, 0);
                let mut data = Box::from_raw(raw as *mut WindowData<A>);
                let _ = KillTimer(Some(hwnd), data.pending.config.timer_id);
                data.pending.app.destroy(&mut data.pending.state);
            }
            PostQuitMessage(0);
            LRESULT(0)
        }
        _ => DefWindowProcW(hwnd, msg, wparam, lparam),
    }
}

unsafe fn window_data<A>(hwnd: HWND) -> Option<&'static mut WindowData<A>>
where
    A: ScreenSaver + 'static,
{
    let raw = GetWindowLongPtrW(hwnd, GWLP_USERDATA);
    (raw != 0).then(|| &mut *(raw as *mut WindowData<A>))
}

unsafe fn should_close_on_mouse_move<A>(hwnd: HWND) -> bool
where
    A: ScreenSaver + 'static,
{
    let Some(data) = window_data::<A>(hwnd) else {
        return false;
    };
    if data.pending.mode != RunMode::ScreenSaver || !data.pending.config.close_on_input {
        return false;
    }

    let mut cursor = POINT::default();
    if GetCursorPos(&mut cursor).is_err() {
        return false;
    }

    let Some(initial) = data.initial_cursor else {
        data.initial_cursor = Some(cursor);
        return false;
    };

    (cursor.x - initial.x).abs() > data.pending.config.mouse_deadzone
        || (cursor.y - initial.y).abs() > data.pending.config.mouse_deadzone
}

struct PendingWindow<A: ScreenSaver> {
    app: A,
    state: A::State,
    config: ScreenSaverConfig,
    mode: RunMode,
}

struct WindowData<A: ScreenSaver> {
    pending: Box<PendingWindow<A>>,
    initial_cursor: Option<POINT>,
    _marker: PhantomData<A>,
}

struct WindowPlacement {
    style: windows::Win32::UI::WindowsAndMessaging::WINDOW_STYLE,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
    parent: Option<HWND>,
}

impl WindowPlacement {
    unsafe fn for_mode(mode: RunMode) -> Self {
        match mode {
            RunMode::ScreenSaver => Self {
                style: WS_POPUP,
                x: 0,
                y: 0,
                width: GetSystemMetrics(SM_CXSCREEN),
                height: GetSystemMetrics(SM_CYSCREEN),
                parent: None,
            },
            RunMode::Preview { parent } => {
                let mut rect = RECT::default();
                let (width, height) = if let Some(parent) = parent {
                    if GetWindowRect(parent, &mut rect).is_ok() {
                        (rect.right - rect.left, rect.bottom - rect.top)
                    } else {
                        (320, 240)
                    }
                } else {
                    (320, 240)
                };

                Self {
                    style: WS_CHILD | WS_VISIBLE | WS_OVERLAPPEDWINDOW,
                    x: 0,
                    y: 0,
                    width,
                    height,
                    parent,
                }
            }
        }
    }
}

fn parse_optional_hwnd(attached: Option<&str>, following: Option<&str>) -> Option<HWND> {
    attached
        .and_then(parse_hwnd)
        .or_else(|| following.and_then(parse_hwnd))
}

fn parse_hwnd(raw: &str) -> Option<HWND> {
    let raw = raw.trim().trim_matches('"').trim_matches('\'');
    if raw.is_empty() {
        return None;
    }

    let value = raw
        .strip_prefix("0x")
        .or_else(|| raw.strip_prefix("0X"))
        .map(|hex| isize::from_str_radix(hex, 16))
        .unwrap_or_else(|| raw.parse::<isize>())
        .ok()?;

    Some(HWND(value as *mut _))
}

fn wide(value: &str) -> Vec<u16> {
    OsStr::new(value).encode_wide().chain(once(0)).collect()
}

#[cfg(test)]
mod tests {
    use super::{parse_args, ScreenSaverMode};
    use windows::Win32::Foundation::HWND;

    fn hwnd(value: isize) -> HWND {
        HWND(value as *mut _)
    }

    #[test]
    fn no_args_opens_configuration() {
        assert_eq!(parse_args([] as [&str; 0]), ScreenSaverMode::default());
    }

    #[test]
    fn parses_fullscreen_switch() {
        assert_eq!(parse_args(["/s"]), ScreenSaverMode::ScreenSaver);
        assert_eq!(parse_args(["-S"]), ScreenSaverMode::ScreenSaver);
    }

    #[test]
    fn parses_configuration_parent_variants() {
        assert_eq!(
            parse_args(["/c:1234"]),
            ScreenSaverMode::Configure {
                parent: Some(hwnd(1234))
            }
        );
        assert_eq!(
            parse_args(["/c", "1234"]),
            ScreenSaverMode::Configure {
                parent: Some(hwnd(1234))
            }
        );
    }

    #[test]
    fn parses_preview_parent_variants() {
        assert_eq!(
            parse_args(["/p", "1234"]),
            ScreenSaverMode::Preview {
                parent: Some(hwnd(1234))
            }
        );
        assert_eq!(
            parse_args(["/p:0x4d2"]),
            ScreenSaverMode::Preview {
                parent: Some(hwnd(1234))
            }
        );
    }

    #[test]
    fn parses_password_parent_variants() {
        assert_eq!(
            parse_args(["/a", "1234"]),
            ScreenSaverMode::ChangePassword {
                parent: Some(hwnd(1234))
            }
        );
        assert_eq!(
            parse_args(["/a:1234"]),
            ScreenSaverMode::ChangePassword {
                parent: Some(hwnd(1234))
            }
        );
    }

    #[test]
    fn invalid_or_unknown_switches_fall_back_to_configuration() {
        assert_eq!(parse_args(["/wat"]), ScreenSaverMode::default());
        assert_eq!(
            parse_args(["/p", "nope"]),
            ScreenSaverMode::Preview { parent: None }
        );
    }
}
