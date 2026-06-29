#![cfg_attr(windows, windows_subsystem = "windows")]
#![allow(unsafe_op_in_unsafe_fn)]

use std::iter::once;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use windows::core::PCWSTR;
use windows::Win32::Foundation::CloseHandle;
use windows::Win32::Storage::FileSystem::{
    CreateDirectoryW, CreateFileW, FindClose, FindFirstFileW, FindNextFileW, GetFileAttributesW,
    GetLogicalDrives, ReadFile, WriteFile, CREATE_ALWAYS, FILE_ATTRIBUTE_DIRECTORY,
    FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_NORMAL, FILE_ATTRIBUTE_SYSTEM, FILE_GENERIC_READ,
    FILE_GENERIC_WRITE, FILE_SHARE_READ, FILE_SHARE_WRITE, OPEN_EXISTING, WIN32_FIND_DATAW,
};

#[derive(Serialize)]
struct FolderNode {
    name: String,
    path: String,
    has_children: bool,
}

#[derive(Deserialize, Serialize, Default)]
struct Settings {
    folders: Vec<String>,
}

fn main() {
    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![
            list_roots,
            list_children,
            load_selection,
            save_selection,
            close_window
        ])
        .run(tauri::generate_context!())
        .expect("run Octoglow configuration UI");
}

#[tauri::command]
fn list_roots() -> Result<Vec<FolderNode>, String> {
    let drives = unsafe { GetLogicalDrives() };
    let mut roots = Vec::new();

    for index in 0..26 {
        if drives & (1 << index) != 0 {
            let letter = (b'A' + index as u8) as char;
            let path = format!("{letter}:\\");
            roots.push(FolderNode {
                name: path.clone(),
                path,
                has_children: true,
            });
        }
    }

    Ok(roots)
}

#[tauri::command]
fn list_children(path: String) -> Result<Vec<FolderNode>, String> {
    let root = PathBuf::from(path);
    let mut folders = Vec::new();
    collect_child_folders(&root, &mut folders);
    folders.sort_by_key(|node| node.name.to_ascii_lowercase());
    Ok(folders)
}

#[tauri::command]
fn load_selection() -> Result<Vec<String>, String> {
    load_config()
        .map(|paths| {
            paths
                .into_iter()
                .map(|path| path.display().to_string())
                .collect()
        })
        .map_err(|error| error.to_string())
}

#[tauri::command]
fn save_selection(paths: Vec<String>) -> Result<(), String> {
    let paths = paths.into_iter().map(PathBuf::from).collect::<Vec<_>>();
    save_config(&paths).map_err(|error| error.to_string())
}

#[tauri::command]
fn close_window(window: tauri::Window) -> Result<(), String> {
    window.close().map_err(|error| error.to_string())
}

fn collect_child_folders(root: &Path, folders: &mut Vec<FolderNode>) {
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
            if is_visible_directory(&find_data) {
                if let Some(name) = filename_from_find_data(&find_data) {
                    if name != "." && name != ".." {
                        let path = root.join(&name);
                        folders.push(FolderNode {
                            name,
                            path: path.display().to_string(),
                            has_children: directory_has_child_folder(&path),
                        });
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

fn is_visible_directory(data: &WIN32_FIND_DATAW) -> bool {
    let attrs = data.dwFileAttributes;
    attrs & FILE_ATTRIBUTE_DIRECTORY.0 != 0
        && attrs & FILE_ATTRIBUTE_SYSTEM.0 == 0
        && attrs & FILE_ATTRIBUTE_HIDDEN.0 == 0
}

fn directory_has_child_folder(path: &Path) -> bool {
    let pattern = path.join("*");
    let mut find_data = WIN32_FIND_DATAW::default();
    let pattern = wide_path(&pattern);

    unsafe {
        let handle = match FindFirstFileW(
            PCWSTR(pattern.as_ptr()),
            &mut find_data as *mut WIN32_FIND_DATAW,
        ) {
            Ok(handle) => handle,
            Err(_) => return false,
        };

        loop {
            if is_visible_directory(&find_data) {
                if let Some(name) = filename_from_find_data(&find_data) {
                    if name != "." && name != ".." {
                        let _ = FindClose(handle);
                        return true;
                    }
                }
            }

            if FindNextFileW(handle, &mut find_data).is_err() {
                break;
            }
        }

        let _ = FindClose(handle);
    }

    false
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

fn save_config(paths: &[PathBuf]) -> windows::core::Result<()> {
    let settings = Settings {
        folders: paths
            .iter()
            .map(|path| path.display().to_string())
            .collect(),
    };
    let contents = toml::to_string_pretty(&settings).unwrap_or_default();
    write_file(&config_file()?, contents.as_bytes())
}

fn ensure_directory(path: &Path) -> windows::core::Result<()> {
    let path_w = wide_path(path);
    unsafe {
        let attrs = GetFileAttributesW(PCWSTR(path_w.as_ptr()));
        if attrs != u32::MAX && attrs & FILE_ATTRIBUTE_DIRECTORY.0 != 0 {
            return Ok(());
        }
        let _ = CreateDirectoryW(PCWSTR(path_w.as_ptr()), None);
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

fn write_file(path: &Path, data: &[u8]) -> windows::core::Result<()> {
    let path_w = wide_path(path);
    unsafe {
        let handle = CreateFileW(
            PCWSTR(path_w.as_ptr()),
            FILE_GENERIC_WRITE.0,
            FILE_SHARE_READ,
            None,
            CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL,
            None,
        )?;
        let mut written = 0u32;
        WriteFile(handle, Some(data), Some(&mut written), None)?;
        CloseHandle(handle)?;
    }
    Ok(())
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

fn wide_path(path: &Path) -> Vec<u16> {
    path.as_os_str().encode_wide().chain(once(0)).collect()
}
