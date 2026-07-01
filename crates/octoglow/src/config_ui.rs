use std::fs;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};
use windows::core::HRESULT;
use windows::Win32::Foundation::HWND;
use windows::Win32::Storage::FileSystem::GetLogicalDrives;

#[derive(Serialize)]
struct FolderNode {
    name: String,
    path: String,
    has_children: bool,
}

#[derive(Deserialize, Serialize, Default)]
struct Settings {
    #[serde(default)]
    folders: Vec<String>,
    #[serde(default = "default_memory_cap_mb")]
    memory_cap_mb: u64,
}

fn default_memory_cap_mb() -> u64 {
    1024
}

pub fn run(_parent: Option<HWND>) -> windows::core::Result<()> {
    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![
            list_roots,
            list_children,
            load_selection,
            save_selection,
            close_window
        ])
        .run(tauri::generate_context!())
        .map_err(|error| {
            windows::core::Error::new(HRESULT(0x80004005u32 as i32), error.to_string())
        })
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
    crate::traversal::for_each_child(root, |entry| {
        if !is_visible_directory(&entry) {
            return true;
        }

        folders.push(FolderNode {
            name: entry.name,
            path: entry.path.display().to_string(),
            has_children: directory_has_child_folder(&entry.path),
        });
        true
    });
}

fn is_visible_directory(entry: &crate::traversal::DirectoryEntry) -> bool {
    entry.is_directory() && !entry.is_hidden_or_system() && !entry.is_reparse_point()
}

fn directory_has_child_folder(path: &Path) -> bool {
    let mut found = false;
    crate::traversal::for_each_child(path, |entry| {
        if !found && is_visible_directory(&entry) {
            found = true;
            return false;
        }
        true
    });
    found
}

fn config_file() -> windows::core::Result<PathBuf> {
    let base = dirs::config_local_dir().unwrap_or_else(|| PathBuf::from("."));
    let dir = base.join("Octoglow");
    ensure_directory(&dir)?;
    Ok(dir.join("settings.toml"))
}

fn load_config() -> windows::core::Result<Vec<PathBuf>> {
    let settings = load_settings()?;
    Ok(settings.folders.into_iter().map(PathBuf::from).collect())
}

fn load_settings() -> windows::core::Result<Settings> {
    let file = config_file()?;
    if !file.exists() {
        return Ok(Settings {
            folders: Vec::new(),
            memory_cap_mb: default_memory_cap_mb(),
        });
    }

    let bytes = fs::read(&file).map_err(io_error)?;
    let text = String::from_utf8_lossy(&bytes);
    Ok(
        toml::from_str::<Settings>(&text).unwrap_or_else(|_| Settings {
            folders: Vec::new(),
            memory_cap_mb: default_memory_cap_mb(),
        }),
    )
}

fn save_config(paths: &[PathBuf]) -> windows::core::Result<()> {
    let mut settings = load_settings()?;
    settings.folders = paths
        .iter()
        .map(|path| path.display().to_string())
        .collect();
    let contents = toml::to_string_pretty(&settings).map_err(|error| {
        windows::core::Error::new(HRESULT(0x80004005u32 as i32), error.to_string())
    })?;
    fs::write(config_file()?, contents).map_err(io_error)
}

fn ensure_directory(path: &Path) -> windows::core::Result<()> {
    fs::create_dir_all(path).map_err(io_error)
}

fn io_error(error: std::io::Error) -> windows::core::Error {
    windows::core::Error::new(HRESULT(0x80004005u32 as i32), error.to_string())
}
