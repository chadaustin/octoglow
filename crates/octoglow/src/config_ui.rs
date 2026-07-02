use std::fs;
use std::io::Write;
use std::iter::once;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};
use windows::core::{HRESULT, PCWSTR};
use windows::Win32::Foundation::HWND;
use windows::Win32::Storage::FileSystem::{
    GetLogicalDrives, MoveFileExW, MOVEFILE_REPLACE_EXISTING, MOVEFILE_WRITE_THROUGH,
};

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
    let settings = load_settings_from_path(&config_file()?)?;
    Ok(settings.folders.into_iter().map(PathBuf::from).collect())
}

fn load_settings_from_path(file: &Path) -> windows::core::Result<Settings> {
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
    save_config_to_path(&config_file()?, paths)
}

fn save_config_to_path(file: &Path, paths: &[PathBuf]) -> windows::core::Result<()> {
    let mut settings = load_settings_from_path(file)?;
    settings.folders = paths
        .iter()
        .map(|path| path.display().to_string())
        .collect();
    save_settings_to_path(file, &settings)
}

fn save_settings_to_path(file: &Path, settings: &Settings) -> windows::core::Result<()> {
    let contents = toml::to_string_pretty(&settings).map_err(|error| {
        windows::core::Error::new(HRESULT(0x80004005u32 as i32), error.to_string())
    })?;
    atomic_write(file, contents.as_bytes())
}

fn ensure_directory(path: &Path) -> windows::core::Result<()> {
    fs::create_dir_all(path).map_err(io_error)
}

fn atomic_write(path: &Path, contents: &[u8]) -> windows::core::Result<()> {
    let dir = path.parent().unwrap_or_else(|| Path::new("."));
    ensure_directory(dir)?;
    let temp = temp_config_path(path);
    {
        let mut file = fs::File::create(&temp).map_err(io_error)?;
        file.write_all(contents).map_err(io_error)?;
        file.sync_all().map_err(io_error)?;
    }

    let temp_w = wide_path(&temp);
    let path_w = wide_path(path);
    let result = unsafe {
        MoveFileExW(
            PCWSTR(temp_w.as_ptr()),
            PCWSTR(path_w.as_ptr()),
            MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH,
        )
    };
    if let Err(error) = result {
        let _ = fs::remove_file(&temp);
        return Err(error);
    }
    Ok(())
}

fn temp_config_path(path: &Path) -> PathBuf {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0);
    let name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("settings.toml");
    path.with_file_name(format!(".{name}.{}.{}.tmp", std::process::id(), stamp))
}

fn io_error(error: std::io::Error) -> windows::core::Error {
    windows::core::Error::new(HRESULT(0x80004005u32 as i32), error.to_string())
}

fn wide_path(path: &Path) -> Vec<u16> {
    path.as_os_str().encode_wide().chain(once(0)).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_config_path(name: &str) -> PathBuf {
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|duration| duration.as_nanos())
            .unwrap_or(0);
        std::env::temp_dir()
            .join("octoglow-config-tests")
            .join(format!("{}-{}-{stamp}", std::process::id(), name))
            .join("settings.toml")
    }

    #[test]
    fn missing_settings_use_defaults() {
        let path = test_config_path("missing");
        let settings = load_settings_from_path(&path).expect("load missing settings");

        assert!(settings.folders.is_empty());
        assert_eq!(settings.memory_cap_mb, default_memory_cap_mb());
    }

    #[test]
    fn save_and_load_settings_round_trip() {
        let path = test_config_path("round-trip");
        let settings = Settings {
            folders: vec!["D:\\Pictures".to_string(), "E:\\Wallpapers".to_string()],
            memory_cap_mb: 512,
        };

        save_settings_to_path(&path, &settings).expect("save settings");
        let loaded = load_settings_from_path(&path).expect("load settings");

        assert_eq!(loaded.folders, settings.folders);
        assert_eq!(loaded.memory_cap_mb, settings.memory_cap_mb);
    }

    #[test]
    fn save_config_preserves_existing_memory_cap() {
        let path = test_config_path("preserve-cap");
        let settings = Settings {
            folders: vec!["D:\\Old".to_string()],
            memory_cap_mb: 256,
        };
        save_settings_to_path(&path, &settings).expect("seed settings");

        save_config_to_path(&path, &[PathBuf::from("N:\\Pictures")]).expect("save config");
        let loaded = load_settings_from_path(&path).expect("load settings");

        assert_eq!(loaded.folders, vec!["N:\\Pictures"]);
        assert_eq!(loaded.memory_cap_mb, 256);
    }

    #[test]
    fn malformed_settings_fall_back_to_defaults() {
        let path = test_config_path("malformed");
        ensure_directory(path.parent().expect("test path has parent")).expect("create dir");
        fs::write(&path, "folders = [").expect("write malformed settings");

        let loaded = load_settings_from_path(&path).expect("load settings");

        assert!(loaded.folders.is_empty());
        assert_eq!(loaded.memory_cap_mb, default_memory_cap_mb());
    }
}
