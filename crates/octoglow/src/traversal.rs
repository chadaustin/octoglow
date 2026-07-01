use std::iter::once;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use windows::core::PCWSTR;
use windows::Win32::Foundation::HANDLE;
use windows::Win32::Storage::FileSystem::{
    FindClose, FindExInfoBasic, FindExSearchNameMatch, FindFirstFileExW, FindNextFileW,
    FILE_ATTRIBUTE_DIRECTORY, FILE_ATTRIBUTE_HIDDEN, FILE_ATTRIBUTE_REPARSE_POINT,
    FILE_ATTRIBUTE_SYSTEM, FIND_FIRST_EX_LARGE_FETCH, WIN32_FIND_DATAW,
};

pub struct DirectoryEntry {
    pub name: String,
    pub path: PathBuf,
    pub attributes: u32,
}

impl DirectoryEntry {
    pub fn is_directory(&self) -> bool {
        self.attributes & FILE_ATTRIBUTE_DIRECTORY.0 != 0
    }

    pub fn is_file(&self) -> bool {
        !self.is_directory()
    }

    pub fn is_hidden_or_system(&self) -> bool {
        self.attributes & (FILE_ATTRIBUTE_HIDDEN.0 | FILE_ATTRIBUTE_SYSTEM.0) != 0
    }

    pub fn is_reparse_point(&self) -> bool {
        self.attributes & FILE_ATTRIBUTE_REPARSE_POINT.0 != 0
    }
}

pub fn for_each_child<F>(root: &Path, mut visit: F)
where
    F: FnMut(DirectoryEntry) -> bool,
{
    let pattern = root.join("*");
    let pattern = wide_path(&pattern);
    let mut find_data = WIN32_FIND_DATAW::default();

    let Ok(handle) = (unsafe {
        FindFirstFileExW(
            PCWSTR(pattern.as_ptr()),
            FindExInfoBasic,
            &mut find_data as *mut WIN32_FIND_DATAW as *mut _,
            FindExSearchNameMatch,
            None,
            FIND_FIRST_EX_LARGE_FETCH,
        )
    }) else {
        return;
    };
    let _guard = FindHandle(handle);

    loop {
        if let Some(name) = filename_from_find_data(&find_data) {
            if name != "." && name != ".." {
                if !visit(DirectoryEntry {
                    path: root.join(&name),
                    name,
                    attributes: find_data.dwFileAttributes,
                }) {
                    break;
                }
            }
        }

        if unsafe { FindNextFileW(handle, &mut find_data) }.is_err() {
            break;
        }
    }
}

struct FindHandle(HANDLE);

impl Drop for FindHandle {
    fn drop(&mut self) {
        let _ = unsafe { FindClose(self.0) };
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

fn wide_path(path: &Path) -> Vec<u16> {
    path.as_os_str().encode_wide().chain(once(0)).collect()
}
