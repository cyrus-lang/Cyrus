// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
use cyrusc_tui_utils::tui_error;
use std::{
    fs::{self, File},
    io::Read,
    path::{Path, PathBuf},
    process::exit,
};

/// Reads a file from the given path and returns its contents along with the file name.
///
/// Behavior
/// - On success: returns `(file_content, file_name)`.
/// - On failure (e.g. file not found or unreadable): prints a TUI error and exits the process.
pub fn read_file<P: AsRef<Path>>(file_path: P) -> (String, String) {
    let path = file_path.as_ref();

    let mut file = File::open(path).unwrap_or_else(|_| {
        tui_error("No such file or directory.".into());
        exit(1);
    });

    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap_or_else(|err| {
        tui_error(format!("Failed to read file content: {}", err));
        exit(1);
    });

    let file_name = path.file_name().and_then(|name| name.to_str()).unwrap_or_else(|| {
        tui_error("Failed to extract file name.".into());
        exit(1);
    });

    (contents, file_name.to_owned())
}

/// Ensures that the given path exists and is a valid directory.
///
/// # Behavior
/// - If the directory does not exist: attempts to create it.
/// - If a non-directory file exists at that path: terminates with an error.
/// - On failure to create directory: prints a TUI error and exits.
pub fn ensure_output_dir<P: AsRef<Path>>(output_dir: P) {
    let path = output_dir.as_ref();

    if !path.exists() {
        fs::create_dir_all(path).unwrap_or_else(|err| {
            tui_error(format!(
                "Failed to create output directory: {} ({})",
                path.display(),
                err
            ));
            exit(1);
        });
    } else if !path.is_dir() {
        tui_error(format!("Output path must be a directory: {}", path.display()));
        exit(1);
    }
}

/// Searches for a file or directory (`file_name`) across multiple source directories.
///
/// Returns the first full path found, or `None` if not found.
pub fn find_file_from_sources<S: AsRef<Path>>(file_name: &str, sources: &[S]) -> Option<PathBuf> {
    let target = Path::new(file_name);

    for src in sources {
        let path = src.as_ref().join(target);

        if path.exists() && (path.is_file() || path.is_dir()) {
            return Some(path);
        }
    }

    None
}

/// Returns the parent directory of a given file path, if it exists.
pub fn get_directory_of_file<P: AsRef<Path>>(file_path: P) -> Option<String> {
    file_path
        .as_ref()
        .parent()
        .and_then(|p| p.to_str())
        .map(|s| s.to_owned())
}

/// Returns the file stem (filename without extension) as a string slice.
pub fn file_stem(file_name: &str) -> Option<&str> {
    Path::new(file_name).file_stem().and_then(|s| s.to_str())
}

/// Returns the platform-specific dynamic library extension.
///
/// - Windows: `dll`
/// - macOS: `dylib`
/// - Linux/Unix: `so`
pub fn dylib_extension() -> &'static str {
    if cfg!(target_os = "windows") {
        "dll"
    } else if cfg!(target_os = "macos") {
        "dylib"
    } else {
        "so"
    }
}

/// Returns the platform-specific executable extension.
///
/// - Windows: `.exe`
/// - Others: ``
pub fn executable_extension() -> &'static str {
    if cfg!(target_os = "windows") { ".exe" } else { "" }
}

/// Splits a file path into directory and filename components.
///
/// Returns `(directory, filename)` as `String`s.
/// Missing parts (e.g. if path has no parent or no file name) become empty strings.
///
/// Example:  
/// `/home/user/file.txt` -> `("/home/user", "file.txt")`
pub fn split_paths<P: AsRef<Path>>(path: P) -> (String, String) {
    let p = path.as_ref();

    let filename = p
        .file_name()
        .map(|f| f.to_string_lossy().into_owned())
        .unwrap_or_default();

    let directory = p.parent().map(|d| d.to_string_lossy().into_owned()).unwrap_or_default();

    (directory, filename)
}

/// Returns the file name without extension from a given file path.
/// Example: "/home/user/foo.rs" -> "foo"
pub fn file_name_without_extension(file_path: &str) -> Option<String> {
    Path::new(file_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
}
