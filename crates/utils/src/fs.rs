use std::{
    fs::{self, File},
    io::Read,
    path::{Path, PathBuf},
    process::exit,
};

use crate::tui::tui_error;

// Reads the file and returns the file content and the name of the file.
pub fn read_file(file_path: String) -> (String, String) {
    let path = Path::new(file_path.as_str());

    let mut file = match File::open(path) {
        Ok(content) => content,
        Err(_) => {
            tui_error("No such file or directory.".to_string());
            exit(1);
        }
    };

    let mut contents = String::new();

    match file.read_to_string(&mut contents) {
        Err(err) => {
            tui_error(format!("Failed to read the file content: {}", err.to_string()));
            exit(1);
        }
        _ => {}
    }

    let file_name = path.file_name().unwrap().to_str().unwrap();

    (contents, file_name.to_string())
}

pub fn ensure_output_dir(output_dir: &Path) {
    if !output_dir.exists() {
        fs::create_dir_all(output_dir).unwrap_or_else(|_| {
            tui_error(format!("Failed to create output directory: {}", output_dir.display()));
            exit(1);
        });
    } else if !output_dir.is_dir() {
        tui_error(format!("Output path must be a directory: {}", output_dir.display()));
        exit(1);
    }
}

/// Converts an absolute path to a relative path based on the given base directory.
/// Returns `None` if the path is not a child of the base directory.
pub fn absolute_to_relative(absolute_path: String, base_dir: String) -> Option<String> {
    let abs_path = Path::new(&absolute_path).canonicalize().ok()?;
    let base_path = Path::new(&base_dir).canonicalize().ok()?;

    let relative_path = abs_path.strip_prefix(base_path).ok()?;

    Some(relative_path.to_string_lossy().replace('\\', "/"))
}

/// Tries to find `file_name` in any of the given `sources` directories.
/// Returns the full path if found, otherwise returns `None`.
pub fn find_file_from_sources(file_name: String, sources: Vec<String>) -> Option<PathBuf> {
    for source in sources.iter() {
        let path = Path::new(source).join(&file_name);
        if path.exists() && (path.is_file() || path.is_dir()) {
            return Some(path);
        }
    }
    None
}

pub fn get_directory_of_file(file_path: String) -> Option<String> {
    let path = Path::new(file_path.as_str());
    path.parent()
        .map(|parent| parent.to_str().unwrap_or_default().to_string())
}

pub fn file_stem(file_name: &str) -> Option<&str> {
    Path::new(file_name)
        .file_stem() // gets "main" from "main.cyr"
        .and_then(|s| s.to_str()) // convert OsStr to &str
}

pub fn dylib_extension() -> &'static str {
    if cfg!(target_os = "windows") {
        "dll"
    } else if cfg!(target_os = "macos") {
        "dylib"
    } else {
        "so"
    }
}

pub fn executable_extension() -> &'static str {
    if cfg!(target_os = "windows") { ".exe" } else { "" }
}
