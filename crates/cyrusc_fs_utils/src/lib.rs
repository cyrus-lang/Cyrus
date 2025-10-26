use std::{
    fs::{self, File},
    io::Read,
    path::{Path, PathBuf},
    process::exit,
};
use cyrusc_tui_utils::tui_error;

/// Reads a file from the given path and returns its contents along with the file name.
///
/// # Behavior
/// - On success: returns `(file_contents, file_name)`.
/// - On failure (e.g. file not found or unreadable): prints a TUI error and exits the process.
pub fn read_file(file_path: String) -> (String, String) {
    let path = Path::new(&file_path);

    // Try opening the file; terminate if it fails.
    let mut file = File::open(path).unwrap_or_else(|_| {
        tui_error("No such file or directory.".to_string());
        exit(1);
    });

    // Read file content into a String buffer.
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap_or_else(|err| {
        tui_error(format!("Failed to read file content: {}", err));
        exit(1);
    });

    // Extract file name from the given path.
    let file_name = path.file_name().and_then(|name| name.to_str()).unwrap_or_else(|| {
        tui_error("Failed to extract file name.".to_string());
        exit(1);
    });

    (contents, file_name.to_string())
}

/// Ensures that the given path exists and is a valid directory.
///
/// # Behavior
/// - If the directory does not exist: attempts to create it.
/// - If a non-directory file exists at that path: terminates with an error.
/// - On failure to create directory: prints a TUI error and exits.
pub fn ensure_output_dir(output_dir: String) {
    let output_dir = Path::new(&output_dir);

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

/// Converts an absolute path to a relative path, using the given base directory as a reference.
///
/// Returns `None` if:
/// - either path cannot be canonicalized, or
/// - the absolute path is not a descendant of the base directory.
///
/// Normalizes path separators to `/` for consistency across platforms.
pub fn absolute_to_relative(absolute_path: String, base_dir: String) -> Option<String> {
    let abs_path = Path::new(&absolute_path).canonicalize().ok()?;
    let base_path = Path::new(&base_dir).canonicalize().ok()?;
    let relative_path = abs_path.strip_prefix(base_path).ok()?;
    Some(relative_path.to_string_lossy().replace('\\', "/"))
}

/// Converts a relative path to an absolute path using the given base directory.
///
/// Returns `None` if canonicalization fails (e.g., the path doesn’t exist).
///
/// Normalizes path separators to `/`.
pub fn relative_to_absolute(relative_path: String, base_dir: String) -> Option<String> {
    let base_path = Path::new(&base_dir);
    let combined = base_path.join(&relative_path);
    let absolute = combined.canonicalize().ok()?;
    Some(absolute.to_string_lossy().replace('\\', "/"))
}

/// Searches for a file or directory (`file_name`) across multiple source directories.
///
/// Returns the first full path found, or `None` if not found.
pub fn find_file_from_sources(file_name: String, sources: Vec<String>) -> Option<PathBuf> {
    for source in sources {
        let path = Path::new(&source).join(&file_name);
        if path.exists() && (path.is_file() || path.is_dir()) {
            return Some(path);
        }
    }
    None
}

/// Returns the parent directory of a given file path, if it exists.
pub fn get_directory_of_file(file_path: String) -> Option<String> {
    Path::new(&file_path)
        .parent()
        .and_then(|p| p.to_str())
        .map(|s| s.to_string())
}

/// Returns the file stem (filename without extension) as a string slice.
///
/// Example: `"foo.rs"` → `"foo"`.
pub fn file_stem(file_name: &str) -> Option<&str> {
    Path::new(file_name).file_stem().and_then(|s| s.to_str())
}

/// Returns the platform-specific dynamic library extension.
///
/// - Windows → `"dll"`
/// - macOS → `"dylib"`
/// - Linux/Unix → `"so"`
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
/// - Windows → `".exe"`
/// - Others → `""`
pub fn executable_extension() -> &'static str {
    if cfg!(target_os = "windows") { ".exe" } else { "" }
}

/// Splits a file path into directory and filename components.
///
/// Returns `(directory, filename)` as `String`s.
/// Missing parts (e.g. if path has no parent or no file name) become empty strings.
///
/// Example:  
/// `/home/user/file.txt` → `("/home/user", "file.txt")`
pub fn split_paths(path: &str) -> (String, String) {
    let path = Path::new(path);

    let filename = path
        .file_name()
        .map(|f| f.to_string_lossy().to_string())
        .unwrap_or_default();

    let directory = path
        .parent()
        .map(|d| d.to_string_lossy().to_string())
        .unwrap_or_default();

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
