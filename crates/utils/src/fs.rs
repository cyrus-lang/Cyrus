use std::{fs::{self, File}, io::Read, path::{Path, PathBuf}, process::exit};
use colorized::{Color, Colors};

// Reads the file and returns the file content and the name of the file.
pub fn read_file(file_path: String) -> (String, String) {
    let path = Path::new(file_path.as_str());

    let mut file = match File::open(path) {
        Ok(content) => content,
        Err(_) => {
            eprintln!("{}: No such file or directory.", "Error".color(Colors::RedFg));
            exit(1);
        }
    };

    let mut contents = String::new();

    match file.read_to_string(&mut contents) {
        Err(err) => {
            eprintln!("{}: Failed to read the file content: {}", "Error".color(Colors::RedFg), err.to_string()); 
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
            eprintln!("{}: Failed to create output directory: {}", 
                "Error".color(Colors::RedFg), 
                output_dir.display());
            exit(1);
        });
    } else if !output_dir.is_dir() {
        eprintln!("{}: Output path must be a directory: {}", 
            "Error".color(Colors::RedFg), 
            output_dir.display());
        exit(1);
    }
}

pub fn get_output_file_path(output_dir: &Path, source_file: &Path) -> PathBuf {
    output_dir.join(
        source_file.file_stem()
            .unwrap_or_else(|| {
                eprintln!("{}: Invalid source file name: {}", 
                    "Error".color(Colors::RedFg), 
                    source_file.display());
                exit(1);
            })
            .to_str()
            .unwrap()
    ).with_extension("o")
}

pub fn handle_file_generation_error(err: impl ToString, file_path: &Path) -> ! {
    eprintln!("{}: Failed to generate object file {}: {}", 
        "Error".color(Colors::RedFg), 
        file_path.display(),
        err.to_string());
    exit(1);
}

/// Converts an absolute path to a relative path based on the given base directory.
/// Returns `None` if the path is not a child of the base directory.
pub fn absolute_to_relative(absolute_path: String, base_dir: String) -> Option<String> {
    let abs_path = Path::new(&absolute_path).canonicalize().ok()?;
    let base_path = Path::new(&base_dir).canonicalize().ok()?;

    let relative_path = abs_path.strip_prefix(base_path).ok()?;

    Some(relative_path.to_string_lossy().replace('\\', "/"))
}