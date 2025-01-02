use std::{fs::File, io::Read, path::Path};

use crate::compiler_error;

// Reads the file and returns the file content and the name of the file.
pub fn read_file(file_path: String) -> (String, String) {
    let path = Path::new(file_path.as_str());

    let mut file = match File::open(path) {
        Ok(content) => content,
        Err(_) => {
            compiler_error!(format!("No such file or directory. -- {}", file_path));
        }
    };

    let mut contents = String::new();

    match file.read_to_string(&mut contents) {
        Err(_) => {
            compiler_error!("Failed to read the file content.");
        }
        _ => {}
    }

    let file_name = path.file_name().unwrap().to_str().unwrap();

    (contents, file_name.to_string())
}
