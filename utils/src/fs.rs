use std::{fs::File, io::Read, path::Path, process::exit};

// Reads the file and returns the file content and the name of the file.
pub fn read_file(file_path: String) -> (String, String) {
    let path = Path::new(file_path.as_str());

    let mut file = match File::open(path) {
        Ok(content) => content,
        Err(_) => {
            println!("(compiler) cyrus: No such file or directory. -- {}", file_path);
            exit(1);
        }
    };

    let mut contents = String::new();

    match file.read_to_string(&mut contents) {
        Err(err) => {
            println!("Failed to read the file content: {}", err);
            exit(1);
        }
        _ => {}
    }

    let file_name = path.file_name().unwrap().to_str().unwrap();

    (contents, file_name.to_string())
}
