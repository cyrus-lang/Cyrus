use clap::Parser;
use parser::Parser as CyrusParser;
use std::{fs::File, io::Read, path::Path};

#[derive(clap::Parser, Clone)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Commands {
    Run { file_path: String },
    Version,
}

pub fn main() {
    let version = env!("CARGO_PKG_VERSION");
    let args = Args::parse();

    match args.cmd {
        Commands::Run { file_path } => {
            // let file = read_file(file_path);
            // let code = file.0;
            // let file_name = file.1;

            // let node = CyrusParser::parse(code).unwrap();

            // let result = compile(node, file_name.as_str());
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}

// Reads the file and returns the file content and the name of the file.
fn read_file(file_path: String) -> (String, String) {
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
