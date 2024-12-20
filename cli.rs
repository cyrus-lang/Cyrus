use clap::Parser;
use cyrusc::*;
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
    LLVM { file_path: String },
    Version,
}

pub fn main() {
    let version = env!("CARGO_PKG_VERSION");
    let args = Args::parse();

    match args.cmd {
        Commands::Run { file_path } => {
            let file = read_file(file_path);
            let code = file.0;
            let file_name = file.1;

            let node = CyrusParser::parse(code).unwrap();

            unsafe {
                let result = compile(node, format!("{}\0", file_name).as_str());

                compile_native(result.0);
            }
        }
        Commands::LLVM { file_path } => {
            let file = read_file(file_path);
            let code = file.0;
            let file_name = file.1;

            let node = CyrusParser::parse(code).unwrap();

            unsafe {
                let result = compile(node, format!("{}\0", file_name).as_str());

                print_llvm_module(result.0);
            }
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}

fn read_file(file_path: String) -> (String, String) {
    let path = Path::new(file_path.as_str());

    let mut file = match File::open(path) {
        Ok(content) => content,
        Err(_) => {
            compiler_error!(format!("No such file or directory. -- {}", file_path));
        }
    };

    let mut contents = String::new();

    file.read_to_string(&mut contents)
        .expect(format!("cyrus: Failed to read the file content").as_str());

    let file_name = path.file_name().unwrap().to_str().unwrap();

    (contents, file_name.to_string())
}
