use std::fs;

use clap::Parser;
use cyrusc::*;
use parser::Parser as CyrusParser;

#[derive(clap::Parser)]
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
            let content = fs::read_to_string(file_path.clone()).expect(format!("cyrus: No such file or directory. -- {}", file_path).as_str());
            compile_program(content);
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}

pub fn compile_program(code: String) {
    let node = CyrusParser::parse(code).unwrap();
    
    unsafe {
        // TODO - Add module name handling
        match compile(node, "sample\0") {
            Ok(result) => print_llvm_module(result.0),
            Err(err) => {
                println!("cyrus: (error) {}", err);
            },
        }
    }
}
