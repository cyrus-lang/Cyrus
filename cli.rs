use clap::Parser as ClapParser;
use compiler::Compiler;
use parser::parse_program;

#[derive(clap::Parser, Clone)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Commands {
    Run { file_path: String },
    Dump { file_path: String, output_path: String },
    Build { file_path: String, output_path: String },
    Version,
}

macro_rules! init_compiler {
    ($file_path:expr) => {{
        let (program, file_name) = parse_program($file_path);
        let mut compiler = Compiler::new(program, $file_path, file_name);

        #[cfg(debug_assertions)]
        compiler.set_debug_info(true);

        compiler.compile();
        compiler
    }};
}

pub fn main() {
    let version = env!("CARGO_PKG_VERSION");
    let args = Args::parse();

    match args.cmd {
        Commands::Run { file_path } => {
            let compiler = init_compiler!(file_path.clone());
            compiler.execute();
        }
        Commands::Dump { file_path, output_path } => {
            let compiler = init_compiler!(file_path.clone());
            compiler.make_dump_file(output_path);
        }
        Commands::Build { file_path, output_path } => {
            let compiler = init_compiler!(file_path.clone());
            compiler.make_executable_file(output_path);
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}
