use ast::ast::{Node, Program};
use clap::Parser as ClapParser;
use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use utils::{compiler_error, fs::read_file};

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
    Version,
}

fn parse_program(file_path: String) -> Program {
    let file = read_file(file_path);
    let code = file.0;

    let mut lexer = Lexer::new(code);
    let mut parser = Parser::new(&mut lexer);

    let program = match parser.parse() {
        Ok(result) => {
            if let Node::Program(program) = result {
                program
            } else {
                compiler_error!("Expected a program given as input to the compiler but got unknown.");
            }
        }
        Err(errors) => {
            for error in errors {
                println!("{}", error);
            }

            std::process::exit(1);
        }
    };

    program
}

macro_rules! init_compiler {
    ($file_path:expr) => {{
        let program = parse_program($file_path);
        let mut compiler = Compiler::new(program);
        compiler.compile();
        #[cfg(debug_assertions)]
        compiler.set_debug_info(true);
        compiler
    }};
}

pub fn main() {
    let version = env!("CARGO_PKG_VERSION");
    let args = Args::parse();

    match args.cmd {
        Commands::Run { file_path } => {
            let compiler = init_compiler!(file_path);
            compiler.execute();
        }
        Commands::Dump { file_path, output_path } => {
            let compiler = init_compiler!(file_path);
            compiler.make_dump_file(output_path);
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}
