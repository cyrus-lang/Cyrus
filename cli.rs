use clap::Parser as ClapParser;
use clap::ValueEnum;
use compiler::{options::CompilerOptions, Compiler};
use parser::parse_program;

#[derive(clap::Parser, Clone)]
#[command(author, version, about, long_about = None)]
#[clap(propagate_version = true)]
struct Args {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum DumpType {
    IR, ASM
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Commands {
    Run {
        file_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },
    Dump {
        file_path: String,
        #[clap(long, value_enum, help = "Dump intermediate representation (IR) or assembly code")]
        dump_type: DumpType,
        output_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },
    Build {
        file_path: String,
        output_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },
    Obj {
        file_path: String,
        output_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },
    Dylib {
        file_path: String,
        output_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },
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
        Commands::Run {
            file_path,
            compiler_options,
        } => {
            let mut compiler = init_compiler!(file_path.clone());
            compiler.set_opts(compiler_options);
            compiler.execute();
        }
        Commands::Dump { file_path, dump_type, output_path, compiler_options  } => {
            let mut compiler = init_compiler!(file_path.clone());
            compiler.set_opts(compiler_options);

            match dump_type {
                DumpType::IR => compiler.make_dump_ir(output_path),
                DumpType::ASM => compiler.make_dump_asm(output_path),
            }
        }
        Commands::Build {
            file_path,
            output_path,
            compiler_options,
        } => {
            let mut compiler = init_compiler!(file_path.clone());
            compiler.set_opts(compiler_options);
            compiler.make_executable_file(output_path);
        }
        Commands::Obj {
            file_path,
            output_path,
            compiler_options,
        } => {
            let mut compiler = init_compiler!(file_path.clone());
            compiler.set_opts(compiler_options);
            compiler.make_object_file(output_path);
        }
        Commands::Dylib {
            file_path,
            output_path,
            compiler_options,
        } => {
            let mut compiler = init_compiler!(file_path.clone());
            compiler.set_opts(compiler_options);
            compiler.make_dynamic_library(output_path);
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}
