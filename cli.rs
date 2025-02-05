use ::parser::parse_program;
use clap::Parser as ClapParser;
use clap::*;
use compiler::options::CompilerOptions;
use compiler::Compiler;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum ClapOptimizationLevel {
    None,
    O1,
    O2,
    O3,
}

impl ClapOptimizationLevel {
    pub fn as_integer(&self) -> i32 {
        match self {
            ClapOptimizationLevel::None => 0,
            ClapOptimizationLevel::O1 => 1,
            ClapOptimizationLevel::O2 => 2,
            ClapOptimizationLevel::O3 => 3,
        }
    }
}

#[derive(Parser, Debug, Clone)]
pub struct ClapCompilerOptions {
    #[clap(long, value_enum, default_value_t = ClapOptimizationLevel::None, help = "Set optimization level")]
    optimization_level: ClapOptimizationLevel,

    #[clap(long, value_name = "PATH", help = "Add a library search path")]
    library_path: Vec<String>,

    #[clap(long = "library", value_name = "LIB", help = "Link a library")]
    libraries: Vec<String>,
}

#[derive(clap::Parser, Clone)]
#[command()]
struct Args {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum DumpType {
    IR,
    ASM,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Commands {
    #[clap(about = "Execute a compiled program")]
    Run {
        file_path: String,
        #[clap(flatten)]
        compiler_options: ClapCompilerOptions,
    },

    #[clap(about = "Dump intermediate representation (IR) or assembly code")]
    Dump {
        file_path: String,
        #[clap(long, value_enum, help = "Dump intermediate representation (IR) or assembly code")]
        dump_type: DumpType,
        output_path: String,
        #[clap(flatten)]
        compiler_options: ClapCompilerOptions,
    },

    #[clap(about = "Compile source code into an executable")]
    Build {
        file_path: String,
        output_path: String,
        #[clap(flatten)]
        compiler_options: ClapCompilerOptions,
    },

    #[clap(about = "Generate an object file")]
    Obj {
        file_path: String,
        output_path: String,
        #[clap(flatten)]
        compiler_options: ClapCompilerOptions,
    },

    #[clap(about = "Generate a dynamic library (shared object)")]
    Dylib {
        file_path: String,
        output_path: String,
        #[clap(flatten)]
        compiler_options: ClapCompilerOptions,
    },

    #[clap(about = "Print version information")]
    Version,
}

macro_rules! init_compiler {
    ($file_path:expr) => {{
        let (program, file_name) = parse_program($file_path);
        let context = Compiler::new_master_context();
        let mut compiler = Compiler::new(context, program, $file_path, file_name);

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
            compiler.set_opts(CompilerOptions {
                optimization_level: compiler_options.optimization_level.as_integer(),
                library_path: compiler_options.library_path,
                libraries: compiler_options.libraries,
            });
            compiler.execute();
        }
        Commands::Dump {
            file_path,
            dump_type,
            output_path,
            compiler_options,
        } => {
            let mut compiler = init_compiler!(file_path.clone());
            compiler.set_opts(CompilerOptions {
                optimization_level: compiler_options.optimization_level.as_integer(),
                library_path: compiler_options.library_path,
                libraries: compiler_options.libraries,
            });

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
            compiler.set_opts(CompilerOptions {
                optimization_level: compiler_options.optimization_level.as_integer(),
                library_path: compiler_options.library_path,
                libraries: compiler_options.libraries,
            });
            compiler.make_executable_file(output_path);
        }
        Commands::Obj {
            file_path,
            output_path,
            compiler_options,
        } => {
            let mut compiler = init_compiler!(file_path.clone());
            compiler.set_opts(CompilerOptions {
                optimization_level: compiler_options.optimization_level.as_integer(),
                library_path: compiler_options.library_path,
                libraries: compiler_options.libraries,
            });
            compiler.make_object_file(output_path);
        }
        Commands::Dylib {
            file_path,
            output_path,
            compiler_options,
        } => {
            let mut compiler = init_compiler!(file_path.clone());
            compiler.set_opts(CompilerOptions {
                optimization_level: compiler_options.optimization_level.as_integer(),
                library_path: compiler_options.library_path,
                libraries: compiler_options.libraries,
            });
            compiler.make_dynamic_library(output_path);
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}
