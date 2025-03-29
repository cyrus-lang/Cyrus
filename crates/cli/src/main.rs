use ::parser::parse_program;
use clap::*;
use codegen_llvm::CodeGenLLVM;
use codegen_llvm::diag::*;
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum OptimizeLevel {
    None,
    O1,
    O2,
    O3,
}

impl OptimizeLevel {
    pub fn as_integer(&self) -> i32 {
        match self {
            OptimizeLevel::None => 0,
            OptimizeLevel::O1 => 1,
            OptimizeLevel::O2 => 2,
            OptimizeLevel::O3 => 3,
        }
    }
}

impl fmt::Display for OptimizeLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptimizeLevel::None => write!(f, "none"),
            OptimizeLevel::O1 => write!(f, "o1"),
            OptimizeLevel::O2 => write!(f, "o2"),
            OptimizeLevel::O3 => write!(f, "o3"),
        }
    }
}

#[derive(Parser, Debug, Clone)]
struct CompilerOptions {
    #[clap(long, value_enum, default_value_t = OptimizeLevel::None, help = "Set optimization level")]
    optimize: OptimizeLevel,

    #[clap(long, value_name = "PATH", help = "Add a library search path")]
    library_path: Vec<String>,

    #[clap(long = "library", value_name = "LIB", help = "Link a library")]
    libraries: Vec<String>,

    #[clap(
        long = "build-dir",
        value_name = "PATH",
        default_value = "./build",
        help = "Specifies the directory where build artifacts will be stored.
This includes compiled binaries, intermediate files, and other outputs 
generated during the build process. If not provided, a default directory \
    (e.g., './build') will be used."
    )]
    build_dir: String,
}

#[derive(clap::Parser, Clone)]
#[command()]
struct Args {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum DumpType {
    Ir,
    Asm,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Commands {
    #[clap(about = "Create a new project")]
    New {
        project_name: String,
        #[clap(long, default_value_t = false)]
        lib: bool,
    },

    #[clap(about = "Execute a compiled program")]
    Run {
        file_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Dump LLVM-ir or Assembly code")]
    Dump {
        file_path: Option<String>,
        dump_type: DumpType,
        output_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Compile source code into an executable")]
    Build {
        file_path: Option<String>,
        output_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Generate an object file")]
    Obj {
        file_path: Option<String>,
        output_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Generate a dynamic library (shared object)")]
    Dylib {
        file_path: Option<String>,
        output_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Print version information")]
    Version,
}

macro_rules! init_compiler {
    ($context:expr, $file_path:expr, $opts:expr) => {{
        if let Some(file_path) = $file_path {
            if !file_path.ends_with(".cyr") {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Invalid file extension.".to_string()),
                    location: None,
                });
                std::process::exit(1);
            }

            let (program, file_name) = parse_program(file_path.clone());
            let codegen_llvm = match CodeGenLLVM::new($context, file_path, file_name.clone(), program, $opts) {
                Ok(instance) => instance,
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(format!("Creating CodeGenLLVM instance failed:{}", err.to_string())),
                        location: None,
                    });
                    std::process::exit(1);
                }
            };
            codegen_llvm
        } else {
            if !std::path::Path::new("Project.toml").exists() {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("'Project.toml' not found in current directory.".to_string()),
                    location: None,
                });
                std::process::exit(1);
            }

            if let Ok(options) = codegen_llvm::opts::Options::read_toml("Project.toml".to_string()) {
                if !std::path::Path::new("src/main.cyr").exists() {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("'src/main.cyr' file not found.".to_string()),
                        location: None,
                    });
                    std::process::exit(1);
                }

                let main_file_path = std::path::Path::new("src/main.cyr")
                    .canonicalize()
                    .unwrap_or_else(|_| {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom("Failed to get absolute path for 'src/main.cyr'.".to_string()),
                            location: None,
                        });
                        std::process::exit(1);
                    })
                    .to_str()
                    .unwrap()
                    .to_string();

                let (program, file_name) = parse_program(main_file_path.clone());
                let codegen_llvm = match CodeGenLLVM::new($context, main_file_path, file_name.clone(), program, options)
                {
                    Ok(instance) => instance,
                    Err(err) => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(format!("Creating CodeGenLLVM instance failed:{}", err.to_string())),
                            location: None,
                        });
                        std::process::exit(1);
                    }
                };

                codegen_llvm
            } else {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("'Project.toml' contains an invalid configuration.".to_string()),
                    location: None,
                });
                std::process::exit(1);
            }
        }
    }};
}

pub fn main() {
    let context = CodeGenLLVM::new_context();
    let version = env!("CARGO_PKG_VERSION");
    let args = Args::parse();

    match args.cmd {
        Commands::New { project_name, lib } => {
            if lib {
                if let Err(err) = layout::create_library_project(project_name) {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err),
                        location: None,
                    });
                    std::process::exit(1);
                }
            } else {
                if let Err(err) = layout::create_project(project_name) {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err),
                        location: None,
                    });
                    std::process::exit(1);
                }
            }
        }
        Commands::Run {
            file_path,
            compiler_options,
        } => {
            let mut codegen_llvm = init_compiler!(
                &context,
                file_path.clone(),
                codegen_llvm::opts::Options {
                    opt_level: compiler_options.optimize.as_integer(),
                    library_path: compiler_options.library_path,
                    libraries: compiler_options.libraries,
                    build_dir: compiler_options.build_dir,
                }
            );
            codegen_llvm.compile();
            codegen_llvm.execute();
        }
        Commands::Dump {
            file_path,
            dump_type,
            output_path,
            compiler_options,
        } => {
            let mut codegen_llvm = init_compiler!(
                &context,
                file_path.clone(),
                codegen_llvm::opts::Options {
                    opt_level: compiler_options.optimize.as_integer(),
                    library_path: compiler_options.library_path,
                    libraries: compiler_options.libraries,
                    build_dir: compiler_options.build_dir,
                }
            );
            codegen_llvm.compile();

            match dump_type {
                DumpType::Ir => codegen_llvm.emit_llvm_ir(output_path),
                DumpType::Asm => codegen_llvm.emit_asm(output_path),
            }
        }
        Commands::Build {
            file_path,
            output_path,
            compiler_options,
        } => {
            let mut codegen_llvm = init_compiler!(
                &context,
                file_path.clone(),
                codegen_llvm::opts::Options {
                    opt_level: compiler_options.optimize.as_integer(),
                    library_path: compiler_options.library_path,
                    libraries: compiler_options.libraries,
                    build_dir: compiler_options.build_dir,
                }
            );
            // compiler.compile();
            // compiler.make_executable_file(output_path);
        }
        Commands::Obj {
            file_path,
            output_path,
            compiler_options,
        } => {
            // let mut compiler = init_compiler!(file_path.clone());
            // compiler.set_opts(CompilerOptions {
            //     opt_level: compiler_options.optimization_level.as_integer(),
            //     library_path: compiler_options.library_path,
            //     libraries: compiler_options.libraries,
            //     build_dir: compiler_options.build_dir,
            // });
            // compiler.compile();
            // compiler.make_object_file(output_path);
        }
        Commands::Dylib {
            file_path,
            output_path,
            compiler_options,
        } => {
            // let mut compiler = init_compiler!(file_path.clone());
            // compiler.set_opts(CompilerOptions {
            //     opt_level: compiler_options.optimization_level.as_integer(),
            //     library_path: compiler_options.library_path,
            //     libraries: compiler_options.libraries,
            //     build_dir: compiler_options.build_dir,
            // });
            // compiler.compile();
            // compiler.make_dynamic_library(output_path);
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}
