use ::parser::parse_program;
use clap::*;
use codegen_llvm::CodeGenLLVM;
use codegen_llvm::diag::*;
use std::process::exit;

const PROJECT_FILE_PATH: &str = "Project.toml";

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum CpuValue {
    None,
    // TODO
}

impl std::fmt::Display for CpuValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CpuValue::None => write!(f, "none"),
        }
    }
}

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

impl std::fmt::Display for OptimizeLevel {
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
    #[clap(long, value_enum, default_value_t = CpuValue::None, help = "Set CPU name")]
    cpu: CpuValue,

    #[clap(long, value_enum, default_value_t = OptimizeLevel::None, help = "Set optimization level")]
    optimize: OptimizeLevel,

    #[clap(long, value_name = "LIBRARY_PATH", help = "Add a library search path")]
    library_path: Vec<String>,

    #[clap(long = "library", value_name = "LIB", help = "Link a library")]
    libraries: Vec<String>,

    #[clap(long = "sources", value_name = "SOURCES", help = "Source files")]
    sources_dir: Vec<String>,

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

impl CompilerOptions {
    pub fn to_compiler_options(&self) -> codegen_llvm::opts::Options {
        codegen_llvm::opts::Options {
            opt_level: self.optimize.as_integer(),
            cpu: self.cpu.to_string(),
            library_path: self.library_path.clone(),
            libraries: self.libraries.clone(),
            build_dir: self.build_dir.clone(),
            sources_dir: self.sources_dir.clone(),
            project_name: None,
            project_version: None,
            cyrus_version: None,
            authors: None,
            project_type: None,
        }
    }
}

#[derive(clap::Parser, Clone)]
#[command()]
struct Args {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(clap::Subcommand, Debug, Clone)]
enum Commands {
    #[clap(about = "Create a new project", display_order = 0)]
    New {
        project_name: String,
        #[clap(long, default_value_t = false)]
        lib: bool,
    },

    #[clap(about = "Execute a compiled program", display_order = 1)]
    Run {
        file_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Fetches a library into vendor directory.", display_order = 2)]
    Fetch { libraries: String },

    #[clap(about = "Compile source code into an executable", display_order = 3)]
    Build {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Generate an object file", display_order = 4)]
    Object {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Generate a dynamic library (shared object)", display_order = 5)]
    Dylib {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Emit LLVM IR as a .ll file per module.", display_order = 6)]
    EmitLLVM {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Emit asm as a .s file per module.", display_order = 7)]
    EmitASM {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Print version information", display_order = 8)]
    Version,
}

fn project_file_required() {
    if !std::path::Path::new(PROJECT_FILE_PATH).exists() {
        display_single_diag(Diag {
            level: DiagLevel::Error,
            kind: DiagKind::Custom(format!("'{}' not found in current directory.", PROJECT_FILE_PATH)),
            location: None,
        });
        std::process::exit(1);
    }
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
            let codegen_llvm = match CodeGenLLVM::new($context, file_path, file_name.clone(), program, $opts, true) {
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
            project_file_required();

            match codegen_llvm::opts::Options::read_toml(PROJECT_FILE_PATH.to_string()) {
                Ok(options) => {
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
                    let codegen_llvm =
                        match CodeGenLLVM::new($context, main_file_path, file_name.clone(), program, options, false) {
                            Ok(instance) => instance,
                            Err(err) => {
                                display_single_diag(Diag {
                                    level: DiagLevel::Error,
                                    kind: DiagKind::Custom(format!(
                                        "Creating CodeGenLLVM instance failed:{}",
                                        err.to_string()
                                    )),
                                    location: None,
                                });
                                std::process::exit(1);
                            }
                        };

                    codegen_llvm
                }
                Err(err) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom(err.to_string()),
                        location: None,
                    });
                    std::process::exit(1);
                }
            }
        }
    }};
}

pub fn main() {
    let context = CodeGenLLVM::new_context();
    let version = env!("CARGO_PKG_VERSION");
    let args = Args::parse();

    match args.cmd {
        Commands::Fetch { .. } => {
            todo!();
        }
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
            if file_path.is_none() {
                project_file_required();
            }

            let mut codegen_llvm = init_compiler!(&context, file_path.clone(), compiler_options.to_compiler_options());
            codegen_llvm.compile();
            codegen_llvm.execute();
        }
        Commands::EmitLLVM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen_llvm = init_compiler!(&context, file_path.clone(), compiler_options.to_compiler_options());
            codegen_llvm.compile();
            codegen_llvm.emit_llvm_ir(output_path);
        }
        Commands::EmitASM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen_llvm = init_compiler!(&context, file_path.clone(), compiler_options.to_compiler_options());
            codegen_llvm.compile();
            codegen_llvm.emit_asm(output_path);
        }
        Commands::Build {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen_llvm = init_compiler!(&context, file_path.clone(), compiler_options.to_compiler_options());
            codegen_llvm.compile();
            codegen_llvm.generate_executable_file(output_path);
        }
        Commands::Object {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen_llvm = init_compiler!(&context, file_path.clone(), compiler_options.to_compiler_options());
            codegen_llvm.compile();
            codegen_llvm.generate_object_file(output_path);
        }
        Commands::Dylib {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen_llvm = init_compiler!(&context, file_path.clone(), compiler_options.to_compiler_options());
            codegen_llvm.compile();
            codegen_llvm.generate_dynamic_library(output_path);
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
    }
}
