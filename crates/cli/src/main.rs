use clap::{Parser, ValueEnum};
use codegen::options::{BuildDir, CodeGenOptions, CodeModelOptions, RelocModeOptions};
use commands::*;
use diagcentral::display_single_custom_diag;
use serde::Deserialize;

mod commands;

const PROJECT_FILE_PATH: &str = "Project.toml";

pub(crate) fn project_file_required() {
    if !std::path::Path::new(PROJECT_FILE_PATH).exists() {
        display_single_custom_diag!(format!("'{}' not found in current directory.", PROJECT_FILE_PATH));
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum OptimizeLevel {
    None,
    O1,
    O2,
    O3,
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
    #[clap(long, default_value_t = String::new(),
        help = "Specify the target triple (e.g., x86_64-pc-linux-gnu, aarch64-apple-darwin). Defaults to host triple if not specified."
    )]
    target_triple: String,

    #[clap(long, default_value_t = String::new(),
        help = "Specify the target CPU name (e.g., skylake, broadwell, generic). Defaults to host CPU if not specified."
    )]
    cpu: String,

    #[clap(long, value_enum, default_value_t = OptimizeLevel::None, help = "Set optimization level.")]
    optimize: OptimizeLevel,

    #[clap(long, value_name = "LIBRARY_PATH", help = "Add a library search path.")]
    library_path: Vec<String>,

    #[clap(long = "library", value_name = "LIB", help = "Link a library.")]
    libraries: Vec<String>,

    #[clap(long = "sources", value_name = "SOURCES", help = "Source files.")]
    source_dirs: Vec<String>,

    #[clap(
        long = "build-dir",
        value_name = "PATH",
        help = "Specifies the directory where build artifacts will be stored."
    )]
    build_dir: Option<String>,

    #[clap(
        long = "base-path",
        value_name = "PATH",
        help = "Specifies the base path for running the command."
    )]
    base_path: Option<String>,

    #[clap(long, short = 'q', help = "Suppress unnecessary output messages.")]
    quiet: bool,

    #[clap(long, help = "Set cyrus standard library path.")]
    stdlib: Option<String>,

    #[clap(long = "target-machine", help = "Display Target Machine information.")]
    display_target_machine: bool,

    #[clap(long, value_enum, default_value_t = RelocMode::Default,
    help = "Set the relocation model for code generation."
    )]
    reloc_mode: RelocMode,

    #[clap(long, value_enum, default_value_t = CodeModel::Default,
    help = "Set the code model for code generation."
    )]
    code_model: CodeModel,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum RelocMode {
    Default,
    Static,
    PIC,
    DynamicNoPic,
}

impl RelocMode {
    pub fn as_compiler_reloc_mode(&self) -> RelocModeOptions {
        match self {
            RelocMode::Default => RelocModeOptions::Default,
            RelocMode::Static => RelocModeOptions::Static,
            RelocMode::PIC => RelocModeOptions::PIC,
            RelocMode::DynamicNoPic => RelocModeOptions::DynamicNoPic,
        }
    }
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum CodeModel {
    Default,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
}

impl CodeModel {
    pub fn as_compiler_code_model(&self) -> CodeModelOptions {
        match self {
            CodeModel::Default => CodeModelOptions::Default,
            CodeModel::Tiny => CodeModelOptions::Tiny,
            CodeModel::Small => CodeModelOptions::Small,
            CodeModel::Kernel => CodeModelOptions::Kernel,
            CodeModel::Medium => CodeModelOptions::Medium,
            CodeModel::Large => CodeModelOptions::Large,
        }
    }
}

impl CompilerOptions {
    pub fn to_compiler_options(&self) -> CodeGenOptions {
        CodeGenOptions {
            base_path: self.base_path.clone(),
            opt_level: match self.optimize {
                OptimizeLevel::None => None,
                OptimizeLevel::O1 => Some(1),
                OptimizeLevel::O2 => Some(2),
                OptimizeLevel::O3 => Some(3),
            },
            library_path: self.library_path.clone(),
            libraries: self.libraries.clone(),
            source_dirs: self.source_dirs.clone(),
            project_name: None,
            project_version: None,
            cyrus_version: None,
            authors: None,
            project_type: None,
            build_dir: {
                match self.build_dir.clone() {
                    Some(path) => BuildDir::Provided(path),
                    None => BuildDir::Default,
                }
            },
            quiet: self.quiet,
            stdlib_path: self.stdlib.clone(),
            display_target_machine: self.display_target_machine,
            reloc_mode: self.reloc_mode.as_compiler_reloc_mode(),
            code_model: self.code_model.as_compiler_code_model(),
            target_triple: {
                if self.target_triple.trim() == "" {
                    None
                } else {
                    Some(self.target_triple.clone())
                }
            },
            cpu: {
                if self.cpu.trim() == "" {
                    None
                } else {
                    Some(self.cpu.to_string())
                }
            },
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

    #[clap(
        name = "emit-bytecode",
        about = "Emit bytecode as a .bc file per module.",
        display_order = 8
    )]
    EmitByteCode {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Lexical analysis only.", display_order = 9)]
    LexOnly { file_path: String },

    #[clap(about = "Display program tree.", display_order = 10)]
    ParseOnly { file_path: String },

    #[clap(about = "Check program correctness syntactically.", display_order = 11)]
    SyntacticOnly { file_path: String },

    #[clap(about = "Print version information", display_order = 12)]
    Version,
}

fn command_new(project_name: String, lib: bool) {
    let result = {
        if lib {
            project_layout::create_library_project(project_name)
        } else {
            project_layout::create_project(project_name)
        }
    };

    if let Err(err) = result {
        display_single_custom_diag!(err);
    }
}

pub fn main() {
    let version = env!("CARGO_PKG_VERSION");
    let args = Args::parse();

    match args.cmd {
        Commands::Fetch { .. } => {
            todo!();
        }
        Commands::New { project_name, lib } => {
            command_new(project_name, lib);
        }
        Commands::Run {
            file_path,
            compiler_options,
        } => {
            command_run(compiler_options, file_path);
        }
        Commands::EmitLLVM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            command_emit_llvm(compiler_options, file_path, output_path);
        }
        Commands::EmitASM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            command_emit_asm(compiler_options, file_path, output_path);
        }
        Commands::EmitByteCode {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            command_emit_bytecode(compiler_options, file_path, output_path);
        }
        Commands::Build {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            command_build(compiler_options, file_path, output_path);
        }
        Commands::Object {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            command_object(compiler_options, file_path, output_path);
        }
        Commands::Dylib {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            command_dylib(compiler_options, file_path, output_path);
        }
        Commands::Version => {
            println!("Cyrus {}", version)
        }
        Commands::LexOnly { file_path } => command_lex_only(file_path),
        Commands::ParseOnly { file_path } => command_parse_only(file_path),
        Commands::SyntacticOnly { file_path } => command_syntactic_only(file_path),
    }
}
