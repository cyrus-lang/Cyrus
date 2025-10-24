use clap::{Parser, ValueEnum};
use codegen_llvm::options::{BuildDir, CodeGenLinkerOptions, CodeGenOptions, CodeGenSanitizer, CodeModelOptions, RelocModeOptions};
use commands::*;
use diagcentral::display_single_custom_diag;
use scaffold::PROJECT_FILE_PATH;
use serde::Deserialize;

mod commands;

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
struct LinkerCompilerOptions {
    /// Produce a fully static ELF
    #[arg(long)]
    pub r#static: bool,

    /// Enable PIE (Position Independent Executable)
    #[arg(long, conflicts_with = "no_pie")]
    pie: bool,

    /// Disable PIE
    #[arg(long, conflicts_with = "pie")]
    no_pie: bool,
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

    #[clap(long = "linker-flags", short = 'z', help = "Adds custom flags to the linker.")]
    linkerflags: Vec<String>,

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

    #[clap(
        long = "linker",
        value_name = "LINKER_NAMe",
        help = "Specifies the linker for linking the object files."
    )]
    linker: Option<String>,

    #[clap(
        long,
        short = 'q',
        help = "Suppress unnecessary output messages.",
        conflicts_with = "verbose"
    )]
    quiet: bool,

    #[clap(long, short = 'V', help = "Increase output verbosity.", conflicts_with = "quiet")]
    verbose: bool,

    #[clap(long, help = "Disables module filesystem cache.")]
    disable_modulefs_cache: bool,

    #[clap(long, help = "Disables all warnings.")]
    disable_warnings: bool,

    #[clap(long, help = "Set cyrus standard library path.")]
    stdlib: Option<String>,

    #[clap(long = "target-machine", help = "Display Target Machine information.")]
    display_target_machine: bool,

    #[clap(long = "sanitize", help = "Enables dynamic code analysis for bug detection.")]
    #[clap(
        value_enum,
        value_delimiter = ','
    )]
    pub sanitizer: Vec<Sanitizer>,

    #[clap(long, value_enum, default_value_t = RelocMode::default(),
    help = "Set the relocation model for code generation."
    )]
    reloc_mode: RelocMode,

    #[clap(long, value_enum, default_value_t = CodeModel::Default,
    help = "Set the code model for code generation."
    )]
    code_model: CodeModel,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum Sanitizer {
    Address,
    Undefined,
    Thread,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum RelocMode {
    Default,
    Static,
    PIC,
    DynamicNoPic,
}

impl Default for RelocMode {
    fn default() -> Self {
        Self::PIC
    }
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

impl LinkerCompilerOptions {
    pub fn to_compiler_linker_options(&self) -> CodeGenLinkerOptions {
        CodeGenLinkerOptions {
            link_static: self.r#static,
            pie: self.pie,
            no_pie: self.no_pie,
        }
    }
}

impl Sanitizer {
    pub fn to_compiler_sanitizer(&self) -> CodeGenSanitizer {
        match self {
            Sanitizer::Address => CodeGenSanitizer::Address,
            Sanitizer::Undefined => CodeGenSanitizer::Undefined,
            Sanitizer::Thread => CodeGenSanitizer::Thread,
        }
    }
}

impl CompilerOptions {
    pub fn to_compiler_options(&self) -> CodeGenOptions {
        CodeGenOptions {
            sanitizer: self.sanitizer.iter().map(|s| s.to_compiler_sanitizer()).collect(),
            linker_flags: self.linkerflags.clone(),
            linker_options: CodeGenLinkerOptions::default(),
            linker: self.linker.clone().or(Some("cc".to_string())),
            disable_modulefs_cache: self.disable_modulefs_cache,
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
            verbose: self.verbose,
            stdlib_path: self.stdlib.clone(),
            display_target_machine: self.display_target_machine,
            disable_warnings: self.disable_warnings,
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

        #[clap(flatten)]
        linker_options: LinkerCompilerOptions,

        #[clap(last = true)]
        program_args: Vec<String>
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

        #[clap(flatten)]
        linker_options: LinkerCompilerOptions,
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

    #[clap(about = "Check program correctness semantically.", display_order = 11)]
    SemanticOnly {
        file_path: String,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },

    #[clap(about = "Print version information", display_order = 12)]
    Version,
}

fn command_new(project_name: String, lib: bool) {
    let result = {
        if lib {
            scaffold::create_library_project(project_name)
        } else {
            scaffold::create_project(project_name)
        }
    };

    if let Err(err) = result {
        display_single_custom_diag!(err);
    }
}

pub fn main() {
    std::panic::set_hook(Box::new(|info| {
        eprintln!("Compiler panic:\n{info}");
    }));

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
            linker_options,
            program_args,
        } => {
            let mut codegen_options = compiler_options.to_compiler_options();
            codegen_options.linker_options = linker_options.to_compiler_linker_options();
            command_run(codegen_options, file_path , program_args);
        }
        Commands::EmitLLVM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let codegen_options = compiler_options.to_compiler_options();

            command_emit_llvm(codegen_options, file_path, output_path);
        }
        Commands::EmitASM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let codegen_options = compiler_options.to_compiler_options();

            command_emit_asm(codegen_options, file_path, output_path);
        }
        Commands::EmitByteCode {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let codegen_options = compiler_options.to_compiler_options();

            command_emit_bytecode(codegen_options, file_path, output_path);
        }
        Commands::Build {
            file_path,
            output_path,
            compiler_options,
            linker_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let mut codegen_options = compiler_options.to_compiler_options();
            codegen_options.linker_options = linker_options.to_compiler_linker_options();
            command_build(codegen_options, file_path, output_path);
        }
        Commands::Object {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let codegen_options = compiler_options.to_compiler_options();

            command_object(codegen_options, file_path, output_path);
        }
        Commands::Dylib {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let codegen_options = compiler_options.to_compiler_options();

            command_dylib(codegen_options, file_path, output_path);
        }
        Commands::SemanticOnly {
            compiler_options,
            file_path,
        } => command_semantic_only(compiler_options, file_path),
        Commands::Version => {
            println!("Cyrus {}", version)
        }
        Commands::LexOnly { file_path } => command_lex_only(file_path),
        Commands::ParseOnly { file_path } => command_parse_only(file_path),
    }
}
