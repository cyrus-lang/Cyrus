/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
use std::{
    env,
    path::{Path, PathBuf},
    process::exit,
};

use clap::{Parser, ValueEnum};
use commands::*;
use cyrusc_compiler::options::{
    BuildDir, CodeGenABI, CodeGenEndianness, CodeGenLinkerOptions, CodeGenOptions, CodeGenSanitizer, CodeModelOptions,
    ModuleKind, RelocModeOptions,
};
use cyrusc_diagcentral::display_single_custom_diag;
use cyrusc_scaffold_parser::{PROJECT_FILE_PATH, ScaffoldConfig, parse_project_toml};
use serde::Deserialize;

use crate::version::CYRUS_COMPILER_VERSION;

mod commands;
mod temp_executable_builder;
mod version;

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

    #[clap(short = 'L', long, value_name = "LIBRARY_PATH", help = "Add a library search path.")]
    library_path: Vec<String>,

    #[clap(short = 'l', long = "library", value_name = "LIB", help = "Link a library.")]
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
    #[clap(value_enum, value_delimiter = ',')]
    pub sanitizer: Vec<Sanitizer>,

    #[clap(long, value_enum, default_value_t = RelocMode::default(),
    help = "Set the relocation model for code generation."
    )]
    reloc_mode: RelocMode,

    #[clap(long, value_enum, default_value_t = CodeModel::Default,
    help = "Set the code model for code generation."
    )]
    code_model: CodeModel,

    #[clap(long, value_enum, default_value_t = ABI::Cyrus,
    help =
    "Select the ABI name mangling scheme for code generation. \
Choices determine how function, global variable, and type names \
are represented in the generated output. For example, 'C' produces \
C-compatible names, while 'Cyrus' uses the compiler's default mangling."
    )]
    abi: ABI,

    #[clap(long, value_enum, help = "Set endianness (default uses target machine endianness).")]
    pub endianness: Option<Endianness>,

    #[clap(long, help = "Number of threads to use for compilation.")]
    pub jobs: Option<usize>,

    /// Module merge mode: `unified` for serial, `separate` for parallel
    #[clap(
        long,
        value_enum,
        help = "Module merge mode: 'unified' for serial, 'separate' for parallel."
    )]
    pub module_merge_mode: Option<ModuleMergeMode>,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, ValueEnum)]
pub enum Endianness {
    Little,
    Big,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, ValueEnum)]
pub enum ModuleMergeMode {
    Unified,
    Separate,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum Sanitizer {
    Address,
    Memory,
    Thread,
    HWAddress,
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

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum ABI {
    Cyrus,
    C,
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
            Sanitizer::Memory => CodeGenSanitizer::Memory,
            Sanitizer::Thread => CodeGenSanitizer::Thread,
            Sanitizer::HWAddress => CodeGenSanitizer::HWAddress,
        }
    }
}

impl ABI {
    pub fn to_compiler_abi(&self) -> CodeGenABI {
        match self {
            ABI::Cyrus => CodeGenABI::Cyrus,
            ABI::C => CodeGenABI::C,
        }
    }
}

fn get_current_dir_as_base_path() -> String {
    let current_dir: PathBuf = env::current_dir().unwrap_or_default();
    current_dir.to_string_lossy().into_owned()
}

impl CompilerOptions {
    pub fn as_codegen_options(&self) -> CodeGenOptions {
        CodeGenOptions {
            abi: Some(self.abi.to_compiler_abi()),
            endianness: self.endianness.and_then(|endianness| match endianness {
                Endianness::Little => Some(CodeGenEndianness::Little),
                Endianness::Big => Some(CodeGenEndianness::Big),
            }),
            module_kind: self
                .module_merge_mode
                .and_then(|module_merge_mode| match module_merge_mode {
                    ModuleMergeMode::Unified => Some(ModuleKind::Unified),
                    ModuleMergeMode::Separate => Some(ModuleKind::Separate),
                }),
            jobs: self.jobs,
            sanitizer: self.sanitizer.iter().map(|s| s.to_compiler_sanitizer()).collect(),
            linker_flags: self.linkerflags.clone(),
            linker_options: CodeGenLinkerOptions::default(),
            linker: self.linker.clone(),
            disable_modulefs_cache: self.disable_modulefs_cache,
            base_path: Some(self.base_path.clone().or(Some(get_current_dir_as_base_path())).unwrap()),
            opt_level: match self.optimize {
                OptimizeLevel::None => None,
                OptimizeLevel::O1 => Some(1),
                OptimizeLevel::O2 => Some(2),
                OptimizeLevel::O3 => Some(3),
            },
            library_paths: self.library_path.clone(),
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
            target_triple: if self.target_triple.trim().is_empty() {
                None
            } else {
                Some(self.target_triple.clone())
            },
            cpu: if self.cpu.trim().is_empty() {
                None
            } else {
                Some(self.cpu.clone())
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
        program_args: Vec<String>,
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
    SharedLib {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CompilerOptions,
    },
    #[clap(about = "Generate a static library", display_order = 5)]
    StaticLib {
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
        name = "emit-bitcode",
        about = "Emit bitcode as a .bc file per module.",
        display_order = 8
    )]
    EmitBitcode {
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
            cyrusc_scaffold::create_library_project(project_name)
        } else {
            cyrusc_scaffold::create_project(project_name)
        }
    };

    if let Err(err) = result {
        display_single_custom_diag!(err);
    }
}

fn compiler_option_from_scaffold_parser(base_path: Option<String>) -> Option<ScaffoldConfig> {
    let base_path = base_path.map(|path| Path::new(&path).to_path_buf()).unwrap_or_default();

    // resolve project file path
    let project_file = env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(&base_path)
        .join(PROJECT_FILE_PATH);

    if !project_file.exists() {
        return None;
    }

    match parse_project_toml(project_file) {
        Ok(scaffold_config) => Some(scaffold_config),
        Err(err) => {
            display_single_custom_diag!(format!("Scaffold Parse Error: {}", err.to_string()));
        }
    }
}

pub fn merge_scaffold_config_with_codegen_options(
    opts: &mut CodeGenOptions,
    scaffold_config_opt: &Option<ScaffoldConfig>,
) {
    let Some(scaffold_config) = scaffold_config_opt else {
        return;
    };

    let scaffold_codegen_options = CodeGenOptions::from_scaffold(scaffold_config);
    *opts = opts.merge(&scaffold_codegen_options);

    CodeGenOptions::validate_compiler_version(CYRUS_COMPILER_VERSION.trim(), scaffold_codegen_options.cyrus_version);

    if opts.validate_paths().is_err() {
        exit(1);
    }
}

pub fn main() {
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
            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            codegen_options.linker_options = linker_options.to_compiler_linker_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_run(codegen_options, file_path, program_args);
        }
        Commands::EmitLLVM {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }
            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

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

            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_emit_asm(codegen_options, file_path, output_path);
        }
        Commands::EmitBitcode {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_emit_bitcode(codegen_options, file_path, output_path);
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

            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            codegen_options.linker_options = linker_options.to_compiler_linker_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

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

            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_object(codegen_options, file_path, output_path);
        }
        Commands::SharedLib {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_shared_lib(codegen_options, file_path, output_path);
        }
        Commands::StaticLib {
            file_path,
            output_path,
            compiler_options,
        } => {
            if file_path.is_none() && output_path.is_none() {
                project_file_required();
            }

            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_static_lib(codegen_options, file_path, output_path);
        }
        Commands::SemanticOnly {
            compiler_options,
            file_path,
        } => {
            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_codegen_options();
            merge_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_semantic_only(codegen_options, file_path)
        }
        Commands::Version => {
            println!("Cyrus {}", CYRUS_COMPILER_VERSION)
        }
        Commands::LexOnly { file_path } => command_lex_only(file_path),
        Commands::ParseOnly { file_path } => command_parse_only(file_path),
    }
}
