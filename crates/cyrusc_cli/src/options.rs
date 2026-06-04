// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::enums::{CliABIOption, CliCodeModelOption, CliModuleMergeModeOption, CliOptimizeLevelOption, CliProfileOption, CliRelocModeOption, CliSanitizerOption};
use clap::Parser;
use std::env;

#[derive(Parser, Debug, Clone)]
pub(crate) struct CliCompilerOptions {
    #[clap(
        long,
        default_value_t = String::new(),
        help = "Compile for a specific architecture and operating system target (e.g., x86_64-pc-linux-gnu). Defaults to the host target if not specified."
    )]
    pub target: String,

    #[clap(long, default_value_t = String::new(),
        help = "Specify the target CPU name (e.g., skylake, broadwell, generic). Defaults to host CPU if not specified."
    )]
    pub cpu: String,

    #[clap(long, value_enum, default_value_t = CliOptimizeLevelOption::None, help = "Set optimization level.")]
    pub optimize: CliOptimizeLevelOption,

    #[clap(short = 'L', long, value_name = "LIBRARY_PATH", help = "Add a library search path.")]
    pub library_path: Vec<String>,

    #[clap(short = 'l', long = "library", value_name = "LIB", help = "Link a library.")]
    pub libraries: Vec<String>,

    #[clap(long = "sources", value_name = "SOURCES", help = "Source files.")]
    pub source_dirs: Vec<String>,

    #[clap(long = "linker-flags", short = 'z', help = "Adds custom flags to the linker.")]
    pub linkerflags: Vec<String>,

    #[clap(
        short = 'g',
        help = "Enable generation of DWARF debug information for use with debuggers (GDB/LLDB)."
    )]
    pub debuginfo_enabled: bool,

    #[clap(
        long = "build-dir",
        value_name = "PATH",
        help = "Specifies the directory where build artifacts will be stored."
    )]
    pub build_dir: Option<String>,

    #[clap(
        long = "base-path",
        value_name = "PATH",
        help = "Specifies the base path for running the command."
    )]
    pub base_path: Option<String>,

    #[clap(
        long = "linker",
        value_name = "LINKER_NAMe",
        help = "Specifies the linker for linking the object files."
    )]
    pub linker: Option<String>,

    #[clap(
        long,
        short = 'q',
        help = "Suppress unnecessary output messages.",
        conflicts_with = "verbose"
    )]
    pub quiet: bool,

    #[clap(long, short = 'V', help = "Increase output verbosity.", conflicts_with = "quiet")]
    pub verbose: bool,

    #[clap(long, help = "Disables module filesystem cache.")]
    pub disable_modulefs_cache: bool,

    #[clap(long, help = "Disables all warnings.")]
    pub disable_warnings: bool,

    #[clap(long, help = "Set cyrus standard library path.")]
    pub stdlib: Option<String>,

    #[clap(long = "target-machine", help = "Display Target Machine information.")]
    pub display_target_machine: bool,

    #[clap(long = "sanitize", help = "Enables dynamic code analysis for bug detection.")]
    #[clap(value_enum, value_delimiter = ',')]
    pub sanitizer: Vec<CliSanitizerOption>,

    #[clap(long, value_enum, default_value_t = CliRelocModeOption::default(),
    help = "Set the relocation model for code generation."
    )]
    pub reloc_mode: CliRelocModeOption,

    #[clap(long, value_enum, default_value_t = CliCodeModelOption::Default,
    help = "Set the code model for code generation."
    )]
    pub code_model: CliCodeModelOption,

    #[clap(long, value_enum, default_value_t = CliProfileOption::Debug,
    help = "Build profile configuration."
    )]
    pub profile: CliProfileOption,

    #[clap(long, value_enum, default_value_t = CliABIOption::Cyrus,
    help =
    "Select the ABI name mangling scheme for code generation. \
Choices determine how function, global variable, and type names \
are represented in the generated output. For example, 'c' produces \
C-compatible names, while 'cyrus' uses the compiler's default mangling."
    )]
    pub abi: CliABIOption,

    #[clap(long, help = "Number of threads to use for compilation.")]
    pub jobs: Option<usize>,

    /// Module merge mode: `unified` for serial, `separate` for parallel
    #[clap(
        long,
        value_enum,
        help = "Module merge mode: 'unified' for serial, 'separate' for parallel."
    )]
    pub module_merge_mode: Option<CliModuleMergeModeOption>,
}

#[derive(Parser, Debug, Clone)]
pub(crate) struct CliLinkerOptions {
    /// Produce a fully static ELF.
    #[arg(long)]
    pub r#static: bool,

    /// Enable PIE (Position Independent Executable).
    #[arg(long, conflicts_with = "no_pie")]
    pub pie: bool,

    /// Disable PIE.
    #[arg(long, conflicts_with = "pie")]
    pub no_pie: bool,
}

#[inline]
pub(crate) fn get_current_dir_as_base_path() -> String {
    let current_dir = env::current_dir().unwrap_or_default();
    current_dir.to_string_lossy().into_owned()
}
