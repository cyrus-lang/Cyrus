// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::options::{CliCompilerOptions, CliLinkerOptions};

#[derive(clap::Parser, Clone)]
#[command()]
pub(crate) struct Args {
    #[command(subcommand)]
    pub cmd: Commands,
}

#[derive(clap::Subcommand, Debug, Clone)]
pub(crate) enum Commands {
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
        compiler_options: CliCompilerOptions,

        #[clap(flatten)]
        linker_options: CliLinkerOptions,

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
        compiler_options: CliCompilerOptions,

        #[clap(flatten)]
        linker_options: CliLinkerOptions,
    },

    #[clap(about = "Clean build directory", display_order = 4)]
    Clean {
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },

    #[clap(about = "Generate an object file", display_order = 5)]
    Object {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },

    #[clap(about = "Generate a dynamic library (shared object)", display_order = 6)]
    SharedLib {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },
    #[clap(about = "Generate a static library", display_order = 7)]
    StaticLib {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },

    #[clap(about = "Emit LLVM IR as a .ll file per module.", display_order = 8)]
    EmitLLVM {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },

    #[clap(about = "Emit asm as a .s file per module.", display_order = 9)]
    EmitASM {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },

    #[clap(
        name = "emit-bitcode",
        about = "Emit bitcode as a .bc file per module.",
        display_order = 10
    )]
    EmitBitcode {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },

    #[clap(
        name = "emit-cir-dump",
        about = "Emit the textual CIR representation as a .cir file per module.",
        display_order = 11
    )]
    EmitCIRDump {
        file_path: Option<String>,
        #[clap(long, short)]
        output_path: Option<String>,
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },

    #[clap(about = "Lexical analysis only.", display_order = 12)]
    LexOnly { file_path: String },

    #[clap(about = "Display program tree.", display_order = 13)]
    ParseOnly { file_path: String },

    #[clap(about = "Check program correctness semantically.", display_order = 14)]
    SemanticOnly {
        file_path: String,
        #[clap(flatten)]
        compiler_options: CliCompilerOptions,
    },

    #[clap(about = "Print version information", display_order = 15)]
    Version,
}
