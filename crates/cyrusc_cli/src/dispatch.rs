// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    args::{Args, Commands},
    commands::*,
    scaffold::*,
};
use cyrusc_compiler::options::validate_compiler_options;
use cyrusc_scaffold::version::CYRUS_COMPILER_VERSION;
use std::path::PathBuf;

/// Macro to handle the common pattern of preparing compiler options
macro_rules! prepare_compiler_options {
    // Commands with linker options and file/output paths
    ($compiler_options:expr, $linker_options:expr, $file_path:expr, $output_path:expr) => {{
        if $file_path.is_none() && $output_path.is_none() {
            project_file_required();
        }
        
        let scaffold_config = compiler_option_from_scaffold_parser($compiler_options.base_path.clone());
        let mut compiler_options = $compiler_options.as_compiler_options();
        compiler_options.linker_options = $linker_options.convert();
        integrate_scaffold_config(&mut compiler_options, &scaffold_config);
        validate_compiler_options(&compiler_options);
        
        (compiler_options, $file_path, $output_path)
    }};
    
    // Commands without linker options but with file and output paths
    ($compiler_options:expr, $file_path:expr, $output_path:expr) => {{
        if $file_path.is_none() && $output_path.is_none() {
            project_file_required();
        }
        
        let scaffold_config = compiler_option_from_scaffold_parser($compiler_options.base_path.clone());
        let mut compiler_options = $compiler_options.as_compiler_options();
        integrate_scaffold_config(&mut compiler_options, &scaffold_config);
        validate_compiler_options(&compiler_options);
        
        (compiler_options, $file_path, $output_path)
    }};
    
    // Commands with only compiler options
    ($compiler_options:expr) => {{
        let scaffold_config = compiler_option_from_scaffold_parser($compiler_options.base_path.clone());
        let mut compiler_options = $compiler_options.as_compiler_options();
        integrate_scaffold_config(&mut compiler_options, &scaffold_config);
        validate_compiler_options(&compiler_options);
        
        compiler_options
    }};
    
    // Commands with compiler options and file path
    ($compiler_options:expr, $file_path:expr) => {{
        let scaffold_config = compiler_option_from_scaffold_parser($compiler_options.base_path.clone());
        let mut compiler_options = $compiler_options.as_compiler_options();
        integrate_scaffold_config(&mut compiler_options, &scaffold_config);
        validate_compiler_options(&compiler_options);
        
        (compiler_options, $file_path)
    }};
}

pub fn dispatch(args: Args) {
    match args.cmd {
        Commands::New { project_name, lib } => {
            command_new(project_name, lib);
        }
        
        Commands::Run {
            file_path,
            compiler_options,
            linker_options,
            program_args,
        } => {
            let (compiler_options, file_path, _) = prepare_compiler_options!(
                compiler_options, 
                linker_options, 
                file_path, 
                None::<Option<PathBuf>>
            );
            command_run(compiler_options, file_path, program_args);
        }
        
        Commands::EmitLLVM {
            file_path,
            output_path,
            compiler_options,
        } => {
            let (compiler_options, file_path, output_path) = 
                prepare_compiler_options!(compiler_options, file_path, output_path);

            command_emit_llvm(compiler_options, file_path, output_path);
        }
        
        Commands::EmitASM {
            file_path,
            output_path,
            compiler_options,
        } => {
            let (compiler_options, file_path, output_path) = 
                prepare_compiler_options!(compiler_options, file_path, output_path);

            command_emit_asm(compiler_options, file_path, output_path);
        }
        
        Commands::EmitBitcode {
            file_path,
            output_path,
            compiler_options,
        } => {
            let (compiler_options, file_path, output_path) = 
                prepare_compiler_options!(compiler_options, file_path, output_path);

            command_emit_bitcode(compiler_options, file_path, output_path);
        }
        
        Commands::Build {
            file_path,
            output_path,
            compiler_options,
            linker_options,
        } => {
            let (compiler_options, file_path, output_path) = 
                prepare_compiler_options!(compiler_options, linker_options, file_path, output_path);

            command_build(compiler_options, file_path, output_path);
        }
        
        Commands::Clean { compiler_options } => {
            let compiler_options = prepare_compiler_options!(compiler_options);
            command_clean(compiler_options);
        }
        
        Commands::Object {
            file_path,
            output_path,
            compiler_options,
        } => {
            let (compiler_options, file_path, output_path) = 
                prepare_compiler_options!(compiler_options, file_path, output_path);

            command_object(compiler_options, file_path, output_path);
        }
        
        Commands::SharedLib {
            file_path,
            output_path,
            compiler_options,
        } => {
            let (compiler_options, file_path, output_path) = 
                prepare_compiler_options!(compiler_options, file_path, output_path);

            command_shared_lib(compiler_options, file_path, output_path);
        }
        
        Commands::StaticLib {
            file_path,
            output_path,
            compiler_options,
        } => {
            let (compiler_options, file_path, output_path) = 
                prepare_compiler_options!(compiler_options, file_path, output_path);

            command_static_lib(compiler_options, file_path, output_path);
        }
        
        Commands::SemanticOnly {
            compiler_options,
            file_path,
        } => {
            let (compiler_options, file_path) = 
                prepare_compiler_options!(compiler_options, file_path);

            command_semantic_only(compiler_options, file_path);
        }
        
        Commands::LexOnly { file_path } => {
            command_lex_only(file_path);
        }
        
        Commands::ParseOnly { file_path } => {
            command_parse_only(file_path);
        }
        
        Commands::EmitCIRDump {
            file_path,
            output_path,
            compiler_options,
        } => {
            let (compiler_options, file_path, output_path) = 
                prepare_compiler_options!(compiler_options, file_path, output_path);

            command_emit_cir_dump(compiler_options, file_path, output_path);
        }
        
        Commands::Fetch { .. } => {
            todo!();
        }
        
        Commands::Version => {
            println!("Cyrus {}", CYRUS_COMPILER_VERSION);
        }
    }
}