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

use crate::{
    args::{Args, Commands},
    commands::*,
    scaffold::*,
};
use cyrusc_scaffold::version::CYRUS_COMPILER_VERSION;

pub fn dispatch(args: Args) {
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
            let mut codegen_options = compiler_options.as_compiler_options();
            codegen_options.linker_options = linker_options.convert();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

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
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

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
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

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
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

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
            let mut codegen_options = compiler_options.as_compiler_options();
            codegen_options.linker_options = linker_options.convert();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_build(codegen_options, file_path, output_path);
        }
        Commands::Clean { compiler_options } => {
            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_clean(codegen_options);
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
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

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
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

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
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_static_lib(codegen_options, file_path, output_path);
        }
        Commands::SemanticOnly {
            compiler_options,
            file_path,
        } => {
            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_semantic_only(codegen_options, file_path)
        }
        Commands::Version => {
            println!("Cyrus {}", CYRUS_COMPILER_VERSION)
        }
        Commands::LexOnly { file_path } => command_lex_only(file_path),
        Commands::ParseOnly { file_path } => command_parse_only(file_path),
        Commands::EmitCIRDump {
            file_path,
            output_path,
            compiler_options,
        } => {
            let scaffold_config = compiler_option_from_scaffold_parser(compiler_options.base_path.clone());
            let mut codegen_options = compiler_options.as_compiler_options();
            merge_and_validate_scaffold_config_with_codegen_options(&mut codegen_options, &scaffold_config);

            command_emit_cir_dump(codegen_options, file_path, output_path);
        }
    }
}
