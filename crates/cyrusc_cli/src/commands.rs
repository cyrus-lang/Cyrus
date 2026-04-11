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

use cyrusc_cir_dump::process_cir_dump_for_modules;
use cyrusc_codegen_llvm::CodeGenLLVM;
use cyrusc_compiler::codegen_traits::CodeGenBackend;
use cyrusc_compiler::driver::{
    build_compilation_bundle, build_semantic_bundle, create_compiler_context, get_assembly_dir_output_path,
    get_bitcode_dir_output_path, get_executable_output_path, get_final_build_directory_path, get_llvm_dir_output_path,
    get_object_dir_output_path, get_shared_lib_dir_output_path, get_static_lib_dir_output_path,
};
use cyrusc_compiler::object_file_info::ObjectFileInfo;
use cyrusc_compiler::options::{CodeGenOptions, LinkerOutputKind};
use cyrusc_diagcentral::exit_with_msg;
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::ensure_output_dir;
use cyrusc_fs_utils::temp::TempExecutableBuilder;
use cyrusc_lexer::Lexer;
use cyrusc_parser::SourceParser;
use cyrusc_source_loc::SourceMap;
use std::fs;
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, exit};
use std::rc::Rc;
use std::sync::Arc;

pub(crate) fn command_run(mut opts: CodeGenOptions, file_path: Option<String>, program_args: Vec<String>) {
    let mut bundle = build_compilation_bundle(&mut opts, file_path);

    let ctx = Rc::new(create_compiler_context(
        opts.clone(),
        &Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
        bundle.target,
        bundle.llvm_target,
        bundle.llvm_target_triple,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        ctx.clone(),
        &ctx.llvm_target,
        &ctx.llvm_target_triple,
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
        ctx.build_manifest.clone(),
        bundle.entry_file.clone(),
    )));

    let temp_exe = TempExecutableBuilder::new()
        .project_name(opts.project_name.clone().unwrap_or_default())
        .entry_file(bundle.entry_file)
        .build()
        .expect("Failed to create temp executable path.");

    let owned_modules = ctx.compile(llvm_backend, &mut bundle.program_trees);
    let object_files: Vec<ObjectFileInfo> = owned_modules
        .iter()
        .map(|owned_module| llvm_backend.save_object_file(owned_module))
        .collect();

    ctx.save_context_build_cache();

    if let Err(err) = ctx.trigger_linker(object_files, &temp_exe.path) {
        exit_with_msg!(err);
    }

    match Command::new(&temp_exe.path)
        .stdin(std::process::Stdio::inherit())
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .args(program_args)
        .output()
    {
        Ok(output) => {
            io::stdout().write_all(&output.stdout).unwrap();
            io::stderr().write_all(&output.stderr).unwrap();
        }
        Err(err) => {
            eprintln!(
                "Failed to execute temporary program '{}': {err}",
                temp_exe.path.display()
            );
        }
    }
}

pub(crate) fn command_build(mut opts: CodeGenOptions, file_path: Option<String>, output_path_opt: Option<String>) {
    let mut bundle = build_compilation_bundle(&mut opts, file_path);

    let output_path_opt = output_path_opt.map(|path| Path::new(&path).to_path_buf());
    let output_path = get_executable_output_path(&opts, &bundle.build_dir, &bundle.entry_file, output_path_opt);

    let ctx = Rc::new(create_compiler_context(
        opts.clone(),
        &Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
        bundle.target,
        bundle.llvm_target,
        bundle.llvm_target_triple,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        ctx.clone(),
        &ctx.llvm_target,
        &ctx.llvm_target_triple,
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
        ctx.build_manifest.clone(),
        bundle.entry_file,
    )));

    let owned_modules = ctx.compile(llvm_backend, &mut bundle.program_trees);
    let object_files: Vec<ObjectFileInfo> = owned_modules
        .iter()
        .map(|owned_module| llvm_backend.save_object_file(owned_module))
        .collect();

    ctx.save_context_build_cache();

    if let Err(err) = ctx.trigger_linker(object_files, &output_path) {
        exit_with_msg!(err);
    }
}

pub(crate) fn command_clean(opts: CodeGenOptions) {
    let build_dir = get_final_build_directory_path(&opts.build_dir);

    if build_dir.exists() {
        if let Err(err) = fs::remove_dir_all(build_dir) {
            exit_with_msg!(format!("Error while cleaning build directory: {}", err.to_string()));
        }
    }
}

pub(crate) fn command_emit_llvm(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    let mut bundle = build_compilation_bundle(&mut opts, file_path);

    let llvm_ir_dir = get_llvm_dir_output_path(&bundle.build_dir, &output_path);

    let ctx = Rc::new(create_compiler_context(
        opts.clone(),
        &Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
        bundle.target,
        bundle.llvm_target,
        bundle.llvm_target_triple,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        ctx.clone(),
        &ctx.llvm_target,
        &ctx.llvm_target_triple,
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
        ctx.build_manifest.clone(),
        bundle.entry_file,
    )));

    let owned_modules = ctx.compile(llvm_backend, &mut bundle.program_trees);
    llvm_backend.save_modules_as_llvm_ir(&owned_modules, &llvm_ir_dir);
}

pub(crate) fn command_emit_bitcode(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    let mut bundle = build_compilation_bundle(&mut opts, file_path);

    let bitcode_dir = get_bitcode_dir_output_path(&bundle.build_dir, &output_path);

    let ctx = Rc::new(create_compiler_context(
        opts.clone(),
        &Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
        bundle.target,
        bundle.llvm_target,
        bundle.llvm_target_triple,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        ctx.clone(),
        &ctx.llvm_target,
        &ctx.llvm_target_triple,
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
        ctx.build_manifest.clone(),
        bundle.entry_file,
    )));

    let owned_modules = ctx.compile(llvm_backend, &mut bundle.program_trees);
    llvm_backend.save_modules_as_bitcode(&owned_modules, &bitcode_dir);
}

pub(crate) fn command_emit_asm(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    let mut bundle = build_compilation_bundle(&mut opts, file_path);

    let assembly_dir = get_assembly_dir_output_path(&bundle.build_dir, &output_path);

    let ctx = Rc::new(create_compiler_context(
        opts.clone(),
        &Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
        bundle.target,
        bundle.llvm_target,
        bundle.llvm_target_triple,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        ctx.clone(),
        &ctx.llvm_target,
        &ctx.llvm_target_triple,
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
        ctx.build_manifest.clone(),
        bundle.entry_file,
    )));

    let owned_modules = ctx.compile(llvm_backend, &mut bundle.program_trees);
    llvm_backend.save_modules_as_assembly(&owned_modules, &assembly_dir);
}

pub(crate) fn command_emit_cir_dump(
    mut opts: CodeGenOptions,
    file_path: Option<String>,
    output_path_opt: Option<String>,
) {
    let bundle = build_compilation_bundle(&mut opts, file_path);

    let output_path_opt = output_path_opt.map(|path| Path::new(&path).to_path_buf());
    let output_path = get_executable_output_path(&opts, &bundle.build_dir, &bundle.entry_file, output_path_opt);

    ensure_output_dir(&output_path);

    process_cir_dump_for_modules(&bundle.program_trees, output_path);
}

pub(crate) fn command_object(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    let mut bundle = build_compilation_bundle(&mut opts, file_path);

    let object_dir = get_object_dir_output_path(&bundle.build_dir, &output_path);

    let ctx = Rc::new(create_compiler_context(
        opts.clone(),
        &Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
        bundle.target,
        bundle.llvm_target,
        bundle.llvm_target_triple,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        ctx.clone(),
        &ctx.llvm_target,
        &ctx.llvm_target_triple,
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
        ctx.build_manifest.clone(),
        bundle.entry_file,
    )));

    let owned_modules = ctx.compile(llvm_backend, &mut bundle.program_trees);
    llvm_backend.save_modules_as_object(&owned_modules, &object_dir);
}

pub(crate) fn command_shared_lib(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    let mut bundle = build_compilation_bundle(&mut opts, file_path);

    let shared_lib_dir = get_shared_lib_dir_output_path(&bundle.build_dir, &output_path);

    let ctx = Rc::new(create_compiler_context(
        opts.clone(),
        &Some(bundle.entry_file.clone()),
        LinkerOutputKind::SharedLib,
        bundle.target,
        bundle.llvm_target,
        bundle.llvm_target_triple,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        ctx.clone(),
        &ctx.llvm_target,
        &ctx.llvm_target_triple,
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
        ctx.build_manifest.clone(),
        bundle.entry_file,
    )));

    let owned_modules = ctx.compile(llvm_backend, &mut bundle.program_trees);
    let object_files: Vec<ObjectFileInfo> = owned_modules
        .iter()
        .map(|owned_module| llvm_backend.save_object_file(owned_module))
        .collect();

    ctx.save_context_build_cache();

    if let Err(err) = ctx.trigger_linker(object_files, &shared_lib_dir) {
        exit_with_msg!(err);
    }
}

pub(crate) fn command_static_lib(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    let mut bundle = build_compilation_bundle(&mut opts, file_path);

    let static_lib_dir = get_static_lib_dir_output_path(&bundle.build_dir, &output_path);

    let ctx = Rc::new(create_compiler_context(
        opts.clone(),
        &Some(bundle.entry_file.clone()),
        LinkerOutputKind::StaticLib,
        bundle.target,
        bundle.llvm_target,
        bundle.llvm_target_triple,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        ctx.clone(),
        &ctx.llvm_target,
        &ctx.llvm_target_triple,
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
        ctx.build_manifest.clone(),
        bundle.entry_file,
    )));

    let owned_modules = ctx.compile(llvm_backend, &mut bundle.program_trees);
    let object_files: Vec<ObjectFileInfo> = owned_modules
        .iter()
        .map(|owned_module| llvm_backend.save_object_file(owned_module))
        .collect();

    ctx.save_context_build_cache();

    if let Err(err) = ctx.trigger_linker(object_files, &static_lib_dir) {
        exit_with_msg!(err);
    }
}

pub(crate) fn command_lex_only(file_path: String) {
    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file_by_loading(file_path);
    let source_file = { source_map.get_file(file_id).unwrap().clone() };

    let reporter = DiagReporter::new(source_map);

    let mut lexer = Lexer::new(&reporter, &source_file);

    loop {
        let token = lexer.next_token();
        if token.kind.is_eof() {
            break;
        }

        println!(
            "Kind={:?} Start={}, End={}, Line={}, Column={}",
            token.kind, token.loc.start, token.loc.end, token.loc.line, token.loc.column
        );
    }
}

pub(crate) fn command_parse_only(file_path: String) {
    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file_by_loading(file_path);
    let source_file = { source_map.get_file(file_id).unwrap().clone() };

    let reporter = Arc::new(DiagReporter::new(source_map));

    let source_parser = SourceParser::new(reporter.clone());

    match source_parser.parse_program(&source_file) {
        Ok(result) => println!("{:#?}", result),
        Err(_) => exit(1),
    }
}

pub(crate) fn command_semantic_only(mut opts: CodeGenOptions, file_path: String) {
    build_semantic_bundle(&mut opts, Some(file_path));
}
