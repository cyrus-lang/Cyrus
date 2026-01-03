// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

use crate::CompilerOptions;
use crate::temp_executable_builder::TempExecutableBuilder;
use cyrusc_ast::token::TokenKind;
use cyrusc_codegen_llvm::CodeGenLLVM;
use cyrusc_compiler::codegen_traits::CodeGenBackend;
use cyrusc_compiler::driver::{build_compilation_bundle, create_compiler_context};
use cyrusc_compiler::object_file_info::ObjectFileInfo;
use cyrusc_compiler::options::{CodeGenOptions, LinkerOutputKind};
use cyrusc_diagcentral::display_single_custom_diag;
use cyrusc_fs_utils::{get_directory_of_file, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_tast::generics::monomorph::MonomorphRegistry;
use std::io;
use std::io::Write;
use std::process::Command;
use std::process::exit;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

pub(crate) fn command_run(mut opts: CodeGenOptions, file_path: Option<String>, program_args: Vec<String>) {
    let bundle = build_compilation_bundle(&mut opts, file_path);

    let context = Rc::new(create_compiler_context(
        opts.clone(),
        Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        context.clone(),
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
    )));

    if opts.display_target_machine {
        println!("{}", context.target_machine_info(llvm_backend));
    }

    let temp_exe = TempExecutableBuilder::new()
        .project_name(opts.project_name.clone().unwrap_or_default())
        .entry_file(bundle.entry_file.clone())
        .build()
        .expect("Failed to create temp executable path.");

    let owned_modules = context.compile(llvm_backend, &bundle.program_trees);
    let object_files: Vec<ObjectFileInfo> = owned_modules
        .iter()
        .map(|owned_module| llvm_backend.save_object_file(owned_module))
        .collect();

    if let Err(err) = context.trigger_linker(object_files, temp_exe.path.to_string_lossy().to_string()) {
        display_single_custom_diag!(err);
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

pub(crate) fn command_emit_llvm(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    // build compilation bundle
    let bundle = build_compilation_bundle(&mut opts, file_path);

    let context = Rc::new(create_compiler_context(
        opts.clone(),
        Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
    ));

    // build llvm codegen context
    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        context.clone(),
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
    )));

    if opts.display_target_machine {
        println!("{}", context.target_machine_info(llvm_backend));
    }

    let owned_modules = context.compile(llvm_backend, &bundle.program_trees);
    llvm_backend.save_modules_llvm_ir(&owned_modules, output_path);
}

#[allow(unused)]
pub(crate) fn command_emit_bytecode(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    todo!();
    // let (opts, file_path, final_build_dir, program_trees, resolver_rc, monomorph_registry) =
    //     prepare_compilation(&mut options, file_path);

    // let output_path = output_path.unwrap_or_else(|| {
    //     display_single_custom_diag!("Output directory must be specified to generate bytecode.".to_string());
    // });

    // ensure_output_dir(output_path.clone());
    // let context = CodeGenContext::new(
    //     final_build_dir,
    //     opts,
    //     OutputKind::ByteCode(output_path),
    //     resolver_rc,
    //     file_path,
    // );
    // context.compile_modules(program_trees, monomorph_registry);
}

#[allow(unused)]
pub(crate) fn command_emit_asm(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    todo!();
    // let (opts, file_path, final_build_dir, program_trees, resolver_rc, monomorph_registry) =
    //     prepare_compilation(&mut options, file_path);

    // let output_path = output_path.unwrap_or_else(|| {
    //     display_single_custom_diag!("Output directory must be specified to generate bytecode.".to_string());
    // });

    // ensure_output_dir(output_path.clone());
    // let context = CodeGenContext::new(
    //     final_build_dir,
    //     opts,
    //     OutputKind::Asm(output_path),
    //     resolver_rc,
    //     file_path,
    // );
    // context.compile_modules(program_trees, monomorph_registry);
}

pub(crate) fn command_build(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    let bundle = build_compilation_bundle(&mut opts, file_path);

    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output directory must be specified to generate executable.".to_string());
    });

    let context = Rc::new(create_compiler_context(
        opts.clone(),
        Some(bundle.entry_file.clone()),
        LinkerOutputKind::Executable,
    ));

    let llvm_backend: &'static CodeGenLLVM = Box::leak(Box::new(CodeGenLLVM::new(
        context.clone(),
        opts.clone(),
        bundle.build_dir,
        bundle.monomorph_registry.clone(),
    )));

    if opts.display_target_machine {
        println!("{}", context.target_machine_info(llvm_backend));
    }

    let owned_modules = context.compile(llvm_backend, &bundle.program_trees);
    let object_files: Vec<ObjectFileInfo> = owned_modules
        .iter()
        .map(|owned_module| llvm_backend.save_object_file(owned_module))
        .collect();

    if let Err(err) = context.trigger_linker(object_files, output_path) {
        display_single_custom_diag!(err);
    }
}

#[allow(unused)]
pub(crate) fn command_object(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    todo!();
    // let (opts, file_path, final_build_dir, program_trees, resolver_rc, monomorph_registry) =
    //     prepare_compilation(&mut options, file_path);

    // let output_path = output_path.unwrap_or_else(|| {
    //     display_single_custom_diag!("Output must be specified to generate object files.".to_string());
    // });

    // ensure_output_dir(output_path.clone());
    // let context = CodeGenContext::new(
    //     final_build_dir,
    //     opts,
    //     OutputKind::ObjectFile(output_path),
    //     resolver_rc,
    //     file_path,
    // );
    // context.compile_modules(program_trees, monomorph_registry);
}

#[allow(unused)]
pub(crate) fn command_dylib(opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    todo!();
    // let (opts, file_path, final_build_dir, program_trees, resolver_rc, monomorph_registry) =
    //     prepare_compilation(&mut options, file_path);

    // let output_path = output_path.unwrap_or_else(|| {
    //     display_single_custom_diag!("Output must be specified to generate object files.".to_string());
    // });

    // ensure_output_dir(output_path.clone());
    // let context = CodeGenContext::new(
    //     final_build_dir,
    //     opts,
    //     OutputKind::Dylib(output_path),
    //     resolver_rc,
    //     file_path,
    // );
    // context.compile_modules(program_trees, monomorph_registry);
}

pub(crate) fn command_lex_only(file_path: String) {
    let (file_content, file_name) = read_file(file_path.clone());
    let mut lexer = Lexer::new(file_content, file_name);
    loop {
        let token = lexer.next_token();
        if token.kind == TokenKind::EOF {
            break;
        }

        println!(
            "{:?} Span({}, {}) Line({}) Column({})",
            token.kind, token.span.start, token.span.end, token.loc.line, token.loc.column
        );
    }
}

pub(crate) fn command_parse_only(file_path: String) {
    let (file_content, file_name) = read_file(file_path.clone());
    let mut lexer = Lexer::new(file_content, file_name.clone());
    let mut parser = Parser::new(lexer.tokenize(), file_name);

    match parser.parse() {
        Ok(result) => println!("{:#?}", result),
        Err(errors) => {
            parser.display_parser_errors(errors.clone());
            exit(1);
        }
    }
}

pub(crate) fn command_semantic_only(mut options: CompilerOptions, file_path: String) {
    let file_content = read_file(file_path.clone()).0;
    let mut lexer = Lexer::new(file_content, file_path.clone());
    let mut parser = Parser::new(lexer.tokenize(), file_path.clone());

    let program = match parser.parse() {
        Ok(program) => program,
        Err(errors) => {
            parser.display_parser_errors(errors.clone());
            exit(1);
        }
    };

    let entry_module_dir_path = get_directory_of_file(file_path.clone()).unwrap();
    options.source_dirs.push(entry_module_dir_path);

    let module_loader_opts = ModuleLoaderOptions {
        base_path: options.base_path.clone().unwrap(),
        stdlib_path: options.stdlib.clone(),
        source_dirs: options.source_dirs.clone(),
    };

    let monomorph_registry = Arc::new(Mutex::new(MonomorphRegistry::new()));

    let mut resolver = Resolver::new(module_loader_opts, monomorph_registry, file_path.clone());
    let module_id = generate_module_id();
    match resolver.resolve_module(module_id, &program, &mut Visiting::new(), true, file_path.clone()) {
        Some(..) => {}
        None => unreachable!(),
    };
    if resolver.reporter.has_errors() {
        resolver.reporter.display();
        exit(1);
    }
}
