use crate::CompilerOptions;
use cyrusc_ast::token::TokenKind;
use cyrusc_codegen_llvm::CodeGenLLVM;
use cyrusc_compiler::driver::{build_compilation_bundle, create_compiler_context};
use cyrusc_compiler::options::{CodeGenOptions, LinkerOutputKind};
use cyrusc_diagcentral::display_single_custom_diag;
use cyrusc_fs_utils::{get_directory_of_file, read_file};
use cyrusc_lexer::Lexer;
use cyrusc_modulefsloader::ModuleLoaderOptions;
use cyrusc_parser::Parser;
use cyrusc_resolver::{Resolver, Visiting, generate_module_id};
use cyrusc_tui_utils::tui_warning;
use std::io::Write;
use std::process::Command;
use std::{env, fs, io};
use std::{process::exit, sync::Arc};

pub(crate) fn command_run(mut opts: CodeGenOptions, file_path: Option<String>, program_args: Vec<String>) {
    // build compilation bundle
    let llvm_backend = CodeGenLLVM::new();
    let compilation_bundle = build_compilation_bundle(&mut opts, file_path);

    let context = create_compiler_context(
        Box::new(opts.clone()),
        Some(compilation_bundle.entry_file.clone()),
        Arc::new(llvm_backend),
        LinkerOutputKind::Executable,
    );

    if opts.display_target_machine {
        println!("{}", context.target_machine_info());
    }

    // create temp executable path
    let mut temp_executable_file = env::temp_dir();
    let exe_name = opts.project_name.clone().unwrap_or_else(|| {
        compilation_bundle
            .entry_file
            .split(std::path::MAIN_SEPARATOR)
            .last()
            .unwrap_or("temp_program")
            .replace(".cyrus", "")
    });
    temp_executable_file.push(exe_name);

    let object_files = context.compile(&compilation_bundle.program_trees);

    // link to the temp executable
    if let Err(err) = context.trigger_linker(object_files, temp_executable_file.to_str().unwrap().to_string()) {
        display_single_custom_diag!(err);
    }

    // run the temporary executable
    match Command::new(&temp_executable_file).args(program_args).output() {
        Ok(output) => {
            io::stdout().write_all(&output.stdout).unwrap();
            io::stderr().write_all(&output.stderr).unwrap();
        }
        Err(err) => {
            eprintln!(
                "Failed to execute temporary program `{}`: {err}",
                temp_executable_file.display()
            );
        }
    }

    // delete temporary executable
    if let Err(err) = fs::remove_file(&temp_executable_file) {
        tui_warning(format!(
            "Failed to remove temp file {}: {err}",
            temp_executable_file.display()
        ));
    }
}

pub(crate) fn command_emit_llvm(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
    // let (opts, file_path, final_build_dir, program_trees, resolver_rc, monomorph_registry) =
    //     prepare_compilation(&mut options, file_path);

    // let output_path = output_path.unwrap_or_else(|| {
    //     display_single_custom_diag!("Output directory must be specified to generate llvm-ir.".to_string());
    // });

    // ensure_output_dir(output_path.clone());
    // let context = CodeGenContext::new(
    //     final_build_dir,
    //     opts,
    //     OutputKind::LlvmIr(output_path),
    //     resolver_rc,
    //     file_path,
    // );
    // context.compile_modules(program_trees, monomorph_registry);
}

pub(crate) fn command_emit_bytecode(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
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

pub(crate) fn command_emit_asm(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
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
    // let (opts, file_path, final_build_dir, program_trees, resolver_rc, monomorph_registry) =
    //     prepare_compilation(&mut options, file_path);

    // let output_path = output_path.unwrap_or_else(|| {
    //     display_single_custom_diag!("Output must be specified to generate executable.".to_string());
    // });

    // let context = CodeGenContext::new(
    //     final_build_dir,
    //     opts,
    //     OutputKind::Executable(output_path),
    //     resolver_rc,
    //     file_path,
    // );
    // context.compile_modules(program_trees, monomorph_registry);
}

pub(crate) fn command_object(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
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

pub(crate) fn command_dylib(mut opts: CodeGenOptions, file_path: Option<String>, output_path: Option<String>) {
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
        stdlib_path: options.stdlib.clone(),
        source_dirs: options.source_dirs.clone(),
    };

    let mut resolver = Resolver::new(module_loader_opts, file_path.clone());
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
