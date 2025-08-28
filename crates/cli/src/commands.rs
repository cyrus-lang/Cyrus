use crate::CompilerOptions;
use ast::token::TokenKind;
use codegen::{
    context::context::CodeGenContext,
    options::{BuildDir, CodeGenOptions, OutputKind},
};
use diagcentral::{display_single_custom_diag, reporter::DiagReporter};
use lexer::Lexer;
use parser::Parser;
use project_layout::{OBJECTS_FILENAME, OUTPUT_FILENAME, PROJECT_FILE_PATH, SOURCES_DIR_PATH};
use resolver::{
    Resolver, Visiting, generate_module_id,
    moduleloader::{ModuleFilePath, ModuleLoaderOptions},
};
use static_analyzer::context::AnalysisContext;
use std::{
    cell::RefCell, env, io::{self, Write}, path::Path, process::exit, rc::Rc, sync::{Arc, Mutex}
};
use typed_ast::{ModuleID, TypedProgramTree};
use utils::fs::{ensure_output_dir, get_directory_of_file};

fn get_program_trees(
    options: &mut CompilerOptions,
    file_path: String,
) -> (
    Vec<(String, ModuleFilePath, ModuleID, Rc<RefCell<TypedProgramTree>>)>,
    Rc<Resolver>,
) {
    let file_content = utils::fs::read_file(file_path.clone()).0;
    let mut lexer = Lexer::new(file_content, file_path.clone());
    let mut parser = Parser::new(lexer.tokenize(), file_path.clone());

    let node = match parser.parse() {
        Ok(node) => node,
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
    match resolver.resolve_module(module_id, node.as_program(), &mut Visiting::new(), true) {
        Some(..) => {}
        None => unreachable!(),
    };
    if resolver.reporter.has_errors() {
        resolver.reporter.display();
        exit(1);
    }

    let entry_points = Arc::new(Mutex::new(Vec::new()));
    let final_program_trees: Vec<(String, ModuleFilePath, ModuleID, Rc<RefCell<TypedProgramTree>>)>;
    let program_trees = resolver.program_trees.lock().unwrap();

    {
        for (_, _, module_id, typed_program_tree) in program_trees.iter() {
            {
                let mut analyzer =
                    AnalysisContext::new(&resolver, *module_id, typed_program_tree.clone(), entry_points.clone(), options.disable_warnings);
                analyzer.analyze();
                DiagReporter::display(&analyzer.reporter);
                if analyzer.reporter.has_errors() {
                    exit(1);
                }
            }
        }
    }

    AnalysisContext::check_entry_points(entry_points);
    final_program_trees = program_trees.clone();
    drop(program_trees);

    (final_program_trees, Rc::new(resolver))
}

fn prepare_compilation(
    mut options: &mut CompilerOptions,
    file_path: Option<String>,
) -> (
    CodeGenOptions,
    String,
    String,
    Vec<(String, ModuleFilePath, ModuleID, Rc<RefCell<TypedProgramTree>>)>,
    Rc<Resolver>,
) {
    let opts = options.to_compiler_options();
    let file_path = get_entry_source_code_path(options.base_path.clone(), file_path);
    let final_build_dir = get_final_build_directory_path(options.base_path.clone(), opts.build_dir.clone());
    ensure_build_dir_subs(options.base_path.clone(), final_build_dir.clone());

    let (program_trees, resolver_rc) = get_program_trees(&mut options, file_path.clone());

    (opts, file_path, final_build_dir, program_trees, resolver_rc)
}

pub(crate) fn command_run(mut options: CompilerOptions, file_path: Option<String>) {
    let (mut opts, file_path, final_build_dir, program_trees, resolver_rc) =
        prepare_compilation(&mut options, file_path);
    opts.disable_modulefs_cache = true;

    let mut temp = env::temp_dir();
    temp.push("path");
    let temp_file_path = temp.to_str().unwrap().to_string();

    let context = CodeGenContext::new(
        final_build_dir,
        opts,
        OutputKind::Executable(temp_file_path.clone()),
        resolver_rc,
        file_path,
    );
    context.compile_modules(program_trees);

    let mut executable_command = std::process::Command::new(&temp_file_path);
    let output = executable_command.output().unwrap();
    io::stdout().write_all(&output.stdout).unwrap();
    io::stderr().write_all(&output.stderr).unwrap();

    if temp.exists() {
        std::fs::remove_file(temp_file_path).unwrap();
    }
}

pub(crate) fn command_emit_llvm(mut options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    options.disable_modulefs_cache = true;
    let (opts, file_path, final_build_dir, program_trees, resolver_rc) = prepare_compilation(&mut options, file_path);

    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output directory must be specified to generate llvm-ir.".to_string());
    });

    ensure_output_dir(output_path.clone());
    let context = CodeGenContext::new(
        final_build_dir,
        opts,
        OutputKind::LlvmIr(output_path),
        resolver_rc,
        file_path,
    );
    context.compile_modules(program_trees);
}

pub(crate) fn command_emit_bytecode(
    mut options: CompilerOptions,
    file_path: Option<String>,
    output_path: Option<String>,
) {
    options.disable_modulefs_cache = true;
    let (opts, file_path, final_build_dir, program_trees, resolver_rc) = prepare_compilation(&mut options, file_path);

    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output directory must be specified to generate bytecode.".to_string());
    });

    if let Some(build_dir) = &options.build_dir {
        ensure_output_dir(build_dir.clone());
    }

    ensure_output_dir(output_path.clone());
    let context = CodeGenContext::new(
        final_build_dir,
        opts,
        OutputKind::ByteCode(output_path),
        resolver_rc,
        file_path,
    );
    context.compile_modules(program_trees);
}

pub(crate) fn command_emit_asm(mut options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    options.disable_modulefs_cache = true;
    let (opts, file_path, final_build_dir, program_trees, resolver_rc) = prepare_compilation(&mut options, file_path);

    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output directory must be specified to generate bytecode.".to_string());
    });

    if let Some(build_dir) = &options.build_dir {
        ensure_output_dir(build_dir.clone());
    }

    ensure_output_dir(output_path.clone());
    let context = CodeGenContext::new(
        final_build_dir,
        opts,
        OutputKind::Asm(output_path),
        resolver_rc,
        file_path,
    );
    context.compile_modules(program_trees);
}

pub(crate) fn command_build(mut options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    let (opts, file_path, final_build_dir, program_trees, resolver_rc) = prepare_compilation(&mut options, file_path);

    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output must be specified to generate executable.".to_string());
    });

    let context = CodeGenContext::new(
        final_build_dir,
        opts,
        OutputKind::Executable(output_path),
        resolver_rc,
        file_path,
    );
    context.compile_modules(program_trees);
}

pub(crate) fn command_object(mut options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    options.disable_modulefs_cache = true;
    let (opts, file_path, final_build_dir, program_trees, resolver_rc) = prepare_compilation(&mut options, file_path);

    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output must be specified to generate object files.".to_string());
    });

    ensure_output_dir(output_path.clone());
    let context = CodeGenContext::new(
        final_build_dir,
        opts,
        OutputKind::ObjectFile(output_path),
        resolver_rc,
        file_path,
    );
    context.compile_modules(program_trees);
}

pub(crate) fn command_dylib(mut options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    options.disable_modulefs_cache = true;
    let (opts, file_path, final_build_dir, program_trees, resolver_rc) = prepare_compilation(&mut options, file_path);

    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output must be specified to generate object files.".to_string());
    });

    ensure_output_dir(output_path.clone());
    let context = CodeGenContext::new(
        final_build_dir,
        opts,
        OutputKind::Dylib(output_path),
        resolver_rc,
        file_path,
    );
    context.compile_modules(program_trees);
}

pub(crate) fn command_lex_only(file_path: String) {
    let (file_content, file_name) = utils::fs::read_file(file_path.clone());
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
    let (file_content, file_name) = utils::fs::read_file(file_path.clone());
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
    let file_content = utils::fs::read_file(file_path.clone()).0;
    let mut lexer = Lexer::new(file_content, file_path.clone());
    let mut parser = Parser::new(lexer.tokenize(), file_path.clone());

    let node = match parser.parse() {
        Ok(node) => node,
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
    match resolver.resolve_module(module_id, node.as_program(), &mut Visiting::new(), true) {
        Some(..) => {}
        None => unreachable!(),
    };
    if resolver.reporter.has_errors() {
        resolver.reporter.display();
        exit(1);
    }
}

fn ensure_build_dir_subs(base_path: Option<String>, build_dir_path: String) {
    let base = base_path.unwrap_or_default();
    let dirs = [SOURCES_DIR_PATH, OBJECTS_FILENAME, OUTPUT_FILENAME];

    let base_build_dir = Path::new(&base).join(build_dir_path);

    for dir in dirs {
        let build_dir = base_build_dir.join(dir);
        ensure_output_dir(build_dir.to_str().unwrap().to_string());
    }
}

fn get_final_build_directory_path(base_path: Option<String>, build_dir: BuildDir) -> String {
    let base = base_path.unwrap_or_default();

    match build_dir {
        BuildDir::Default => {
            let project_file = env::current_dir().unwrap().join(&base).join(PROJECT_FILE_PATH);

            if !project_file.exists() {
                return env::temp_dir().to_str().unwrap().to_string();
            }

            let build_dir = CodeGenOptions::read_toml(project_file.to_str().unwrap().to_string())
                .unwrap_or_else(|err| {
                    display_single_custom_diag!(err);
                })
                .build_dir;

            match build_dir {
                BuildDir::Default => env::temp_dir().to_str().unwrap().to_string(),
                BuildDir::Provided(path) => {
                    let build_dir = Path::new(&base).join(&path);
                    ensure_output_dir(build_dir.to_str().unwrap().to_string());
                    path
                }
            }
        }
        BuildDir::Provided(path) => path,
    }
}

fn get_entry_source_code_path(base_path: Option<String>, input_file_path: Option<String>) -> String {
    if let Some(path) = input_file_path {
        return path;
    }

    let base = base_path.unwrap_or_default();
    let main_file = env::current_dir().unwrap().join(&base).join("src/main.cyr");

    if !main_file.exists() {
        display_single_custom_diag!("Project.toml not found in the current directory.".to_string());
    }

    main_file.to_str().unwrap().to_string()
}
