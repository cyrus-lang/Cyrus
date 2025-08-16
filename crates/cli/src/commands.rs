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
use std::{cell::RefCell, env, path::Path, process::exit, rc::Rc};
use typed_ast::{ModuleID, TypedProgramTree};
use utils::fs::ensure_output_dir;

fn get_program_trees(
    options: &CompilerOptions,
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

    let final_program_trees: Vec<(String, ModuleFilePath, ModuleID, Rc<RefCell<TypedProgramTree>>)>;
    let program_trees = resolver.program_trees.lock().unwrap();

    {
        for (_, _, _, typed_program_tree) in program_trees.iter() {
            let mut typed_program_tree_borrowed = typed_program_tree.borrow_mut();
            let mut analyzer = AnalysisContext::new(&resolver, module_id, &mut typed_program_tree_borrowed);
            analyzer.analyze();
            if analyzer.reporter.has_errors() {
                DiagReporter::display(&analyzer.reporter);
                exit(1);
            }
        }
    }
    final_program_trees = program_trees.clone();
    drop(program_trees);

    (final_program_trees, Rc::new(resolver))
}

pub(crate) fn command_run(options: CompilerOptions, file_path: Option<String>) {
    let opts = options.to_compiler_options();
    let file_path = get_entry_source_code_path(options.base_path.clone(), file_path);
    let final_build_dir = get_final_build_directory_path(options.base_path.clone(), opts.build_dir.clone());
    ensure_build_dir_subs(options.base_path.clone(), final_build_dir.clone());
    let (program_trees, resolver_rc) = get_program_trees(&options, file_path);

    let mut temp = env::temp_dir();
    temp.push("path");
    let temp_file_path = temp.to_str().unwrap().to_string();

    let context = CodeGenContext::new(
        final_build_dir,
        opts,
        OutputKind::Executable(temp_file_path.clone()),
        resolver_rc,
    );
    context.compile_modules(program_trees);

    if temp.exists() {
        std::fs::remove_file(temp_file_path).unwrap();
    }
}

pub(crate) fn command_emit_llvm(options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    let opts = options.to_compiler_options();
    let file_path = get_entry_source_code_path(options.base_path.clone(), file_path);
    let final_build_dir = get_final_build_directory_path(options.base_path.clone(), opts.build_dir.clone());
    ensure_build_dir_subs(options.base_path.clone(), final_build_dir.clone());
    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output directory must be specified to generate llvm-ir.".to_string());
    });

    ensure_output_dir(output_path.clone());
    let (program_trees, resolver_rc) = get_program_trees(&options, file_path);

    let context = CodeGenContext::new(final_build_dir, opts, OutputKind::LlvmIr(output_path), resolver_rc);
    context.compile_modules(program_trees);
}

pub(crate) fn command_emit_bytecode(options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    let opts = options.to_compiler_options();
    let file_path = get_entry_source_code_path(options.base_path.clone(), file_path);
    let final_build_dir = get_final_build_directory_path(options.base_path.clone(), opts.build_dir.clone());
    ensure_build_dir_subs(options.base_path.clone(), final_build_dir.clone());
    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output directory must be specified to generate bytecode.".to_string());
    });

    if let Some(build_dir) = &options.build_dir {
        dbg!(build_dir.clone());
        ensure_output_dir(build_dir.clone());
    }

    ensure_output_dir(output_path.clone());
    let (program_trees, resolver_rc) = get_program_trees(&options, file_path);

    let context = CodeGenContext::new(final_build_dir, opts, OutputKind::ByteCode(output_path), resolver_rc);
    context.compile_modules(program_trees);
}

pub(crate) fn command_emit_asm(options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
}

pub(crate) fn command_build(options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
}

pub(crate) fn command_object(options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
}

pub(crate) fn command_dylib(options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
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

pub(crate) fn command_syntactic_only(file_path: String) {
    // let (file_content, file_name) = read_file(file_path.clone());
    // let mut lexer = Lexer::new(file_content, file_name);

    // let program_tree = match CyrusParser::new(&mut lexer).parse() {
    //     Ok(node) => node.as_program(),
    //     Err(errors) => {
    //         for err in errors {
    //             err.print();
    //         }
    //     }
    // };

    // let resolver = Resolver::new();
    // let module_id = generate_module_id();
    // let typed_program_tree = resolver.resolve_module(module_id, program_tree);

    // dbg!(typed_program_tree);

    // println!("Program is correct grammatically.");
}

fn ensure_build_dir_subs(base_path: Option<String>, build_dir_path: String) {
    let base_path = base_path.unwrap_or(String::new());

    ensure_output_dir(format!("{}/{}/{}", base_path, build_dir_path, SOURCES_DIR_PATH));
    ensure_output_dir(format!("{}/{}/{}", base_path, build_dir_path, OBJECTS_FILENAME));
    ensure_output_dir(format!("{}/{}/{}", base_path, build_dir_path, OUTPUT_FILENAME));
}

fn get_final_build_directory_path(base_path: Option<String>, build_dir: BuildDir) -> String {
    let base_path = base_path.unwrap_or(String::new());

    match build_dir {
        BuildDir::Default => {
            let project_file = env::current_dir().unwrap().join(&base_path).join(PROJECT_FILE_PATH);

            if project_file.exists() {
                let build_dir = CodeGenOptions::read_toml(project_file.to_str().unwrap().to_string())
                    .unwrap_or_else(|err| {
                        display_single_custom_diag!(err);
                    })
                    .build_dir;

                match build_dir {
                    BuildDir::Default => env::temp_dir().to_str().unwrap().to_string(),
                    BuildDir::Provided(provided_path) => {
                        ensure_output_dir(format!("{}/{}", base_path, provided_path));
                        provided_path
                    }
                }
            } else {
                env::temp_dir().to_str().unwrap().to_string()
            }
        }
        BuildDir::Provided(provided_path) => provided_path,
    }
}

fn get_entry_source_code_path(base_path: Option<String>, input_file_path: Option<String>) -> String {
    input_file_path.clone().unwrap_or({
        let base_path = base_path.unwrap_or(String::new());

        let file_path_str = {
            if let Some(file_path) = &input_file_path {
                file_path.clone()
            } else {
                let main_file = env::current_dir().unwrap().join(&base_path).join("src/main.cyr");
                main_file.to_str().unwrap().to_string()
            }
        };

        let file_path = Path::new(&file_path_str);

        if !file_path.exists() {
            display_single_custom_diag!("Project.toml not found in the current directory.".to_string());
        }

        file_path_str
    })
}
