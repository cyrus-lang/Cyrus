use crate::CompilerOptions;
use ast::token::TokenKind;
use codegen::{context::context::CodeGenContext, options::OutputKind};
use diagcentral::{display_single_custom_diag, reporter::DiagReporter};
use lexer::Lexer;
use parser::Parser;
use resolver::{Resolver, Visiting, generate_module_id, moduleloader::ModuleLoaderOptions};
use static_analyzer::context::AnalysisContext;
use std::{cell::RefCell, env, process::exit, rc::Rc};
use typed_ast::{ModuleID, TypedProgramTree};

fn get_program_trees(
    options: &CompilerOptions,
    file_path: String,
) -> (Vec<(String, ModuleID, Rc<RefCell<TypedProgramTree>>)>, Rc<Resolver>) {
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

    let final_program_trees: Vec<(String, ModuleID, Rc<RefCell<TypedProgramTree>>)>;
    let program_trees = resolver.program_trees.lock().unwrap();

    {
        for (_, _, typed_program_tree) in program_trees.iter() {
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
    let (program_trees, resolver_rc) = get_program_trees(&options, file_path.unwrap());

    let context = CodeGenContext::new(options.to_compiler_options(), OutputKind::None, resolver_rc);
    context.compile_modules(program_trees);

    let mut temp = env::temp_dir();
    temp.push("path");
    let temp_file_path = temp.to_str().unwrap().to_string();

    context.emit_exec(temp_file_path.clone());
    if temp.exists() {
        std::fs::remove_file(temp_file_path).unwrap();
    }
}

pub(crate) fn command_emit_llvm(options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    let output_path = output_path.unwrap_or_else(|| {
        display_single_custom_diag!("Output directory must be specified to generate llvm-ir.".to_string());
    });

    let (program_trees, resolver_rc) = get_program_trees(&options, file_path.unwrap());

    let context = CodeGenContext::new(
        options.to_compiler_options(),
        OutputKind::LlvmIr(output_path),
        resolver_rc,
    );
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
