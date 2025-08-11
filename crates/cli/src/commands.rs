use ast::token::TokenKind;
use lexer::Lexer;

use crate::CompilerOptions;

pub(crate) fn command_run(compiler_options: CompilerOptions, file_path: Option<String>) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
}

pub(crate) fn command_emit_llvm(
    compiler_options: CompilerOptions,
    file_path: Option<String>,
    output_path: Option<String>,
) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
}

pub(crate) fn command_emit_asm(
    compiler_options: CompilerOptions,
    file_path: Option<String>,
    output_path: Option<String>,
) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
}

pub(crate) fn command_build(compiler_options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
}

pub(crate) fn command_object(
    compiler_options: CompilerOptions,
    file_path: Option<String>,
    output_path: Option<String>,
) {
    // let context = CodeGenContext::new(options, output_kind);
    todo!()
}

pub(crate) fn command_dylib(compiler_options: CompilerOptions, file_path: Option<String>, output_path: Option<String>) {
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
    // let (file_content, file_name) = read_file(file_path.clone());
    // let mut lexer = Lexer::new(file_content, file_name);

    // match CyrusParser::new(&mut lexer).parse() {
    //     Ok(result) => println!("{:#?}", result),
    //     Err(errors) => {
    //         for err in errors {
    //             err.print();
    //         }
    //     }
    // }
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
