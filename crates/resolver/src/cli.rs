use lexer::Lexer;
use parser::Parser;
use resolver::{Resolver, generate_module_id};
use std::{env, process::exit};
use utils::fs::read_file;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let file_content = read_file(file_path.clone()).0;
    let mut lexer = Lexer::new(file_content, file_path.clone());
    let mut parser = Parser::new(&mut lexer);

    match parser.parse() {
        Ok(node) => {
            let mut resolver = Resolver::new(file_path);
            let module_id = generate_module_id();
            let typed_program_tree = resolver.resolve_module(module_id, node.as_program());
            if resolver.reporter.has_errors() {
                resolver.reporter.display();
                exit(1);
            }

            dbg!(typed_program_tree);
        }
        Err(errors) => {
            parser.display_parser_errors(errors.clone());
            exit(1);
        }
    }
}
