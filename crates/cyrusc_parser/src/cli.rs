use fs_utils::read_file;
use lexer::Lexer;
use parser::Parser;
use std::env;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let file_content = read_file(file_path.clone()).0;
    let mut lexer = Lexer::new(file_content, file_path.clone());

    let mut parser = Parser::new(lexer.tokenize(), file_path);

    match parser.parse() {
        Ok(result) => println!("{:#?}", result),
        Err(errors) => {
            parser.display_parser_errors(errors);
        }
    }
}
