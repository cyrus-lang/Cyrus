use std::env;

use lexer::Lexer;
use parser::Parser;
use utils::fs::read_file;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let file_content = read_file(file_path.clone()).0;
    let mut lexer = Lexer::new(file_content, file_path);

    match Parser::new(&mut lexer).parse() {
        Ok(result) => println!("{:#?}", result),
        Err(errors) => {
            for err in errors {
                println!("{}", err);
            }
        }
    }
}
