use std::env;

use lexer::Lexer;
use utils::fs::read_file;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let file_content = read_file(file_path.clone()).0;

    let mut lexer = Lexer::new(file_content, file_path);
    let tokens = lexer.tokenize();

    let mut peekable = tokens.iter().peekable();
    
    // dbg!(tokens.clone());
}
