use std::env;

use lexer::Lexer;
use utils::fs::read_file;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let file_content = read_file(file_path).0;
    let lexer = Lexer::new(file_content);

    for token in lexer {
        println!("{:?}", token.kind);
    }
}
