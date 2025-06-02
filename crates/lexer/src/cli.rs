use std::env;

use lexer::{new_lexer_debugger};
use utils::fs::read_file;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = args[1].clone();
    let file_content = read_file(file_path.clone()).0;
    
    new_lexer_debugger(file_content);
}
