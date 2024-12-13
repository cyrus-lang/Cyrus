use cyrusc::{
    compile,
    print_llvm_module
};

use ast::ast::*;
use llvm_sys::core::LLVMDumpValue;


pub fn main() {
    // fs::read_to_string("");
    
    let code = "200 * 10";
    let node =  parser::Parser::parse(code.to_string()).unwrap();

    unsafe {
        let result = compile(node, "sample\0").unwrap();
        print_llvm_module(result.0);
    }
}