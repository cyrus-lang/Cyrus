use cyrusc::{
    compile,
    print_llvm_module
};

use ast::ast::*;


pub fn main() {
    // fs::read_to_string("");
    
    // let code = "1";
    // let node =  parser::Parser::parse(code.to_string()).unwrap();

    let node = Node::Expression(
        Expression::Literal(Literal::Integer(
            IntegerLiteral::I32(10)
        ))
    );

    unsafe {
        let module = compile(node, "sample\0");
        print_llvm_module(module);
    }
}