use llvm_sys::prelude::*;
use ast::ast::*;
use compile_expressions::*;

pub mod compile_expressions;
mod compile_expressions_test;

pub fn compile(node: Node) -> LLVMValueRef {
    match node {
        Node::Program(program) => todo!(),
        Node::Statement(statement) => todo!(),
        Node::Expression(expression) => compile_expressions(expression),
    }
}

