use ast::ast::{Statement, Variable};
use llvm_sys::prelude::LLVMValueRef;

pub fn compile_block_statements(statements: &Vec<Statement>) -> LLVMValueRef {
    todo!()
}

pub fn compile_statement(statement: Statement) -> LLVMValueRef {
    match statement {
        Statement::Variable(variable) => todo!(),
        Statement::Expression(expression) => todo!(),
        Statement::If(_) => todo!(),
        Statement::Return(_) => todo!(),
        Statement::Function(function) => todo!(),
        Statement::For(_) => todo!(),
        Statement::Match(_) => todo!(),
        Statement::Struct(_) => todo!(),
        Statement::Package(package) => todo!(),
        Statement::Import(import) => todo!(),
    }
}

pub fn compile_variable_statement(variable: Variable)  {
    
}