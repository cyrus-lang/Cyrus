use std::{any::Any, ops::DerefMut, os::raw::c_char, result};

use ast::{
    ast::{Statement, Variable},
    token::TokenKind,
};
use llvm_sys::{
    core::{
        LLVMBuildAlloca, LLVMBuildStore, LLVMConstNull, LLVMConstPointerNull, LLVMGetUndef,
        LLVMInt32Type, LLVMVoidTypeInContext,
    },
    prelude::{LLVMTypeRef, LLVMValueRef},
};

use crate::{CompileResult, Compiler};

impl Compiler {
    pub fn compile_statement(&mut self, statement: Statement) -> CompileResult<LLVMValueRef> {
        match statement {
            Statement::Variable(variable) => self.compile_variable_statement(variable),
            Statement::Expression(expression) => self.compile_expressions(expression),
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

    pub fn compile_variable_statement(
        &mut self,
        variable: Variable,
    ) -> CompileResult<LLVMValueRef> {
        let mut varty: LLVMTypeRef;
        let mut var_name: *const c_char;
        let var_value = self.compile_expressions(variable.expr)?;

        unsafe {
            match variable.ty {
                Some(ty) => match ty {
                    TokenKind::I32 => {
                        varty = LLVMInt32Type();
                    }
                    TokenKind::I64 => {
                        todo!()
                    }
                    TokenKind::U32 => {
                        todo!()
                    }
                    TokenKind::U64 => {
                        todo!()
                    }
                    TokenKind::F32 => {
                        todo!()
                    }
                    TokenKind::F64 => {
                        todo!()
                    }
                    TokenKind::String => {
                        todo!()
                    }
                    _ => return Err("this kind of allocation not supported yet"),
                },
                None => return Err("dynamic allocation not supported yet"),
            }

            match variable.identifier.kind {
                TokenKind::Identifier { name } => {
                    var_name = name.as_str().as_ptr() as *const c_char;
                }
                _ => return Err("invalid token entered for the token"),
            }

            let alloc = LLVMBuildAlloca(self.builder, varty, var_name);
            LLVMBuildStore(self.builder, alloc, var_value);
        }

        Ok(var_value)
    }

    pub fn compile_block_statements(
        &mut self,
        statements: Vec<Statement>,
    ) -> CompileResult<LLVMValueRef> {
        unsafe {
            let void_type = LLVMVoidTypeInContext(self.context);
            let mut last_result: LLVMValueRef = LLVMGetUndef(void_type);

            for statement in statements {
                last_result = self.compile_statement(statement.clone())?; // REVIEW - consider to remove clone
            }

            Ok(last_result)
        }
    }
}
