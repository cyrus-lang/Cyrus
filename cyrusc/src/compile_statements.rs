use std::{any::Any, ops::DerefMut, os::raw::c_char, result};

use ast::{
    ast::{Expression, Function, Literal, Statement, Variable},
    token::TokenKind,
};
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::{CompileResult, Compiler};

impl Compiler {
    pub fn compile_statement(&mut self, statement: Statement) -> CompileResult<LLVMValueRef> {
        match statement {
            Statement::Variable(variable) => self.compile_variable_statement(variable),
            Statement::Expression(expression) => self.compile_expressions(expression),
            Statement::If(_) => todo!(),
            Statement::Return(_) => todo!(),
            Statement::Function(function) => self.compile_function(function),
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Package(package) => todo!(),
            Statement::Import(import) => todo!(),
        }
    }

    pub fn compile_function(&mut self, function: Function) -> CompileResult<LLVMValueRef> {
        todo!()
    }

    pub fn compile_variable_statement(
        &mut self,
        variable: Variable,
    ) -> CompileResult<LLVMValueRef> {
        let mut var_type: LLVMTypeRef;
        let var_value = self.compile_expressions(variable.expr.clone())?;

        match variable.identifier.kind {
            TokenKind::Identifier { name } => {
                unsafe {
                    match variable.ty {
                        Some(ty) => match ty {
                            TokenKind::I32 => {
                                var_type = LLVMInt32Type();
                            }
                            TokenKind::I64 => {
                                var_type = LLVMInt64Type();
                            }
                            TokenKind::U32 => {
                                var_type = LLVMInt32Type();
                            }
                            TokenKind::U64 => {
                                var_type = LLVMInt32Type();
                            }
                            TokenKind::F32 => {
                                var_type = LLVMFloatType();
                            }
                            TokenKind::F64 => {
                                var_type = LLVMFloatType();
                            }
                            TokenKind::String => {
                                let i8_type = LLVMInt8Type();
                                let len: u64;
                                let value: String;

                                if let Expression::Literal(literal) = variable.expr {
                                    if let Literal::String(string_value) = literal {
                                        value = string_value.raw.clone();

                                        // +1 for null terminator
                                        len = (string_value.raw.len() + 1).try_into().unwrap();
                                    } else {
                                        return Err("invalid literal set for the string variable");
                                    }
                                } else {
                                    return Err("invalid expression set for the string variable");
                                }

                                let array_type = LLVMArrayType2(i8_type, len);
                                let alloc = LLVMBuildAlloca(
                                    self.builder,
                                    array_type,
                                    name.as_ptr() as *const c_char,
                                );

                                for (i, b) in value.bytes().enumerate() {
                                    let index = LLVMConstInt(
                                        LLVMInt32TypeInContext(self.context),
                                        i as u64,
                                        0,
                                    );

                                    let element_ptr = LLVMBuildGEP2(
                                        self.builder,
                                        array_type,
                                        alloc,
                                        [index].as_mut_ptr(),
                                        1,
                                        format!("{}_element_{}", name, i).as_ptr() as *const i8,
                                    );

                                    let byte_value = LLVMConstInt(i8_type, b as u64, 0);
                                    LLVMBuildStore(self.builder, byte_value, element_ptr);
                                }

                                // Null-terminate the string
                                let null_index = LLVMConstInt(
                                    LLVMInt32TypeInContext(self.context),
                                    len as u64 - 1,
                                    0,
                                );

                                let null_ptr = LLVMBuildGEP2(
                                    self.builder,
                                    array_type,
                                    alloc,
                                    [null_index].as_mut_ptr(),
                                    1,
                                    "null_terminator".as_ptr() as *const i8,
                                );
                                let null_value = LLVMConstInt(i8_type, 0, 0);
                                LLVMBuildStore(self.builder, null_value, null_ptr);

                                return Ok(alloc);
                            }
                            _ => return Err("this kind of allocation not supported yet"),
                        },
                        None => return Err("dynamic allocation not supported yet"),
                    }

                    let alloc =
                        LLVMBuildAlloca(self.builder, var_type, name.as_ptr() as *const c_char);
                    LLVMBuildStore(self.builder, alloc, var_value);
                }

                return Ok(var_value);
            }
            _ => return Err("invalid token entered for the variable name"),
        }
    }

    pub fn compile_block_statements(
        &mut self,
        statements: Vec<Statement>,
    ) -> CompileResult<LLVMValueRef> {
        unsafe {
            let void_type = LLVMVoidTypeInContext(self.context);
            let mut last_result: LLVMValueRef = LLVMGetUndef(void_type);

            for statement in statements {
                self.compile_statement(statement.clone())?; // REVIEW - consider to remove clone
            }

            Ok(last_result)
        }
    }
}
