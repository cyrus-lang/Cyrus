use std::{ffi::CString, os::raw::c_char};

use ast::{
    ast::{Function, Return, Statement, Variable},
    token::TokenKind,
};
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::{compiler_error, undefined_expression_error, AllocTable, Compiler};

impl Compiler {
    pub fn compile_statement(&mut self, statement: Statement) -> Option<LLVMValueRef> {
        match statement {
            Statement::Variable(variable) => self.compile_variable_statement(variable),
            Statement::Expression(expression) => self.compile_expressions(expression),
            Statement::If(_) => todo!(),
            Statement::Return(ret) => self.compile_return(ret),
            Statement::Function(function) => self.compile_function(function),
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Package(package) => todo!(),
            Statement::Import(import) => todo!(),
        }
    }

    pub fn compile_return(&mut self, ret: Return) -> Option<LLVMValueRef> {
        if let Some(result) = self.compile_expressions(ret.argument) {
            unsafe {
                LLVMBuildRet(self.builder, result);
            }

            return Some(result);
        } else {
            compiler_error!("")
        }
    }

    pub fn compile_function(&mut self, function: Function) -> Option<LLVMValueRef> {
        let name = function.name.as_ptr() as *const c_char;

        unsafe {
            if let Some(return_type) = function.return_type {
                let ret_type = self.determine_llvm_type(return_type.kind);
                let mut param_types = vec![];

                for param in function.params {
                    if let Some(token_kind) = param.ty {
                        param_types.push(self.determine_llvm_type(token_kind));
                    }
                }

                let func_type = LLVMFunctionType(
                    ret_type,
                    param_types.as_mut_ptr(),
                    param_types.len() as u32,
                    0,
                );

                let func = LLVMAddFunction(self.module, name, func_type);
                let entry_name = CString::new("entry").unwrap();
                let entry_block =
                    LLVMAppendBasicBlockInContext(self.context, func, entry_name.as_ptr());
                LLVMPositionBuilderAtEnd(self.builder, entry_block);

                self.compile_block_statements(function.body.body);

                return Some(func);
            } else {
                compiler_error!("dynamic return type in function declaration not supported yet");
            }
        }
    }

    pub unsafe fn determine_llvm_type(&mut self, token_kind: TokenKind) -> LLVMTypeRef {
        let result = match token_kind {
            TokenKind::I32 => LLVMInt32Type(),
            TokenKind::I64 => LLVMInt64Type(),
            TokenKind::U32 => LLVMInt32Type(),
            TokenKind::U64 => LLVMInt32Type(),
            TokenKind::F32 => LLVMFloatType(),
            TokenKind::F64 => LLVMFloatType(),
            _ => compiler_error!("unsupported type passed to determine_llvm_type method"),
        };

        result
    }

    pub fn compile_variable_statement(&mut self, variable: Variable) -> Option<LLVMValueRef> {
        if let Some(expr) = self.compile_expressions(variable.expr.clone()) {
            match variable.ty {
                Some(ty) => unsafe {
                    match ty {
                        TokenKind::String => {
                            let name = variable.name.as_ptr() as *const i8;

                            let char_ptr_type =
                                LLVMPointerType(LLVMInt8TypeInContext(self.context), 0);

                            let alloc = LLVMBuildAlloca(self.builder, char_ptr_type, name);

                            LLVMBuildStore(self.builder, expr, alloc);

                            self.alloc_table.lock().unwrap().insert(
                                variable.name,
                                AllocTable {
                                    alloc,
                                    ty: char_ptr_type,
                                },
                            );

                            return None;
                        }
                        TokenKind::I32
                        | TokenKind::I64
                        | TokenKind::U32
                        | TokenKind::U64
                        | TokenKind::F32
                        | TokenKind::F64 => {
                            let var_type: (LLVMTypeRef, bool) = match ty {
                                TokenKind::I32 => (LLVMInt32Type(), true),
                                TokenKind::U32 => (LLVMInt32Type(), false),
                                TokenKind::I64 => (LLVMInt64Type(), true),
                                TokenKind::U64 => (LLVMInt64Type(), false),
                                TokenKind::F32 => (LLVMFloatType(), false),
                                TokenKind::F64 => (LLVMFloatType(), false),
                                _ => compiler_error!(
                                    "invalid variable type given in variable assignment"
                                ),
                            };

                            let alloc = LLVMBuildAlloca(
                                self.builder,
                                var_type.0,
                                variable.name.as_ptr() as *const i8,
                            );

                            let store = LLVMBuildStore(self.builder, alloc, expr);
                            let var_align = LLVMGetAlignment(alloc);

                            LLVMSetAlignment(store, var_align);

                            self.alloc_table.lock().unwrap().insert(
                                variable.name,
                                AllocTable {
                                    alloc,
                                    ty: var_type.0,
                                },
                            );
                        }
                        _ => compiler_error!("invalid variable type given in variable assignment"),
                    }

                    return None;
                },
                None => compiler_error!("dynamic variable allocation not supported yet"),
            }
        } else {
            undefined_expression_error!();
        }
    }

    pub fn compile_block_statements(&mut self, statements: Vec<Statement>) -> Option<LLVMValueRef> {
        unsafe {
            let void_type = LLVMVoidTypeInContext(self.context);
            let last_result: LLVMValueRef = LLVMGetUndef(void_type);

            for statement in statements {
                self.compile_statement(statement.clone()); // REVIEW - consider to remove clone
            }

            return Some(last_result);
        }
    }
}
