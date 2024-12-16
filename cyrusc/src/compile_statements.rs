use std::{
    ffi::{CStr, CString},
    fs::soft_link,
    os::raw::c_char,
};

use ast::{
    ast::{Expression, Function, Literal, Return, Statement, Variable},
    token::TokenKind,
};
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::{CompileErr, CompileResult, Compiler};

impl Compiler {
    pub fn compile_statement(&mut self, statement: Statement) -> CompileResult<LLVMValueRef> {
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

    pub fn compile_return(&mut self, ret: Return) -> CompileResult<LLVMValueRef> {
        let result = self.compile_expressions(ret.argument)?;

        unsafe {
            LLVMBuildRet(self.builder, result);
        }

        Ok(result)
    }

    pub fn compile_function(&mut self, function: Function) -> CompileResult<LLVMValueRef> {
        let name = function.name.as_ptr() as *const i8;

        unsafe {
            if let Some(return_type) = function.return_type {
                let ret_type = self.determine_llvm_type(return_type.kind)?;
                let mut param_types = vec![];

                for param in function.params {
                    if let Some(token_kind) = param.ty {
                        param_types.push(self.determine_llvm_type(token_kind)?);
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

                self.compile_block_statements(function.body.body)?;

                Ok(func)
            } else {
                return Err("dynamic return type in function declaration not supported yet");
            }
        }
    }

    pub unsafe fn determine_llvm_type(
        &mut self,
        token_kind: TokenKind,
    ) -> Result<LLVMTypeRef, CompileErr> {
        let result = match token_kind {
            TokenKind::I32 => LLVMInt32Type(),
            TokenKind::I64 => LLVMInt64Type(),
            TokenKind::U32 => LLVMInt32Type(),
            TokenKind::U64 => LLVMInt32Type(),
            TokenKind::F32 => LLVMFloatType(),
            TokenKind::F64 => LLVMFloatType(),
            _ => return Err("unsupported type passed to determine_llvm_type method"),
        };

        Ok(result)
    }

    pub fn compile_variable_statement(
        &mut self,
        variable: Variable,
    ) -> CompileResult<LLVMValueRef> {
        let alignment: u32 = 4;
        let var_type: LLVMTypeRef;
        let var_value = self.compile_expressions(variable.expr.clone())?;

        match variable.identifier.kind {
            TokenKind::Identifier { name } => {
                unsafe {
                    match variable.ty {
                        Some(ty) => {
                            match ty {
                                TokenKind::I32 => {
                                    var_type = LLVMInt32TypeInContext(self.context);
                                }
                                TokenKind::I64 => {
                                    var_type = LLVMInt64TypeInContext(self.context);
                                }
                                TokenKind::U32 => {
                                    var_type = LLVMInt32TypeInContext(self.context);
                                }
                                TokenKind::U64 => {
                                    var_type = LLVMInt32TypeInContext(self.context);
                                }
                                TokenKind::F32 => {
                                    var_type = LLVMFloatTypeInContext(self.context);
                                }
                                TokenKind::F64 => {
                                    var_type = LLVMFloatTypeInContext(self.context);
                                }
                                TokenKind::String => {
                                    let mut var_value_len: usize = 0;
                                    let var_value_str = LLVMGetAsString(var_value, &mut var_value_len);

                                    // FIXME later
                                    dbg!(CStr::from_ptr(var_value_str).to_str().unwrap()); 
                                    
                                    let string_value = LLVMConstStringInContext(
                                        self.context,
                                        var_value_str,
                                        var_value_len as u32,
                                        0,
                                    );
    
                                    let global_name = "hello".as_ptr() as *const i8;
    
                                    let global_var = LLVMAddGlobal(self.module, LLVMTypeOf(string_value), global_name);
    
                                   return Ok(string_value);
                                }
                                _ => panic!("this kind of allocation not supported yet"),
                            }
                        },
                        None => return Err("dynamic allocation not supported yet"),
                    }

                    let alloc =
                        LLVMBuildAlloca(self.builder, var_type, name.as_ptr() as *const c_char);

                    let store = LLVMBuildStore(self.builder, alloc, var_value);

                    LLVMSetAlignment(store, alignment);

                    self.alloc_table.lock().unwrap().insert(
                        name,
                        crate::AllocTable {
                            alloc,
                            ty: var_type,
                        },
                    );
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
            let last_result: LLVMValueRef = LLVMGetUndef(void_type);

            for statement in statements {
                self.compile_statement(statement.clone())?; // REVIEW - consider to remove clone
            }

            Ok(last_result)
        }
    }
}
