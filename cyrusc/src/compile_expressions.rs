use crate::allloc::get_alloc_value;
use crate::CompileResult;
use crate::Compiler;
use ast::ast::*;
use ast::token::*;
use core::alloc;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::ffi::CString;
use std::os::raw::c_char;

impl Compiler {
    pub fn compile_expressions(&mut self, expression: Expression) -> CompileResult<LLVMValueRef> {
        match expression {
            Expression::Identifier(identifier) => self.compile_identifer_expression(identifier),
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Prefix(unary_expression) => {
                self.compile_prefix_expression(unary_expression)
            }
            Expression::Infix(binary_expression) => {
                self.compile_infix_expression(binary_expression)
            }
            Expression::FunctionCall(function_call) => todo!(),
            Expression::UnaryOperator(unary_operator) => todo!(),
        }
    }
    pub fn compile_identifer_expression(
        &mut self,
        identifier: Identifier,
    ) -> CompileResult<LLVMValueRef> {
        unsafe {
            let alloc_table = self.alloc_table.lock().unwrap();
            let variable = alloc_table.get(&identifier.name);

            if let Some(variable) = variable {
                let value = get_alloc_value(self.builder, variable.alloc, variable.ty);
                return Ok(value);
            } else {
                return Err("failed to get the variable from alloc_table by it's name");
            }
        }
    }

    pub fn compile_infix_expression(
        &mut self,
        binary_expression: BinaryExpression,
    ) -> CompileResult<LLVMValueRef> {
        let lhs = self.compile_expressions(*binary_expression.left)?;
        let rhs = self.compile_expressions(*binary_expression.right)?;

        let val: LLVMValueRef;

        unsafe {
            match binary_expression.operator.kind {
                TokenKind::Plus => {
                    let name = CString::new("add").unwrap();
                    val = LLVMBuildAdd(self.builder, lhs, rhs, name.as_ptr());
                }
                TokenKind::Minus => {
                    let name = CString::new("sub").unwrap();
                    val = LLVMBuildSub(self.builder, lhs, rhs, name.as_ptr());
                }
                TokenKind::Asterisk => {
                    let name: CString = CString::new("mul").unwrap();
                    val = LLVMBuildMul(self.builder, lhs, rhs, name.as_ptr());
                }
                TokenKind::Slash => {
                    let name = CString::new("dev").unwrap();
                    val = LLVMBuildSDiv(self.builder, lhs, rhs, name.as_ptr());
                }

                // REVIEW - more operations
                _ => return Err("invalid operation for infix expression"),
            }
        }

        Ok(val)
    }

    pub fn compile_prefix_expression(
        &mut self,
        unary_expression: UnaryExpression,
    ) -> CompileResult<LLVMValueRef> {
        unsafe {
            match unary_expression.operator.kind {
                // REVIEW
                // I guess it requires more attention to see that it works
                // aptly or not. good luck to my future version =))
                TokenKind::Bang => {
                    let name = CString::new("not").unwrap();
                    let value = self.compile_expressions(*unary_expression.operand)?;
                    Ok(LLVMBuildNot(self.builder, value, name.as_ptr()))
                }
                TokenKind::Minus => {
                    let name = CString::new("neg").unwrap();
                    let value = self.compile_expressions(*unary_expression.operand)?;

                    Ok(LLVMBuildNeg(self.builder, value, name.as_ptr()))
                }
                _ => Err("unknown operator for unary expression"),
            }
        }
    }

    pub fn compile_literal(&mut self, literal: Literal) -> CompileResult<LLVMValueRef> {
        let ty: LLVMTypeRef;
        let val: LLVMValueRef;

        match literal {
            Literal::Integer(integer_literal) => match integer_literal {
                IntegerLiteral::I32(value) => unsafe {
                    ty = LLVMInt32Type();
                    val = LLVMConstInt(ty, (value as i32).try_into().unwrap(), 1);
                },
                IntegerLiteral::I64(value) => unsafe {
                    ty = LLVMInt64Type();
                    val = LLVMConstInt(ty, (value as i64).try_into().unwrap(), 1);
                },
                IntegerLiteral::U32(value) => unsafe {
                    ty = LLVMInt32Type();
                    val = LLVMConstInt(ty, value.into(), 0);
                },
                IntegerLiteral::U64(value) => unsafe {
                    ty = LLVMInt64Type();
                    val = LLVMConstInt(ty, value.into(), 0);
                },
            },
            Literal::Float(float_literal) => unsafe {
                ty = LLVMFloatType();

                match float_literal {
                    FloatLiteral::F32(value) => {
                        val = LLVMConstReal(ty, value.into());
                    }
                    FloatLiteral::F64(value) => {
                        val = LLVMConstReal(ty, value);
                    }
                }
            },
            Literal::Boolean(boolean) => unsafe {
                ty = LLVMInt1Type();

                let llvm_bool_val: u64 = {
                    if boolean.raw == true {
                        1
                    } else {
                        0
                    }
                };

                val = LLVMConstInt(ty, llvm_bool_val, 0)
            },
            Literal::String(string_type) => unsafe {
                let value: *const c_char = string_type.raw.as_ptr() as *const c_char;
                let length: u32 = string_type.raw.len().try_into().unwrap();
                val = LLVMConstString(value, length, 0);
            },
        }

        Ok(val)
    }
}
