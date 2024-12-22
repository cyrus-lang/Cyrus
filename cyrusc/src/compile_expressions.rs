use crate::compile_builtins::generate_printf;
use crate::compiler_error;
use crate::undefined_expression_error;
use crate::Compiler;
use ast::ast::*;
use ast::token::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::ffi::CString;
use std::os::raw::c_char;

impl Compiler {
    pub fn compile_expressions(&mut self, expression: Expression) -> Option<LLVMValueRef> {
        match expression {
            Expression::Identifier(identifier) => self.compile_identifer_expression(identifier),
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Prefix(unary_expression) => {
                self.compile_prefix_expression(unary_expression)
            }
            Expression::Infix(binary_expression) => {
                self.compile_infix_expression(binary_expression)
            }
            Expression::FunctionCall(function_call) => self.compile_function_call(function_call),
            Expression::UnaryOperator(unary_operator) => {
                self.compile_unary_operator(unary_operator)
            }
        }
    }

    pub fn compile_function_call(&mut self, function_call: FunctionCall) -> Option<LLVMValueRef> {
        let function_name = CString::new(function_call.function_name.name.clone()).unwrap();

        unsafe {
            let mut arguments: Vec<LLVMValueRef> = vec![];

            for expr in function_call.arguments {
                if let Some(value) = self.compile_expressions(expr) {
                    arguments.push(value);
                }
            }

            let result = match function_call.function_name.name.as_str() {
                "printf" => generate_printf(self.module, self.context, self.builder, &arguments),
                _ => {
                    let function = LLVMGetNamedFunction(self.module, function_name.as_ptr());

                    if function.is_null() {
                        compiler_error!(format!(
                            "Function '{}' not defined in current module.",
                            function_name.to_str().unwrap()
                        ));
                    }

                    let call_name = CString::new("call_result").unwrap();

                    LLVMBuildCall2(
                        self.builder,
                        LLVMTypeOf(function),
                        function,
                        arguments.as_mut_ptr(),
                        arguments.len() as u32,
                        call_name.as_ptr(),
                    )
                }
            };

            Some(result)
        }
    }

    // TODO
    pub fn compile_unary_operator(
        &mut self,
        unary_operator: UnaryOperator,
    ) -> Option<LLVMValueRef> {
        match unary_operator.ty {
            UnaryOperatorType::PreIncrement => todo!(),
            UnaryOperatorType::PreDecrement => todo!(),
            UnaryOperatorType::PostIncrement => todo!(),
            UnaryOperatorType::PostDecrement => todo!(),
        }
    }

    pub fn compile_identifer_expression(&mut self, identifier: Identifier) -> Option<LLVMValueRef> {
        if let Some(variable) = self.alloc_table.lock().unwrap().get(&identifier.name) {
            unsafe {
                let name = CString::new(format!("loaded_{}", identifier.name)).unwrap();
                let value = LLVMBuildLoad2(
                    self.builder,
                    variable.ty,
                    variable.alloc,
                    name.as_ptr() as *const i8,
                );

                return Some(value);
            }
        } else {
            compiler_error!("failed to get the variable from alloc_table by it's name");
        }
    }

    pub fn compile_infix_expression(
        &mut self,
        binary_expression: BinaryExpression,
    ) -> Option<LLVMValueRef> {
        if let Some(lhs) = self.compile_expressions(*binary_expression.left) {
            if let Some(rhs) = self.compile_expressions(*binary_expression.right) {
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
                        TokenKind::Equal => {
                            // ANCHOR
                            todo!()
                            // val = LLVMBuilddev
                        }

                        // REVIEW - more operations
                        _ => compiler_error!("invalid operation for infix expression"),
                    }
                }

                return Some(val);
            } else {
                undefined_expression_error!();
            }
        } else {
            undefined_expression_error!();
        }
    }

    pub fn compile_prefix_expression(
        &mut self,
        unary_expression: UnaryExpression,
    ) -> Option<LLVMValueRef> {
        unsafe {
            match unary_expression.operator.kind {
                // REVIEW
                // I guess it requires more attention to see that it works
                // aptly or not. good luck to my future version =))
                TokenKind::Bang => {
                    let name = CString::new("not").unwrap();
                    if let Some(value) = self.compile_expressions(*unary_expression.operand) {
                        return Some(LLVMBuildNot(self.builder, value, name.as_ptr()));
                    } else {
                        compiler_error!("bang prefix operator used for invalid expression");
                    }
                }
                TokenKind::Minus => {
                    let name = CString::new("neg").unwrap();

                    if let Some(value) = self.compile_expressions(*unary_expression.operand) {
                        return Some(LLVMBuildNeg(self.builder, value, name.as_ptr()));
                    } else {
                        compiler_error!("minus prefix operator used for invalid expression");
                    }
                }
                _ => compiler_error!(format!(
                    "unknown operator for unary expression: {}",
                    unary_expression.operator.kind
                )),
            }
        }
    }

    pub fn compile_literal(&mut self, literal: Literal) -> Option<LLVMValueRef> {
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

        Some(val)
    }
}
