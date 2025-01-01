use ast::ast::*;
use ast::{
    ast::{Function, If, Return, Statement, Variable},
    token::TokenKind,
};
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::ffi::{CStr, CString};
use std::path::is_separator;
use std::slice;
use std::str::from_utf8_unchecked;

use crate::types::cstr;
use crate::{compiler_error, AllocTable, Compiler};

impl Compiler {
    pub fn compile_statement(&mut self, statement: Statement) -> Option<LLVMValueRef> {
        match statement {
            Statement::Variable(variable) => self.compile_variable_statement(variable),
            Statement::Expression(expression) => self.compile_expressions(expression),
            Statement::If(if_statement) => self.compile_if_statement(if_statement),
            Statement::Return(ret) => self.compile_return(ret),
            Statement::Function(function) => self.compile_function(function),
            Statement::For(_) => todo!(),
            Statement::Match(_) => todo!(),
            Statement::Struct(_) => todo!(),
            Statement::Package(package) => todo!(),
            Statement::Import(import) => todo!(),
        }
    }

    pub fn compile_if_statement(&mut self, if_statement: If) -> Option<LLVMValueRef> {
        // ANCHOR
        todo!();

        // unsafe  {
        //     LLVMBuildCondBr(self.builder, If, Then, Else)
        // }
    }

    pub fn compile_return(&mut self, ret: Return) -> Option<LLVMValueRef> {
        if let Some(result) = self.compile_expressions(ret.argument) {
            unsafe {
                LLVMBuildRet(self.builder, result);
            }

            return Some(result);
        } else {
            return Some(unsafe { self.null_const(self.i32_type()) });
        }
    }

    pub fn compile_function(&mut self, function: Function) -> Option<LLVMValueRef> {
        let name = CString::new(function.name).unwrap();

        if let Some(return_type) = function.return_type {
            let ret_type = self.determine_llvm_type(return_type.kind);
            let mut param_types = vec![];

            for param in function.params {
                if let Some(token_kind) = param.ty {
                    param_types.push(self.determine_llvm_type(token_kind));
                }
            }

            let func_type = unsafe { LLVMFunctionType(ret_type, param_types.as_mut_ptr(), param_types.len() as u32, 0) };

            let func = unsafe { LLVMAddFunction(self.module, name.as_ptr(), func_type) };

            let entry_name = CString::new("entry").unwrap();
            let entry_block = unsafe { LLVMAppendBasicBlockInContext(self.context, func, entry_name.as_ptr()) };

            unsafe { LLVMPositionBuilderAtEnd(self.builder, entry_block) };

            self.compile_block_statements(function.body.body);

            return Some(func);
        } else {
            compiler_error!("Dynamic return type in function declaration not supported yet.");
        }
    }

    // ANCHOR - REFACTOR
    pub fn determine_llvm_type(&mut self, token_kind: TokenKind) -> LLVMTypeRef {
        let result = match token_kind {
            TokenKind::I32 => unsafe { LLVMInt32Type() },
            TokenKind::I64 => unsafe { LLVMInt64Type() },
            TokenKind::U32 => unsafe { LLVMInt32Type() },
            TokenKind::U64 => unsafe { LLVMInt32Type() },
            TokenKind::F32 => unsafe { LLVMFloatType() },
            TokenKind::F64 => unsafe { LLVMFloatType() },
            _ => compiler_error!("Unsupported type."),
        };

        result
    }

    pub fn compile_identifer_expression(&mut self, identifier: Identifier) -> Option<LLVMValueRef> {
        if let Some(variable) = self.alloc_table.lock().unwrap().get(&identifier.name) {
            let name = CString::new(format!("loaded_{}", identifier.name)).unwrap();

            match unsafe { LLVMGetTypeKind(variable.ty) } {
                llvm_sys::LLVMTypeKind::LLVMArrayTypeKind => {
                    let mut indices = [unsafe { LLVMConstInt(LLVMInt32TypeInContext(self.context), 0, 0) }];

                    unsafe {
                        let gep = LLVMBuildGEP2(self.builder, variable.ty, variable.alloc, indices.as_mut_ptr(), 1, name.as_ptr());

                        return Some(gep);
                    }
                }
                _ => {
                    let value = unsafe { LLVMBuildLoad2(self.builder, self.string_type(), variable.alloc, name.as_ptr() as *const i8) };
                    return Some(value);
                }
            }
        } else {
            compiler_error!(format!("Variable '{}' not defined in current scope or module.", identifier.name));
        }
    }

  
    // ANCHOR - Code style refactor required
    pub fn compile_variable_statement(&mut self, variable: Variable) -> Option<LLVMValueRef> {
        let name = CString::new(variable.name.clone()).unwrap();

        if let Some(expr) = self.compile_expressions(variable.expr.clone()) {
            match variable.ty {
                Some(ty) => {
                    match ty {
                        TokenKind::String => {
                            unsafe {
                                let str_ptr = LLVMPrintValueToString(expr);
                                let str_len = CStr::from_ptr(str_ptr).to_str().unwrap().len();
                                let str = from_utf8_unchecked(slice::from_raw_parts(str_ptr as *const u8, 14));

                                dbg!(str);
                                dbg!(str_len);

                                let variable_name = CString::new(format!("{}", variable.name)).unwrap();

                                let format_str_val = LLVMConstStringInContext(self.context, str_ptr, str_len as u32, 0);
                                let ty = LLVMArrayType2(LLVMInt8TypeInContext(self.context), str_len as u64);

                                let format_str_global = LLVMAddGlobal(self.module, ty, variable_name.as_ptr() as *const _);

                                LLVMSetInitializer(format_str_global, format_str_val);

                                self.alloc_table.lock().unwrap().insert(
                                    variable.name,
                                    AllocTable {
                                        alloc: format_str_global,
                                        ty,
                                    },
                                );
                            }

                            return None;
                        }
                        TokenKind::I32 | TokenKind::I64 | TokenKind::U32 | TokenKind::U64 | TokenKind::F32 | TokenKind::F64 => {
                            let var_type: (LLVMTypeRef, bool) = unsafe {
                                match ty {
                                    TokenKind::I32 => (LLVMInt32Type(), true),
                                    TokenKind::U32 => (LLVMInt32Type(), false),
                                    TokenKind::I64 => (LLVMInt64Type(), true),
                                    TokenKind::U64 => (LLVMInt64Type(), false),
                                    TokenKind::F32 => (LLVMFloatType(), false),
                                    TokenKind::F64 => (LLVMFloatType(), false),
                                    _ => compiler_error!("Invalid variable type given in variable assignment."),
                                }
                            };

                            let alloc = unsafe { LLVMBuildAlloca(self.builder, var_type.0, name.as_ptr()) };

                            unsafe { LLVMBuildStore(self.builder, expr, alloc) };

                            self.alloc_table
                                .lock()
                                .unwrap()
                                .insert(variable.name.clone(), AllocTable { alloc, ty: var_type.0 });
                        }
                        _ => compiler_error!("invalid variable type given in variable assignment"),
                    }

                    return None;
                }
                None => compiler_error!("dynamic variable allocation not supported yet"),
            }
        }

        None
    }

    pub fn compile_block_statements(&mut self, statements: Vec<Statement>) -> Option<LLVMValueRef> {
        let void_type = unsafe { LLVMVoidTypeInContext(self.context) };
        let last_result: LLVMValueRef = unsafe { LLVMGetUndef(void_type) };

        for statement in statements {
            self.compile_statement(statement.clone());
        }

        return Some(last_result);
    }

    pub fn compile_expressions(&mut self, expression: Expression) -> Option<LLVMValueRef> {
        match expression {
            Expression::Identifier(identifier) => self.compile_identifer_expression(identifier),
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Prefix(unary_expression) => self.compile_prefix_expression(unary_expression),
            Expression::Infix(binary_expression) => self.compile_infix_expression(binary_expression),
            Expression::FunctionCall(function_call) => self.compile_function_call(function_call),
            Expression::UnaryOperator(unary_operator) => self.compile_unary_operator(unary_operator),
        }
    }

    pub fn compile_function_call(&mut self, function_call: FunctionCall) -> Option<LLVMValueRef> {
        let func_name = CString::new(function_call.function_name.name.clone()).unwrap();

        let mut arguments: Vec<LLVMValueRef> = Vec::new();

        for expr in function_call.arguments {
            if let Some(value) = self.compile_expressions(expr) {
                arguments.push(value);
            }
        }

        let result = match function_call.function_name.name.as_str() {
            "printf" => self.builtin_printf_func(&arguments),
            _ => {
                let func = unsafe { LLVMGetNamedFunction(self.module, func_name.as_ptr()) };

                if func.is_null() {
                    compiler_error!(format!("Function '{}' not defined in current module.", func_name.to_str().unwrap()));
                }

                let func_type = unsafe { LLVMTypeOf(func) };

                unsafe { LLVMBuildCall2(self.builder, func_type, func, arguments.as_mut_ptr(), arguments.len() as u32, cstr("")) }
            }
        };

        Some(result)
    }

    // TODO
    pub fn compile_unary_operator(&mut self, unary_operator: UnaryOperator) -> Option<LLVMValueRef> {
        match unary_operator.ty {
            UnaryOperatorType::PreIncrement => todo!(),
            UnaryOperatorType::PreDecrement => todo!(),
            UnaryOperatorType::PostIncrement => todo!(),
            UnaryOperatorType::PostDecrement => todo!(),
        }
    }

    pub fn compile_infix_expression(&mut self, binary_expression: BinaryExpression) -> Option<LLVMValueRef> {
        if let Some(lhs) = self.compile_expressions(*binary_expression.left) {
            if let Some(rhs) = self.compile_expressions(*binary_expression.right) {
                let val: LLVMValueRef;

                match binary_expression.operator.kind {
                    TokenKind::Plus => {
                        let name = CString::new("add").unwrap();
                        val = unsafe { LLVMBuildAdd(self.builder, lhs, rhs, name.as_ptr()) };
                    }
                    TokenKind::Minus => {
                        let name = CString::new("sub").unwrap();
                        val = unsafe { LLVMBuildSub(self.builder, lhs, rhs, name.as_ptr()) };
                    }
                    TokenKind::Asterisk => {
                        let name: CString = CString::new("mul").unwrap();
                        val = unsafe { LLVMBuildMul(self.builder, lhs, rhs, name.as_ptr()) };
                    }
                    TokenKind::Slash => {
                        let name = CString::new("dev").unwrap();
                        val = unsafe { LLVMBuildSDiv(self.builder, lhs, rhs, name.as_ptr()) };
                    }
                    // TODO - Impl the other ops
                    _ => compiler_error!("Invalid operation for infix expression."),
                }

                return Some(val);
            }
        }

        None
    }

    pub fn compile_prefix_expression(&mut self, unary_expression: UnaryExpression) -> Option<LLVMValueRef> {
        match unary_expression.operator.kind {
            TokenKind::Bang => {
                let name = CString::new("not").unwrap();
                if let Some(value) = self.compile_expressions(*unary_expression.operand) {
                    return Some(unsafe { LLVMBuildNot(self.builder, value, name.as_ptr()) });
                } else {
                    compiler_error!("Bang prefix operator used for invalid expression.");
                }
            }
            TokenKind::Minus => {
                let name = CString::new("neg").unwrap();

                if let Some(value) = self.compile_expressions(*unary_expression.operand) {
                    return Some(unsafe { LLVMBuildNeg(self.builder, value, name.as_ptr()) });
                } else {
                    compiler_error!("Minus prefix operator used for invalid expression.");
                }
            }
            _ => compiler_error!(format!("Unknown operator for unary expression: {}", unary_expression.operator.kind)),
        }
    }

    pub fn compile_literal(&mut self, literal: Literal) -> Option<LLVMValueRef> {
        let val: LLVMValueRef;

        match literal {
            Literal::Integer(integer_literal) => match integer_literal {
                IntegerLiteral::I32(value) => unsafe {
                    val = self.i32_const(value);
                },
                IntegerLiteral::I64(value) => unsafe {
                    val = self.i64_const(value);
                },
                IntegerLiteral::U32(value) => unsafe {
                    val = self.i32_const(value.try_into().unwrap());
                },
                IntegerLiteral::U64(value) => unsafe {
                    val = self.i64_const(value.try_into().unwrap());
                },
            },
            Literal::Float(float_literal) => unsafe {
                match float_literal {
                    FloatLiteral::F32(value) => {
                        val = self.float_const(value.into());
                    }
                    FloatLiteral::F64(value) => {
                        val = self.float_const(value);
                    }
                }
            },
            Literal::Boolean(boolean) => unsafe {
                let ty = LLVMInt1Type();
                let llvm_bool_val: u64 = if boolean.raw == true { 1 } else { 0 };
                val = LLVMConstInt(ty, llvm_bool_val, 0)
            },
            Literal::String(string_type) => unsafe {
                let value: *const i8 = string_type.raw.as_ptr() as *const i8;
                let length: u32 = string_type.raw.len().try_into().unwrap();
                val = LLVMConstStringInContext(self.context, value, length, 0);
            },
        }

        Some(val)
    }
}
