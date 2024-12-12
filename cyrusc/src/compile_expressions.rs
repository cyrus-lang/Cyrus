use std::ffi::CStr;
use std::ffi::CString;
use std::os::raw::c_char;

use ast::ast::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

pub fn compile_expressions(expression: Expression) -> LLVMValueRef {
    match expression {
        Expression::Identifier(identifier) => todo!(),
        Expression::Literal(literal) => compile_literal(literal),
        Expression::Prefix(unary_expression) => todo!(),
        Expression::Infix(binary_expression) => todo!(),
        Expression::FunctionCall(function_call) => todo!(),
        Expression::UnaryOperator(unary_operator) => todo!(),
    }
}

pub fn compile_literal(literal: Literal) -> LLVMValueRef {
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

    return val;
}
