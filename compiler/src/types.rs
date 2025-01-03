use ast::token::TokenKind;
use gccjit_sys::*;
use utils::compiler_error;

use crate::Compiler;

impl Compiler {
    pub fn void_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_INT8_T) }
    }

    pub fn i8_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_INT) }
    }

    pub fn i32_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_INT32_T) }
    }

    pub fn i64_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_INT64_T) }
    }

    pub fn u32_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_UINT32_T) }
    }

    pub fn u64_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_UINT64_T) }
    }

    pub fn f32_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_FLOAT32) }
    }

    pub fn f64_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_VOID) }
    }

    pub fn string_type(&self) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(self.context, gcc_jit_types::GCC_JIT_TYPE_CONST_CHAR_PTR) }
    }

    pub fn as_type(&self, token_kind: TokenKind) -> *mut gcc_jit_type {
        match token_kind {
            TokenKind::I32 => self.i32_type(),
            TokenKind::I64 => self.i64_type(),
            TokenKind::U32 => self.u32_type(),
            TokenKind::U64 => self.u64_type(),
            TokenKind::Void => self.void_type(),
            _ => compiler_error!("Invalid token given to cast to a GCCJIT type."),
        }
    }

    pub fn cbool(&self, val: bool) -> i32 {
        if val {
            1
        } else {
            0
        }
    }
}
