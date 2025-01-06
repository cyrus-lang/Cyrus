use ast::token::TokenKind;
use gccjit_sys::*;
use utils::compiler_error;

use crate::Compiler;

impl Compiler {
    pub fn void_ptr_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_VOID_PTR) }
    }

    pub fn void_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_VOID) }
    }

    pub fn i8_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_INT8_T) }
    }

    pub fn i16_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_INT16_T) }
    }

    pub fn i32_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_INT32_T) }
    }

    pub fn i64_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_INT64_T) }
    }

    pub fn i128_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_INT128_T) }
    }

    pub fn u8_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_UINT8_T) }
    }

    pub fn u16_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_UINT16_T) }
    }

    pub fn u32_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_UINT32_T) }
    }

    pub fn u64_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_UINT64_T) }
    }

    pub fn u128_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_UINT128_T) }
    }

    pub fn f32_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_DOUBLE) }
        // FIXME
    }

    pub fn f64_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_DOUBLE) }
    }

    pub fn string_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_CONST_CHAR_PTR) }
    }

    pub fn char_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_CHAR) }
    }

    pub fn bool_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_BOOL) }
    }

    pub fn token_as_data_type(context: *mut gcc_jit_context, token_kind: TokenKind) -> *mut gcc_jit_type {
        match token_kind {
            TokenKind::I8 => Compiler::i8_type(context),
            TokenKind::I16 => Compiler::i16_type(context),
            TokenKind::I32 => Compiler::i32_type(context),
            TokenKind::I64 => Compiler::i64_type(context),
            TokenKind::I128 => Compiler::i128_type(context),
            TokenKind::U8 => Compiler::u8_type(context),
            TokenKind::U16 => Compiler::u16_type(context),
            TokenKind::U32 => Compiler::u32_type(context),
            TokenKind::U64 => Compiler::u64_type(context),
            TokenKind::U128 => Compiler::u128_type(context),
            TokenKind::Void => Compiler::void_type(context),
            TokenKind::F32 => Compiler::f32_type(context),
            TokenKind::F64 => Compiler::f64_type(context),
            TokenKind::Bool => Compiler::bool_type(context),
            TokenKind::String => Compiler::string_type(context),
            TokenKind::Char => Compiler::char_type(context),
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

    pub fn purify_string(&self, str: String) -> String {
        str.replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\b", r"\b")
            .replace("\\a", r"\a")
            .replace("\\v", r"\v")
            .replace("\\f", r"\f")
            .replace("\\'", r"\'")
    }

    pub fn is_float_data_type(&mut self, type1: *mut gcc_jit_type) -> bool {
        type1 == Compiler::f32_type(self.context) || type1 == Compiler::f64_type(self.context)
    }

    pub fn widest_data_type(&mut self, type1: *mut gcc_jit_type, type2: *mut gcc_jit_type) -> *mut gcc_jit_type {
        if self.is_float_data_type(type1) && self.is_float_data_type(type2) {
            // compare floats
            if type1 > type2 {
                type1
            } else {
                type2
            }
        } else if self.is_float_data_type(type1) {
            type1
        } else if self.is_float_data_type(type2) {
            type2
        } else if type1 > type2 {
            type1
        } else if type1 < type2 {
            type2
        } else if type1 == type2 {
            type1
        } else {
            compiler_error!("Failed to determine widest data type when comparing type1 with type2.");
        }
    }
}
