use std::ptr::null_mut;

use ast::{
    ast::{integer_literal_as_value, FromPackage, Identifier},
    token::TokenKind,
};
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
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_FLOAT) }
    }

    pub fn f64_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_DOUBLE) }
    }

    pub fn size_t_type(context: *mut gcc_jit_context) -> *mut gcc_jit_type {
        unsafe { gcc_jit_context_get_type(context, gcc_jit_types::GCC_JIT_TYPE_SIZE_T) }
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

    pub fn is_user_defined_type(&self, from_package: FromPackage) -> bool {
        match self.global_struct_table.borrow_mut().iter().find(|&item| {
            if let Some(import_from_package) = &item.1.import_from_package {
                *import_from_package == from_package.to_string()
            } else {
                from_package.identifier.name == *item.0
            }
        }) {
            Some(_) => true,
            None => false,
        }
    }

    pub fn token_as_data_type(&mut self, context: *mut gcc_jit_context, token_kind: TokenKind) -> *mut gcc_jit_type {
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
            TokenKind::Float => Compiler::f32_type(context),
            TokenKind::Double => Compiler::f64_type(context),
            TokenKind::Bool => Compiler::bool_type(context),
            TokenKind::String => Compiler::string_type(context),
            TokenKind::Char => Compiler::char_type(context),
            TokenKind::SizeT => Compiler::size_t_type(context),
            TokenKind::Dereference(data_type) => unsafe {
                gcc_jit_type_get_pointer(self.token_as_data_type(context, *data_type))
            },
            TokenKind::UserDefinedType(identifier) => {
                match self.global_struct_table.borrow_mut().get(&identifier.name) {
                    Some(struct_statement) => unsafe { gcc_jit_struct_as_type(struct_statement.struct_type) },
                    None => compiler_error!("Unknown data type."),
                }
            }
            TokenKind::Array(data_type, dimensions) => {
                // TODO
                // FIXME
                // This should be fixed when implementing dynamic arrays using by Vector.

                let mut array_type: *mut gcc_jit_type = null_mut();
                let mut multi_dimensional = false;

                for item in dimensions.iter().rev().into_iter() {
                    if let Some(capacity) = item {
                        match capacity {
                            TokenKind::Literal(literal) => {
                                match literal {
                                    ast::ast::Literal::Integer(integer_literal) => {
                                        let capacity_raw: u64 =
                                            integer_literal_as_value(integer_literal.clone()).try_into().unwrap();

                                        if !multi_dimensional {
                                            let elements_data_type =
                                                self.token_as_data_type(context, *data_type.clone());

                                            array_type = unsafe {
                                                gcc_jit_context_new_array_type(
                                                    context,
                                                    null_mut(),
                                                    elements_data_type,
                                                    capacity_raw,
                                                )
                                            };
                                            multi_dimensional = true;
                                        } else {
                                            array_type = unsafe {
                                                gcc_jit_context_new_array_type(
                                                    context,
                                                    null_mut(),
                                                    array_type,
                                                    capacity_raw,
                                                )
                                            };
                                        }
                                    }
                                    _ => compiler_error!("Invalid capacity for array data type."),
                                };
                            }
                            _ => compiler_error!("Invalid token given to cast to a GCCJIT type."),
                        }
                    }
                }

                array_type
            }
            _ => compiler_error!("Invalid token given to cast as a GCCJIT type."),
        }
    }

    pub fn cbool(&self, val: bool) -> i32 {
        if val {
            1
        } else {
            0
        }
    }

    pub fn is_float_data_type(&mut self, type1: *mut gcc_jit_type) -> bool {
        type1 == Compiler::f32_type(self.context) || type1 == Compiler::f64_type(self.context)
    }

    pub fn is_int_data_type(&self, type1: *mut gcc_jit_type) -> bool {
        type1 == Compiler::i8_type(self.context)
            || type1 == Compiler::i16_type(self.context)
            || type1 == Compiler::i32_type(self.context)
            || type1 == Compiler::i64_type(self.context)
            || type1 == Compiler::i128_type(self.context)
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

    pub fn auto_castable_data_types(&mut self, type_token: TokenKind) -> bool {
        match type_token {
            TokenKind::I8
            | TokenKind::I16
            | TokenKind::I32
            | TokenKind::I64
            | TokenKind::I128
            | TokenKind::U8
            | TokenKind::U16
            | TokenKind::U32
            | TokenKind::U64
            | TokenKind::U128
            | TokenKind::Void
            | TokenKind::Float
            | TokenKind::Double
            | TokenKind::Bool
            | TokenKind::String
            | TokenKind::Char
            | TokenKind::SizeT
            | TokenKind::AddressOf(_)
            | TokenKind::Dereference(_) => true,
            _ => false,
        }
    }
}
