use ast::token::*;
use inkwell::AddressSpace;
use inkwell::types::AnyTypeEnum;
use utils::compile_time_errors::errors::*;
use utils::compiler_error;

use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_type(&self, token_kind: TokenKind) -> AnyTypeEnum {
        match token_kind {
            TokenKind::I8 => AnyTypeEnum::IntType(self.context.i8_type()),
            TokenKind::I16 => AnyTypeEnum::IntType(self.context.i16_type()),
            TokenKind::I32 => AnyTypeEnum::IntType(self.context.i32_type()),
            TokenKind::I64 => AnyTypeEnum::IntType(self.context.i64_type()),
            TokenKind::I128 => AnyTypeEnum::IntType(self.context.i128_type()),
            TokenKind::U8 => AnyTypeEnum::IntType(self.context.i8_type()),
            TokenKind::U16 => AnyTypeEnum::IntType(self.context.i16_type()),
            TokenKind::U32 => AnyTypeEnum::IntType(self.context.i32_type()),
            TokenKind::U64 => AnyTypeEnum::IntType(self.context.i64_type()),
            TokenKind::U128 => AnyTypeEnum::IntType(self.context.i128_type()),
            TokenKind::Char => AnyTypeEnum::IntType(self.context.i8_type()),
            TokenKind::Float => AnyTypeEnum::FloatType(self.context.f32_type()),
            TokenKind::Double => AnyTypeEnum::FloatType(self.context.f64_type()),
            TokenKind::Void => AnyTypeEnum::VoidType(self.context.void_type()),
            TokenKind::Bool => AnyTypeEnum::IntType(self.context.bool_type()),
            TokenKind::String => {
                // string is a ptr to i8 array
                AnyTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default()))
            }
            TokenKind::UserDefinedType(identifier) => todo!(),
            TokenKind::AddressOf(inner_token_kind) => {
                let data_type = self.build_type(*inner_token_kind);
                inkwell::types::AnyTypeEnum::PointerType(data_type.into_pointer_type())
            }
            TokenKind::Dereference(inner_token_kind) => {
                if let AnyTypeEnum::PointerType(_) = self.build_type(*inner_token_kind) {
                    self.context.ptr_type(AddressSpace::default()).into()
                } else {
                    compiler_error!("Cannot dereference a non-pointer type!", self.file_path.clone());
                }
            }
            TokenKind::Array(data_type, dimensions) => todo!(),
            _ => {
                compiler_error!(format!("Invalid type token: {}", token_kind), self.file_path.clone());
            }
        }
    }
}
