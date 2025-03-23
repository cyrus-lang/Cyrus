use ast::ast::*;
use ast::token::*;
use inkwell::module::Linkage;
use inkwell::types::AnyTypeEnum;
use inkwell::AddressSpace;
use utils::compiler_error;
use utils::compile_time_errors::errors::*;

use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn vis_type_as_linkage(&self, vis_type: VisType) -> Linkage {
        match vis_type {
            VisType::Extern => Linkage::External,
            VisType::Pub => Linkage::AvailableExternally,
            VisType::Internal => Linkage::Private,
            VisType::Inline => todo!(),
        }
    }

    pub(crate) fn token_as_data_type(&self, token_kind: TokenKind) -> AnyTypeEnum {
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
            },
            TokenKind::UserDefinedType(identifier) => todo!(),
            TokenKind::AddressOf(inner_token_kind) => todo!(),
            TokenKind::Dereference(inner_token_kind) => {
                // Dereference returns the type that the pointer points to.
                if let AnyTypeEnum::PointerType(ptr_type) = self.token_as_data_type(*inner_token_kind) {
                    todo!();
                } else {
                    compiler_error!("Cannot dereference a non-pointer type", self.file_path.clone());
                }
            },
            TokenKind::Array(data_type, dimensions) => todo!(),
            _ => {
                compiler_error!(format!("Invalid token type: {}", token_kind), self.file_path.clone());
            }
        }
    }
}
