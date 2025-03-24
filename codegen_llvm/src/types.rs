use std::process::exit;

use crate::CodeGenLLVM;
use crate::diag::*;
use ast::token::*;
use inkwell::AddressSpace;
use inkwell::types::AnyTypeEnum;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_type(&self, token_kind: TokenKind, loc: Location, span_end: usize) -> AnyTypeEnum {
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
                AnyTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default()))
            }
            TokenKind::UserDefinedType(identifier) => todo!(),
            TokenKind::AddressOf(inner_token_kind) => {
                let data_type = self.build_type(*inner_token_kind, loc.clone(), span_end);
                inkwell::types::AnyTypeEnum::PointerType(data_type.into_pointer_type())
            }
            TokenKind::Dereference(inner_token_kind) => {
                if let AnyTypeEnum::PointerType(_) = self.build_type(*inner_token_kind, loc.clone(), span_end) {
                    self.context.ptr_type(AddressSpace::default()).into()
                } else {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::DerefNonPointerType,
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            }
            TokenKind::Array(data_type, dimensions) => todo!(),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::InvalidTypeToken,
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
        }
    }
}
