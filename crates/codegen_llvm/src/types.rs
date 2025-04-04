use crate::AnyValue;
use crate::CodeGenLLVM;
use crate::diag::*;
use ast::token::*;
use inkwell::AddressSpace;
use inkwell::types::AnyTypeEnum;
use std::process::exit;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_type(&self, token_kind: TokenKind, loc: Location, span_end: usize) -> AnyTypeEnum<'ctx> {
        match token_kind {
            TokenKind::UserDefinedType(identifier) => todo!(),
            TokenKind::I8 | TokenKind::U8 | TokenKind::Char => AnyTypeEnum::IntType(self.context.i8_type()),
            TokenKind::I16 | TokenKind::U16 => AnyTypeEnum::IntType(self.context.i16_type()),
            TokenKind::I32 | TokenKind::U32 => AnyTypeEnum::IntType(self.context.i32_type()),
            TokenKind::I64 | TokenKind::U64 => AnyTypeEnum::IntType(self.context.i64_type()),
            TokenKind::I128 | TokenKind::U128 => AnyTypeEnum::IntType(self.context.i128_type()),
            TokenKind::F16 => AnyTypeEnum::FloatType(self.context.f16_type()),
            TokenKind::F32 => AnyTypeEnum::FloatType(self.context.f32_type()),
            TokenKind::F64 => AnyTypeEnum::FloatType(self.context.f64_type()),
            TokenKind::F128 => AnyTypeEnum::FloatType(self.context.f128_type()),
            TokenKind::SizeT => todo!(),
            TokenKind::Void => AnyTypeEnum::VoidType(self.context.void_type()),
            TokenKind::Bool => AnyTypeEnum::IntType(self.context.bool_type()),
            TokenKind::String => AnyTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default())),
            TokenKind::Dereference(_) => self.context.ptr_type(AddressSpace::default()).into(),
            TokenKind::Array(type_token, dimensions) => self.build_array_type(*type_token, dimensions, loc, span_end),
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

    fn build_array_type(
        &self,
        type_token: TokenKind,
        dimensions: Vec<TokenKind>,
        loc: Location,
        span_end: usize,
    ) -> AnyTypeEnum<'ctx> {
        let mut data_type = self.build_type(type_token, loc.clone(), span_end);

        for item in dimensions {
            let capacity = match item {
                TokenKind::Literal(literal) => {
                    let literal_value = self.build_literal(literal);
                    match literal_value {
                        AnyValue::IntValue(int_value) => int_value,
                        _ => {
                            display_single_diag(Diag {
                                level: DiagLevel::Error,
                                kind: DiagKind::InvalidTokenAsArrayCapacity,
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
                TokenKind::Dyn => todo!(),
                _ => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::InvalidTokenAsArrayCapacity,
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
            .get_zero_extended_constant()
            .unwrap();

            data_type = match data_type {
                AnyTypeEnum::IntType(int_type) => {
                    AnyTypeEnum::ArrayType(int_type.array_type(capacity.try_into().unwrap()))
                }
                AnyTypeEnum::FloatType(float_type) => {
                    AnyTypeEnum::ArrayType(float_type.array_type(capacity.try_into().unwrap()))
                }
                AnyTypeEnum::ArrayType(array_type) => {
                    AnyTypeEnum::ArrayType(array_type.array_type(capacity.try_into().unwrap()))
                }
                AnyTypeEnum::StructType(struct_type) => {
                    AnyTypeEnum::ArrayType(struct_type.array_type(capacity.try_into().unwrap()))
                }
                AnyTypeEnum::VectorType(vector_type) => {
                    AnyTypeEnum::ArrayType(vector_type.array_type(capacity.try_into().unwrap()))
                }
                AnyTypeEnum::PointerType(pointer_type) => {
                    AnyTypeEnum::ArrayType(pointer_type.array_type(capacity.try_into().unwrap()))
                }
                AnyTypeEnum::VoidType(_) => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Void cannot be an array element type.".to_string()),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
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
            };
        }
        data_type
    }
}
