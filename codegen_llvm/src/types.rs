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
            TokenKind::F16 => AnyTypeEnum::FloatType(self.context.f16_type()),
            TokenKind::F32 => AnyTypeEnum::FloatType(self.context.f32_type()),
            TokenKind::F64 => AnyTypeEnum::FloatType(self.context.f64_type()),
            TokenKind::F128 => AnyTypeEnum::FloatType(self.context.f128_type()),
            TokenKind::Void => AnyTypeEnum::VoidType(self.context.void_type()),
            TokenKind::Bool => AnyTypeEnum::IntType(self.context.bool_type()),
            TokenKind::String => AnyTypeEnum::PointerType(self.context.ptr_type(AddressSpace::default())),
            TokenKind::UserDefinedType(identifier) => todo!(),
            TokenKind::Dereference(_) => self.context.ptr_type(AddressSpace::default()).into(),
            TokenKind::Array(type_token, dimensions) => {
                let mut data_type = self.build_type(*type_token, loc.clone(), span_end);
                for item in dimensions {
                    let capacity = {
                        match item {
                            TokenKind::Literal(literal) => {
                                let literal_value = self.build_literal(literal);
                                match literal_value {
                                    inkwell::values::AnyValueEnum::IntValue(int_value) => int_value,
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
                            TokenKind::Dyn => {
                                todo!()
                            }
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
                        .unwrap()
                    };

                    match data_type {
                        AnyTypeEnum::IntType(int_type) => {
                            data_type = AnyTypeEnum::ArrayType(int_type.array_type(capacity.try_into().unwrap()));
                        }
                        AnyTypeEnum::FloatType(float_type) => {
                            data_type = AnyTypeEnum::ArrayType(float_type.array_type(capacity.try_into().unwrap()));
                        }
                        AnyTypeEnum::ArrayType(array_type) => {
                            data_type = AnyTypeEnum::ArrayType(array_type.array_type(capacity.try_into().unwrap()));
                        }
                        AnyTypeEnum::StructType(struct_type) => {
                            data_type = AnyTypeEnum::ArrayType(struct_type.array_type(capacity.try_into().unwrap()));
                        }
                        AnyTypeEnum::VectorType(vector_type) => {
                            data_type = AnyTypeEnum::ArrayType(vector_type.array_type(capacity.try_into().unwrap()));
                        }
                        AnyTypeEnum::VoidType(void_type) => {
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
                        AnyTypeEnum::PointerType(pointer_type) => {
                            data_type = AnyTypeEnum::ArrayType(pointer_type.array_type(capacity.try_into().unwrap()));
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
                    }
                }
                data_type
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
        }
    }
}
