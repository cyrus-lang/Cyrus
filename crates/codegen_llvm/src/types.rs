use crate::CodeGenLLVM;
use crate::diag::*;
use ast::ast::ArrayCapacity;
use ast::ast::TypeSpecifier;
use ast::token::*;
use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::types::ArrayType;
use inkwell::types::AsTypeRef;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FloatType;
use inkwell::types::IntType;
use inkwell::types::PointerType;
use inkwell::types::StructType;
use inkwell::types::VectorType;
use inkwell::types::VoidType;
use std::process::exit;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InternalType<'a> {
    IntType(IntType<'a>),
    FloatType(FloatType<'a>),
    ArrayType(ArrayType<'a>),
    StructType(StructType<'a>),
    VectorType(VectorType<'a>),
    StringType(StringType<'a>),
    VoidType(VoidType<'a>),
    PointerType(Box<TypedPointerType<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TypedPointerType<'a> {
    pub ptr_type: PointerType<'a>,
    pub pointee_ty: InternalType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StringType<'a> {
    pub struct_type: StructType<'a>,
}

impl<'a> TryFrom<BasicTypeEnum<'a>> for InternalType<'a> {
    type Error = &'static str;

    fn try_from(ty: BasicTypeEnum<'a>) -> Result<Self, Self::Error> {
        match ty {
            BasicTypeEnum::IntType(v) => Ok(InternalType::IntType(v)),
            BasicTypeEnum::FloatType(v) => Ok(InternalType::FloatType(v)),
            BasicTypeEnum::ArrayType(v) => Ok(InternalType::ArrayType(v)),
            BasicTypeEnum::StructType(v) => Ok(InternalType::StructType(v)),
            BasicTypeEnum::VectorType(v) => Ok(InternalType::VectorType(v)),
            BasicTypeEnum::PointerType(_) => Err("Cannot infer pointee type from opaque PointerType."),
        }
    }
}

impl<'a> InternalType<'a> {
    pub fn is_int_type(&self) -> bool {
        matches!(self, InternalType::IntType(_))
    }

    pub fn is_float_type(&self) -> bool {
        matches!(self, InternalType::FloatType(_))
    }

    pub fn is_array_type(&self) -> bool {
        matches!(self, InternalType::ArrayType(_))
    }

    pub fn is_struct_type(&self) -> bool {
        matches!(self, InternalType::StructType(_))
    }

    pub fn is_vector_type(&self) -> bool {
        matches!(self, InternalType::VectorType(_))
    }

    pub fn is_string_type(&self) -> bool {
        matches!(self, InternalType::StringType(_))
    }

    pub fn is_void_type(&self) -> bool {
        matches!(self, InternalType::VoidType(_))
    }

    pub fn is_pointer_type(&self) -> bool {
        matches!(self, InternalType::PointerType(_))
    }

    pub fn to_basic_type(&self, ptr_type: PointerType<'a>) -> BasicTypeEnum<'a> {
        match self {
            InternalType::IntType(t) => (*t).as_basic_type_enum(),
            InternalType::FloatType(t) => (*t).as_basic_type_enum(),
            InternalType::ArrayType(t) => (*t).as_basic_type_enum(),
            InternalType::StructType(t) => (*t).as_basic_type_enum(),
            InternalType::VectorType(t) => (*t).as_basic_type_enum(),
            InternalType::PointerType(t) => t.ptr_type.as_basic_type_enum(),
            InternalType::StringType(t) => (*t).struct_type.as_basic_type_enum(),
            InternalType::VoidType(_) => BasicTypeEnum::PointerType(ptr_type),
        }
    }

    pub fn as_type_ref(&self) -> LLVMTypeRef {
        match self {
            InternalType::IntType(t) => t.as_type_ref(),
            InternalType::FloatType(t) => t.as_type_ref(),
            InternalType::ArrayType(t) => t.as_type_ref(),
            InternalType::StructType(t) => t.as_type_ref(),
            InternalType::VectorType(t) => t.as_type_ref(),
            InternalType::PointerType(t) => t.ptr_type.as_type_ref(),
            InternalType::StringType(t) => t.struct_type.as_type_ref(),
            InternalType::VoidType(t) => inkwell::types::AnyType::as_any_type_enum(t).as_type_ref(),
        }
    }
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_string_type(context: &'ctx Context) -> StringType<'ctx> {
        let i8_ptr_type = context.ptr_type(AddressSpace::default());
        let i64_type = context.i64_type();

        let struct_type = context.opaque_struct_type("str");
        struct_type.set_body(&[i8_ptr_type.into(), i64_type.into()], false);

        StringType { struct_type }
    }

    pub(crate) fn build_type(
        &self,
        type_specifier: TypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> InternalType<'ctx> {
        match type_specifier {
            TypeSpecifier::Identifier(identifier) => todo!(),
            TypeSpecifier::ModuleImport(module_import) => todo!(),
            TypeSpecifier::Const(type_specifier) => todo!(),
            TypeSpecifier::AddressOf(type_specifier) => todo!(),
            TypeSpecifier::Dereference(type_specifier) => {
                let pointee_ty = self.build_type(*type_specifier, loc.clone(), span_end);
                InternalType::PointerType(Box::new(TypedPointerType {
                    ptr_type: self.context.ptr_type(AddressSpace::default()).into(),
                    pointee_ty,
                }))
            }
            TypeSpecifier::Array(type_token, dimensions) => {
                self.build_array_type(*type_token, dimensions, loc, span_end)
            }
            TypeSpecifier::TypeToken(token) => self.build_type_token(token, loc.clone()),
        }
    }

    pub(crate) fn build_type_token(&self, type_token: Token, loc: Location) -> InternalType<'ctx> {
        match type_token.kind {
            TokenKind::Identifier { name } => {
                match self.struct_table.get(&name) {
                    Some(struct_metadata) => {
                        dbg!(struct_metadata);

                        todo!();
                        // TODO
                    }
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::UndefinedDataType(name),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: loc.line,
                                column: loc.column,
                                length: type_token.span.end,
                            }),
                        });
                        exit(1);
                    }
                }
            }
            TokenKind::Int => {
                let data_layout = self.target_machine.get_target_data();
                InternalType::IntType(self.context.ptr_sized_int_type(&data_layout, None))
            }
            TokenKind::Int8 | TokenKind::UInt8 | TokenKind::Char => InternalType::IntType(self.context.i8_type()),
            TokenKind::Int16 | TokenKind::UInt16 => InternalType::IntType(self.context.i16_type()),
            TokenKind::Int32 | TokenKind::UInt32 => InternalType::IntType(self.context.i32_type()),
            TokenKind::Int64 | TokenKind::UInt64 => InternalType::IntType(self.context.i64_type()),
            TokenKind::Int128 | TokenKind::UInt128 => InternalType::IntType(self.context.i128_type()),
            TokenKind::Float16 => InternalType::FloatType(self.context.f16_type()),
            TokenKind::Float32 => InternalType::FloatType(self.context.f32_type()),
            TokenKind::Float64 => InternalType::FloatType(self.context.f64_type()),
            TokenKind::Float128 => InternalType::FloatType(self.context.f128_type()),
            TokenKind::Void => InternalType::VoidType(self.context.void_type()),
            TokenKind::Bool => InternalType::IntType(self.context.bool_type()),
            TokenKind::String => InternalType::StringType(self.string_type.clone()),
            _ => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::InvalidTypeToken,
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: type_token.span.end,
                    }),
                });
                exit(1);
            }
        }
    }

    fn build_array_type(
        &self,
        type_specifier: TypeSpecifier,
        dimensions: Vec<ArrayCapacity>,
        loc: Location,
        span_end: usize,
    ) -> InternalType<'ctx> {
        // FIXME
        todo!();

        // let mut data_type = self.build_type(type_token, loc.clone(), span_end);

        // for array_capacity in dimensions {
        //     let capacity = match array_capacity {
        //         ArrayCapacity::Static(token_kind) => {
        //             if let TokenKind::Literal(literal) = token_kind {
        //                 let literal_value = self.build_literal(literal);
        //                 match literal_value {
        //                     InternalValue::IntValue(int_value) => int_value,
        //                     _ => {
        //                         display_single_diag(Diag {
        //                             level: DiagLevel::Error,
        //                             kind: DiagKind::InvalidTokenAsArrayCapacity,
        //                             location: Some(DiagLoc {
        //                                 file: self.file_path.clone(),
        //                                 line: loc.line,
        //                                 column: loc.column,
        //                                 length: span_end,
        //                             }),
        //                         });
        //                         exit(1);
        //                     }
        //                 }
        //             } else {
        //                 display_single_diag(Diag {
        //                     level: DiagLevel::Error,
        //                     kind: DiagKind::InvalidTypeToken,
        //                     location: Some(DiagLoc {
        //                         file: self.file_path.clone(),
        //                         line: loc.line,
        //                         column: loc.column,
        //                         length: span_end,
        //                     }),
        //                 });
        //                 exit(1);
        //             }
        //         }
        //         ArrayCapacity::Dynamic => todo!(),
        //     }
        //     .get_zero_extended_constant()
        //     .unwrap();

        //     data_type = match data_type {
        //         InternalType::StringType(string_type) => {
        //             InternalType::ArrayType(string_type.struct_type.array_type(capacity.try_into().unwrap()))
        //         }
        //         InternalType::IntType(int_type) => InternalType::ArrayType(int_type.array_type(capacity.try_into().unwrap())),
        //         InternalType::FloatType(float_type) => {
        //             InternalType::ArrayType(float_type.array_type(capacity.try_into().unwrap()))
        //         }
        //         InternalType::ArrayType(array_type) => {
        //             InternalType::ArrayType(array_type.array_type(capacity.try_into().unwrap()))
        //         }
        //         InternalType::StructType(struct_type) => {
        //             InternalType::ArrayType(struct_type.array_type(capacity.try_into().unwrap()))
        //         }
        //         InternalType::VectorType(vector_type) => {
        //             InternalType::ArrayType(vector_type.array_type(capacity.try_into().unwrap()))
        //         }
        //         InternalType::PointerType(pointer_type) => {
        //             InternalType::ArrayType((*pointer_type).ptr_type.array_type(capacity.try_into().unwrap()))
        //         }
        //         InternalType::VoidType(_) => {
        //             display_single_diag(Diag {
        //                 level: DiagLevel::Error,
        //                 kind: DiagKind::Custom("Void cannot be an array element type.".to_string()),
        //                 location: Some(DiagLoc {
        //                     file: self.file_path.clone(),
        //                     line: loc.line,
        //                     column: loc.column,
        //                     length: span_end,
        //                 }),
        //             });
        //             exit(1);
        //         }
        //         _ => {
        //             display_single_diag(Diag {
        //                 level: DiagLevel::Error,
        //                 kind: DiagKind::InvalidTypeToken,
        //                 location: Some(DiagLoc {
        //                     file: self.file_path.clone(),
        //                     line: loc.line,
        //                     column: loc.column,
        //                     length: span_end,
        //                 }),
        //             });
        //             exit(1);
        //         }
        //     };
        // }
        // data_type
    }
}
