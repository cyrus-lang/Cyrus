use crate::AnyValue;
use crate::CodeGenLLVM;
use crate::diag::*;
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
pub(crate) enum AnyType<'a> {
    IntType(IntType<'a>),
    FloatType(FloatType<'a>),
    ArrayType(ArrayType<'a>),
    StructType(StructType<'a>),
    VectorType(VectorType<'a>),
    StringType(StringType<'a>),
    VoidType(VoidType<'a>),
    OpaquePointer(PointerType<'a>),
    PointerType(Box<TypedPointerType<'a>>),
    ImportedModuleValue,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TypedPointerType<'a> {
    pub ptr_type: PointerType<'a>,
    pub pointee_ty: AnyType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StringType<'a> {
    pub struct_type: StructType<'a>,
}

impl<'a> TryFrom<BasicTypeEnum<'a>> for AnyType<'a> {
    type Error = &'static str;

    fn try_from(ty: BasicTypeEnum<'a>) -> Result<Self, Self::Error> {
        match ty {
            BasicTypeEnum::IntType(v) => Ok(AnyType::IntType(v)),
            BasicTypeEnum::FloatType(v) => Ok(AnyType::FloatType(v)),
            BasicTypeEnum::ArrayType(v) => Ok(AnyType::ArrayType(v)),
            BasicTypeEnum::StructType(v) => Ok(AnyType::StructType(v)),
            BasicTypeEnum::VectorType(v) => Ok(AnyType::VectorType(v)),
            BasicTypeEnum::PointerType(_) => Err("Cannot infer pointee type from opaque PointerType."),
        }
    }
}

impl<'a> AnyType<'a> {
    pub fn to_basic_type(&self) -> BasicTypeEnum<'a> {
        match self {
            AnyType::IntType(t) => (*t).as_basic_type_enum(),
            AnyType::FloatType(t) => (*t).as_basic_type_enum(),
            AnyType::ArrayType(t) => (*t).as_basic_type_enum(),
            AnyType::StructType(t) => (*t).as_basic_type_enum(),
            AnyType::VectorType(t) => (*t).as_basic_type_enum(),
            AnyType::PointerType(t) => t.ptr_type.as_basic_type_enum(),
            AnyType::StringType(t) => (*t).struct_type.as_basic_type_enum(),
            AnyType::OpaquePointer(t) => t.as_basic_type_enum(),
            AnyType::VoidType(t) => inkwell::types::AnyType::as_any_type_enum(t).try_into().unwrap(),
            AnyType::ImportedModuleValue => unreachable!(),
        }
    }

    pub fn as_type_ref(&self) -> LLVMTypeRef {
        match self {
            AnyType::IntType(t) => t.as_type_ref(),
            AnyType::FloatType(t) => t.as_type_ref(),
            AnyType::ArrayType(t) => t.as_type_ref(),
            AnyType::StructType(t) => t.as_type_ref(),
            AnyType::VectorType(t) => t.as_type_ref(),
            AnyType::PointerType(t) => t.ptr_type.as_type_ref(),
            AnyType::StringType(t) => t.struct_type.as_type_ref(),
            AnyType::OpaquePointer(t) => t.as_type_ref(),
            AnyType::VoidType(t) => inkwell::types::AnyType::as_any_type_enum(t).as_type_ref(),
            AnyType::ImportedModuleValue => unreachable!(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            AnyType::IntType(t) => format!("int{}", t.get_bit_width()),
            AnyType::FloatType(t) => {
                // FIXME
                format!("f{}", t.get_alignment().get_type().get_bit_width())
            }
            AnyType::ArrayType(t) => {
                format!("{}[{}]", t.get_element_type(), t.len(),)
            }
            AnyType::StructType(t) => {
                if let Some(name) = t.get_name() {
                    format!("struct {}", name.to_str().unwrap())
                } else {
                    "anonymous struct".to_string()
                }
            }
            AnyType::VectorType(t) => {
                todo!()
            }
            AnyType::PointerType(tp) => {
                format!("{}*", tp.pointee_ty.to_string())
            }
            AnyType::StringType(_) => "string".to_string(),
            AnyType::VoidType(_) => "void".to_string(),
            AnyType::OpaquePointer(_) => "ptr".to_string(),
            AnyType::ImportedModuleValue => unreachable!(),
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

    pub(crate) fn build_type(&self, token_kind: TokenKind, loc: Location, span_end: usize) -> AnyType<'ctx> {
        match token_kind {
            TokenKind::UserDefinedType(identifier) => todo!(),
            TokenKind::SizeT => {
                let data_layout = self.target_machine.get_target_data();
                AnyType::IntType(self.context
                    .ptr_sized_int_type(&data_layout, None))
            },
            TokenKind::I8 | TokenKind::U8 | TokenKind::Char => AnyType::IntType(self.context.i8_type()),
            TokenKind::I16 | TokenKind::U16 => AnyType::IntType(self.context.i16_type()),
            TokenKind::I32 | TokenKind::U32 => AnyType::IntType(self.context.i32_type()),
            TokenKind::I64 | TokenKind::U64 => AnyType::IntType(self.context.i64_type()),
            TokenKind::I128 | TokenKind::U128 => AnyType::IntType(self.context.i128_type()),
            TokenKind::F16 => AnyType::FloatType(self.context.f16_type()),
            TokenKind::F32 => AnyType::FloatType(self.context.f32_type()),
            TokenKind::F64 => AnyType::FloatType(self.context.f64_type()),
            TokenKind::F128 => AnyType::FloatType(self.context.f128_type()),
            TokenKind::Void => AnyType::VoidType(self.context.void_type()),
            TokenKind::Bool => AnyType::IntType(self.context.bool_type()),
            TokenKind::String => AnyType::StringType(self.string_type.clone()),
            TokenKind::Dereference(inner_data_type) => {
                let pointee_ty = self.build_type(*inner_data_type, loc.clone(), span_end);
                AnyType::PointerType(Box::new(TypedPointerType {
                    ptr_type: self.context.ptr_type(AddressSpace::default()).into(),
                    pointee_ty,
                }))
            }
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
    ) -> AnyType<'ctx> {
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
                AnyType::StringType(string_type) => {
                    AnyType::ArrayType(string_type.struct_type.array_type(capacity.try_into().unwrap()))
                }
                AnyType::IntType(int_type) => AnyType::ArrayType(int_type.array_type(capacity.try_into().unwrap())),
                AnyType::FloatType(float_type) => {
                    AnyType::ArrayType(float_type.array_type(capacity.try_into().unwrap()))
                }
                AnyType::ArrayType(array_type) => {
                    AnyType::ArrayType(array_type.array_type(capacity.try_into().unwrap()))
                }
                AnyType::StructType(struct_type) => {
                    AnyType::ArrayType(struct_type.array_type(capacity.try_into().unwrap()))
                }
                AnyType::VectorType(vector_type) => {
                    AnyType::ArrayType(vector_type.array_type(capacity.try_into().unwrap()))
                }
                AnyType::PointerType(pointer_type) => {
                    AnyType::ArrayType((*pointer_type).ptr_type.array_type(capacity.try_into().unwrap()))
                }
                AnyType::VoidType(_) => {
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
