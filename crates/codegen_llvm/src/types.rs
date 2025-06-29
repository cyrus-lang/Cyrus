use crate::CodeGenLLVM;
use crate::InternalValue;
use crate::StringValue;
use crate::diag::*;
use crate::structs::StructMetadata;
use crate::structs::UnnamedStructTypeMetadata;
use crate::values::Lvalue;
use crate::values::TypedPointerValue;
use ast::ast::ArrayCapacity;
use ast::ast::ArrayTypeSpecifier;
use ast::ast::ModuleImport;
use ast::ast::ModuleSegment;
use ast::ast::TypeSpecifier;
use ast::token::*;
use inkwell::AddressSpace;
use inkwell::llvm_sys::prelude::LLVMTypeRef;
use inkwell::types::AnyType;
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
use std::fmt;
use std::process::exit;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InternalType<'a> {
    BoolType(InternalBoolType<'a>),
    IntType(InternalIntType<'a>),
    FloatType(InternalFloatType<'a>),
    StructType(InternalStructType<'a>),
    UnnamedStruct(InternalUnnamedStructType<'a>),
    VectorType(InternalVectorType<'a>),
    StringType(InternalStringType<'a>),
    VoidType(InternalVoidType<'a>),
    PointerType(Box<InternalPointerType<'a>>),
    Lvalue(Box<InternalLvalueType<'a>>),
    ArrayType(InternalArrayType<'a>),
    ConstType(InternalConstType<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalArrayType<'a> {
    pub type_str: String,
    pub inner_type: Box<InternalType<'a>>,
    pub array_type: ArrayType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalConstType<'a> {
    pub type_str: String,
    pub inner_type: Box<InternalType<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalBoolType<'a> {
    pub type_str: String,
    pub bool_type: IntType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalIntType<'a> {
    pub type_str: String,
    pub int_type: IntType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalFloatType<'a> {
    pub type_str: String,
    pub float_type: FloatType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalStructType<'a> {
    pub type_str: String,
    pub struct_metadata: StructMetadata<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalUnnamedStructType<'a> {
    pub type_str: String,
    pub unnamed_struct_metadata: UnnamedStructTypeMetadata<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalVectorType<'a> {
    pub type_str: String,
    pub vector_type: VectorType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalVoidType<'a> {
    pub type_str: String,
    pub void_type: VoidType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct InternalLvalueType<'a> {
    pub ptr_type: PointerType<'a>,
    pub pointee_ty: InternalType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct InternalPointerType<'a> {
    pub ptr_type: PointerType<'a>,
    pub pointee_ty: InternalType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct InternalStringType<'a> {
    pub type_str: String,
    pub struct_type: StructType<'a>,
}

#[derive(Debug, Clone)]
pub(crate) enum DefinedType<'a> {
    Struct(InternalStructType<'a>),
    // Typedef(...),
}

impl<'a> DefinedType<'a> {
    pub fn into_internal_type(&self) -> InternalType<'a> {
        match self {
            DefinedType::Struct(internal_struct_type) => InternalType::StructType(internal_struct_type.clone()),
        }
    }
}

impl<'a> fmt::Display for InternalType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InternalType::BoolType(_) => write!(f, "bool"),
            InternalType::IntType(internal_int_type) => {
                write!(f, "{}", internal_int_type.type_str)
            }
            InternalType::FloatType(internal_float_type) => {
                write!(f, "{}", internal_float_type.type_str)
            }
            InternalType::StructType(internal_struct_type) => {
                write!(f, "{}", internal_struct_type.type_str)
            }
            InternalType::UnnamedStruct(internal_unnamed_struct_type) => {
                write!(f, "{}", internal_unnamed_struct_type.type_str)
            }
            InternalType::VectorType(internal_vector_type) => {
                write!(f, "{}", internal_vector_type.type_str)
            }
            InternalType::StringType(_) => {
                write!(f, "string")
            }
            InternalType::VoidType(internal_void_type) => {
                write!(f, "{}", internal_void_type.type_str)
            }
            InternalType::PointerType(internal_pointer_type) => {
                write!(f, "{}*", internal_pointer_type.pointee_ty)
            }
            InternalType::Lvalue(internal_lvalue_type) => {
                write!(f, "{}", internal_lvalue_type.pointee_ty)
            }
            InternalType::ArrayType(internal_array_type) => {
                write!(
                    f,
                    "{}[{}]",
                    internal_array_type.inner_type,
                    internal_array_type.array_type.len()
                )
            }
            InternalType::ConstType(internal_const_type) => {
                write!(f, "const {}", internal_const_type.inner_type)
            }
        }
    }
}

impl<'a> InternalType<'a> {
    pub fn into_array_type(&self, size: u32, type_str: String) -> Result<InternalType<'a>, String> {
        match self {
            InternalType::IntType(internal_int_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::IntType(internal_int_type.clone())),
                array_type: internal_int_type.int_type.array_type(size),
            })),
            InternalType::FloatType(internal_float_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::FloatType(internal_float_type.clone())),
                array_type: internal_float_type.float_type.array_type(size),
            })),
            InternalType::ArrayType(internal_array_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: internal_array_type.inner_type.clone(),
                array_type: internal_array_type.array_type.array_type(size),
            })),
            InternalType::StructType(internal_struct_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::StructType(internal_struct_type.clone())),
                array_type: internal_struct_type.struct_metadata.struct_type.array_type(size),
            })),
            InternalType::VectorType(internal_vector_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::VectorType(internal_vector_type.clone())),
                array_type: internal_vector_type.vector_type.array_type(size),
            })),
            InternalType::StringType(string_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::StringType(string_type.clone())),
                array_type: string_type.struct_type.array_type(size),
            })),
            InternalType::PointerType(typed_pointer_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::PointerType(typed_pointer_type.clone())),
                array_type: typed_pointer_type.ptr_type.array_type(size),
            })),
            InternalType::ConstType(internal_const_type) => {
                internal_const_type.inner_type.into_array_type(size, type_str)
            }
            InternalType::BoolType(internal_bool_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::BoolType(internal_bool_type.clone())),
                array_type: internal_bool_type.bool_type.array_type(size),
            })),
            InternalType::UnnamedStruct(unnamed_struct_metadata) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::UnnamedStruct(unnamed_struct_metadata.clone())),
                array_type: unnamed_struct_metadata
                    .unnamed_struct_metadata
                    .struct_type
                    .array_type(size),
            })),
            InternalType::VoidType(_) => Err("VoidType cannot be converted to an array type.".to_string()),
            InternalType::Lvalue(_) => Err("Lvalue cannot be converted to an array type.".to_string()),
        }
    }

    pub fn into_internal_value(
        &self,
        value: inkwell::values::BasicValueEnum<'a>,
    ) -> Result<InternalValue<'a>, &'static str> {
        match self {
            InternalType::BoolType(internal_bool_type) => Ok(InternalValue::BoolValue(
                value.into_int_value(),
                InternalType::BoolType(internal_bool_type.clone()),
            )),
            InternalType::IntType(internal_int_type) => Ok(InternalValue::IntValue(
                value.into_int_value(),
                InternalType::IntType(internal_int_type.clone()),
            )),
            InternalType::FloatType(internal_float_type) => Ok(InternalValue::FloatValue(
                value.into_float_value(),
                InternalType::FloatType(internal_float_type.clone()),
            )),
            InternalType::ArrayType(internal_array_type) => Ok(InternalValue::ArrayValue(
                value.into_array_value(),
                InternalType::ArrayType(internal_array_type.clone()),
            )),
            InternalType::StructType(internal_struct_type) => Ok(InternalValue::StructValue(
                value.into_struct_value(),
                InternalType::StructType(internal_struct_type.clone()),
            )),
            InternalType::UnnamedStruct(internal_unnamed_struct_type) => Ok(InternalValue::UnnamedStructValue(
                value.into_struct_value(),
                InternalType::UnnamedStruct(internal_unnamed_struct_type.clone()),
            )),
            InternalType::VectorType(internal_vector_type) => Ok(InternalValue::VectorValue(
                value.into_vector_value(),
                InternalType::VectorType(internal_vector_type.clone()),
            )),
            InternalType::PointerType(ptr_ty) => Ok(InternalValue::PointerValue(TypedPointerValue {
                type_str: format!("{}*", ptr_ty.pointee_ty.to_string()),
                ptr: value.into_pointer_value(),
                pointee_ty: ptr_ty.pointee_ty.clone(),
            })),
            InternalType::Lvalue(ptr_ty) => Ok(InternalValue::Lvalue(Lvalue {
                ptr: value.into_pointer_value(),
                pointee_ty: ptr_ty.pointee_ty.clone(),
            })),
            InternalType::StringType(_) => Ok(InternalValue::StringValue(StringValue {
                struct_value: value.into_struct_value(),
            })),
            InternalType::ConstType(internal_const_type) => internal_const_type.inner_type.into_internal_value(value),
            InternalType::VoidType(_) => {
                panic!() // FIXME How we can convert void into internal_type? |:
            }
        }
    }

    #[allow(unused)]
    pub fn is_int_type(&self) -> bool {
        matches!(self, InternalType::IntType(_))
    }

    #[allow(unused)]
    pub fn is_float_type(&self) -> bool {
        matches!(self, InternalType::FloatType(_))
    }

    #[allow(unused)]
    pub fn is_array_type(&self) -> bool {
        matches!(self, InternalType::ArrayType(_))
    }

    #[allow(unused)]
    pub fn is_struct_type(&self) -> bool {
        matches!(self, InternalType::StructType(_))
    }

    #[allow(unused)]
    pub fn is_vector_type(&self) -> bool {
        matches!(self, InternalType::VectorType(_))
    }

    #[allow(unused)]
    pub fn is_string_type(&self) -> bool {
        matches!(self, InternalType::StringType(_))
    }

    pub fn is_void_type(&self) -> bool {
        matches!(self, InternalType::VoidType(_))
    }

    #[allow(unused)]
    pub fn is_pointer_type(&self) -> bool {
        matches!(self, InternalType::PointerType(_))
    }

    #[allow(unused)]
    pub fn is_const_type(&self) -> bool {
        matches!(self, InternalType::ConstType(_))
    }

    #[allow(unused)]
    pub fn is_lvalue_type(&self) -> bool {
        matches!(self, InternalType::Lvalue(_))
    }

    #[allow(unused)]
    pub fn is_bool_type(&self) -> bool {
        matches!(self, InternalType::BoolType(_))
    }

    pub fn to_basic_type(&self, ptr_type: PointerType<'a>) -> Result<BasicTypeEnum<'a>, &'a str> {
        match self {
            InternalType::IntType(t) => Ok(t.int_type.as_basic_type_enum()),
            InternalType::FloatType(t) => Ok(t.float_type.as_basic_type_enum()),
            InternalType::ArrayType(t) => Ok(t.array_type.as_basic_type_enum()),
            InternalType::StructType(t) => Ok(t.struct_metadata.struct_type.as_basic_type_enum()),
            InternalType::VectorType(t) => Ok(t.vector_type.as_basic_type_enum()),
            InternalType::PointerType(t) => Ok(t.ptr_type.as_basic_type_enum()),
            InternalType::Lvalue(t) => Ok(t.ptr_type.as_basic_type_enum()),
            InternalType::StringType(t) => Ok(t.struct_type.as_basic_type_enum()),
            InternalType::BoolType(t) => Ok(t.bool_type.as_basic_type_enum()),
            InternalType::UnnamedStruct(t) => Ok(t.unnamed_struct_metadata.struct_type.as_basic_type_enum()),
            InternalType::ConstType(t) => t.inner_type.to_basic_type(ptr_type),
            InternalType::VoidType(_) => Err("InternalVoidType cannot be convert to basic llvm type."),
        }
    }

    pub fn as_type_ref(&self) -> LLVMTypeRef {
        match self {
            InternalType::IntType(t) => t.int_type.as_type_ref(),
            InternalType::FloatType(t) => t.float_type.as_type_ref(),
            InternalType::ArrayType(t) => t.array_type.as_type_ref(),
            InternalType::StructType(t) => t.struct_metadata.struct_type.as_type_ref(),
            InternalType::VectorType(t) => t.vector_type.as_type_ref(),
            InternalType::PointerType(t) => t.ptr_type.as_type_ref(),
            InternalType::Lvalue(t) => t.ptr_type.as_type_ref(),
            InternalType::StringType(t) => t.struct_type.as_type_ref(),
            InternalType::ConstType(t) => t.inner_type.as_type_ref(),
            InternalType::BoolType(t) => t.bool_type.as_type_ref(),
            InternalType::UnnamedStruct(t) => t.unnamed_struct_metadata.struct_type.as_type_ref(),
            InternalType::VoidType(t) => AnyType::as_any_type_enum(&t.void_type).as_type_ref(),
        }
    }
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn compatible_types(&self, lvalue_type: InternalType<'ctx>, rvalue_type: InternalType<'ctx>) -> bool {
        match (lvalue_type, rvalue_type) {
            (InternalType::IntType(_), InternalType::IntType(_)) => true,
            (InternalType::FloatType(_), InternalType::FloatType(_)) => true,
            (InternalType::FloatType(_), InternalType::IntType(_)) => true,
            (InternalType::IntType(_), InternalType::FloatType(_)) => true,
            (InternalType::PointerType(_), InternalType::PointerType(_)) => true,
            (InternalType::StructType(struct_metadata1), InternalType::StructType(struct_metadata2)) => {
                struct_metadata1.struct_metadata.struct_type == struct_metadata2.struct_metadata.struct_type
                    && struct_metadata1.struct_metadata.fields.len() == struct_metadata2.struct_metadata.fields.len()
            }
            (
                InternalType::UnnamedStruct(unnamed_struct_metadata1),
                InternalType::UnnamedStruct(unnamed_struct_metadata2),
            ) => {
                unnamed_struct_metadata1.unnamed_struct_metadata.struct_type
                    == unnamed_struct_metadata2.unnamed_struct_metadata.struct_type
                    && unnamed_struct_metadata1.unnamed_struct_metadata.fields.len()
                        == unnamed_struct_metadata2.unnamed_struct_metadata.fields.len()
            }
            (InternalType::VectorType(_), InternalType::VectorType(_)) => true,
            (InternalType::ArrayType(internal_array_type1), InternalType::ArrayType(internal_array_type2)) => {
                (internal_array_type1.array_type.len() == internal_array_type2.array_type.len())
                    && self.compatible_types(*internal_array_type1.inner_type, *internal_array_type2.inner_type)
            }
            (InternalType::StringType(_), InternalType::StringType(_)) => true,
            (InternalType::StringType(_), InternalType::PointerType(typed_pointer_type)) => {
                // allow const_str assignment to StringType
                typed_pointer_type.pointee_ty.is_array_type()
            }
            (InternalType::PointerType(_), InternalType::StringType(_)) => {
                // allow StringType assignment to char*
                true
            }
            (InternalType::VoidType(_), _) => false,
            (InternalType::ConstType(internal_const_type), rvalue_type) => {
                self.compatible_types(*internal_const_type.inner_type, rvalue_type)
            }
            _ => false,
        }
    }

    pub(crate) fn find_defined_type(
        &self,
        module_import: ModuleImport,
        loc: Location,
        span_end: usize,
    ) -> DefinedType<'ctx> {
        if module_import.segments.len() == 1 {
            let first_segment = module_import.segments[0].clone();
            let ast::ast::ModuleSegment::SubModule(identifier) = first_segment;

            match self.struct_table.get(&identifier.name.clone()) {
                Some(internal_struct_type) => DefinedType::Struct(internal_struct_type.clone()),
                None => {
                    // TODO Lookup in typedef table

                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::UndefinedDataType(identifier.name),
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
        } else {
            // TODO
            todo!("Implement module import for find_type");
        }
    }

    pub(crate) fn build_type(
        &self,
        type_specifier: TypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> InternalType<'ctx> {
        match type_specifier.clone() {
            TypeSpecifier::Const(inner_type_specifier) => InternalType::ConstType(InternalConstType {
                type_str: type_specifier.to_string(),
                inner_type: Box::new(self.build_type(*inner_type_specifier.clone(), loc, span_end)),
            }),
            TypeSpecifier::Identifier(identifier) => self
                .find_defined_type(
                    ModuleImport {
                        segments: vec![ModuleSegment::SubModule(identifier.clone())],
                        span: identifier.span.clone(),
                        loc: identifier.loc.clone(),
                    },
                    loc,
                    span_end,
                )
                .into_internal_type(),
            TypeSpecifier::ModuleImport(module_import) => todo!(),
            TypeSpecifier::Dereference(inner_type_specifier) => {
                let pointee_ty = self.build_type(*inner_type_specifier, loc.clone(), span_end);
                InternalType::PointerType(Box::new(InternalPointerType {
                    ptr_type: self.context.ptr_type(AddressSpace::default()).into(),
                    pointee_ty,
                }))
            }
            TypeSpecifier::Array(array_type_specifier) => self.build_array_type(array_type_specifier, loc, span_end),
            TypeSpecifier::TypeToken(token) => self.build_type_token(token, loc.clone()),
            TypeSpecifier::UnnamedStruct(unnamed_struct) => self.build_unnamed_struct_type(unnamed_struct),
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
            token_kind @ TokenKind::Int | token_kind @ TokenKind::UInt => {
                let data_layout = self.target_machine.get_target_data();
                InternalType::IntType(InternalIntType {
                    type_str: token_kind.to_string(),
                    int_type: self.context.ptr_sized_int_type(&data_layout, None),
                })
            }
            token_kind @ TokenKind::Int8 | token_kind @ TokenKind::UInt8 | token_kind @ TokenKind::Char => {
                InternalType::IntType(InternalIntType {
                    type_str: token_kind.to_string(),
                    int_type: self.context.i8_type(),
                })
            }
            token_kind @ TokenKind::Int16 | token_kind @ TokenKind::UInt16 => InternalType::IntType(InternalIntType {
                type_str: token_kind.to_string(),
                int_type: self.context.i16_type(),
            }),
            token_kind @ TokenKind::Int32 | token_kind @ TokenKind::UInt32 => InternalType::IntType(InternalIntType {
                type_str: token_kind.to_string(),
                int_type: self.context.i32_type(),
            }),
            token_kind @ TokenKind::Int64 | token_kind @ TokenKind::UInt64 => InternalType::IntType(InternalIntType {
                type_str: token_kind.to_string(),
                int_type: self.context.i64_type(),
            }),
            token_kind @ TokenKind::Int128 | token_kind @ TokenKind::UInt128 => {
                InternalType::IntType(InternalIntType {
                    type_str: token_kind.to_string(),
                    int_type: self.context.i128_type(),
                })
            }
            token_kind @ TokenKind::Float16 => InternalType::FloatType(InternalFloatType {
                type_str: token_kind.to_string(),
                float_type: self.context.f16_type(),
            }),
            token_kind @ TokenKind::Float32 => InternalType::FloatType(InternalFloatType {
                type_str: token_kind.to_string(),
                float_type: self.context.f32_type(),
            }),
            token_kind @ TokenKind::Float64 => InternalType::FloatType(InternalFloatType {
                type_str: token_kind.to_string(),
                float_type: self.context.f64_type(),
            }),
            token_kind @ TokenKind::Float128 => InternalType::FloatType(InternalFloatType {
                type_str: token_kind.to_string(),
                float_type: self.context.f128_type(),
            }),
            token_kind @ TokenKind::Void => InternalType::VoidType(InternalVoidType {
                type_str: token_kind.to_string(),
                void_type: self.context.void_type(),
            }),
            token_kind @ TokenKind::Bool => InternalType::IntType(InternalIntType {
                type_str: token_kind.to_string(),
                int_type: self.context.bool_type(),
            }),
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

    pub(crate) fn build_array_type(
        &self,
        array_type_specifier: ArrayTypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> InternalType<'ctx> {
        let element_type = self.build_type(*array_type_specifier.element_type.clone(), loc.clone(), span_end);
        let array_type = match element_type.into_array_type(
            self.build_array_capacity(array_type_specifier.size, loc.clone(), span_end)
                .try_into()
                .unwrap(),
            array_type_specifier.element_type.to_string(),
        ) {
            Ok(t) => t,
            Err(err) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(err),
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

        array_type
    }

    pub(crate) fn build_array_capacity(&self, array_capacity: ArrayCapacity, loc: Location, span_end: usize) -> u64 {
        match array_capacity {
            ArrayCapacity::Fixed(token_kind) => {
                if let TokenKind::Literal(literal) = token_kind {
                    match self.build_literal(literal.clone()) {
                        InternalValue::IntValue(int_value, ..) => int_value,
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
                } else {
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
            ArrayCapacity::Dynamic => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(
                        "Cannot build array with unspecified size. Consider to use vector if you need a dynamic array."
                            .to_string(),
                    ),
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
    }
}
