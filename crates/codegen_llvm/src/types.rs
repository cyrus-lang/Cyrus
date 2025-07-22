use crate::context::CodeGenLLVM;
use crate::diag::*;
use crate::resolver::MetadataResolverResult;
use crate::structs::StructID;
use crate::structs::StructMethodMetadata;
use crate::structs::UnnamedStructTypeMetadata;
use crate::values::InternalValue;
use crate::values::Lvalue;
use crate::values::TypedPointerValue;
use ast::ast::AccessSpecifier;
use ast::ast::ArrayCapacity;
use ast::ast::ArrayTypeSpecifier;
use ast::ast::Field;
use ast::ast::ModuleImport;
use ast::ast::ModuleSegment;
use ast::ast::TypeSpecifier;
use ast::ast::Typedef;
use ast::format::module_segments_as_string;
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
use std::collections::HashMap;
use std::fmt;
use std::process::exit;

pub type TypedefTable<'a> = HashMap<String, TypedefMetadata<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct TypedefMetadata<'a> {
    pub internal_type: InternalType<'a>,
    pub access_specifier: AccessSpecifier,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum InternalType<'a> {
    BoolType(InternalBoolType<'a>),
    IntType(InternalIntType<'a>),
    FloatType(InternalFloatType<'a>),
    StructType(InternalStructType<'a>),
    UnnamedStruct(InternalUnnamedStructType<'a>),
    VectorType(InternalVectorType<'a>),
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
    pub int_kind: TokenKind,
    pub int_type: IntType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalFloatType<'a> {
    pub type_str: String,
    pub float_type: FloatType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InternalStructType<'a> {
    pub struct_id: StructID,
    pub struct_name: ModuleImport,
    pub struct_type: StructType<'a>,
    pub fields: Vec<Field>,
    pub methods: Vec<StructMethodMetadata<'a>>,
    pub type_str: String,
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
                array_type: internal_struct_type.struct_type.array_type(size),
            })),
            InternalType::VectorType(internal_vector_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::VectorType(internal_vector_type.clone())),
                array_type: internal_vector_type.vector_type.array_type(size),
            })),
            InternalType::PointerType(internal_pointer_type) => Ok(InternalType::ArrayType(InternalArrayType {
                type_str,
                inner_type: Box::new(InternalType::PointerType(internal_pointer_type.clone())),
                array_type: internal_pointer_type.ptr_type.array_type(size),
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
            InternalType::ConstType(internal_const_type) => internal_const_type.inner_type.into_internal_value(value),
            InternalType::VoidType(_) => unreachable!(),
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
            InternalType::StructType(t) => Ok(t.struct_type.as_basic_type_enum()),
            InternalType::VectorType(t) => Ok(t.vector_type.as_basic_type_enum()),
            InternalType::PointerType(t) => Ok(t.ptr_type.as_basic_type_enum()),
            InternalType::Lvalue(t) => Ok(t.ptr_type.as_basic_type_enum()),
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
            InternalType::StructType(t) => t.struct_type.as_type_ref(),
            InternalType::VectorType(t) => t.vector_type.as_type_ref(),
            InternalType::PointerType(t) => t.ptr_type.as_type_ref(),
            InternalType::Lvalue(t) => t.ptr_type.as_type_ref(),
            InternalType::ConstType(t) => t.inner_type.as_type_ref(),
            InternalType::BoolType(t) => t.bool_type.as_type_ref(),
            InternalType::UnnamedStruct(t) => t.unnamed_struct_metadata.struct_type.as_type_ref(),
            InternalType::VoidType(t) => AnyType::as_any_type_enum(&t.void_type).as_type_ref(),
        }
    }
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_typedef(&mut self, typedef: Typedef) {
        self.error_if_already_declared(typedef.identifier.name.clone(), typedef.loc.clone(), typedef.span.end);

        let internal_type = self.build_type(typedef.type_specifier.clone(), typedef.loc.clone(), typedef.span.end);
        let mut module_metadata = self.get_module_metadata_by_module_id(self.module_id).unwrap();
        let typedef_metadata = TypedefMetadata {
            internal_type,
            access_specifier: typedef.access_specifier.clone(),
        };
        module_metadata.insert_typedef(typedef.identifier.name, typedef_metadata);
        drop(module_metadata);
    }

    pub(crate) fn compatible_types(&self, lvalue_type: InternalType<'ctx>, rvalue_type: InternalType<'ctx>) -> bool {
        match (lvalue_type, rvalue_type) {
            (InternalType::BoolType(_), InternalType::BoolType(_)) => true,
            (InternalType::IntType(_), InternalType::IntType(_)) => true,
            (InternalType::FloatType(_), InternalType::FloatType(_)) => true,
            (InternalType::FloatType(_), InternalType::IntType(_)) => true,
            (InternalType::IntType(_), InternalType::FloatType(_)) => true,
            (InternalType::PointerType(_), InternalType::PointerType(_)) => true,
            (InternalType::IntType(_), InternalType::PointerType(_))
            | (InternalType::PointerType(_), InternalType::IntType(_)) => true,
            (InternalType::StructType(struct_metadata1), InternalType::StructType(struct_metadata2)) => {
                struct_metadata1.struct_type == struct_metadata2.struct_type
                    && struct_metadata1.struct_type.count_fields() == struct_metadata2.struct_type.count_fields()
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
            (InternalType::VoidType(_), _) => false,
            (InternalType::ConstType(internal_const_type), rvalue_type) => {
                self.compatible_types(*internal_const_type.inner_type, rvalue_type)
            }
            _ => false,
        }
    }

    pub(crate) fn get_defined_type(&self, mut module_import: ModuleImport) -> Option<InternalType<'ctx>> {
        match module_import.as_identifier() {
            Some(identifier) => match self.resolve_metadata(self.module_id, identifier.name) {
                Some(metadata_resolver_result) => match metadata_resolver_result {
                    MetadataResolverResult::Struct(struct_metadata) => {
                        Some(InternalType::StructType(struct_metadata.as_internal_struct_type()))
                    }
                    MetadataResolverResult::Typedef(typedef_metadata) => Some(typedef_metadata.internal_type),
                    MetadataResolverResult::GlobalVariable(_) => None,
                    MetadataResolverResult::Func(_) => None,
                },
                None => None,
            },
            None => {
                let type_name = module_import.segments.pop().unwrap().as_identifier().name;
                let module_id = match self.get_imported_module(module_import.segments.clone()) {
                    Some(imported_module) => imported_module.module_id,
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::UndefinedDataType(module_segments_as_string(
                                module_import.segments.clone(),
                            )),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: module_import.loc.line,
                                column: module_import.loc.column,
                                length: module_import.span.end,
                            }),
                        });
                        exit(1);
                    }
                };

                let module_metadata = self.get_module_metadata_by_module_id(module_id).unwrap();

                let internal_type = match module_metadata.get_defined_type(type_name.clone()) {
                    Some(internal_type) => internal_type,
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::SymbolNotFoundInModule(
                                type_name,
                                module_segments_as_string(module_import.segments.clone()),
                            ),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: module_import.loc.line,
                                column: module_import.loc.column,
                                length: module_import.span.end,
                            }),
                        });
                        exit(1);
                    }
                };

                drop(module_metadata);
                Some(internal_type)
            }
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
            TypeSpecifier::Identifier(identifier) => {
                match self.get_defined_type(ModuleImport {
                    segments: vec![ModuleSegment::SubModule(identifier.clone())],
                    span: identifier.span.clone(),
                    loc: identifier.loc.clone(),
                }) {
                    Some(internal_type) => internal_type,
                    None => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::UndefinedDataType(identifier.name),
                            location: Some(DiagLoc {
                                file: self.file_path.clone(),
                                line: identifier.loc.line,
                                column: identifier.loc.column,
                                length: identifier.span.end,
                            }),
                        });
                        exit(1);
                    }
                }
            }
            TypeSpecifier::ModuleImport(module_import) => match self.get_defined_type(module_import.clone()) {
                Some(internal_type) => internal_type,
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::UndefinedDataType(module_segments_as_string(module_import.segments)),
                        location: Some(DiagLoc {
                            file: self.file_path.clone(),
                            line: loc.line,
                            column: loc.column,
                            length: span_end,
                        }),
                    });
                    exit(1);
                }
            },
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
            TokenKind::Identifier { name } => match self.resolve_typedef(self.module_id, name.clone()) {
                Some(typedef_metadata) => typedef_metadata.internal_type,
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
            },
            token_kind @ TokenKind::UIntPtr | token_kind @ TokenKind::IntPtr | token_kind @ TokenKind::SizeT => {
                let data_layout = self.target_machine.get_target_data();
                InternalType::IntType(InternalIntType {
                    type_str: token_kind.to_string(),
                    int_kind: token_kind,
                    int_type: self.context.ptr_sized_int_type(&data_layout, None),
                })
            }
            token_kind @ TokenKind::Int | token_kind @ TokenKind::UInt => InternalType::IntType(InternalIntType {
                type_str: token_kind.to_string(),
                int_kind: token_kind,
                int_type: self.context.i32_type(),
            }),
            token_kind @ TokenKind::Int8 | token_kind @ TokenKind::UInt8 | token_kind @ TokenKind::Char => {
                InternalType::IntType(InternalIntType {
                    type_str: token_kind.to_string(),
                    int_kind: token_kind,
                    int_type: self.context.i8_type(),
                })
            }
            token_kind @ TokenKind::Int16 | token_kind @ TokenKind::UInt16 => InternalType::IntType(InternalIntType {
                type_str: token_kind.to_string(),
                int_kind: token_kind,
                int_type: self.context.i16_type(),
            }),
            token_kind @ TokenKind::Int32 | token_kind @ TokenKind::UInt32 => InternalType::IntType(InternalIntType {
                type_str: token_kind.to_string(),
                int_kind: token_kind,
                int_type: self.context.i32_type(),
            }),
            token_kind @ TokenKind::Int64 | token_kind @ TokenKind::UInt64 => InternalType::IntType(InternalIntType {
                type_str: token_kind.to_string(),
                int_kind: token_kind,
                int_type: self.context.i64_type(),
            }),
            token_kind @ TokenKind::Int128 | token_kind @ TokenKind::UInt128 => {
                InternalType::IntType(InternalIntType {
                    type_str: token_kind.to_string(),
                    int_kind: token_kind,
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
            token_kind @ TokenKind::Bool => InternalType::BoolType(InternalBoolType {
                type_str: token_kind.to_string(),
                bool_type: self.context.bool_type(),
            }),
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
