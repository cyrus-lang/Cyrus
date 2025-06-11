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
    BoolType(IntType<'a>),
    IntType(IntType<'a>),
    FloatType(FloatType<'a>),
    StructType(StructMetadata<'a>),
    UnnamedStruct(UnnamedStructTypeMetadata<'a>),
    VectorType(VectorType<'a>),
    StringType(StringType<'a>),
    VoidType(VoidType<'a>),
    PointerType(Box<TypedPointerType<'a>>),
    ConstType(Box<InternalType<'a>>),
    Lvalue(Box<LvalueType<'a>>),
    ArrayType(Box<InternalType<'a>>, ArrayType<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct LvalueType<'a> {
    pub ptr_type: PointerType<'a>,
    pub pointee_ty: InternalType<'a>,
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

#[derive(Debug, Clone)]
pub(crate) enum DefinedType<'a> {
    Struct(StructMetadata<'a>),
    // Typedef(...),
}

impl<'a> DefinedType<'a> {
    pub fn into_internal_type(&self) -> InternalType<'a> {
        match self {
            DefinedType::Struct(struct_metadata) => InternalType::StructType(struct_metadata.clone()),
        }
    }
}

impl<'a> InternalType<'a> {
    pub fn into_array_type(&self, size: u32) -> Result<InternalType<'a>, String> {
        match self {
            InternalType::IntType(int_type) => Ok(InternalType::ArrayType(
                Box::new(InternalType::IntType(*int_type)),
                int_type.array_type(size),
            )),
            InternalType::FloatType(float_type) => Ok(InternalType::ArrayType(
                Box::new(InternalType::FloatType(*float_type)),
                float_type.array_type(size),
            )),
            InternalType::ArrayType(element_type, array_type) => Ok(InternalType::ArrayType(
                element_type.clone(),
                array_type.array_type(size),
            )),
            InternalType::StructType(struct_metadata) => Ok(InternalType::ArrayType(
                Box::new(InternalType::StructType(struct_metadata.clone())),
                struct_metadata.struct_type.array_type(size),
            )),
            InternalType::VectorType(vector_type) => Ok(InternalType::ArrayType(
                Box::new(InternalType::VectorType(*vector_type)),
                vector_type.array_type(size),
            )),
            InternalType::StringType(string_type) => Ok(InternalType::ArrayType(
                Box::new(InternalType::StringType(string_type.clone())),
                string_type.struct_type.array_type(size),
            )),
            InternalType::PointerType(typed_pointer_type) => Ok(InternalType::ArrayType(
                Box::new(InternalType::PointerType(typed_pointer_type.clone())),
                typed_pointer_type.ptr_type.array_type(size),
            )),
            InternalType::ConstType(internal_type) => internal_type.into_array_type(size),
            InternalType::VoidType(_) => Err("VoidType cannot be converted to an array type.".to_string()),
            InternalType::Lvalue(_) => Err("Lvalue cannot be converted to an array type.".to_string()),
            InternalType::BoolType(int_type) => Ok(InternalType::ArrayType(
                Box::new(InternalType::BoolType(*int_type)),
                int_type.array_type(size),
            )),
            InternalType::UnnamedStruct(unnamed_struct_metadata) => Ok(InternalType::ArrayType(
                Box::new(InternalType::UnnamedStruct(unnamed_struct_metadata.clone())),
                unnamed_struct_metadata.struct_type.array_type(size),
            )),
        }
    }

    pub fn into_internal_value(
        &self,
        value: inkwell::values::BasicValueEnum<'a>,
    ) -> Result<InternalValue<'a>, &'static str> {
        match self {
            InternalType::BoolType(_) => Ok(InternalValue::BoolValue(value.into_int_value())),
            InternalType::IntType(ty) => Ok(InternalValue::IntValue(
                value.into_int_value(),
                InternalType::IntType(ty.clone()),
            )),
            InternalType::FloatType(ty) => Ok(InternalValue::FloatValue(
                value.into_float_value(),
                InternalType::FloatType(ty.clone()),
            )),
            InternalType::ArrayType(element_type, ty) => Ok(InternalValue::ArrayValue(
                value.into_array_value(),
                InternalType::ArrayType(element_type.clone(), ty.clone()),
            )),
            InternalType::StructType(ty) => Ok(InternalValue::StructValue(
                value.into_struct_value(),
                InternalType::StructType(ty.clone()),
            )),
            InternalType::UnnamedStruct(ty) => Ok(InternalValue::UnnamedStructValue(
                value.into_struct_value(),
                InternalType::UnnamedStruct(ty.clone()),
            )),
            InternalType::VectorType(ty) => Ok(InternalValue::VectorValue(
                value.into_vector_value(),
                InternalType::VectorType(ty.clone()),
            )),
            InternalType::StringType(_) => Ok(InternalValue::StringValue(StringValue {
                struct_value: value.into_struct_value(),
            })),
            InternalType::PointerType(ptr_ty) => Ok(InternalValue::PointerValue(TypedPointerValue {
                ptr: value.into_pointer_value(),
                pointee_ty: ptr_ty.pointee_ty.clone(),
            })),
            InternalType::Lvalue(ptr_ty) => Ok(InternalValue::Lvalue(Lvalue {
                ptr: value.into_pointer_value(),
                pointee_ty: ptr_ty.pointee_ty.clone(),
            })),
            InternalType::VoidType(ty) => Ok(InternalValue::PointerValue(TypedPointerValue {
                ptr: value.into_pointer_value(),
                pointee_ty: InternalType::VoidType(*ty),
            })),
            InternalType::ConstType(internal_type) => internal_type.into_internal_value(value),
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
        matches!(self, InternalType::ArrayType(_, _))
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

    pub fn to_basic_type(&self, ptr_type: PointerType<'a>) -> BasicTypeEnum<'a> {
        match self {
            InternalType::IntType(t) => (*t).as_basic_type_enum(),
            InternalType::FloatType(t) => (*t).as_basic_type_enum(),
            InternalType::ArrayType(_, t) => (*t).as_basic_type_enum(),
            InternalType::StructType(t) => (*t).struct_type.as_basic_type_enum(),
            InternalType::VectorType(t) => (*t).as_basic_type_enum(),
            InternalType::PointerType(t) => t.ptr_type.as_basic_type_enum(),
            InternalType::Lvalue(t) => t.ptr_type.as_basic_type_enum(),
            InternalType::StringType(t) => (*t).struct_type.as_basic_type_enum(),
            InternalType::VoidType(_) => BasicTypeEnum::PointerType(ptr_type),
            InternalType::ConstType(t) => t.to_basic_type(ptr_type),
            InternalType::BoolType(t) => (*t).as_basic_type_enum(),
            InternalType::UnnamedStruct(t) => (*t).struct_type.as_basic_type_enum(),
        }
    }

    pub fn as_type_ref(&self) -> LLVMTypeRef {
        match self {
            InternalType::IntType(t) => t.as_type_ref(),
            InternalType::FloatType(t) => t.as_type_ref(),
            InternalType::ArrayType(_, t) => t.as_type_ref(),
            InternalType::StructType(t) => t.struct_type.as_type_ref(),
            InternalType::VectorType(t) => t.as_type_ref(),
            InternalType::PointerType(t) => t.ptr_type.as_type_ref(),
            InternalType::Lvalue(t) => t.ptr_type.as_type_ref(),
            InternalType::StringType(t) => t.struct_type.as_type_ref(),
            InternalType::VoidType(t) => inkwell::types::AnyType::as_any_type_enum(t).as_type_ref(),
            InternalType::ConstType(t) => t.as_type_ref(),
            InternalType::BoolType(t) => t.as_type_ref(),
            InternalType::UnnamedStruct(t) => t.struct_type.as_type_ref(),
        }
    }
}

// TODO
// impl<'a> fmt::Display for InternalType<'a> {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             InternalType::IntType(int_type) => {
//                 match int_type.get_bit_width() {
//                     1 => write!(f, "bool"),
//                     8 => write!(f, "i8"),
//                     16 => write!(f, "i16"),
//                     32 => write!(f, "i32"),
//                     64 => write!(f, "i64"),
//                     128 => write!(f, "i128"),
//                 }
//             },
//             InternalType::FloatType(float_type) => todo!(),
//             InternalType::ArrayType(array_type) => todo!(),
//             InternalType::StructType(struct_type) => todo!(),
//             InternalType::VectorType(vector_type) => todo!(),
//             InternalType::StringType(string_type) => todo!(),
//             InternalType::VoidType(void_type) => todo!(),
//             InternalType::PointerType(typed_pointer_type) => todo!(),
//             InternalType::ConstType(internal_type) => todo!(),
//             InternalType::Lvalue(lvalue_type) => todo!(),
//         }
//     }
// }

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn compatible_types(&self, lvalue_type: InternalType<'ctx>, rvalue_type: InternalType<'ctx>) -> bool {
        match (lvalue_type, rvalue_type) {
            (InternalType::IntType(_), InternalType::IntType(_)) => true,
            (InternalType::FloatType(_), InternalType::FloatType(_)) => true,
            (InternalType::FloatType(_), InternalType::IntType(_)) => true,
            (InternalType::IntType(_), InternalType::FloatType(_)) => true,
            (InternalType::PointerType(_), InternalType::PointerType(_)) => true,
            (InternalType::StructType(struct_metadata1), InternalType::StructType(struct_metadata2)) => {
                struct_metadata1.struct_type == struct_metadata2.struct_type
                    && struct_metadata1.fields.len() == struct_metadata2.fields.len()
            }
            (
                InternalType::UnnamedStruct(unnamed_struct_metadata1),
                InternalType::UnnamedStruct(unnamed_struct_metadata2),
            ) => {
                unnamed_struct_metadata1.struct_type == unnamed_struct_metadata2.struct_type
                    && unnamed_struct_metadata1.fields.len() == unnamed_struct_metadata2.fields.len()
            }
            (InternalType::VectorType(_), InternalType::VectorType(_)) => true,
            (InternalType::ArrayType(element_type1, arr1), InternalType::ArrayType(element_type2, arr2)) => {
                (arr1.len() == arr2.len()) && self.compatible_types(*element_type1, *element_type2)
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
            (InternalType::ConstType(inner_type), rvalue_type) => self.compatible_types(*inner_type, rvalue_type),
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
                Some(struct_metadata) => DefinedType::Struct(struct_metadata.clone()),
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
        match type_specifier {
            TypeSpecifier::Const(inner_type_specifier) => {
                InternalType::ConstType(Box::new(self.build_type(*inner_type_specifier, loc, span_end)))
            }
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
            TypeSpecifier::AddressOf(type_specifier) => todo!(),
            TypeSpecifier::Dereference(inner_type_specifier) => {
                let pointee_ty = self.build_type(*inner_type_specifier, loc.clone(), span_end);
                InternalType::PointerType(Box::new(TypedPointerType {
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
            TokenKind::Int | TokenKind::UInt => {
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

    pub(crate) fn build_array_type(
        &self,
        array_type_specifier: ArrayTypeSpecifier,
        loc: Location,
        span_end: usize,
    ) -> InternalType<'ctx> {
        let element_type = self.build_type(*array_type_specifier.element_type, loc.clone(), span_end);
        let array_type = match element_type.into_array_type(
            self.build_array_capacity(array_type_specifier.size, loc.clone(), span_end)
                .try_into()
                .unwrap(),
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
                        "Cannot build array with dynamic memory management. Consider to use vector instead."
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
