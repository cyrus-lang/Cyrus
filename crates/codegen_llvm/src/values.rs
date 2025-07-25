use crate::{
    context::CodeGenLLVM,
    diag::*,
    types::{InternalBoolType, InternalPointerType, InternalType},
};
use ast::token::Location;
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    values::{
        AnyValue, ArrayValue, BasicMetadataValueEnum, BasicValueEnum, FloatValue, IntValue, PointerValue, StructValue,
        VectorValue,
    },
};
use std::process::exit;

#[derive(Debug, Clone)]
pub(crate) enum InternalValue<'a> {
    BoolValue(IntValue<'a>, InternalType<'a>),
    IntValue(IntValue<'a>, InternalType<'a>),
    FloatValue(FloatValue<'a>, InternalType<'a>),
    ArrayValue(ArrayValue<'a>, InternalType<'a>),
    StructValue(StructValue<'a>, InternalType<'a>),
    UnnamedStructValue(StructValue<'a>, InternalType<'a>),
    VectorValue(VectorValue<'a>, InternalType<'a>),
    PointerValue(TypedPointerValue<'a>),
    EnumVariantValue(StructValue<'a>, InternalType<'a>),
    Lvalue(Lvalue<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct Lvalue<'a> {
    pub ptr: PointerValue<'a>,
    pub pointee_ty: InternalType<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedPointerValue<'a> {
    pub type_str: String,
    pub ptr: PointerValue<'a>,
    pub pointee_ty: InternalType<'a>,
}

impl<'a> InternalValue<'a> {
    pub fn is_const(&self) -> bool {
        match self {
            InternalValue::BoolValue(..) => true,
            InternalValue::IntValue(..) => true,
            InternalValue::FloatValue(..) => true,
            InternalValue::Lvalue(..) => false,
            InternalValue::PointerValue(..) => false,
            InternalValue::ArrayValue(array_value, ..) => array_value.is_const(),
            InternalValue::StructValue(struct_value, ..) => struct_value.is_const(),
            InternalValue::UnnamedStructValue(struct_value, ..) => struct_value.is_const(),
            InternalValue::VectorValue(vector_value, ..) => vector_value.is_const(),
            InternalValue::EnumVariantValue(struct_value, ..) => struct_value.is_const(),
        }
    }

    pub(crate) fn get_type(&self) -> InternalType<'a> {
        match self {
            InternalValue::BoolValue(_, ty) => ty.clone(),
            InternalValue::IntValue(_, ty) => ty.clone(),
            InternalValue::FloatValue(_, ty, ..) => ty.clone(),
            InternalValue::ArrayValue(_, ty, ..) => ty.clone(),
            InternalValue::StructValue(_, ty, ..) => ty.clone(),
            InternalValue::VectorValue(_, ty, ..) => ty.clone(),
            InternalValue::EnumVariantValue(_, ty) => ty.clone(),
            InternalValue::PointerValue(v) => InternalType::PointerType(Box::new(InternalPointerType {
                ptr_type: v.ptr.get_type(),
                pointee_ty: v.pointee_ty.clone(),
            })),
            InternalValue::Lvalue(v) => InternalType::PointerType(Box::new(InternalPointerType {
                ptr_type: v.ptr.get_type(),
                pointee_ty: v.pointee_ty.clone(),
            })),
            InternalValue::UnnamedStructValue(_, internal_type) => internal_type.clone(),
        }
    }
}

impl<'a> From<TypedPointerValue<'a>> for InternalValue<'a> {
    fn from(val: TypedPointerValue<'a>) -> Self {
        InternalValue::PointerValue(val)
    }
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn internal_value_to_basic_metadata(
        &self,
        internal_value: InternalValue<'ctx>,
    ) -> BasicMetadataValueEnum<'ctx> {
        match internal_value {
            InternalValue::BoolValue(v, ..) => BasicMetadataValueEnum::IntValue(v),
            InternalValue::IntValue(v, ..) => BasicMetadataValueEnum::IntValue(v),
            InternalValue::FloatValue(v, ..) => BasicMetadataValueEnum::FloatValue(v),
            InternalValue::ArrayValue(v, ..) => BasicMetadataValueEnum::ArrayValue(v),
            InternalValue::StructValue(v, ..) => BasicMetadataValueEnum::StructValue(v),
            InternalValue::VectorValue(v, ..) => BasicMetadataValueEnum::VectorValue(v),
            InternalValue::PointerValue(v) => BasicMetadataValueEnum::PointerValue(v.ptr),
            InternalValue::Lvalue(v) => BasicMetadataValueEnum::PointerValue(v.ptr),
            InternalValue::UnnamedStructValue(struct_value, _) => BasicMetadataValueEnum::StructValue(struct_value),
            InternalValue::EnumVariantValue(v, ..) => BasicMetadataValueEnum::StructValue(v),
        }
    }

    pub(crate) fn build_zero_initialized_internal_value(
        &self,
        value_type: InternalType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match value_type {
            InternalType::IntType(internal_int_type) => InternalValue::IntValue(
                internal_int_type.int_type.const_zero(),
                InternalType::IntType(internal_int_type),
            ),
            InternalType::FloatType(internal_float_type) => InternalValue::FloatValue(
                internal_float_type.float_type.const_zero(),
                InternalType::FloatType(internal_float_type),
            ),
            InternalType::ArrayType(internal_array_type) => InternalValue::ArrayValue(
                internal_array_type.array_type.const_zero(),
                InternalType::ArrayType(internal_array_type),
            ),
            InternalType::StructType(internal_struct_type) => InternalValue::StructValue(
                internal_struct_type.struct_type.const_zero(),
                InternalType::StructType(internal_struct_type),
            ),
            InternalType::UnnamedStruct(internal_unnamed_struct_type) => InternalValue::UnnamedStructValue(
                internal_unnamed_struct_type
                    .unnamed_struct_metadata
                    .struct_type
                    .const_zero(),
                InternalType::UnnamedStruct(internal_unnamed_struct_type),
            ),
            InternalType::VectorType(internal_vector_type) => InternalValue::VectorValue(
                internal_vector_type.vector_type.const_zero(),
                InternalType::VectorType(internal_vector_type),
            ),
            InternalType::PointerType(internal_pointer_type) => InternalValue::PointerValue(TypedPointerValue {
                type_str: format!("{}*", internal_pointer_type.pointee_ty.to_string()),
                ptr: internal_pointer_type.ptr_type.const_zero(),
                pointee_ty: internal_pointer_type.pointee_ty,
            }),
            InternalType::ConstType(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(
                        "Cannot build zero-initialized value for const without inner type.".to_string(),
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
            InternalType::VoidType(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot build zero-initialized value for void type.".to_string()),
                    location: Some(DiagLoc {
                        file: self.file_path.clone(),
                        line: loc.line,
                        column: loc.column,
                        length: span_end,
                    }),
                });
                exit(1);
            }
            InternalType::BoolType(internal_bool_type) => InternalValue::BoolValue(
                internal_bool_type.bool_type.const_zero(),
                InternalType::BoolType(internal_bool_type),
            ),
            InternalType::Lvalue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom(
                        "References must be initialized with a valid object. Zero-initialization is not allowed."
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
            InternalType::EnumType(internal_enum_type) => {
                let enum_metadata = self
                    .resolve_enum_metadata_with_struct_id(internal_enum_type.enum_id)
                    .unwrap();

                self.build_construct_enum(enum_metadata, 0, loc, span_end)
            }
        }
    }

    pub(crate) fn new_internal_value(
        &self,
        value: BasicValueEnum<'ctx>,
        value_type: InternalType<'ctx>,
    ) -> InternalValue<'ctx> {
        match value_type {
            InternalType::BoolType(internal_bool_type) => {
                InternalValue::BoolValue(value.into_int_value(), InternalType::BoolType(internal_bool_type))
            }
            InternalType::IntType(internal_int_type) => {
                InternalValue::IntValue(value.into_int_value(), InternalType::IntType(internal_int_type))
            }
            InternalType::FloatType(internal_float_type) => {
                InternalValue::FloatValue(value.into_float_value(), InternalType::FloatType(internal_float_type))
            }
            InternalType::ArrayType(internal_array_type) => {
                InternalValue::ArrayValue(value.into_array_value(), InternalType::ArrayType(internal_array_type))
            }
            InternalType::StructType(internal_struct_type) => InternalValue::StructValue(
                value.into_struct_value(),
                InternalType::StructType(internal_struct_type),
            ),
            InternalType::VectorType(internal_vector_type) => InternalValue::VectorValue(
                value.into_vector_value(),
                InternalType::VectorType(internal_vector_type),
            ),
            InternalType::PointerType(internal_pointer_type) => InternalValue::PointerValue(TypedPointerValue {
                type_str: format!("{}*", internal_pointer_type.pointee_ty.to_string()),
                ptr: value.into_pointer_value(),
                pointee_ty: internal_pointer_type.pointee_ty,
            }),
            InternalType::Lvalue(internal_pointer_type) => InternalValue::Lvalue(Lvalue {
                ptr: value.into_pointer_value(),
                pointee_ty: internal_pointer_type.pointee_ty,
            }),
            InternalType::VoidType(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot create InternalValue from VoidType.".to_string()),
                    location: None,
                });
                exit(1);
            }
            InternalType::ConstType(_) => unimplemented!(),
            InternalType::UnnamedStruct(unnamed_struct_metadata) => InternalValue::UnnamedStructValue(
                value.into_struct_value(),
                InternalType::UnnamedStruct(unnamed_struct_metadata),
            ),
            InternalType::EnumType(internal_enum_type) => todo!(),
        }
    }

    pub(crate) fn implicit_cast(
        &self,
        rvalue: InternalValue<'ctx>,
        target_type: InternalType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> BasicValueEnum<'ctx> {
        self.internal_value_to_basic_metadata(self.build_cast_expression_internal(rvalue, target_type, loc, span_end))
            .as_any_value_enum()
            .try_into()
            .unwrap()
    }

    pub(crate) fn internal_value_as_rvalue(
        &self,
        internal_value: InternalValue<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match internal_value {
            InternalValue::BoolValue(int_value, internal_type) => InternalValue::BoolValue(int_value, internal_type),
            InternalValue::PointerValue(typed_pointer_value) => InternalValue::PointerValue(typed_pointer_value),
            InternalValue::IntValue(v, ty) => InternalValue::IntValue(v, ty),
            InternalValue::FloatValue(v, ty) => InternalValue::FloatValue(v, ty),
            InternalValue::ArrayValue(v, ty) => InternalValue::ArrayValue(v, ty),
            InternalValue::StructValue(v, ty) => InternalValue::StructValue(v, ty),
            InternalValue::EnumVariantValue(v, ty) => InternalValue::StructValue(v, ty),
            InternalValue::UnnamedStructValue(v, ty) => InternalValue::UnnamedStructValue(v, ty),
            InternalValue::VectorValue(v, ty) => InternalValue::VectorValue(v, ty),
            InternalValue::Lvalue(typed_pointer_value) => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());

                let basic_type = match typed_pointer_value.pointee_ty.to_basic_type(ptr_type) {
                    Ok(basic_type) => basic_type,
                    Err(err) => {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom(err.to_string()),
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

                let value = self
                    .builder
                    .build_load(basic_type, typed_pointer_value.ptr, "load")
                    .unwrap();

                typed_pointer_value
                    .pointee_ty
                    .into_internal_value(value)
                    .unwrap_or_else(|_| {
                        display_single_diag(Diag {
                            level: DiagLevel::Error,
                            kind: DiagKind::Custom("Failed to convert loaded value to InternalValue.".to_string()),
                            location: None,
                        });
                        exit(1);
                    })
            }
        }
    }

    fn int_to_float_cast(
        &self,
        value: inkwell::values::IntValue<'ctx>,
        value_ty: InternalType,
        target_float_ty: inkwell::types::FloatType<'ctx>,
    ) -> inkwell::values::FloatValue<'ctx> {
        if let InternalType::IntType(internal_int_type) = value_ty {
            if self.is_integer_signed(internal_int_type.int_kind) {
                self.builder
                    .build_signed_int_to_float(value, target_float_ty, "cast")
                    .unwrap()
            } else {
                self.builder
                    .build_unsigned_int_to_float(value, target_float_ty, "cast")
                    .unwrap()
            }
        } else {
            unreachable!()
        }
    }

    pub(crate) fn bin_op_add<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(
                InternalValue::BoolValue(self.builder.build_int_add(left, right, "add").unwrap(), left_ty),
            ),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, right_ty)) => {
                let right = self.int_to_float_cast(right, right_ty, left.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, left_ty), InternalValue::FloatValue(right, right_ty)) => {
                let left = self.int_to_float_cast(left, left_ty, right.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                    right_ty,
                ))
            }
            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Ok(
                InternalValue::FloatValue(self.builder.build_float_add(left, right, "add").unwrap(), left_ty),
            ),
            (InternalValue::PointerValue(_), InternalValue::PointerValue(_)) => {
                Err("Pointer addition gives undefined. Consider to use pointer arithmetic instead.".to_string())
            }
            _ => Err("Unsupported types for add operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_sub<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(
                InternalValue::BoolValue(self.builder.build_int_sub(left, right, "sub").unwrap(), left_ty),
            ),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, right_ty)) => {
                let right = self.int_to_float_cast(right, right_ty, left.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, left_ty), InternalValue::FloatValue(right, right_ty)) => {
                let left = self.int_to_float_cast(left, left_ty, right.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                    right_ty,
                ))
            }
            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Ok(
                InternalValue::FloatValue(self.builder.build_float_sub(left, right, "sub").unwrap(), left_ty),
            ),
            (InternalValue::PointerValue(_), InternalValue::PointerValue(_)) => {
                Err("Pointer subtracting gives undefined. Consider to use pointer arithmetic instead.".to_string())
            }
            _ => Err("Unsupported types for subtract operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_mul<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(
                InternalValue::BoolValue(self.builder.build_int_mul(left, right, "mul").unwrap(), left_ty),
            ),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, right_ty)) => {
                let right = self.int_to_float_cast(right, right_ty, left.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, left_ty), InternalValue::FloatValue(right, right_ty)) => {
                let left = self.int_to_float_cast(left, left_ty, right.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                    right_ty,
                ))
            }
            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Ok(
                InternalValue::FloatValue(self.builder.build_float_mul(left, right, "mul").unwrap(), left_ty),
            ),
            (InternalValue::PointerValue(_), InternalValue::PointerValue(_)) => {
                Err("Pointer multiplication gives undefined. Consider to use pointer arithmetic instead.".to_string())
            }
            _ => Err("Unsupported types for multiply operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_div<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(
                InternalValue::BoolValue(self.builder.build_int_signed_div(left, right, "div").unwrap(), left_ty),
            ),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, right_ty)) => {
                let right = self.int_to_float_cast(right, right_ty, left.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, left_ty), InternalValue::FloatValue(right, right_ty)) => {
                let left = self.int_to_float_cast(left, left_ty, right.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                    right_ty,
                ))
            }
            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Ok(
                InternalValue::FloatValue(self.builder.build_float_div(left, right, "div").unwrap(), left_ty),
            ),
            (InternalValue::PointerValue(_), InternalValue::PointerValue(_)) => {
                Err("Pointer dividing gives undefined. Consider to use pointer arithmetic instead.".to_string())
            }
            _ => Err("Unsupported types for divide operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_rem<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(
                InternalValue::BoolValue(self.builder.build_int_signed_rem(left, right, "rem").unwrap(), left_ty),
            ),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, right_ty)) => {
                let right = self.int_to_float_cast(right, right_ty, left.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, left_ty), InternalValue::FloatValue(right, right_ty)) => {
                let left = self.int_to_float_cast(left, left_ty, right.get_type());
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                    right_ty,
                ))
            }
            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Ok(
                InternalValue::FloatValue(self.builder.build_float_rem(left, right, "rem").unwrap(), left_ty),
            ),
            (InternalValue::PointerValue(_), InternalValue::PointerValue(_)) => {
                Err("Pointer remnant gives undefined. Consider to use pointer arithmetic instead.".to_string())
            }
            _ => Err("Unsupported types for remainder operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_lt<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "cmp_lt")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    Ok(InternalValue::BoolValue(
                        self.builder
                            .build_int_compare(
                                IntPredicate::ULT,
                                typed_pointer_value1.ptr,
                                typed_pointer_value2.ptr,
                                "cmp_lt",
                            )
                            .unwrap(),
                        InternalType::BoolType(InternalBoolType {
                            type_str: "bool".to_string(),
                            bool_type: self.context.bool_type(),
                        }),
                    ))
                } else {
                    Err("Pointer types do not match for less-than operator.".to_string())
                }
            }
            _ => Err("Unsupported types for less-than operator.".to_string()),
        }
    }

    // ANCHOR
    // FIXME Refactor signed/unsigned comparisons

    pub(crate) fn bin_op_le<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "cmp_le")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    Ok(InternalValue::BoolValue(
                        self.builder
                            .build_int_compare(
                                IntPredicate::ULE,
                                typed_pointer_value1.ptr,
                                typed_pointer_value2.ptr,
                                "cmp_le",
                            )
                            .unwrap(),
                        InternalType::BoolType(InternalBoolType {
                            type_str: "bool".to_string(),
                            bool_type: self.context.bool_type(),
                        }),
                    ))
                } else {
                    Err("Pointer types do not match for less-equal operator.".to_string())
                }
            }
            _ => Err("Unsupported types for less-equal operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_gt<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "cmp_gt")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    Ok(InternalValue::BoolValue(
                        self.builder
                            .build_int_compare(
                                IntPredicate::UGT,
                                typed_pointer_value1.ptr,
                                typed_pointer_value2.ptr,
                                "cmp_gt",
                            )
                            .unwrap(),
                        InternalType::BoolType(InternalBoolType {
                            type_str: "bool".to_string(),
                            bool_type: self.context.bool_type(),
                        }),
                    ))
                } else {
                    Err("Pointer types do not match for greater-than operator.".to_string())
                }
            }
            _ => Err("Unsupported types for greater-than operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_ge<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "cmp_ge")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    Ok(InternalValue::BoolValue(
                        self.builder
                            .build_int_compare(
                                IntPredicate::UGE,
                                typed_pointer_value1.ptr,
                                typed_pointer_value2.ptr,
                                "cmp_ge",
                            )
                            .unwrap(),
                        InternalType::BoolType(InternalBoolType {
                            type_str: "bool".to_string(),
                            bool_type: self.context.bool_type(),
                        }),
                    ))
                } else {
                    Err("Pointer types do not match for greater-equal operator.".to_string())
                }
            }
            _ => Err("Unsupported types for greater-equal operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_eq<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::BoolValue(left, _), InternalValue::BoolValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "cmp_eq")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "cmp_eq")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    Ok(InternalValue::BoolValue(
                        self.builder
                            .build_int_compare(
                                IntPredicate::EQ,
                                typed_pointer_value1.ptr,
                                typed_pointer_value2.ptr,
                                "cmp_eq",
                            )
                            .unwrap(),
                        InternalType::BoolType(InternalBoolType {
                            type_str: "bool".to_string(),
                            bool_type: self.context.bool_type(),
                        }),
                    ))
                } else {
                    Err("Pointer types do not match for equal operator.".to_string())
                }
            }
            _ => Err("Unsupported types for equal operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_neq<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::BoolValue(left, _), InternalValue::BoolValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "cmp_eq")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "cmp_neq")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::BoolValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                        .unwrap(),
                    InternalType::BoolType(InternalBoolType {
                        type_str: "bool".to_string(),
                        bool_type: self.context.bool_type(),
                    }),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder
                    .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                    .unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    Ok(InternalValue::BoolValue(
                        self.builder
                            .build_int_compare(
                                IntPredicate::NE,
                                typed_pointer_value1.ptr,
                                typed_pointer_value2.ptr,
                                "cmp_neq",
                            )
                            .unwrap(),
                        InternalType::BoolType(InternalBoolType {
                            type_str: "bool".to_string(),
                            bool_type: self.context.bool_type(),
                        }),
                    ))
                } else {
                    Err("Pointer types do not match for unequal operator.".to_string())
                }
            }
            _ => Err("Unsupported types for unequal operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_logical_or<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder.build_or(left, right, "or").unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    // TODO Implement LogicalOr for Pointers
                    // PHI is a good choice here probably.
                    unimplemented!();
                } else {
                    Err("Pointer types do not match for logical-or operator.".to_string())
                }
            }
            _ => Err("Unsupported types for logical-or operator.".to_string()),
        }
    }

    pub(crate) fn bin_op_logical_and<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::BoolValue(
                self.builder.build_and(left, right, "and").unwrap(),
                InternalType::BoolType(InternalBoolType {
                    type_str: "bool".to_string(),
                    bool_type: self.context.bool_type(),
                }),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    // TODO Implement LogicalAnd for Pointers
                    // PHI is a good choice here probably.
                    unimplemented!();
                } else {
                    Err("Pointer types do not match for logical-and operator.".to_string())
                }
            }
            _ => Err("Unsupported types for logical-and operator.".to_string()),
        }
    }
}
