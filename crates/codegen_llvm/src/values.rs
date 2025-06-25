use crate::{
    CodeGenLLVM, InternalType, StringType, diag::*, funcs::FuncMetadata, modules::ModuleMetadata,
    types::TypedPointerType,
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
    BoolValue(IntValue<'a>),
    IntValue(IntValue<'a>, InternalType<'a>),
    FloatValue(FloatValue<'a>, InternalType<'a>),
    ArrayValue(ArrayValue<'a>, InternalType<'a>),
    StructValue(StructValue<'a>, InternalType<'a>),
    UnnamedStructValue(StructValue<'a>, InternalType<'a>),
    VectorValue(VectorValue<'a>, InternalType<'a>),
    StrValue(PointerValue<'a>, InternalType<'a>),
    StringValue(StringValue<'a>),
    PointerValue(TypedPointerValue<'a>),
    ModuleValue(ModuleMetadata<'a>),
    FunctionValue(FuncMetadata<'a>),
    Lvalue(Lvalue<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct StringValue<'a> {
    pub struct_value: StructValue<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct Lvalue<'a> {
    pub ptr: PointerValue<'a>,
    pub pointee_ty: InternalType<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedPointerValue<'a> {
    pub ptr: PointerValue<'a>,
    pub pointee_ty: InternalType<'a>,
}

impl<'a> InternalValue<'a> {
    pub fn is_const(&self) -> bool {
        match self {
            InternalValue::BoolValue(..) => true,
            InternalValue::IntValue(..) => true,
            InternalValue::FloatValue(..) => true,
            InternalValue::StrValue(..) => true,
            InternalValue::StringValue(..) => false,
            InternalValue::Lvalue(..) => false,
            InternalValue::PointerValue(..) => false,
            InternalValue::ModuleValue(..) => unreachable!(),
            InternalValue::FunctionValue(..) => unreachable!(),
            InternalValue::ArrayValue(array_value, ..) => array_value.is_const(),
            InternalValue::StructValue(struct_value, ..) => struct_value.is_const(),
            InternalValue::UnnamedStructValue(struct_value, ..) => struct_value.is_const(),
            InternalValue::VectorValue(vector_value, ..) => vector_value.is_const(),
        }
    }

    pub fn to_basic_metadata(&self) -> BasicMetadataValueEnum<'a> {
        match self {
            InternalValue::BoolValue(v) => BasicMetadataValueEnum::IntValue(*v),
            InternalValue::IntValue(v, ..) => BasicMetadataValueEnum::IntValue(*v),
            InternalValue::FloatValue(v, ..) => BasicMetadataValueEnum::FloatValue(*v),
            InternalValue::ArrayValue(v, ..) => BasicMetadataValueEnum::ArrayValue(*v),
            InternalValue::StructValue(v, ..) => BasicMetadataValueEnum::StructValue(*v),
            InternalValue::VectorValue(v, ..) => BasicMetadataValueEnum::VectorValue(*v),
            InternalValue::StringValue(v) => BasicMetadataValueEnum::StructValue(v.struct_value),
            InternalValue::PointerValue(v) => BasicMetadataValueEnum::PointerValue(v.ptr),
            InternalValue::Lvalue(v) => BasicMetadataValueEnum::PointerValue(v.ptr),
            InternalValue::StrValue(v, ..) => BasicMetadataValueEnum::PointerValue(*v),
            InternalValue::ModuleValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot convert ModuleValue to BasicMetadataValueEnum.".to_string()),
                    location: None,
                });
                exit(1);
            }
            InternalValue::FunctionValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot convert FunctionValue to BasicMetadataValueEnum.".to_string()),
                    location: None,
                });
                exit(1);
            }
            InternalValue::UnnamedStructValue(struct_value, _) => BasicMetadataValueEnum::StructValue(*struct_value),
        }
    }

    pub(crate) fn get_type(&self, string_type: StringType<'a>) -> InternalType<'a> {
        match self {
            InternalValue::BoolValue(v) => InternalType::BoolType(v.get_type()),
            InternalValue::IntValue(_, ty) => ty.clone(),
            InternalValue::FloatValue(_, ty, ..) => ty.clone(),
            InternalValue::ArrayValue(_, ty, ..) => ty.clone(),
            InternalValue::StructValue(_, ty, ..) => ty.clone(),
            InternalValue::VectorValue(_, ty, ..) => ty.clone(),
            InternalValue::PointerValue(v) => InternalType::PointerType(Box::new(TypedPointerType {
                ptr_type: v.ptr.get_type(),
                pointee_ty: v.pointee_ty.clone(),
            })),
            InternalValue::Lvalue(v) => InternalType::PointerType(Box::new(TypedPointerType {
                ptr_type: v.ptr.get_type(),
                pointee_ty: v.pointee_ty.clone(),
            })),
            InternalValue::StrValue(ptr, ty) => InternalType::PointerType(Box::new(TypedPointerType {
                ptr_type: ptr.get_type(),
                pointee_ty: ty.clone(),
            })),
            InternalValue::StringValue(_) => InternalType::StringType(string_type),
            InternalValue::ModuleValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot get type of an ModuleValue.".to_string()),
                    location: None,
                });
                exit(1);
            }
            InternalValue::FunctionValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot get type of an FunctionValue.".to_string()),
                    location: None,
                });
                exit(1);
            }
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
    pub(crate) fn build_zero_initialized_internal_value(
        &self,
        value_type: InternalType<'ctx>,
        loc: Location,
        span_end: usize,
    ) -> InternalValue<'ctx> {
        match value_type {
            InternalType::IntType(int_type) => {
                InternalValue::IntValue(int_type.const_zero(), InternalType::IntType(int_type))
            }
            InternalType::FloatType(float_type) => {
                InternalValue::FloatValue(float_type.const_zero(), InternalType::FloatType(float_type))
            }
            InternalType::ArrayType(element_type, array_type) => InternalValue::ArrayValue(
                array_type.const_zero(),
                InternalType::ArrayType(element_type, array_type),
            ),
            InternalType::StructType(struct_metadata) => InternalValue::StructValue(
                struct_metadata.struct_type.const_zero(),
                InternalType::StructType(struct_metadata),
            ),
            InternalType::UnnamedStruct(unnamed_struct_metadata) => InternalValue::UnnamedStructValue(
                unnamed_struct_metadata.struct_type.const_zero(),
                InternalType::UnnamedStruct(unnamed_struct_metadata),
            ),
            InternalType::VectorType(vector_type) => {
                InternalValue::VectorValue(vector_type.const_zero(), InternalType::VectorType(vector_type))
            }
            InternalType::StringType(_) => self.build_zeroinit_string(),
            InternalType::PointerType(typed_pointer_type) => InternalValue::PointerValue(TypedPointerValue {
                ptr: typed_pointer_type.ptr_type.const_zero(),
                pointee_ty: typed_pointer_type.pointee_ty,
            }),
            InternalType::ConstType(_) => unreachable!(),
            InternalType::VoidType(_) => unreachable!(),
            InternalType::BoolType(int_type) => InternalValue::BoolValue(int_type.const_zero()),
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
        }
    }

    pub(crate) fn new_internal_value(
        &self,
        value: BasicValueEnum<'ctx>,
        value_type: InternalType<'ctx>,
    ) -> InternalValue<'ctx> {
        match value_type {
            InternalType::BoolType(_) => InternalValue::BoolValue(value.into_int_value()),
            InternalType::IntType(int_type) => {
                InternalValue::IntValue(value.into_int_value(), InternalType::IntType(int_type))
            }
            InternalType::FloatType(float_type) => {
                InternalValue::FloatValue(value.into_float_value(), InternalType::FloatType(float_type))
            }
            InternalType::ArrayType(element_type, array_type) => InternalValue::ArrayValue(
                value.into_array_value(),
                InternalType::ArrayType(element_type, array_type),
            ),
            InternalType::StructType(struct_type) => {
                InternalValue::StructValue(value.into_struct_value(), InternalType::StructType(struct_type))
            }
            InternalType::VectorType(vector_type) => {
                InternalValue::VectorValue(value.into_vector_value(), InternalType::VectorType(vector_type))
            }
            InternalType::StringType(_) => InternalValue::StringValue(StringValue {
                struct_value: value.into_struct_value(),
            }),
            InternalType::PointerType(typed_pointer_type) => InternalValue::PointerValue(TypedPointerValue {
                ptr: value.into_pointer_value(),
                pointee_ty: typed_pointer_type.pointee_ty,
            }),
            InternalType::Lvalue(typed_pointer_type) => InternalValue::Lvalue(Lvalue {
                ptr: value.into_pointer_value(),
                pointee_ty: typed_pointer_type.pointee_ty,
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
        }
    }

    pub(crate) fn implicit_cast(
        &self,
        rvalue: InternalValue<'ctx>,
        target_type: InternalType<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.build_cast_expression_internal(rvalue, target_type, Location::default(), 0)
            .to_basic_metadata()
            .as_any_value_enum()
            .try_into()
            .unwrap()
    }

    pub(crate) fn internal_value_as_rvalue(&self, internal_value: InternalValue<'ctx>) -> InternalValue<'ctx> {
        match internal_value {
            InternalValue::BoolValue(int_value) => InternalValue::BoolValue(int_value),
            InternalValue::PointerValue(typed_pointer_value) => InternalValue::PointerValue(typed_pointer_value),
            InternalValue::IntValue(v, ty) => InternalValue::IntValue(v, ty),
            InternalValue::FloatValue(v, ty) => InternalValue::FloatValue(v, ty),
            InternalValue::ArrayValue(v, ty) => InternalValue::ArrayValue(v, ty),
            InternalValue::StructValue(v, ty) => InternalValue::StructValue(v, ty),
            InternalValue::UnnamedStructValue(v, ty) => InternalValue::UnnamedStructValue(v, ty),
            InternalValue::VectorValue(v, ty) => InternalValue::VectorValue(v, ty),
            InternalValue::StrValue(v, ty) => InternalValue::PointerValue(TypedPointerValue { ptr: v, pointee_ty: ty }),
            InternalValue::StringValue(v) => InternalValue::StringValue(v),
            InternalValue::Lvalue(typed_pointer_value) => {
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let value = self
                    .builder
                    .build_load(
                        typed_pointer_value.pointee_ty.to_basic_type(ptr_type),
                        typed_pointer_value.ptr,
                        "load",
                    )
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
            InternalValue::ModuleValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot load ModuleValue as an rvalue.".to_string()),
                    location: None,
                });
                exit(1);
            }
            InternalValue::FunctionValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot load FunctionValue as an rvalue.".to_string()),
                    location: None,
                });
                exit(1);
            }
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
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder.build_int_add(left, right, "add").unwrap(),
                left_ty,
            )),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
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
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder.build_int_sub(left, right, "sub").unwrap(),
                left_ty,
            )),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();

                Ok(InternalValue::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();

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
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder.build_int_mul(left, right, "mul").unwrap(),
                left_ty,
            )),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
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
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder.build_int_signed_div(left, right, "div").unwrap(),
                left_ty,
            )),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
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
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder.build_int_signed_rem(left, right, "rem").unwrap(),
                left_ty,
            )),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
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
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "cmp_lt")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
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
                    ))
                } else {
                    Err("Pointer types do not match for equality comparison.".to_string())
                }
            }
            _ => Err("Unsupported types for inequality comparison.".to_string()),
        }
    }

    pub(crate) fn bin_op_le<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "cmp_le")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
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
                    ))
                } else {
                    Err("Pointer types do not match for equality comparison.".to_string())
                }
            }
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "cmp_gt")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
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
                    ))
                } else {
                    Err("Pointer types do not match for equality comparison.".to_string())
                }
            }
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "cmp_ge")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
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
                    ))
                } else {
                    Err("Pointer types do not match for equality comparison.".to_string())
                }
            }
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "cmp_eq")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
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
                    ))
                } else {
                    Err("Pointer types do not match for equality comparison.".to_string())
                }
            }
            _ => Err("Unsupported types for inequality comparison.".to_string()),
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
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "cmp_neq")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Ok(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                        .unwrap(),
                    InternalType::BoolType(self.context.bool_type()),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                    .unwrap(),
                InternalType::BoolType(self.context.bool_type()),
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
                    ))
                } else {
                    Err("Pointer types do not match for equality comparison.".to_string())
                }
            }
            _ => Err("Unsupported types for inequality comparison.".to_string()),
        }
    }

    // ANCHOR

    pub(crate) fn bin_op_or<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder.build_or(left, right, "or").unwrap(),
                InternalType::BoolType(self.context.bool_type()),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    // TODO Implement LogicalOr for Pointers
                    // PHI is a good choice here probably.
                    unimplemented!();
                } else {
                    Err("Pointer types do not match for equality comparison.".to_string())
                }
            }
            _ => Err("Unsupported types for inequality comparison.".to_string()),
        }
    }

    pub(crate) fn bin_op_and<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Result<InternalValue<'ctx>, String>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Ok(InternalValue::IntValue(
                self.builder.build_and(left, right, "and").unwrap(),
                InternalType::BoolType(self.context.bool_type()),
            )),
            (InternalValue::PointerValue(typed_pointer_value1), InternalValue::PointerValue(typed_pointer_value2)) => {
                if typed_pointer_value1.ptr.get_type() == typed_pointer_value2.ptr.get_type() {
                    // TODO Implement LogicalAnd for Pointers
                    // PHI is a good choice here probably.
                    unimplemented!();
                } else {
                    Err("Pointer types do not match for equality comparison.".to_string())
                }
            }
            _ => Err("Unsupported types for inequality comparison.".to_string()),
        }
    }
}
