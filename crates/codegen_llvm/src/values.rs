use crate::{
    CodeGenLLVM, InternalType, StringType, diag::*, funcs::FuncMetadata, modules::ModuleMetadata, scope::ScopeRef,
    types::TypedPointerType,
};
use ast::token::Location;
use inkwell::{
    FloatPredicate, IntPredicate,
    types::{ArrayType, FloatType, IntType, StructType, VectorType},
    values::{
        AnyValue, ArrayValue, BasicMetadataValueEnum, BasicValueEnum, FloatValue, IntValue, PointerValue, StructValue,
        VectorValue,
    },
};
use std::process::exit;

#[derive(Debug, Clone)]
pub(crate) enum InternalValue<'a> {
    IntValue(IntValue<'a>, IntType<'a>),
    FloatValue(FloatValue<'a>, FloatType<'a>),
    ArrayValue(ArrayValue<'a>, ArrayType<'a>),
    StructValue(StructValue<'a>, StructType<'a>),
    VectorValue(VectorValue<'a>, VectorType<'a>),
    StringValue(StringValue<'a>),
    PointerValue(TypedPointerValue<'a>),
    ModuleValue(ModuleMetadata<'a>),
    FunctionValue(FuncMetadata<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct StringValue<'a> {
    pub struct_value: StructValue<'a>,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedPointerValue<'a> {
    pub ptr: PointerValue<'a>,
    pub pointee_ty: InternalType<'a>,
}

impl<'a> InternalValue<'a> {
    pub fn to_basic_metadata(&self) -> BasicMetadataValueEnum<'a> {
        match self {
            InternalValue::IntValue(v, ..) => BasicMetadataValueEnum::IntValue(*v),
            InternalValue::FloatValue(v, ..) => BasicMetadataValueEnum::FloatValue(*v),
            InternalValue::ArrayValue(v, ..) => BasicMetadataValueEnum::ArrayValue(*v),
            InternalValue::StructValue(v, ..) => BasicMetadataValueEnum::StructValue(*v),
            InternalValue::VectorValue(v, ..) => BasicMetadataValueEnum::VectorValue(*v),
            InternalValue::StringValue(v) => BasicMetadataValueEnum::StructValue(v.struct_value),
            InternalValue::PointerValue(v) => BasicMetadataValueEnum::PointerValue(v.ptr),
            InternalValue::ModuleValue(_) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot convert ModuleValue to BasicMetadataValueEnum.".to_string()),
                    location: None,
                });
                exit(1);
            }
            InternalValue::FunctionValue(func_metadata) => {
                display_single_diag(Diag {
                    level: DiagLevel::Error,
                    kind: DiagKind::Custom("Cannot convert FunctionValue to BasicMetadataValueEnum.".to_string()),
                    location: None,
                });
                exit(1);
            }
        }
    }

    pub(crate) fn get_type(&self, string_type: StringType<'a>) -> InternalType<'a> {
        match self {
            InternalValue::IntValue(_, ty) => InternalType::IntType(ty.clone()),
            InternalValue::FloatValue(_, ty, ..) => InternalType::FloatType(ty.clone()),
            InternalValue::ArrayValue(_, ty, ..) => InternalType::ArrayType(ty.clone()),
            InternalValue::StructValue(_, ty, ..) => InternalType::StructType(ty.clone()),
            InternalValue::VectorValue(_, ty, ..) => InternalType::VectorType(ty.clone()),
            InternalValue::PointerValue(v) => InternalType::PointerType(Box::new(TypedPointerType {
                ptr_type: v.ptr.get_type(),
                pointee_ty: v.pointee_ty.clone(),
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
        }
    }
}

impl<'a> From<TypedPointerValue<'a>> for InternalValue<'a> {
    fn from(val: TypedPointerValue<'a>) -> Self {
        InternalValue::PointerValue(val)
    }
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn new_internal_value_from_basic_value_enum(
        &self,
        value: BasicValueEnum<'ctx>,
        value_type: InternalType<'ctx>,
    ) -> InternalValue<'ctx> {
        match value_type {
            InternalType::IntType(int_type) => InternalValue::IntValue(value.into_int_value(), int_type),
            InternalType::FloatType(float_type) => InternalValue::FloatValue(value.into_float_value(), float_type),
            InternalType::ArrayType(array_type) => InternalValue::ArrayValue(value.into_array_value(), array_type),
            InternalType::StructType(struct_type) => InternalValue::StructValue(value.into_struct_value(), struct_type),
            InternalType::VectorType(vector_type) => InternalValue::VectorValue(value.into_vector_value(), vector_type),
            InternalType::StringType(string_type) => todo!(),
            InternalType::VoidType(void_type) => todo!(),
            InternalType::PointerType(typed_pointer_type) => InternalValue::PointerValue(TypedPointerValue {
                ptr: value.into_pointer_value(),
                pointee_ty: typed_pointer_type.pointee_ty,
            }),
        }
    }

    // FIXME
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
            InternalValue::IntValue(v, ty) => InternalValue::IntValue(v, ty),
            InternalValue::FloatValue(v, ty) => InternalValue::FloatValue(v, ty),
            InternalValue::ArrayValue(v, ty) => InternalValue::ArrayValue(v, ty),
            InternalValue::StructValue(v, ty) => InternalValue::StructValue(v, ty),
            InternalValue::VectorValue(v, ty) => InternalValue::VectorValue(v, ty),
            InternalValue::StringValue(v) => todo!(),
            InternalValue::PointerValue(typed_pointer_value) => {
                // FIXME
                todo!();
                // let ptr_type = self.context.ptr_type(AddressSpace::default());
                // self.builder
                //     .build_load(
                //         typed_pointer_value.pointee_ty.to_basic_type(ptr_type),
                //         typed_pointer_value.ptr,
                //         "load",
                //     )
                //     .unwrap()
                //     .try_into()
                //     .unwrap()
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
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Some(
                InternalValue::IntValue(self.builder.build_int_add(left, right, "add").unwrap(), left_ty),
            ),
            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                    left_ty,
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                    right_ty,
                ))
            }
            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Some(
                InternalValue::FloatValue(self.builder.build_float_add(left, right, "add").unwrap(), left_ty),
            ),
            _ => None,
        }
    }

    pub(crate) fn bin_op_sub<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Some(
                InternalValue::IntValue(self.builder.build_int_sub(left, right, "sub").unwrap(), left_ty),
            ),

            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();

                Some(InternalValue::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                    left_ty,
                ))
            }

            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();

                Some(InternalValue::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                    right_ty,
                ))
            }

            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Some(
                InternalValue::FloatValue(self.builder.build_float_sub(left, right, "sub").unwrap(), left_ty),
            ),

            _ => None,
        }
    }

    pub(crate) fn bin_op_mul<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Some(
                InternalValue::IntValue(self.builder.build_int_mul(left, right, "mul").unwrap(), left_ty),
            ),

            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                    left_ty,
                ))
            }

            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                    right_ty,
                ))
            }

            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Some(
                InternalValue::FloatValue(self.builder.build_float_mul(left, right, "mul").unwrap(), left_ty),
            ),

            _ => None,
        }
    }

    pub(crate) fn bin_op_div<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Some(
                InternalValue::IntValue(self.builder.build_int_signed_div(left, right, "div").unwrap(), left_ty),
            ),

            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                    left_ty,
                ))
            }

            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                    right_ty,
                ))
            }

            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Some(
                InternalValue::FloatValue(self.builder.build_float_div(left, right, "div").unwrap(), left_ty),
            ),

            _ => None,
        }
    }

    pub(crate) fn bin_op_rem<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, left_ty), InternalValue::IntValue(right, _)) => Some(
                InternalValue::IntValue(self.builder.build_int_signed_rem(left, right, "rem").unwrap(), left_ty),
            ),

            (InternalValue::FloatValue(left, left_ty), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                    left_ty,
                ))
            }

            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, right_ty)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                    right_ty,
                ))
            }

            (InternalValue::FloatValue(left, left_ty), InternalValue::FloatValue(right, _)) => Some(
                InternalValue::FloatValue(self.builder.build_float_rem(left, right, "rem").unwrap(), left_ty),
            ),

            _ => None,
        }
    }

    pub(crate) fn bin_op_lt<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "cmp_lt")
                    .unwrap(),
                self.context.bool_type(),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                    .unwrap(),
                self.context.bool_type(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_le<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "cmp_le")
                    .unwrap(),
                self.context.bool_type(),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLE, left, right, "cmp_le")
                    .unwrap(),
                self.context.bool_type(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_gt<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "cmp_gt")
                    .unwrap(),
                self.context.bool_type(),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGT, left, right, "cmp_gt")
                    .unwrap(),
                self.context.bool_type(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_ge<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "cmp_ge")
                    .unwrap(),
                self.context.bool_type(),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGE, left, right, "cmp_ge")
                    .unwrap(),
                self.context.bool_type(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_eq<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "cmp_eq")
                    .unwrap(),
                self.context.bool_type(),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_eq")
                    .unwrap(),
                self.context.bool_type(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_neq<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "cmp_neq")
                    .unwrap(),
                self.context.bool_type(),
            )),
            (InternalValue::FloatValue(left, _), InternalValue::IntValue(right, _)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::IntValue(left, _), InternalValue::FloatValue(right, _)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(InternalValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                        .unwrap(),
                    self.context.bool_type(),
                ))
            }
            (InternalValue::FloatValue(left, _), InternalValue::FloatValue(right, _)) => Some(InternalValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::ONE, left, right, "cmp_neq")
                    .unwrap(),
                self.context.bool_type(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_or<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Some(InternalValue::IntValue(
                self.builder.build_or(left, right, "or").unwrap(),
                self.context.bool_type(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_and<'a>(
        &self,
        left_value: InternalValue<'a>,
        right_value: InternalValue<'a>,
    ) -> Option<InternalValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (InternalValue::IntValue(left, _), InternalValue::IntValue(right, _)) => Some(InternalValue::IntValue(
                self.builder.build_and(left, right, "and").unwrap(),
                self.context.bool_type(),
            )),
            _ => None,
        }
    }
}
