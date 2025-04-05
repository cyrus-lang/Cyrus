use crate::{types::TypedPointerType, AnyType, CodeGenLLVM, StringType};
use inkwell::{
    context::Context, values::{ArrayValue, BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue, StructValue, VectorValue}, FloatPredicate, IntPredicate
};

#[derive(Debug, Clone)]
pub(crate) enum AnyValue<'a> {
    IntValue(IntValue<'a>),
    FloatValue(FloatValue<'a>),
    ArrayValue(ArrayValue<'a>),
    StructValue(StructValue<'a>),
    VectorValue(VectorValue<'a>),
    PointerValue(TypedPointerValue<'a>),
    StringValue(StringValue<'a>),
}

#[derive(Debug, Clone)]
pub(crate) struct StringValue<'a> {
    pub data_ptr: PointerValue<'a>,
    pub len: IntValue<'a>
}

#[derive(Debug, Clone)]
pub(crate) struct TypedPointerValue<'a> {
    pub ptr: PointerValue<'a>,
    pub pointee_ty: AnyType<'a>,
}

impl<'a> AnyValue<'a> {
    pub fn get_type(&self, string_type: StringType<'a>) -> AnyType<'a> {
        match self {
            AnyValue::IntValue(v) => AnyType::IntType(v.get_type()),
            AnyValue::FloatValue(v) => AnyType::FloatType(v.get_type()),
            AnyValue::ArrayValue(v) => AnyType::ArrayType(v.get_type()),
            AnyValue::StructValue(v) => AnyType::StructType(v.get_type()),
            AnyValue::VectorValue(v) => AnyType::VectorType(v.get_type()),
            AnyValue::PointerValue(v) => AnyType::PointerType(Box::new(TypedPointerType {
                ptr_type: v.ptr.get_type(),
                pointee_ty: v.pointee_ty.clone(),
            })),
            AnyValue::StringValue(_) => AnyType::StringType(string_type),
        }
    }
}

impl<'a> From<IntValue<'a>> for AnyValue<'a> {
    fn from(val: IntValue<'a>) -> Self {
        AnyValue::IntValue(val)
    }
}

impl<'a> From<FloatValue<'a>> for AnyValue<'a> {
    fn from(val: FloatValue<'a>) -> Self {
        AnyValue::FloatValue(val)
    }
}

impl<'a> From<ArrayValue<'a>> for AnyValue<'a> {
    fn from(val: ArrayValue<'a>) -> Self {
        AnyValue::ArrayValue(val)
    }
}

impl<'a> From<StructValue<'a>> for AnyValue<'a> {
    fn from(val: StructValue<'a>) -> Self {
        AnyValue::StructValue(val)
    }
}

impl<'a> From<VectorValue<'a>> for AnyValue<'a> {
    fn from(val: VectorValue<'a>) -> Self {
        AnyValue::VectorValue(val)
    }
}

impl<'a> From<TypedPointerValue<'a>> for AnyValue<'a> {
    fn from(val: TypedPointerValue<'a>) -> Self {
        AnyValue::PointerValue(val)
    }
}

impl<'a> From<AnyValue<'a>> for BasicValueEnum<'a> {
    fn from(value: AnyValue<'a>) -> Self {
        match value {
            AnyValue::IntValue(v) => v.as_basic_value_enum(),
            AnyValue::FloatValue(v) => v.as_basic_value_enum(),
            AnyValue::ArrayValue(v) => v.as_basic_value_enum(),
            AnyValue::StructValue(v) => v.as_basic_value_enum(),
            AnyValue::VectorValue(v) => v.as_basic_value_enum(),
            AnyValue::PointerValue(v) => v.ptr.as_basic_value_enum(),
            AnyValue::StringValue(v) => todo!(),
        }
    }
}

impl<'a> TryFrom<BasicValueEnum<'a>> for AnyValue<'a> {
    type Error = &'static str;

    fn try_from(value: BasicValueEnum<'a>) -> Result<Self, Self::Error> {
        match value {
            BasicValueEnum::IntValue(v) => Ok(AnyValue::IntValue(v)),
            BasicValueEnum::FloatValue(v) => Ok(AnyValue::FloatValue(v)),
            BasicValueEnum::ArrayValue(v) => Ok(AnyValue::ArrayValue(v)),
            BasicValueEnum::StructValue(v) => Ok(AnyValue::StructValue(v)),
            BasicValueEnum::VectorValue(v) => Ok(AnyValue::VectorValue(v)),
            BasicValueEnum::PointerValue(_) => Err("Cannot infer pointee type from BasicValueEnum::PointerValue"),
        }
    }
}

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn bin_op_add<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder.build_int_add(left, right, "add").unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::FloatValue(
                self.builder.build_float_add(left, right, "add").unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_sub<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder.build_int_sub(left, right, "sub").unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::FloatValue(
                self.builder.build_float_sub(left, right, "sub").unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_mul<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder.build_int_mul(left, right, "mul").unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::FloatValue(
                self.builder.build_float_mul(left, right, "mul").unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_div<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder.build_int_signed_div(left, right, "div").unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::FloatValue(
                self.builder.build_float_div(left, right, "div").unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_rem<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder.build_int_signed_rem(left, right, "rem").unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::FloatValue(
                self.builder.build_float_rem(left, right, "rem").unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_lt<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_le<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_gt<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGT, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_ge<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_eq<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_neq<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValue::FloatValue(left), AnyValue::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::IntValue(left), AnyValue::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValue::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValue::FloatValue(left), AnyValue::FloatValue(right)) => Some(AnyValue::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::ONE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub(crate) fn bin_op_or<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => {
                Some(AnyValue::IntValue(self.builder.build_or(left, right, "or").unwrap()))
            }
            _ => None,
        }
    }

    pub(crate) fn bin_op_and<'a>(&self, left_value: AnyValue<'a>, right_value: AnyValue<'a>) -> Option<AnyValue<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValue::IntValue(left), AnyValue::IntValue(right)) => {
                Some(AnyValue::IntValue(self.builder.build_and(left, right, "and").unwrap()))
            }
            _ => None,
        }
    }
}
