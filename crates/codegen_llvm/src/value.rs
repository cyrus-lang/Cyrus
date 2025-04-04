use crate::CodeGenLLVM;
use inkwell::{FloatPredicate, IntPredicate, values::AnyValueEnum};

impl<'ctx> CodeGenLLVM<'ctx> {
    pub fn bin_op_add<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder.build_int_add(left, right, "add").unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_add(left, right, "add").unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::FloatValue(
                self.builder.build_float_add(left, right, "add").unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_sub<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder.build_int_sub(left, right, "sub").unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_sub(left, right, "sub").unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::FloatValue(
                self.builder.build_float_sub(left, right, "sub").unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_mul<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder.build_int_mul(left, right, "mul").unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_mul(left, right, "mul").unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::FloatValue(
                self.builder.build_float_mul(left, right, "mul").unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_div<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder.build_int_signed_div(left, right, "div").unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_div(left, right, "div").unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::FloatValue(
                self.builder.build_float_div(left, right, "div").unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_rem<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder.build_int_signed_rem(left, right, "rem").unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::FloatValue(
                    self.builder.build_float_rem(left, right, "rem").unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::FloatValue(
                self.builder.build_float_rem(left, right, "rem").unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_lt<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLT, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_le<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OLE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OLE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_gt<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGT, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGT, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_ge<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OGE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OGE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_eq<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::OEQ, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }

    pub fn bin_op_neq<'a>(
        &self,
        left_value: AnyValueEnum<'a>,
        right_value: AnyValueEnum<'a>,
    ) -> Option<AnyValueEnum<'ctx>>
    where
        'a: 'ctx,
    {
        match (left_value, right_value) {
            (AnyValueEnum::IntValue(left), AnyValueEnum::IntValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            (AnyValueEnum::FloatValue(left), AnyValueEnum::IntValue(right)) => {
                let right = self
                    .builder
                    .build_signed_int_to_float(right, left.get_type(), "cmp_lt")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::IntValue(left), AnyValueEnum::FloatValue(right)) => {
                let left = self
                    .builder
                    .build_signed_int_to_float(left, right.get_type(), "cast")
                    .unwrap();
                Some(AnyValueEnum::IntValue(
                    self.builder
                        .build_float_compare(FloatPredicate::ONE, left, right, "cmp_lt")
                        .unwrap(),
                ))
            }
            (AnyValueEnum::FloatValue(left), AnyValueEnum::FloatValue(right)) => Some(AnyValueEnum::IntValue(
                self.builder
                    .build_float_compare(FloatPredicate::ONE, left, right, "cmp_lt")
                    .unwrap(),
            )),
            _ => None,
        }
    }
}
