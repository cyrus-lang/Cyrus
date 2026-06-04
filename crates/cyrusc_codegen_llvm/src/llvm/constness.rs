// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language
use inkwell::values::BasicValueEnum;

pub fn is_basic_value_constant<'a>(basic_value: BasicValueEnum<'a>) -> bool {
    match basic_value {
        BasicValueEnum::IntValue(int_value) => int_value.is_const(),
        BasicValueEnum::FloatValue(float_value) => float_value.is_const(),
        BasicValueEnum::PointerValue(ptr_value) => ptr_value.is_const(),
        BasicValueEnum::StructValue(struct_value) => struct_value.is_const(),
        BasicValueEnum::ArrayValue(array_value) => array_value.is_const(),
        BasicValueEnum::VectorValue(vector_value) => vector_value.is_const(),
        BasicValueEnum::ScalableVectorValue(scalable_value) => scalable_value.is_const(),
    }
}
