// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
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
