/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
