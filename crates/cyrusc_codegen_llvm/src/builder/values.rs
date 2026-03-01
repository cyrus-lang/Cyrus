/*
 * Copyright (c) 2026 The Cyrus Language
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

use crate::builder::builder::IRBuilderCtx;
use cyrusc_internal::cir::{cir::CIRExpr, types::CIRTy};
use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

#[derive(Debug, Clone)]
pub struct InternalValue<'a> {
    pub ty: CIRTy,
    pub kind: InternalValueKind<'a>,
}

#[derive(Debug, Clone)]
pub enum InternalValueKind<'a> {
    LValue(PointerValue<'a>),
    RValue(BasicValueEnum<'a>),
    FuncValue(FunctionValue<'a>),
}

impl<'a> InternalValue<'a> {
    pub fn new(ty: CIRTy, kind: InternalValueKind<'a>) -> Self {
        InternalValue { ty, kind }
    }

    pub fn as_basic_value(&self) -> BasicValueEnum<'a> {
        match &self.kind {
            InternalValueKind::LValue(pointer) => (*pointer).into(),
            InternalValueKind::RValue(value) => value.clone(),
            InternalValueKind::FuncValue(fn_value) => fn_value.as_global_value().as_pointer_value().into(),
        }
    }

    pub fn as_func(&self) -> Option<&FunctionValue<'a>> {
        match &self.kind {
            InternalValueKind::FuncValue(fn_value) => Some(fn_value),
            _ => None,
        }
    }
}

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_store(&self, ptr: PointerValue<'ll>, mut rvalue: InternalValue<'ll>, target_cir_ty: CIRTy) {
        let target_ty: BasicTypeEnum<'ll> = self.emit_ty(target_cir_ty.clone()).try_into().unwrap();

        if let BasicTypeEnum::IntType(int_ty) = target_ty {
            let rvalue_int_value = rvalue.as_basic_value().into_int_value();
            let rvalue_bit_width = rvalue_int_value.get_type().get_bit_width();
            let target_bit_width = int_ty.get_bit_width();

            if rvalue_bit_width != target_bit_width {
                let signed = rvalue.ty.as_plain().map_or(false, |plain| plain.is_signed());
                let widened = if signed {
                    self.llvmbuilder
                        .build_int_s_extend(rvalue_int_value, int_ty, "widen_store")
                        .unwrap()
                } else {
                    self.llvmbuilder
                        .build_int_z_extend(rvalue_int_value, int_ty, "widen_store")
                        .unwrap()
                };
                rvalue = InternalValue::new(rvalue.ty.clone(), InternalValueKind::RValue(widened.into()));
            }
        }

        if target_cir_ty.is_union() {
            self.emit_memcpy(ptr, rvalue.as_basic_value());
            return;
        }

        self.llvmbuilder.build_store(ptr, rvalue.as_basic_value()).unwrap();
    }

    pub(crate) fn widen_int_arg(&self, value: InternalValue<'ll>, signed: bool) -> InternalValue<'ll> {
        let target_bit_width = self.target.info.int_bit_width();

        let int_value = value.as_basic_value().into_int_value();
        let target_ty = self.llvmctx.custom_width_int_type(target_bit_width);

        let widened = if signed {
            self.llvmbuilder
                .build_int_s_extend(int_value, target_ty, "sext")
                .unwrap()
        } else {
            self.llvmbuilder
                .build_int_z_extend(int_value, target_ty, "zext")
                .unwrap()
        };

        InternalValue::new(value.ty.clone(), InternalValueKind::RValue(widened.into()))
    }

    pub(crate) fn widen_int_pair(
        &self,
        lhs: InternalValue<'ll>,
        rhs: InternalValue<'ll>,
    ) -> (InternalValue<'ll>, InternalValue<'ll>) {
        let lhs_iv = lhs.as_basic_value().into_int_value();
        let rhs_iv = rhs.as_basic_value().into_int_value();

        let lhs_bw = lhs_iv.get_type().get_bit_width();
        let rhs_bw = rhs_iv.get_type().get_bit_width();

        // skip if same width
        if lhs_bw == rhs_bw {
            return (lhs, rhs);
        }

        // target width
        let target_bw = lhs_bw.max(rhs_bw);
        let target_ty = self.llvmctx.custom_width_int_type(target_bw);

        let widen = |v: InternalValue<'ll>, iv: IntValue<'ll>| {
            let signed = v.ty.as_plain().unwrap().is_signed();
            let widened = if signed {
                self.llvmbuilder.build_int_s_extend(iv, target_ty, "sext").unwrap()
            } else {
                self.llvmbuilder.build_int_z_extend(iv, target_ty, "zext").unwrap()
            };

            InternalValue::new(v.ty.clone(), InternalValueKind::RValue(widened.into()))
        };

        let lhs_w = if lhs_bw < target_bw { widen(lhs, lhs_iv) } else { lhs };
        let rhs_w = if rhs_bw < target_bw { widen(rhs, rhs_iv) } else { rhs };

        (lhs_w, rhs_w)
    }

    pub(crate) fn internal_value_as_bool_i1(&self, internal_value: InternalValue<'ll>) -> InternalValue<'ll> {
        assert!(internal_value.ty.is_bool());

        let basic_value = internal_value.as_basic_value();
        let int_value = basic_value.into_int_value();
        let i1_int_value = self.int_value_as_bool_i1(int_value);

        InternalValue::new(
            internal_value.ty.clone(),
            InternalValueKind::RValue(i1_int_value.into()),
        )
    }

    pub(crate) fn int_value_as_bool_i1(&self, int_value: IntValue<'ll>) -> IntValue<'ll> {
        let bit_width = int_value.get_type().get_bit_width();
        if bit_width == 1 {
            int_value
        } else {
            self.llvmbuilder
                .build_int_truncate(int_value, self.llvmctx.bool_type(), "bool_trunc")
                .unwrap()
        }
    }

    pub(crate) fn emit_cond(&mut self, cir_expr: &CIRExpr) -> IntValue<'ll> {
        let lvalue = self.emit_expr(cir_expr);
        let rvalue = self.load_rvalue(lvalue);

        assert!(rvalue.ty.is_integer_or_bool());

        let basic_value = rvalue.as_basic_value();
        let int_value = basic_value.into_int_value();
        self.int_value_as_bool_i1(int_value)
    }

    pub(crate) fn load_rvalue(&self, internal_value: InternalValue<'ll>) -> InternalValue<'ll> {
        match internal_value.kind {
            InternalValueKind::LValue(pointer_value) => {
                let ty: BasicTypeEnum<'ll> = self.emit_ty(internal_value.ty.clone()).try_into().unwrap();
                let basic_value = self.llvmbuilder.build_load(ty, pointer_value, "rvalue").unwrap();

                if internal_value.ty.is_bool() {
                    InternalValue::new(
                        internal_value.ty,
                        InternalValueKind::RValue(
                            self.int_value_as_bool_i1(basic_value.into_int_value())
                                .as_basic_value_enum(),
                        ),
                    )
                } else {
                    InternalValue::new(internal_value.ty, InternalValueKind::RValue(basic_value))
                }
            }
            _ => internal_value,
        }
    }
}
