// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use crate::builder::builder::IRBuilderCtx;
use cyrusc_cir::{CIRExpr, types::CIRTy};
use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
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

    fn int_value_as_bool_i1(&self, int_value: IntValue<'ll>) -> IntValue<'ll> {
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

        assert!(rvalue.ty.is_bool());

        let basic_value = rvalue.as_basic_value();
        let int_value = basic_value.into_int_value();
        self.int_value_as_bool_i1(int_value)
    }

    pub(crate) fn load_rvalue(&self, internal_value: InternalValue<'ll>) -> InternalValue<'ll> {
        match internal_value.kind {
            InternalValueKind::LValue(pointer_value) => {
                let ty: BasicTypeEnum<'ll> = self.emit_ty(internal_value.ty.clone()).try_into().unwrap();
                let loaded = self.llvmbuilder.build_load(ty, pointer_value, "rvalue").unwrap();
                InternalValue::new(internal_value.ty, InternalValueKind::RValue(loaded))
            }
            _ => internal_value,
        }
    }
}
