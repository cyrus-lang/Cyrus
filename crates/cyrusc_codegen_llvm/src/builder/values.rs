use cyrusc_cir::types::CIRTy;
use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, PointerValue},
};

use crate::builder::builder::IRBuilderCtx;

#[derive(Debug, Clone)]
pub struct InternalValue<'a> {
    pub kind: InternalValueKind<'a>,
}

#[derive(Debug, Clone)]
pub enum InternalValueKind<'a> {
    LValue(PointerValue<'a>),
    RValue(BasicValueEnum<'a>),
    FuncValue(FunctionValue<'a>),
}

impl<'a> InternalValue<'a> {
    pub fn new(kind: InternalValueKind<'a>) -> Self {
        InternalValue { kind }
    }

    pub fn as_basic_value(&self) -> BasicValueEnum<'a> {
        match &self.kind {
            InternalValueKind::LValue(pointer) => (*pointer).into(),
            InternalValueKind::RValue(value) => value.clone(),
            InternalValueKind::FuncValue(fn_value) => fn_value.as_global_value().as_pointer_value().into(),
        }
    }
}

impl<'ll> IRBuilderCtx<'ll> {
    pub fn zero_init(&self, ty: CIRTy) -> BasicValueEnum<'ll> {
        let ty: BasicTypeEnum<'ll> = self.emit_ty(ty).try_into().unwrap();
        ty.const_zero()
    }
}
