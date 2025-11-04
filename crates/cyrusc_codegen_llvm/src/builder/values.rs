use crate::builder::builder::IRBuilderCtx;
use cyrusc_cir::types::CIRTy;
use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, PointerValue},
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
    pub fn load_rvalue(&self, internal_value: InternalValue<'ll>) -> InternalValue<'ll> {
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
