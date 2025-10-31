use cyrusc_cir::CIRExpr;

use crate::builder::{builder::IRBuilderCtx, values::InternalValue};

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_expr(&self, expr: &CIRExpr) -> InternalValue<'ll> {
        todo!();
    }
}
