use std::rc::Rc;

use crate::cir_builder::builder::CIRBuilderCtx;
use cyrusc_cir::{CIRBlockStmt, CIRFuncDeclStmt};
use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};

pub fn emit_func_decl<'ll>(ctx: Rc<CIRBuilderCtx<'ll>>, func_decl: &CIRFuncDeclStmt) -> FunctionValue<'ll> {
    todo!();
}

pub fn emit_func_body<'ll>(ctx: Rc<CIRBuilderCtx<'ll>>, body: &'ll CIRBlockStmt, fn_value: &'ll FunctionValue<'ll>) {
    todo!();
}
