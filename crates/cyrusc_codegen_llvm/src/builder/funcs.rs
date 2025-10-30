use crate::builder::builder::IRBuilderCtx;
use cyrusc_cir::{CIRBlockStmt, CIRFuncDeclStmt};
use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};
use std::rc::Rc;

pub fn emit_func_decl<'ll>(ctx: Rc<IRBuilderCtx<'ll>>, func_decl: &CIRFuncDeclStmt) -> FunctionValue<'ll> {
    todo!();
}

pub fn emit_func_body<'ll>(ctx: Rc<IRBuilderCtx<'ll>>, body: &'ll CIRBlockStmt, fn_value: &'ll FunctionValue<'ll>) {
    todo!();
}
