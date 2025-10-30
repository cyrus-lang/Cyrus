use crate::{
    OwnedModule,
    builder::{
        exprs::emit_expr,
        funcs::{emit_func_body, emit_func_decl},
    },
};
use cyrusc_cir::{CIRProgramTree, CIRStmt, cir_func_def_as_decl};
use inkwell::{builder::Builder, context::Context, module::Module, targets::TargetMachine};
use std::{cell::RefCell, rc::Rc, sync::Arc};

pub(crate) struct IRBuilderCtx<'ll> {
    pub(crate) llvmctx: Arc<Context>,
    pub(crate) llvmbuilder: Rc<Builder<'ll>>,
    pub(crate) llvmmodule: Rc<RefCell<Module<'ll>>>,
    pub(crate) llvmtm: &'ll TargetMachine,
}

impl<'ll> IRBuilderCtx<'ll> {
    pub fn new(owned_module: &'ll OwnedModule, llvmbuilder: Rc<Builder<'ll>>, llvmtm: &'ll TargetMachine) -> Self {
        let llvmmodule = unsafe {
            std::mem::transmute::<Rc<RefCell<Module<'static>>>, Rc<RefCell<Module<'ll>>>>(owned_module.module.clone())
        };

        Self {
            llvmctx: owned_module.context.clone(),
            llvmbuilder,
            llvmmodule,
            llvmtm,
        }
    }
}

pub fn emit_cir_program_tree<'ll>(
    owned_module: &'ll OwnedModule,
    builder: Rc<Builder<'ll>>,
    cir_program_tree: &'ll CIRProgramTree,
    llvmtm: &'ll TargetMachine,
) {
    for cir_stmt in &cir_program_tree.body {
        let local_ctx = Rc::new(IRBuilderCtx::new(owned_module, builder.clone(), &llvmtm));

        match cir_stmt {
            CIRStmt::Variable(var_stmt) => todo!(),
            CIRStmt::GlobalVar(global_var_stmt) => todo!(),
            CIRStmt::FuncDef(func_def_stmt) => {
                let func_decl = cir_func_def_as_decl(func_def_stmt);
                let fn_value = emit_func_decl(local_ctx.clone(), &func_decl);
                emit_func_body(local_ctx, &func_def_stmt.body, &fn_value);
            }
            CIRStmt::FuncDecl(func_decl_stmt) => {
                emit_func_decl(local_ctx, &func_decl_stmt);
            }
            CIRStmt::Block(block_stmt) => todo!(),
            CIRStmt::If(if_stmt) => todo!(),
            CIRStmt::For(for_stmt) => todo!(),
            CIRStmt::While(while_stmt) => todo!(),
            CIRStmt::SwitchInteger(switch_integer_stmt) => todo!(),
            CIRStmt::SwitchValue(switch_value_stmt) => todo!(),
            CIRStmt::SwitchEnumVariant(switch_enum_variant_stmt) => todo!(),
            CIRStmt::Return(return_stmt) => todo!(),
            CIRStmt::Break(break_stmt) => todo!(),
            CIRStmt::Continue(continue_stmt) => todo!(),
            CIRStmt::Struct(struct_stmt) => todo!(),
            CIRStmt::Enum(enum_stmt) => todo!(),
            CIRStmt::Union(union_stmt) => todo!(),
            CIRStmt::ExportTuple(export_tuple_stmt) => todo!(),
            CIRStmt::Expr(expr) => emit_expr(expr),
        }
    }
}
