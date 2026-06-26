// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    OwnedModule,
    builder::{
        control_flow::CFEntry,
        irreg::{LocalIRValueRegistry, LocalIRValueRegistryRef},
        types::CodegenIRBuilderTypeCache,
    },
    llvm::debug_info::{BlockScope, DebugContext, create_debug_lexical_block, debug_current_scope, set_debug_location},
};
use cyrusc_internal::{
    abi::{args::ABIFunctionInfo, target::ABITarget},
    cir::{
        cir::{CIRBlockStmt, CIRModule, CIRStmt, cir_func_decl_as_func_type, cir_func_def_as_decl},
        typectx::CIRTypeContext,
    },
    vtable::VTableRegistry,
};
use cyrusc_source_loc::SourceMap;
use cyrusc_typed_ast::LabelID;
use inkwell::{
    basic_block::BasicBlock, builder::Builder, context::Context, module::Module, targets::TargetMachine,
    values::FunctionValue,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc};

pub(crate) struct CodeGenIRBuilder<'ll> {
    pub(crate) target: &'ll ABITarget,
    pub(crate) llvm_ctx: &'ll Context,
    pub(crate) llvmbuilder: &'ll Builder<'ll>,
    pub(crate) llvm_module: Rc<RefCell<Module<'ll>>>,
    pub(crate) llvmtm: &'ll TargetMachine,
    pub(crate) irreg: LocalIRValueRegistryRef<'ll>,
    pub(crate) cur_func: Option<FunctionValue<'ll>>,
    pub(crate) cur_abi_func_info: Option<ABIFunctionInfo>,
    pub(crate) blockreg: BlockRegistry<'ll>,
    pub(crate) defer_stack: Vec<Vec<CIRStmt>>,

    pub(crate) cir_module: &'ll CIRModule,

    // Lambda ABI name.
    pub(crate) lambda_id: usize,

    // Only if debug info enabled in compiler options.
    pub(crate) dctx: Option<DebugContext>,

    pub(crate) tctx: Arc<CIRTypeContext>,
    pub(crate) type_cache: CodegenIRBuilderTypeCache<'ll>,

    pub(crate) vtable_registry: Arc<VTableRegistry>,
    pub(crate) source_map: Arc<SourceMap>,
}

#[derive(Debug, Clone)]
pub(crate) struct BlockRegistry<'ll> {
    pub(crate) control_flow_stack: Vec<CFEntry<'ll>>,
    pub(crate) first_block: Option<BasicBlock<'ll>>,
    pub(crate) cur_block: Option<BasicBlock<'ll>>,
    pub(crate) labels: HashMap<LabelID, (BasicBlock<'ll>, usize)>,
}

impl<'ll> CodeGenIRBuilder<'ll> {
    pub fn new(
        owned_module: &'ll OwnedModule,
        cir_module: &'ll CIRModule,
        target: &'ll ABITarget,
        llvmbuilder: &'ll Builder<'ll>,
        llvmtm: &'ll TargetMachine,
        dctx: Option<DebugContext>,
        tctx: Arc<CIRTypeContext>,
        vtable_registry: Arc<VTableRegistry>,
        source_map: Arc<SourceMap>,
    ) -> Self {
        let llvm_module = unsafe {
            std::mem::transmute::<Rc<RefCell<Module<'static>>>, Rc<RefCell<Module<'ll>>>>(owned_module.module.clone())
        };

        let irreg = Rc::new(RefCell::new(LocalIRValueRegistry::new()));

        let blockreg = BlockRegistry::default();

        Self {
            target,
            cir_module,
            llvm_ctx: &owned_module.context,
            llvmbuilder,
            llvm_module,
            llvmtm,
            irreg,
            cur_func: None,
            cur_abi_func_info: None,
            blockreg,
            lambda_id: 0,
            defer_stack: Vec::new(),
            dctx,
            tctx,
            vtable_registry,
            source_map,
            type_cache: CodegenIRBuilderTypeCache::new(),
        }
    }

    pub fn emit_module(&mut self) {
        self.emit_vtable_decls();

        for cir_stmt in &self.cir_module.stmts {
            self.emit_stmt(cir_stmt);
        }

        self.emit_vtable_defs();
    }

    pub(crate) fn emit_stmt(&mut self, stmt: &CIRStmt) {
        match stmt {
            CIRStmt::Variable(var_stmt) => self.emit_var(var_stmt),
            CIRStmt::FuncDef(func_def_stmt) => {
                let func_decl = cir_func_def_as_decl(func_def_stmt);
                let cir_func_ty = cir_func_decl_as_func_type(&func_decl);
                let llvm_func_value = self.emit_func_decl(&func_decl);
                self.set_current_func(llvm_func_value, func_def_stmt.abi_func_info.clone().unwrap());

                let func_meta = {
                    if self.dctx.is_some() {
                        Some(self.emit_func_meta(&cir_func_ty))
                    } else {
                        None
                    }
                };

                self.emit_func_body(
                    &func_decl.params,
                    &func_def_stmt.abi_func_info.as_ref().unwrap(),
                    &func_def_stmt.body,
                    func_meta,
                    func_decl.loc,
                );
            }
            CIRStmt::GlobalVar(global_var_stmt) => {
                let is_extern = global_var_stmt
                    .modifiers
                    .linkage
                    .as_ref()
                    .map(|linkage| linkage.is_extern())
                    .unwrap_or(false);

                if !is_extern {
                    self.emit_global_var(global_var_stmt);
                }
            }
            CIRStmt::FuncDecl(_) => {
                // early emitting causes symtab bloat;
                // only emitted when used.
            }
            CIRStmt::Block(block_stmt) => self.emit_scope_block(block_stmt),
            CIRStmt::Expr(expr) => {
                self.emit_expr(expr, &None);
            }
            CIRStmt::Switch(switch_stmt) => self.emit_switch(switch_stmt),
            CIRStmt::If(if_stmt) => self.emit_if(if_stmt),
            CIRStmt::For(for_stmt) => self.emit_for(for_stmt),
            CIRStmt::While(while_stmt) => self.emit_while(while_stmt),
            CIRStmt::Return(return_stmt) => self.emit_return(return_stmt),
            CIRStmt::Label(label_stmt) => self.emit_label(label_stmt),
            CIRStmt::Goto(goto_stmt) => self.emit_goto(goto_stmt),
            CIRStmt::Break(break_stmt) => self.emit_break(break_stmt),
            CIRStmt::Continue(continue_stmt) => self.emit_continue(continue_stmt),

            CIRStmt::Defer(_) => unreachable!(),
        }

        if let Some(dctx) = &self.dctx {
            unsafe {
                set_debug_location(
                    &dctx,
                    self.llvm_ctx,
                    self.llvmbuilder,
                    stmt.loc().line.try_into().unwrap(),
                    stmt.loc().column.try_into().unwrap(),
                )
            };
        }
    }

    pub(crate) fn emit_scope_block(&mut self, block: &CIRBlockStmt) {
        if let Some(dctx) = &mut self.dctx {
            let parent = debug_current_scope(&dctx);

            let lexical_block =
                unsafe { create_debug_lexical_block(&dctx, parent, block.loc.line as u32, block.loc.column as u32) };

            dctx.block_stack.push(BlockScope {
                lexical_block,
                inline_loc: std::ptr::null_mut(),
            });
        }

        self.emit_body(block);

        if let Some(dctx) = &mut self.dctx {
            dctx.block_stack.pop();
        }
    }

    pub(crate) fn emit_body(&mut self, block: &CIRBlockStmt) {
        self.emit_predefine_labels(block);

        self.defer_stack.push(block.defers.clone());

        for stmt in &block.stmts {
            if let Some(basic_block) = &self.blockreg.cur_block {
                if basic_block.get_terminator().is_some() {
                    break;
                }
            }

            self.emit_stmt(stmt);
        }

        if let Some(basic_block) = &self.blockreg.cur_block {
            if !basic_block.get_terminator().is_some() {
                self.emit_scope_defers();
            }
        }

        self.defer_stack.pop();
    }

    pub(crate) fn set_current_func(&mut self, llvm_func_value: FunctionValue<'ll>, abi_func_info: ABIFunctionInfo) {
        self.cur_func = Some(llvm_func_value);
        self.cur_abi_func_info = Some(abi_func_info);
    }
}

impl<'ll> Default for BlockRegistry<'ll> {
    fn default() -> Self {
        Self {
            control_flow_stack: Default::default(),
            cur_block: Default::default(),
            first_block: Default::default(),
            labels: Default::default(),
        }
    }
}
