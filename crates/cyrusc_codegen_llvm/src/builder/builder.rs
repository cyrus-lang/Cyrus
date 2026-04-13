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

use crate::{
    OwnedModule,
    builder::{
        control_flow::CFEntry,
        irreg::{LocalIRValue, LocalIRValueRegistry, LocalIRValueRegistryRef},
        values::{InternalValue, InternalValueKind},
    },
    llvm::{
        abi::modifiers::apply_global_var_modifiers,
        debug_info::{
            BlockScope, DebugContext, create_debug_lexical_block, create_debug_variable, debug_current_scope,
            emit_dbg_declare, emit_global_debug, set_debug_location,
        },
    },
};
use cyrusc_internal::{
    abi::{args::ABIFunctionInfo, target::ABITarget},
    cir::cir::{
        CIRBlockStmt, CIRModule, CIRStmt, cir_enum_as_enum_type, cir_func_decl_as_func_type, cir_func_def_as_decl,
        cir_struct_as_struct_type, cir_union_as_union_type,
    },
};
use cyrusc_tui_utils::tui_compiled;
use cyrusc_typed_ast::LabelID;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::TargetMachine,
    types::BasicTypeEnum,
    values::{AsValueRef, FunctionValue, GlobalValue, PointerValue},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub(crate) struct CodeGenIRBuilder<'ll> {
    pub(crate) target: &'ll ABITarget,
    pub(crate) llvmctx: &'ll Context,
    pub(crate) llvmbuilder: &'ll Builder<'ll>,
    pub(crate) llvmmodule: Rc<RefCell<Module<'ll>>>,
    pub(crate) llvmtm: &'ll TargetMachine,
    pub(crate) irreg: LocalIRValueRegistryRef<'ll>,
    pub(crate) cur_func: Option<FunctionValue<'ll>>,
    pub(crate) cur_abi_func_info: Option<ABIFunctionInfo>,
    pub(crate) blockreg: BlockRegistry<'ll>,
    pub(crate) defer_stack: Vec<Vec<CIRStmt>>,

    pub(crate) cir_module: &'ll CIRModule,

    // lambda abi name (auto increment)
    pub(crate) lambda_id: usize,

    pub(crate) dctx: DebugContext,
}

#[derive(Debug, Clone)]
pub(crate) struct BlockRegistry<'ll> {
    pub(crate) control_flow_stack: Vec<CFEntry<'ll>>,
    pub(crate) first_block: Option<BasicBlock<'ll>>,
    pub(crate) cur_block: Option<BasicBlock<'ll>>,
    pub(crate) labels: HashMap<LabelID, BasicBlock<'ll>>,
}

impl<'ll> CodeGenIRBuilder<'ll> {
    pub fn new(
        owned_module: &'ll OwnedModule,
        cir_module: &'ll CIRModule,
        target: &'ll ABITarget,
        llvmbuilder: &'ll Builder<'ll>,
        llvmtm: &'ll TargetMachine,
        dctx: DebugContext,
    ) -> Self {
        let llvmmodule = unsafe {
            std::mem::transmute::<Rc<RefCell<Module<'static>>>, Rc<RefCell<Module<'ll>>>>(owned_module.module.clone())
        };

        let irreg = Rc::new(RefCell::new(LocalIRValueRegistry::new()));

        let blockreg = BlockRegistry::default();

        Self {
            target,
            cir_module,
            llvmctx: &owned_module.context,
            llvmbuilder,
            llvmmodule,
            llvmtm,
            irreg,
            cur_func: None,
            cur_abi_func_info: None,
            blockreg,
            lambda_id: 0,
            defer_stack: Vec::new(),
            dctx,
        }
    }

    pub fn emit_module(&mut self) {
        for cir_stmt in &self.cir_module.stmts {
            self.emit_stmt(cir_stmt);
        }

        tui_compiled(self.cir_module.file_path.clone());
    }

    pub(crate) fn emit_stmt(&mut self, stmt: &CIRStmt) {
        match stmt {
            CIRStmt::Variable(var_stmt) => self.emit_var(var_stmt),
            CIRStmt::FuncDef(func_def_stmt) => {
                let func_decl = cir_func_def_as_decl(func_def_stmt);
                let cir_func_ty = cir_func_decl_as_func_type(&func_decl);
                let llvm_func_value = self.emit_func_decl(&func_decl);
                self.set_current_func(llvm_func_value, func_def_stmt.abi_func_info.clone().unwrap());
                let func_metadata = self.emit_func_metadata(&cir_func_ty);

                self.emit_func_body(
                    &func_decl.params,
                    &func_def_stmt.abi_func_info.as_ref().unwrap(),
                    &func_def_stmt.body,
                    func_metadata,
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
            CIRStmt::Struct(struct_stmt) => {
                self.emit_struct_ty(cir_struct_as_struct_type(struct_stmt));
            }
            CIRStmt::Enum(enum_stmt) => {
                self.emit_enum_ty(cir_enum_as_enum_type(enum_stmt));
            }
            CIRStmt::Union(union_stmt) => {
                self.emit_union_ty(cir_union_as_union_type(union_stmt));
            }
            CIRStmt::Expr(expr) => {
                self.emit_expr(expr);
            }
            CIRStmt::Switch(switch_stmt) => self.emit_switch(switch_stmt),
            CIRStmt::SwitchOnEnum(switch_on_enum_stmt) => self.emit_switch_on_enum(switch_on_enum_stmt),
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

        unsafe {
            set_debug_location(
                &self.dctx,
                self.llvmctx,
                self.llvmbuilder,
                stmt.loc().line.try_into().unwrap(),
                stmt.loc().column.try_into().unwrap(),
            )
        };
    }

    pub(crate) fn emit_scope_block(&mut self, block: &CIRBlockStmt) {
        let parent = debug_current_scope(&self.dctx);

        let lexical_block =
            unsafe { create_debug_lexical_block(&self.dctx, parent, block.loc.line as u32, block.loc.column as u32) };

        self.dctx.block_stack.push(BlockScope {
            lexical_block,
            inline_loc: std::ptr::null_mut(),
        });

        self.emit_body(block);

        self.dctx.block_stack.pop();
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
