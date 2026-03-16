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
            BlockScope, DebugContext, create_debug_global_var_metadata, create_debug_lexical_block,
            create_debug_var_metadata, debug_current_scope, debug_func_type, emit_dbg_declare, set_debug_location,
        },
    },
};
use cyrusc_internal::{
    abi::{
        args::ABIFunctionInfo,
        layout::{ABITypeLayout, type_layout},
        target::ABITarget,
    },
    cir::{
        cir::{
            CIRBlockStmt, CIRGlobalVarStmt, CIRProgramTree, CIRStmt, CIRVarStmt, cir_enum_as_enum_ty,
            cir_func_decl_as_func_ty, cir_func_def_as_decl, cir_struct_as_struct_ty, cir_union_as_union_ty,
        },
        monomorph::CIRMonomorphRegistry,
    },
};
use cyrusc_tast::LabelID;
use cyrusc_tui_utils::tui_compiled;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::{AsContextRef, Context},
    llvm_sys::{
        LLVMOpaqueMetadata, core::LLVMSetCurrentDebugLocation2, debuginfo::LLVMDIBuilderCreateDebugLocation,
        prelude::LLVMMetadataRef,
    },
    module::Module,
    targets::TargetMachine,
    types::{AsTypeRef, BasicTypeEnum},
    values::{AsValueRef, FunctionValue, GlobalValue, PointerValue},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub(crate) struct IRBuilderCtx<'ll> {
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
    pub(crate) monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,

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

impl<'ll> IRBuilderCtx<'ll> {
    pub fn new(
        owned_module: &'ll OwnedModule,
        target: &'ll ABITarget,
        llvmbuilder: &'ll Builder<'ll>,
        llvmtm: &'ll TargetMachine,
        monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
        dctx: DebugContext,
    ) -> Self {
        let llvmmodule = unsafe {
            std::mem::transmute::<Rc<RefCell<Module<'static>>>, Rc<RefCell<Module<'ll>>>>(owned_module.module.clone())
        };

        let irreg = Rc::new(RefCell::new(LocalIRValueRegistry::new()));

        let blockreg = BlockRegistry::default();

        Self {
            target,
            llvmctx: &owned_module.context,
            llvmbuilder,
            llvmmodule,
            llvmtm,
            irreg,
            cur_func: None,
            cur_abi_func_info: None,
            blockreg,
            monomorph_registry,
            lambda_id: 0,
            defer_stack: Vec::new(),
            dctx,
        }
    }

    pub fn emit_program_tree(&mut self, cir_program_tree: &CIRProgramTree) {
        for cir_stmt in &cir_program_tree.body {
            self.emit_stmt(cir_stmt);
        }

        tui_compiled(cir_program_tree.file_path.clone());
    }

    pub(crate) fn emit_stmt(&mut self, stmt: &CIRStmt) {
        match stmt {
            CIRStmt::Variable(var_stmt) => self.emit_var(var_stmt),
            CIRStmt::GlobalVar(global_var_stmt) => {
                self.emit_global_var(global_var_stmt);
            }
            CIRStmt::FuncDef(func_def_stmt) => {
                let func_decl = cir_func_def_as_decl(func_def_stmt);
                let cir_func_ty = cir_func_decl_as_func_ty(&func_decl);
                let fn_value = self.emit_func_decl(&func_decl);
                self.set_current_func(fn_value, func_def_stmt.abi_func_info.clone().unwrap());
                let func_metadata = self.emit_func_metadata(&cir_func_ty);

                self.emit_func_body(
                    &func_decl.params,
                    &func_def_stmt.abi_func_info.as_ref().unwrap(),
                    &func_def_stmt.body,
                    func_metadata,
                    &func_decl.loc,
                );
            }
            CIRStmt::FuncDecl(func_decl_stmt) => {
                self.emit_func_decl(func_decl_stmt);
            }
            CIRStmt::Block(block_stmt) => self.emit_scope_block(block_stmt),
            CIRStmt::Struct(struct_stmt) => {
                self.emit_struct_ty(cir_struct_as_struct_ty(struct_stmt));
            }
            CIRStmt::Enum(enum_stmt) => {
                self.emit_enum_ty(cir_enum_as_enum_ty(enum_stmt));
            }
            CIRStmt::Union(union_stmt) => {
                self.emit_union_ty(cir_union_as_union_ty(union_stmt));
            }
            CIRStmt::Expr(expr) => {
                self.emit_expr(expr);
            }
            CIRStmt::Switch(switch_stmt) => self.emit_switch(switch_stmt),
            CIRStmt::SwitchOnEnum(switch_on_enum_stmt) => self.emit_switch_on_enum(switch_on_enum_stmt),
            CIRStmt::If(if_stmt) => self.emit_if(if_stmt),
            CIRStmt::For(for_stmt) => self.emit_for(for_stmt),
            CIRStmt::While(while_stmt) => self.emit_while(while_stmt),
            CIRStmt::Return(return_stmt) => self.emit_ret(return_stmt),
            CIRStmt::Label(label_stmt) => self.emit_label(label_stmt),
            CIRStmt::Goto(goto_stmt) => self.emit_goto(goto_stmt),
            CIRStmt::Break(loc) => self.emit_break(loc),
            CIRStmt::Continue(loc) => self.emit_continue(loc),
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

    pub(crate) fn emit_var(&mut self, cir_var: &CIRVarStmt) {
        let ty: BasicTypeEnum<'ll> = self.emit_ty(cir_var.ty.clone()).try_into().unwrap();
        let layout = type_layout(&self.target.info, &cir_var.ty);

        let ptr = self.llvmbuilder.build_alloca(ty, &cir_var.name).unwrap();
        let alloca_instr = ptr.as_instruction().unwrap();

        self.emit_var_metadata(&layout, &ptr, cir_var);

        if let Some(expr) = &cir_var.expr {
            let lvalue = self.emit_expr(expr);
            let rvalue = self.load_rvalue(lvalue);
            self.emit_store(ptr, rvalue, cir_var.ty.clone());
        } else {
            // zero init
            let zero_internal_value =
                InternalValue::new(cir_var.ty.clone(), InternalValueKind::RValue(ty.const_zero()));

            self.emit_store(ptr, zero_internal_value, cir_var.ty.clone());
        }

        alloca_instr.set_alignment(layout.align).unwrap();

        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(cir_var.irv_id, LocalIRValue::LValue(ptr, cir_var.ty.clone()));
        drop(irreg);
    }

    fn emit_var_metadata(&self, layout: &ABITypeLayout, ptr: &PointerValue<'ll>, cir_var: &CIRVarStmt) {
        let var_ty_metadata = self.emit_debug_ty_metadata(&cir_var.ty);

        let scope = debug_current_scope(&self.dctx);
        let file = self.dctx.file.metadata;
        let var_meta = unsafe {
            create_debug_var_metadata(
                &self.dctx,
                &cir_var.name,
                scope,
                file,
                cir_var.loc.line.try_into().unwrap(),
                var_ty_metadata,
                layout.align,
            )
        };

        unsafe {
            emit_dbg_declare(
                &self.dctx,
                self.llvmctx,
                self.llvmbuilder,
                ptr.as_value_ref(),
                var_meta,
                cir_var.loc.line.try_into().unwrap(),
                cir_var.loc.column.try_into().unwrap(),
            )
        };
    }

    fn emit_global_var_metadata(
        &self,
        layout: &ABITypeLayout,
        ptr: &PointerValue<'ll>,
        cir_global_var: &CIRGlobalVarStmt,
    ) {
        let var_ty_metadata = self.emit_debug_ty_metadata(&cir_global_var.ty);

        let is_global_var_local = !cir_global_var
            .modifiers
            .linkage
            .clone()
            .map(|linkage| linkage.is_extern())
            .unwrap_or(false);

        let scope = debug_current_scope(&self.dctx);
        let file = self.dctx.file.metadata;
        let var_meta = unsafe {
            create_debug_global_var_metadata(
                &self.dctx,
                scope,
                &cir_global_var.name,
                &cir_global_var.name,
                file,
                cir_global_var.loc.line.try_into().unwrap(),
                var_ty_metadata,
                is_global_var_local,
                layout.align,
            )
        };

        unsafe {
            emit_dbg_declare(
                &self.dctx,
                self.llvmctx,
                self.llvmbuilder,
                ptr.as_value_ref(),
                var_meta,
                cir_global_var.loc.line.try_into().unwrap(),
                cir_global_var.loc.column.try_into().unwrap(),
            )
        };
    }

    pub(crate) fn emit_global_var(&mut self, cir_global_var: &CIRGlobalVarStmt) -> GlobalValue<'ll> {
        {
            let irreg = self.irreg.borrow();
            if let Some(local_ir_value) = irreg.get(cir_global_var.irv_id) {
                return local_ir_value.as_global().cloned().unwrap();
            }
        }

        let llvmmodule = self.llvmmodule.borrow();

        if let Some(global_value) = llvmmodule.get_global(&cir_global_var.name) {
            return global_value;
        }

        let ty: BasicTypeEnum<'ll> = self.emit_ty(cir_global_var.ty.clone()).try_into().unwrap();
        let global_value = llvmmodule.add_global(ty, None, &cir_global_var.name);
        drop(llvmmodule);

        let layout = type_layout(&self.target.info, &cir_global_var.ty);
        self.emit_global_var_metadata(&layout, &global_value.as_pointer_value(), cir_global_var);

        if let Some(expr) = &cir_global_var.expr {
            let lvalue = self.emit_expr(&expr);
            let rvalue = self.load_rvalue(lvalue).as_basic_value();
            global_value.set_initializer(&rvalue);
        } else {
            if cir_global_var.modifiers.linkage.is_none() {
                // zero init
                global_value.set_initializer(&ty.const_zero());
            }
        }

        apply_global_var_modifiers(&global_value, &cir_global_var.modifiers);

        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(
            cir_global_var.irv_id,
            LocalIRValue::Global(global_value, cir_global_var.ty.clone()),
        );
        drop(irreg);

        global_value
    }

    pub(crate) fn set_current_func(&mut self, fn_value: FunctionValue<'ll>, abi_func_info: ABIFunctionInfo) {
        self.cur_func = Some(fn_value);
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
