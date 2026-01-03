/* 
 * Copyright (c) 2026 The Cyrus Team
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
 */use crate::{
    OwnedModule,
    builder::{
        control_flow::CFEntry,
        irreg::{LocalIRValue, LocalIRValueRegistry, LocalIRValueRegistryRef},
        values::{InternalValue, InternalValueKind},
    },
    llvm::abi::modifiers::apply_global_var_modifiers,
};
use cyrusc_cir::{
    CIRBlockStmt, CIRGlobalVarStmt, CIRProgramTree, CIRReturnStmt, CIRStmt, CIRVarStmt, cir_enum_as_enum_ty,
    cir_func_def_as_decl, cir_struct_as_struct_ty, cir_union_as_union_ty, monomorph::CIRMonomorphRegistry,
};
use cyrusc_tast::LabelID;
use cyrusc_tui_utils::tui_compiled;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::TargetMachine,
    types::{AnyType, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue, GlobalValue},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::{Arc, Mutex},
};

pub(crate) struct IRBuilderCtx<'ll> {
    pub(crate) llvmctx: &'ll Context,
    pub(crate) llvmbuilder: &'ll Builder<'ll>,
    pub(crate) llvmmodule: Rc<RefCell<Module<'ll>>>,
    pub(crate) llvmtm: &'ll TargetMachine,
    pub(crate) irreg: LocalIRValueRegistryRef<'ll>,
    pub(crate) cur_fn: Option<FunctionValue<'ll>>,
    pub(crate) blockreg: BlockRegistry<'ll>,
    pub(crate) monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
    // lambda name (auto increment)
    pub(crate) lambda_id: usize,
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
        llvmbuilder: &'ll Builder<'ll>,
        llvmtm: &'ll TargetMachine,
        monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
    ) -> Self {
        let llvmmodule = unsafe {
            std::mem::transmute::<Rc<RefCell<Module<'static>>>, Rc<RefCell<Module<'ll>>>>(owned_module.module.clone())
        };

        let irreg = Rc::new(RefCell::new(LocalIRValueRegistry::new()));

        let blockreg = BlockRegistry::default();

        Self {
            llvmctx: &owned_module.context,
            llvmbuilder,
            llvmmodule,
            llvmtm,
            irreg,
            cur_fn: None,
            blockreg,
            monomorph_registry,
            lambda_id: 0,
        }
    }

    pub fn emit_program_tree(&mut self, cir_program_tree: &CIRProgramTree) {
        for cir_stmt in &cir_program_tree.body {
            self.emit_stmt(cir_stmt);
        }

        tui_compiled(cir_program_tree.file_path.clone());
    }

    pub(crate) fn emit_stmt(&mut self, cir_stmt: &CIRStmt) {
        match cir_stmt {
            CIRStmt::Variable(var_stmt) => self.emit_var(var_stmt),
            CIRStmt::GlobalVar(global_var_stmt) => {
                self.emit_global_var(global_var_stmt);
            }
            CIRStmt::FuncDef(func_def_stmt) => {
                let func_decl = cir_func_def_as_decl(func_def_stmt);
                let fn_value = self.emit_func_decl(&func_decl);
                self.cur_fn = Some(fn_value);
                self.emit_func_body(&func_decl.params, &func_def_stmt.body);
            }
            CIRStmt::FuncDecl(func_decl_stmt) => {
                self.emit_func_decl(func_decl_stmt);
            }
            CIRStmt::Block(block_stmt) => self.emit_body(block_stmt),
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
            CIRStmt::Break => self.emit_break(),
            CIRStmt::Continue => self.emit_continue(),
        }
    }

    pub(crate) fn emit_body(&mut self, cir_block: &CIRBlockStmt) {
        self.emit_predefine_labels(cir_block);

        for stmt in &cir_block.stmts {
            if self.blockreg.cur_block.is_none() {
                break;
            }
            self.emit_stmt(stmt);
        }
    }

    pub(crate) fn emit_ret(&mut self, return_stmt: &CIRReturnStmt) {
        let cur_fn = self.cur_fn.unwrap();

        if let Some(expr) = &return_stmt.arg {
            let lvalue = self.emit_expr(&expr);
            let rvalue = self.load_rvalue(lvalue);

            let ret_ty = cur_fn.get_type().get_return_type().unwrap();
            let casted: BasicValueEnum<'ll> = self.emit_cast(ret_ty.as_any_type_enum(), rvalue).try_into().unwrap();

            self.llvmbuilder.build_return(Some(&casted)).unwrap();
        } else {
            self.llvmbuilder.build_return(None).unwrap();
        }
    }

    pub(crate) fn emit_var(&mut self, cir_var: &CIRVarStmt) {
        let ty: BasicTypeEnum<'ll> = self.emit_ty(cir_var.ty.clone()).try_into().unwrap();
        let ptr = self.llvmbuilder.build_alloca(ty, &cir_var.name).unwrap();

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

        let mut irreg = self.irreg.borrow_mut();
        irreg.insert(cir_var.irv_id, LocalIRValue::LValue(ptr, cir_var.ty.clone()));
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
