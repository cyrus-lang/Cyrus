// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_ast::{abi::Linkage, modifiers::GlobalVarModifiers};
use cyrusc_internal::{
    abi::layout::ABITypeLayout,
    cir::cir::{CIRGlobalVarStmt, CIRVarStmt, IRValueID},
};
use inkwell::{
    types::BasicTypeEnum,
    values::{AsValueRef, GlobalValue, PointerValue},
};

use crate::{
    builder::{
        builder::CodeGenIRBuilder,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    llvm::{
        abi::modifiers::apply_global_var_modifiers,
        debug_info::{create_debug_variable, emit_dbg_declare, emit_global_debug},
    },
};

// GlobalVar.
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_global_var(&mut self, cir_global_var: &CIRGlobalVarStmt) -> GlobalValue<'ll> {
        if let Some(ir_value) = self.lookup_local_ir_value(cir_global_var.irv_id) {
            return *ir_value.as_global().unwrap();
        }

        let llvm_module = self.llvm_module.borrow();

        if let Some(global_value) = llvm_module.get_global(&cir_global_var.name) {
            return global_value;
        }

        let ty: BasicTypeEnum<'ll> = self.emit_ty(cir_global_var.ty.clone()).try_into().unwrap();
        let global_value = llvm_module.add_global(ty, None, &cir_global_var.name);
        drop(llvm_module);

        let type_id = self.tctx.register(cir_global_var.ty.clone());
        let layout = self.tctx.get_or_compute_layout(type_id);

        self.emit_debug_global_var(&layout, &global_value, cir_global_var);

        if let Some(expr) = &cir_global_var.expr {
            let lvalue = self.emit_expr(&expr, &Some(cir_global_var.ty.clone()));
            let rvalue = self.load_rvalue(lvalue).as_basic_value();
            global_value.set_initializer(&rvalue);
        } else {
            if cir_global_var.modifiers.linkage.is_none() {
                // zero init
                global_value.set_initializer(&ty.const_zero());
            }
        }

        apply_global_var_modifiers(&global_value, &cir_global_var.modifiers);

        self.insert_local_ir_value(
            cir_global_var.irv_id,
            LocalIRValue::Global(global_value, cir_global_var.ty.clone()),
        );

        global_value
    }

    pub(crate) fn get_or_declare_global(&mut self, irv_id: IRValueID) -> InternalValue<'ll> {
        if let Some(local) = self.lookup_local_ir_value(irv_id) {
            if let LocalIRValue::Global(global, ty) = local {
                return InternalValue::new(ty, InternalValueKind::LValue(global.as_pointer_value()));
            }
        }

        let mut global_decl = self
            .cir_module
            .global_var_decls
            .get(&irv_id)
            .cloned()
            .expect("Missing CIR global declaration");

        global_decl.modifiers = GlobalVarModifiers {
            linkage: Some(Linkage::Extern(None)),
            ..global_decl.modifiers
        };

        let llvm_global = self.emit_global_var(&global_decl);

        InternalValue::new(
            global_decl.ty.clone(),
            InternalValueKind::LValue(llvm_global.as_pointer_value()),
        )
    }
}

// Var.
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_var(&mut self, cir_var: &CIRVarStmt) {
        let type_id = self.tctx.register(cir_var.ty.clone());
        let layout = self.tctx.get_or_compute_layout(type_id);

        let ty: BasicTypeEnum<'ll> = self.emit_ty(cir_var.ty.clone()).try_into().unwrap();

        let ptr = self.llvmbuilder.build_alloca(ty, &cir_var.name).unwrap();
        let alloca_instr = ptr.as_instruction().unwrap();

        self.emit_debug_var(&layout, &ptr, cir_var);

        if let Some(expr) = &cir_var.expr {
            let lvalue = self.emit_expr(expr, &Some(cir_var.ty.clone()));
            let rvalue = self.load_rvalue(lvalue);
            self.emit_store(ptr, rvalue, cir_var.ty.clone());
        } else {
            // zero init
            let zero_internal_value =
                InternalValue::new(cir_var.ty.clone(), InternalValueKind::RValue(ty.const_zero()));

            self.emit_store(ptr, zero_internal_value, cir_var.ty.clone());
        }

        alloca_instr.set_alignment(layout.align).unwrap();

        self.insert_local_ir_value(cir_var.irv_id, LocalIRValue::LValue(ptr, cir_var.ty.clone()));
    }
}

// Debug Metadata.
impl<'ll> CodeGenIRBuilder<'ll> {
    fn emit_debug_global_var(
        &self,
        _layout: &ABITypeLayout,
        global_value: &GlobalValue<'ll>,
        cir_global_var: &CIRGlobalVarStmt,
    ) {
        let ty_meta = self.emit_debug_ty_metadata(&cir_global_var.ty);

        let is_local = !cir_global_var
            .modifiers
            .linkage
            .clone()
            .map(|l| l.is_extern())
            .unwrap_or(false);

        let file = self.dctx.file.metadata;

        // globals should use the compile unit as scope
        let scope = self.dctx.compile_unit;

        unsafe {
            emit_global_debug(
                &self.dctx,
                global_value.as_value_ref(),
                scope,
                file,
                &cir_global_var.name,
                &cir_global_var.name,
                cir_global_var.loc.line as u32,
                ty_meta,
                is_local,
            );
        }
    }

    fn emit_debug_var(&self, layout: &ABITypeLayout, ptr: &PointerValue<'ll>, cir_var: &CIRVarStmt) {
        let var_ty_metadata = self.emit_debug_ty_metadata(&cir_var.ty);

        let var_meta = unsafe {
            create_debug_variable(
                &self.dctx,
                &cir_var.name,
                cir_var.loc.line.try_into().unwrap(),
                var_ty_metadata,
                layout.align,
            )
        };

        unsafe {
            emit_dbg_declare(
                &self.dctx,
                self.llvm_ctx,
                self.llvmbuilder,
                ptr.as_value_ref(),
                var_meta,
                cir_var.loc.line.try_into().unwrap(),
                cir_var.loc.column.try_into().unwrap(),
            )
        };
    }
}
