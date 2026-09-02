// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

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
use cyrusc_internal::{
    abi::layout::ABITypeLayout,
    cir::cir::{CIRExpr, CIRExprKind, CIRGlobalVarStmt, CIRVarStmt, IRValueID},
};
use inkwell::{
    module::Linkage,
    types::BasicTypeEnum,
    values::{AsValueRef, GlobalValue, PointerValue},
};

#[derive(Debug, Clone)]
pub(crate) struct GlobalVarLazyInitializer<'a> {
    pub global_value: GlobalValue<'a>,
    pub expr: CIRExpr,
}

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

        let ty: BasicTypeEnum<'ll> = self.emit_type(cir_global_var.ty.clone()).try_into().unwrap();
        let global_value = llvm_module.add_global(ty, None, &cir_global_var.name);

        drop(llvm_module);

        let layout = self.tctx.layout_of(&cir_global_var.ty);

        if self.dctx.is_some() {
            self.emit_debug_global_var(&layout, &global_value, cir_global_var);
        }

        if let Some(expr) = &cir_global_var.expr {
            if global_var_expr_includes_enum_init(&expr.kind) {
                self.global_var_lazy_initializers.push(GlobalVarLazyInitializer {
                    global_value,
                    expr: expr.clone(),
                });
                global_value.set_initializer(&ty.const_zero());
            } else {
                let lvalue = self.emit_expr(&expr, &Some(cir_global_var.ty.clone()));
                let rvalue = self.load_rvalue(lvalue).as_basic_value();
                global_value.set_initializer(&rvalue);
            }
        } else {
            // Zero init only if not declared undefined
            if cir_global_var.modifiers.extrn.is_none() && !cir_global_var.is_undef {
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

        let global_decl = self
            .cir_module
            .global_var_decls
            .get(&irv_id)
            .cloned()
            .expect("Missing CIR global declaration");

        let llvm_global = self.emit_global_var(&global_decl);

        // AvailableExternally offers more inlining oppurtunities for the optimizer
        llvm_global.set_linkage(Linkage::AvailableExternally);

        InternalValue::new(
            global_decl.ty.clone(),
            InternalValueKind::LValue(llvm_global.as_pointer_value()),
        )
    }
}

// Local Variable.
impl<'ll> CodeGenIRBuilder<'ll> {
    pub(crate) fn emit_var(&mut self, cir_var: &CIRVarStmt) {
        let layout = self.tctx.layout_of(&cir_var.ty);

        let ty: BasicTypeEnum<'ll> = self.emit_type(cir_var.ty.clone()).try_into().unwrap();

        let ptr = self.llvm_builder.build_alloca(ty, &cir_var.name).unwrap();
        let alloca_instr = ptr.as_instruction().unwrap();

        if self.dctx.is_some() {
            self.emit_debug_var(&layout, &ptr, cir_var);
        }

        if let Some(expr) = &cir_var.expr {
            let lvalue = self.emit_expr(expr, &Some(cir_var.ty.clone()));
            let rvalue = self.load_rvalue(lvalue);
            self.emit_store(ptr, rvalue, cir_var.ty.clone());
        } else {
            if !cir_var.is_undef {
                // zero init only if not declared undefined
                let zero_internal_value =
                    InternalValue::new(cir_var.ty.clone(), InternalValueKind::RValue(ty.const_zero()));

                self.emit_store(ptr, zero_internal_value, cir_var.ty.clone());
            }
        }

        alloca_instr.set_alignment(layout.align).unwrap();

        self.insert_local_ir_value(cir_var.irv_id, LocalIRValue::LValue(ptr, cir_var.ty.clone()));
    }
}

// Debug Metadata.
impl<'ll> CodeGenIRBuilder<'ll> {
    fn emit_debug_global_var(
        &mut self,
        _layout: &ABITypeLayout,
        global_value: &GlobalValue<'ll>,
        cir_global_var: &CIRGlobalVarStmt,
    ) {
        assert!(self.dctx.is_some());

        let ty_meta = self.emit_debug_type_metadata(&cir_global_var.ty);

        let is_local = cir_global_var.modifiers.extrn.is_none();

        let dctx = self.dctx.as_ref().unwrap();

        let file = dctx.file.metadata;

        // globals should use the compile unit as scope
        let scope = dctx.compile_unit;

        unsafe {
            emit_global_debug(
                &dctx,
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

    fn emit_debug_var(&mut self, layout: &ABITypeLayout, ptr: &PointerValue<'ll>, cir_var: &CIRVarStmt) {
        assert!(self.dctx.is_some());

        let var_ty_meta = self.emit_debug_type_metadata(&cir_var.ty);

        let dctx = self.dctx.as_ref().unwrap();

        let var_meta = unsafe {
            create_debug_variable(
                &dctx,
                &cir_var.name,
                cir_var.loc.line.try_into().unwrap(),
                var_ty_meta,
                layout.align,
            )
        };

        unsafe {
            emit_dbg_declare(
                &dctx,
                self.llvm_ctx,
                self.llvm_builder,
                ptr.as_value_ref(),
                var_meta,
                cir_var.loc.line.try_into().unwrap(),
                cir_var.loc.column.try_into().unwrap(),
            )
        };
    }
}

fn global_var_expr_includes_enum_init(expr_kind: &CIRExprKind) -> bool {
    match expr_kind {
        CIRExprKind::EnumInit(_) => true,

        CIRExprKind::Prefix(_)
        | CIRExprKind::Infix(_)
        | CIRExprKind::Literal(_)
        | CIRExprKind::Load(_)
        | CIRExprKind::InlineAsm(_)
        | CIRExprKind::Type(_)
        | CIRExprKind::Lambda(_) => false,

        CIRExprKind::Dynamic(dynamic) => global_var_expr_includes_enum_init(&dynamic.data_expr.kind),

        CIRExprKind::Assign(assign) => {
            global_var_expr_includes_enum_init(&assign.lhs.kind) || global_var_expr_includes_enum_init(&assign.rhs.kind)
        }

        CIRExprKind::AddrOf(addr_of) => global_var_expr_includes_enum_init(&addr_of.operand.kind),

        CIRExprKind::Deref(deref) => global_var_expr_includes_enum_init(&deref.operand.kind),

        CIRExprKind::ArrayIndex(array_index) => global_var_expr_includes_enum_init(&array_index.operand.kind),

        CIRExprKind::Array(array) => array
            .elements
            .iter()
            .any(|expr| global_var_expr_includes_enum_init(&expr.kind)),

        CIRExprKind::Tuple(tuple) => tuple
            .elements
            .iter()
            .any(|expr| global_var_expr_includes_enum_init(&expr.kind)),

        CIRExprKind::TupleAccess(tuple_access) => global_var_expr_includes_enum_init(&tuple_access.operand.kind),

        CIRExprKind::StructInit(struct_init) => struct_init
            .fields
            .iter()
            .any(|expr| global_var_expr_includes_enum_init(&expr.kind)),

        CIRExprKind::Call(call) => call
            .args
            .iter()
            .any(|expr| global_var_expr_includes_enum_init(&expr.kind)),

        CIRExprKind::UnionInit(union_init) => global_var_expr_includes_enum_init(&union_init.expr.kind),

        CIRExprKind::FieldAccess(field_access) => global_var_expr_includes_enum_init(&field_access.operand.kind),
    }
}
