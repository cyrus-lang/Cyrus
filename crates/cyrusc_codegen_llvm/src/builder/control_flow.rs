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
    builder::{
        builder::IRBuilderCtx,
        irreg::LocalIRValue,
        values::{InternalValue, InternalValueKind},
    },
    c,
    llvm::abi::abi_type::abi_type_to_llvm_type,
};
use cyrusc_internal::{
    abi::{args::ABIRetInfoKind, layout::type_layout, types::ABIType},
    cir::{
        cir::{
            CIRBlockStmt, CIRForStmt, CIRGotoStmt, CIRIfStmt, CIRLabelStmt, CIRReturnStmt, CIRStmt,
            CIRSwitchOnEnumPattern, CIRSwitchOnEnumStmt, CIRSwitchStmt, CIRWhileStmt,
        },
        types::{CIREnumTy, CIRTupleTy, CIRTy},
    },
};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::exprs::TypedIdent;
use inkwell::{
    basic_block::BasicBlock,
    context::AsContextRef,
    llvm_sys::{
        core::{
            LLVMBuildBr, LLVMBuildCondBr, LLVMConstIntGetZExtValue, LLVMDeleteBasicBlock, LLVMGetFirstInstruction,
            LLVMIsAConstantInt,
        },
        prelude::{LLVMBasicBlockRef, LLVMValueRef},
    },
    types::{BasicTypeEnum, StructType},
    values::{AsValueRef, BasicValueEnum, FunctionValue, InstructionOpcode, IntValue},
};
use inkwell::{
    llvm_sys::{
        LLVMUse,
        core::{
            LLVMAppendExistingBasicBlock, LLVMBasicBlockAsValue, LLVMCreateBasicBlockInContext,
            LLVMGetBasicBlockParent, LLVMGetFirstUse,
        },
    },
    values::PointerValue,
};

#[derive(Debug, Clone)]
pub(crate) enum CFEntry<'ll> {
    Loop(CFLoop<'ll>),
}

#[derive(Debug, Clone)]
pub(crate) struct CFLoop<'ll> {
    pub cond_block: Option<BasicBlock<'ll>>,
    pub inc_block: Option<BasicBlock<'ll>>,
    pub exit_block: BasicBlock<'ll>,
}

impl<'ll> CFLoop<'ll> {
    pub(crate) fn new(
        cond_block: Option<BasicBlock<'ll>>,
        inc_block: Option<BasicBlock<'ll>>,
        exit_block: BasicBlock<'ll>,
    ) -> Self {
        Self {
            cond_block,
            inc_block,
            exit_block,
        }
    }
}

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_predefine_labels(&mut self, cir_block: &CIRBlockStmt) {
        for cir_stmt in &cir_block.stmts {
            if let CIRStmt::Label(label_stmt) = cir_stmt {
                let label_basic_block = self.new_basic_block(&format!("label.{}", label_stmt.name));
                self.blockreg
                    .labels
                    .insert(label_stmt.label_id.clone(), label_basic_block);
            }
        }
    }

    pub(crate) fn ensure_entry_block(&mut self) {
        let entry_block = self.new_basic_block("entry");
        self.blockreg.first_block = Some(entry_block);

        self.emit_block(entry_block);
    }

    pub(crate) fn emit_label(&mut self, label_stmt: &CIRLabelStmt) {
        if let Some(parent_block) = self.blockreg.cur_block {
            if parent_block.get_terminator().is_none() {
                let label_block = self.blockreg.labels.get(&label_stmt.label_id).unwrap();
                self.llvmbuilder.build_unconditional_branch(*label_block).unwrap();
            }
        }

        if let Some(basic_block) = self.blockreg.labels.get(&label_stmt.label_id) {
            self.emit_block(*basic_block);
        } else {
            panic!("Label block for '{}' not predefined.", label_stmt.name);
        }
    }

    pub(crate) fn emit_goto(&mut self, goto_stmt: &CIRGotoStmt) {
        let target_block = *self.blockreg.labels.get(&goto_stmt.label_id).unwrap();

        let cur_block = self.blockreg.cur_block.unwrap();

        if cur_block.get_terminator().is_none() {
            self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
        }

        self.blockreg.cur_block = None;
    }

    pub(crate) fn emit_scope_defers(&mut self) {
        let scope = match self.defer_stack.last() {
            Some(scope) => scope.clone(),
            None => return,
        };

        for stmt in scope.iter().rev() {
            self.emit_stmt(&stmt);
        }
    }

    pub(crate) fn emit_all_defers(&mut self) {
        let scopes: Vec<_> = self.defer_stack.iter().cloned().collect();

        for scope in scopes.iter().rev() {
            for stmt in scope.iter().rev() {
                if self.blockreg.cur_block.is_none() {
                    break;
                }

                self.emit_stmt(&stmt);
            }
        }
    }

    pub(crate) fn emit_ret(&mut self, return_stmt: &CIRReturnStmt) {
        let cur_fn = self.cur_func.unwrap();
        let cur_abi_func_info = self.cur_abi_func_info.clone().unwrap();
        let ret_info = &cur_abi_func_info.ret_info;

        match (&return_stmt.arg, &ret_info.kind) {
            (None, _) => {
                self.emit_all_defers();
                self.llvmbuilder.build_return(None).unwrap();
            }
            (Some(expr), ABIRetInfoKind::Ignore) => {
                self.emit_expr(expr);
                self.emit_all_defers();
                self.llvmbuilder.build_return(None).unwrap();
            }
            (Some(expr), ABIRetInfoKind::Indirect { sret }) => {
                let lvalue = self.emit_expr(expr);
                let rvalue = self.load_rvalue(lvalue.clone());

                if *sret {
                    self.emit_compute_indirect_sret(cur_fn, &lvalue, &rvalue);
                    self.emit_all_defers();
                    self.llvmbuilder.build_return(None).unwrap();
                    return;
                }
            }
            (Some(expr), ABIRetInfoKind::Direct { coerce_to }) => {
                let lvalue = self.emit_expr(expr);
                let rvalue = self.load_rvalue(lvalue);

                let ret_ty = abi_type_to_llvm_type(self.llvmctx, &self.target.info, &ret_info.abi_type);

                let value: BasicValueEnum<'ll> = if let Some(coerce) = coerce_to {
                    let coerce_ty = abi_type_to_llvm_type(self.llvmctx, &self.target.info, coerce);
                    self.emit_cast(coerce_ty, rvalue).try_into().unwrap()
                } else {
                    self.emit_cast(ret_ty, rvalue).try_into().unwrap()
                };

                let return_value =
                    self.intrinsic_coerce_through_alloca(value, ret_ty.try_into().unwrap(), "coerce.ret");

                self.emit_all_defers();
                self.llvmbuilder.build_return(Some(&return_value)).unwrap();
            }
            (Some(expr), ABIRetInfoKind::DirectPair { lo, hi }) => {
                let lvalue = self.emit_expr(expr);
                let rvalue = self.load_rvalue(lvalue);

                let return_value = self.emit_compute_return_direct_pair(rvalue, lo, hi, &ret_info.abi_type);

                self.emit_all_defers();
                self.llvmbuilder.build_return(Some(&return_value)).unwrap();
            }
        }
    }

    fn emit_compute_indirect_sret(
        &mut self,
        cur_fn: FunctionValue<'ll>,
        lvalue: &InternalValue<'ll>,
        rvalue: &InternalValue<'ll>,
    ) {
        let sret_param = cur_fn.get_first_param().unwrap();
        let sret_ptr = sret_param.into_pointer_value();

        let struct_ty = rvalue.ty.clone();
        let struct_layout = type_layout(&self.target.info, &struct_ty);

        let size_val = self.llvmctx.i64_type().const_int(struct_layout.size as u64, false);

        let src_ptr = match &lvalue.kind {
            InternalValueKind::LValue(ptr) => *ptr,
            InternalValueKind::RValue(val) => {
                let temp_alloca = self.llvmbuilder.build_alloca(val.get_type(), "sret.temp").unwrap();

                self.llvmbuilder.build_store(temp_alloca, *val).unwrap();
                temp_alloca
            }
            InternalValueKind::FuncValue(_) => unreachable!(),
        };

        self.llvmbuilder
            .build_memcpy(
                sret_ptr,
                self.target.info.pointer_align(),
                src_ptr,
                self.target.info.pointer_align(),
                size_val,
            )
            .unwrap();
    }

    fn emit_compute_return_direct_pair(
        &mut self,
        rvalue: InternalValue<'ll>,
        lo: &ABIType,
        hi: &ABIType,
        abi_ret_type: &ABIType,
    ) -> BasicValueEnum<'ll> {
        let value = rvalue.as_basic_value();

        let lo_ty: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvmctx, &self.target.info, lo)
            .try_into()
            .unwrap();

        let hi_ty: BasicTypeEnum<'ll> = abi_type_to_llvm_type(self.llvmctx, &self.target.info, hi)
            .try_into()
            .unwrap();

        let pair_ty = self.emit_abi_pair_llvm_type(lo, hi);

        let src_alloca = self
            .llvmbuilder
            .build_alloca(value.get_type(), "ret.src.alloca")
            .unwrap();

        self.llvmbuilder.build_store(src_alloca, value).unwrap();

        let lo_ptr = self
            .llvmbuilder
            .build_struct_gep(pair_ty, src_alloca, 0, "ret.lo.ptr")
            .unwrap();

        let lo_val = self.llvmbuilder.build_load(lo_ty, lo_ptr, "ret.lo").unwrap();

        let hi_ptr = self
            .llvmbuilder
            .build_struct_gep(pair_ty, src_alloca, 1, "ret.hi.ptr")
            .unwrap();

        let hi_val = self.llvmbuilder.build_load(hi_ty, hi_ptr, "ret.hi").unwrap();

        // construct ABI return struct
        let ret_struct_ty: StructType = abi_type_to_llvm_type(self.llvmctx, &self.target.info, abi_ret_type)
            .try_into()
            .unwrap();

        let mut pair_struct = ret_struct_ty.get_undef();

        pair_struct = self
            .llvmbuilder
            .build_insert_value(pair_struct, lo_val, 0, "ret.insert.lo")
            .unwrap()
            .into_struct_value();

        pair_struct = self
            .llvmbuilder
            .build_insert_value(pair_struct, hi_val, 1, "ret.insert.hi")
            .unwrap()
            .into_struct_value();

        pair_struct.into()
    }

    pub(crate) fn emit_switch_on_enum_export_fields(
        &self,
        payload_alloca: PointerValue<'ll>,
        variant_field_types: Vec<CIRTy>,
        payload_struct_ty: StructType<'ll>,
        exported_fields: &Vec<(TypedIdent, CIRTy)>,
        loc: Loc,
    ) {
        let mut irreg = self.irreg.borrow_mut();

        let elements: Vec<CIRTy> = exported_fields.iter().map(|(_, ty)| ty.clone()).collect();
        let tuple_type = CIRTy::Tuple(CIRTupleTy { elements, loc });
        let layout = type_layout(&self.target.info, &tuple_type);

        for (i, (exported_field, _)) in exported_fields.iter().enumerate() {
            let index = layout.lookup_field_index(i).unwrap();

            let ptr = self
                .llvmbuilder
                .build_struct_gep(
                    payload_struct_ty,
                    payload_alloca,
                    index,
                    &format!("export_field.{}", exported_field.name),
                )
                .unwrap();

            let field_ty = &variant_field_types[i];
            irreg.insert(
                exported_field.symbol_id.0,
                LocalIRValue::LValue(ptr, field_ty.clone()),
            );
        }

        drop(irreg);
    }

    fn emit_switch_on_scalar_enum(
        &mut self,
        switch_on_enum_stmt: &CIRSwitchOnEnumStmt,
        enum_value: IntValue<'ll>,
        enum_ty: &CIREnumTy,
    ) {
        let parent_block = self.blockreg.cur_block.unwrap();
        let exit_block = self.new_basic_block("switch_on_enum.exit");

        // for scalar enums the value itself is the tag
        let enum_idx_int_value = enum_value;

        let else_block = if let Some(block_stmt) = &switch_on_enum_stmt.default {
            let else_block = self.new_basic_block("switch_on_enum.default");

            self.emit_block(else_block);
            self.emit_body(block_stmt);

            if let Some(cur_block) = &self.blockreg.cur_block {
                if cur_block.get_terminator().is_none() {
                    self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                }
            }

            else_block
        } else {
            exit_block
        };

        let mut cases: Vec<(IntValue<'ll>, BasicBlock<'ll>)> = Vec::new();

        for case in &switch_on_enum_stmt.cases {
            let case_block = self.new_basic_block("switch_on_enum.case");

            for pattern in &case.patterns {
                let tag = enum_ty.compute_variant_tag(pattern.variant_name()).unwrap() as u64;

                let pattern_value = enum_value.get_type().const_int(tag, false);

                cases.push((pattern_value, case_block));
            }

            self.emit_block(case_block);

            // payload binding
            for pattern in &case.patterns {
                if let CIRSwitchOnEnumPattern::Valued(_, _, (ident, expr_ty)) = pattern {
                    let payload_ty: BasicTypeEnum<'ll> = self.emit_ty(expr_ty.ty.clone()).try_into().unwrap();
                    let payload_alloca = self.llvmbuilder.build_alloca(payload_ty, "enum_payload").unwrap();

                    // For scalar enums the value itself is the payload
                    self.llvmbuilder.build_store(payload_alloca, enum_value).unwrap();

                    let mut irreg = self.irreg.borrow_mut();
                    irreg.insert(
                        ident.symbol_id.0,
                        LocalIRValue::LValue(payload_alloca, expr_ty.ty.clone()),
                    );
                }
            }

            self.emit_body(&case.body);

            if let Some(cur_block) = &self.blockreg.cur_block {
                if cur_block.get_terminator().is_none() {
                    self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                }
            }
        }

        // emit switch
        self.llvmbuilder.position_at_end(parent_block);

        self.llvmbuilder
            .build_switch(enum_idx_int_value, else_block, &cases)
            .unwrap();

        // unreachable optimization
        let all_cases_return = cases.iter().all(|(_, bb)| {
            bb.get_terminator()
                .map_or(false, |inst| inst.get_opcode() == InstructionOpcode::Return)
        });

        if all_cases_return && switch_on_enum_stmt.cases.len() == enum_ty.variants.len() {
            self.emit_block(exit_block);
            self.llvmbuilder.build_unreachable().unwrap();
        }

        let exit_in_use: bool = unsafe {
            let first_use: *const LLVMUse = LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr()));
            !first_use.is_null()
        };

        if exit_in_use {
            self.emit_block(exit_block);
        }
    }

    pub(crate) fn emit_switch_on_enum(&mut self, switch_on_enum_stmt: &CIRSwitchOnEnumStmt) {
        let lvalue = self.emit_expr(&switch_on_enum_stmt.value);
        let rvalue = self.load_rvalue(lvalue);
        let enum_ty = rvalue.ty.as_enum().unwrap();

        {
            let ty = self.emit_enum_ty(enum_ty.clone());

            if ty.is_int_type() {
                let enum_value = rvalue.as_basic_value().into_int_value();
                self.emit_switch_on_scalar_enum(switch_on_enum_stmt, enum_value, &enum_ty);
                return;
            }
        }

        let enum_struct_value = rvalue.as_basic_value().into_struct_value();
        let enum_idx_int_value = self.extract_enum_tag(enum_struct_value);

        let parent_block = self.blockreg.cur_block.unwrap();
        let exit_block = self.new_basic_block("switch_on_enum.exit");

        let else_block = if let Some(block_stmt) = &switch_on_enum_stmt.default {
            let else_block = self.new_basic_block("switch_on_enum.default");
            self.emit_block(else_block);
            self.emit_body(block_stmt);

            if let Some(cur_block) = &self.blockreg.cur_block {
                if cur_block.get_terminator().is_none() {
                    self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                }
            }

            else_block
        } else {
            exit_block
        };

        let tag_type = self.emit_ty(*enum_ty.tag_type_or_infer_or_default()).into_int_type();

        let mut cases: Vec<(IntValue<'ll>, BasicBlock<'ll>)> = Vec::new();

        for case in &switch_on_enum_stmt.cases {
            let case_block = self.new_basic_block("switch_on_enum.case");
            self.emit_block(case_block);

            for pattern in &case.patterns {
                let pattern_int_value = tag_type.const_int(pattern.variant_idx().try_into().unwrap(), false);

                if let CIRSwitchOnEnumPattern::ExportFields(_, variant_idx, exported_fields) = pattern {
                    self.emit_block(case_block);

                    let enum_payload = self.extract_enum_payload(enum_struct_value);

                    let payload_struct_type = self
                        .emit_enum_fielded_variant_payload_ty(*variant_idx, &enum_ty)
                        .unwrap();
                    let payload_struct_value = self.intrinsic_copy_buffer_to_struct(enum_payload, payload_struct_type);

                    let payload_alloca = self.llvmbuilder.build_alloca(payload_struct_type, "alloca").unwrap();
                    self.llvmbuilder
                        .build_store(payload_alloca, payload_struct_value)
                        .unwrap();

                    let variant_field_types = enum_ty.variants[*variant_idx].as_fielded().unwrap();

                    self.emit_switch_on_enum_export_fields(
                        payload_alloca,
                        variant_field_types.to_vec(),
                        payload_struct_type,
                        exported_fields,
                        switch_on_enum_stmt.loc,
                    );
                } else if let CIRSwitchOnEnumPattern::Valued(_, _, (ident, expr)) = pattern {
                    self.emit_block(case_block);

                    let variant_expr_type = &expr.ty;
                    let llvm_variant_expr_type: BasicTypeEnum<'ll> =
                        self.emit_ty(variant_expr_type.clone()).try_into().unwrap();

                    // reinterpret payload buffer as expr.ty

                    let enum_payload = self.extract_enum_payload(enum_struct_value);

                    let alloca = self
                        .llvmbuilder
                        .build_alloca(llvm_variant_expr_type, "reinterpret.variant")
                        .unwrap();

                    self.llvmbuilder.build_store(alloca, enum_payload).unwrap();

                    let mut irreg = self.irreg.borrow_mut();
                    irreg.insert(
                        ident.symbol_id.0,
                        LocalIRValue::LValue(alloca, variant_expr_type.clone()),
                    );
                    drop(irreg);
                }

                cases.push((pattern_int_value, case_block));
            }

            self.emit_block(case_block);
            self.emit_body(&case.body);

            if let Some(cur_block) = &self.blockreg.cur_block {
                self.llvmbuilder.position_at_end(*cur_block);
                if cur_block.get_terminator().is_none() {
                    self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                }
            }
        }

        // back to parent block to build switch instruction
        self.llvmbuilder.position_at_end(parent_block);

        self.llvmbuilder
            .build_switch(enum_idx_int_value, else_block, &cases)
            .unwrap();

        let all_cases_return = cases.iter().all(|(_, bb)| {
            bb.get_terminator()
                .map_or(false, |inst| inst.get_opcode() == InstructionOpcode::Return)
        });

        if all_cases_return && switch_on_enum_stmt.cases.len() == enum_ty.variants.len() {
            self.emit_block(exit_block);
            self.llvmbuilder.build_unreachable().unwrap();
        }

        let exit_in_use: bool = unsafe {
            let first_use: *const LLVMUse = LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr()));
            !first_use.is_null()
        };
        if exit_in_use {
            self.emit_block(exit_block);
        }
    }

    pub(crate) fn emit_switch(&mut self, switch_stmt: &CIRSwitchStmt) {
        let lvalue = self.emit_expr(&switch_stmt.value);
        let rvalue = self.load_rvalue(lvalue);

        let parent_block = self.blockreg.cur_block.unwrap();
        let exit_block = self.new_basic_block("switch.exit");

        let else_block = if let Some(block_stmt) = &switch_stmt.default {
            let else_block = self.new_basic_block("switch.default");
            self.emit_block(else_block);
            self.emit_body(block_stmt);

            if let Some(cur_block) = &self.blockreg.cur_block {
                if cur_block.get_terminator().is_none() {
                    self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                }
            }

            else_block
        } else {
            exit_block
        };

        let mut cases: Vec<(IntValue<'ll>, BasicBlock<'ll>)> = Vec::new();

        for case in &switch_stmt.cases {
            let case_block = self.new_basic_block("switch.case");
            self.emit_block(case_block);
            self.emit_body(&case.body);

            if let Some(cur_block) = &self.blockreg.cur_block {
                if cur_block.get_terminator().is_none() {
                    self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                }
            }

            for pattern in &case.patterns {
                let pattern_lvalue = self.emit_expr(pattern);
                let pattern_rvalue = self.load_rvalue(pattern_lvalue);
                let pattern_int_value = pattern_rvalue.as_basic_value().into_int_value();
                cases.push((pattern_int_value, case_block));
            }
        }

        // back to parent block to build switch instruction
        self.llvmbuilder.position_at_end(parent_block);

        self.llvmbuilder
            .build_switch(rvalue.as_basic_value().into_int_value(), else_block, &cases)
            .unwrap();

        let all_cases_return = cases.iter().all(|(_, bb)| {
            bb.get_terminator()
                .map_or(false, |inst| inst.get_opcode() == InstructionOpcode::Return)
        });

        if all_cases_return && switch_stmt.all_cases_covered {
            self.emit_block(exit_block);
            self.llvmbuilder.build_unreachable().unwrap();
        }

        let exit_in_use: bool = unsafe {
            let first_use: *const LLVMUse = LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr()));
            !first_use.is_null()
        };
        if exit_in_use {
            self.emit_block(exit_block);
        }
    }

    pub(crate) fn emit_for(&mut self, for_stmt: &CIRForStmt) {
        let cond_block = for_stmt.cond.as_ref().map(|_| self.new_basic_block("for.cond"));
        let inc_block = for_stmt.increment.as_ref().map(|_| self.new_basic_block("for_inc"));
        let body_block = self.new_basic_block("for.body");
        let exit_block = self.new_basic_block("for.exit");

        self.blockreg
            .control_flow_stack
            .push(CFEntry::Loop(CFLoop::new(cond_block, inc_block, exit_block)));

        if let Some(initializer) = &for_stmt.initializer {
            self.emit_var(initializer);
        }

        let cur_block = self.blockreg.cur_block.unwrap();
        self.emit_block(cur_block);
        if cur_block.get_terminator().is_none() {
            let first_block = cond_block.unwrap_or(body_block);
            self.llvmbuilder.build_unconditional_branch(first_block).unwrap();
        }

        if let (Some(cond_expr), Some(cond_bb)) = (&for_stmt.cond, cond_block) {
            self.emit_block(cond_bb);
            let cond = self.emit_cond(cond_expr);

            if cond_bb.get_terminator().is_none() {
                self.llvmbuilder
                    .build_conditional_branch(cond, body_block, exit_block)
                    .unwrap();
            }
        }

        if let (Some(inc_expr), Some(inc_bb)) = (&for_stmt.increment, inc_block) {
            self.emit_block(inc_bb);
            self.emit_expr(inc_expr);

            if inc_bb.get_terminator().is_none() {
                let next_after_inc = cond_block.unwrap_or(body_block);
                self.llvmbuilder.build_unconditional_branch(next_after_inc).unwrap();
            }
        }

        self.emit_block(body_block);
        self.emit_body(&for_stmt.body);

        if let Some(cur_block) = &self.blockreg.cur_block {
            if cur_block.get_terminator().is_none() {
                self.emit_block(*cur_block);

                let next_block: BasicBlock<'ll>;
                if for_stmt.cond.is_some() {
                    next_block = inc_block.or(cond_block).unwrap_or(exit_block);
                } else {
                    next_block = body_block; // unconditional for loop
                }

                self.llvmbuilder.build_unconditional_branch(next_block).unwrap();
            }
        }

        let exit_in_use: bool = unsafe {
            let first_use: *const LLVMUse = LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr()));
            !first_use.is_null()
        };
        if exit_in_use {
            self.emit_block(exit_block);
        }

        self.blockreg.control_flow_stack.pop();
    }

    pub(crate) fn emit_while(&mut self, while_stmt: &CIRWhileStmt) {
        let cur_fn = self.cur_func.unwrap();

        let cond_block = self.llvmctx.append_basic_block(cur_fn, "while.cond");
        let body_block = self.llvmctx.append_basic_block(cur_fn, "while.body");
        let exit_block = self.llvmctx.append_basic_block(cur_fn, "while.exit");

        self.blockreg
            .control_flow_stack
            .push(CFEntry::Loop(CFLoop::new(Some(cond_block), None, exit_block)));

        let cur_block = self.blockreg.cur_block.unwrap();
        self.llvmbuilder.position_at_end(cur_block);
        if cur_block.get_terminator().is_none() {
            self.llvmbuilder.build_unconditional_branch(cond_block).unwrap();
        }

        self.llvmbuilder.position_at_end(cond_block);
        self.blockreg.cur_block = Some(cond_block);

        let cond = self.emit_cond(&while_stmt.cond);

        let cond_block_now = self.blockreg.cur_block.unwrap();
        if cond_block_now.get_terminator().is_none() {
            self.llvmbuilder
                .build_conditional_branch(cond, body_block, exit_block)
                .unwrap();
        }

        self.llvmbuilder.position_at_end(body_block);
        self.blockreg.cur_block = Some(body_block);
        self.emit_body(&while_stmt.body);

        if let Some(body_block_now) = &self.blockreg.cur_block {
            if body_block_now.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(cond_block).unwrap();
                self.blockreg.cur_block = Some(cond_block);
            }
        }

        let exit_in_use: bool = unsafe {
            let first_use: *const LLVMUse = LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr()));
            !first_use.is_null()
        };
        if exit_in_use {
            self.llvmbuilder.position_at_end(exit_block);
            self.blockreg.cur_block = Some(exit_block);
        }

        self.blockreg.control_flow_stack.pop().unwrap();
    }

    pub(crate) fn emit_break(&mut self, _: &Loc) {
        let entry = self.blockreg.control_flow_stack.last().unwrap();

        let CFEntry::Loop(cf_loop) = entry;

        let exit_block = cf_loop.exit_block;
        let cur_block = self.blockreg.cur_block.unwrap();

        if cur_block.get_terminator().is_none() {
            self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
        }
        self.blockreg.cur_block = None;
    }

    pub(crate) fn emit_continue(&mut self, _: &Loc) {
        let entry = self.blockreg.control_flow_stack.last().unwrap();

        let CFEntry::Loop(cf_loop) = entry;

        let target_block = cf_loop.inc_block.or(cf_loop.cond_block).unwrap();

        let cur_block = self.blockreg.cur_block.unwrap();

        if cur_block.get_terminator().is_none() {
            self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
        }
        self.blockreg.cur_block = None;
    }

    pub(crate) fn emit_if(&mut self, if_stmt: &CIRIfStmt) {
        let exit_block = self.new_basic_block("if.exit");
        let mut then_block = exit_block;
        let mut else_block = exit_block;

        if !if_stmt.then_block.stmts.is_empty() {
            then_block = self.new_basic_block("if.then");
        }

        if if_stmt.else_block.as_ref().map_or(false, |b| !b.stmts.is_empty()) {
            else_block = self.new_basic_block("if.else");
        }

        let cond = self.emit_cond(&if_stmt.cond);

        let mut llvm_cur_block = self
            .blockreg
            .cur_block
            .as_ref()
            .map(|basic_block| basic_block.as_mut_ptr());

        if unsafe { llvm_get_current_block_if_in_use(&mut llvm_cur_block).is_none() } {
            return;
        }

        #[allow(unused_assignments)]
        let mut exit_in_use = true;

        unsafe {
            if LLVMIsAConstantInt(cond.as_value_ref()) != std::ptr::null_mut() && then_block != else_block {
                if LLVMConstIntGetZExtValue(cond.as_value_ref()) != 0 {
                    self.llvm_emit_br(then_block.as_mut_ptr());
                    else_block = exit_block;
                } else {
                    self.llvm_emit_br(else_block.as_mut_ptr());
                    then_block = exit_block;
                }
                self.blockreg.cur_block = None;
            } else {
                if then_block != else_block {
                    self.llvm_emit_cond_br(cond.as_value_ref(), then_block.as_mut_ptr(), else_block.as_mut_ptr());
                    self.blockreg.cur_block = None;
                } else {
                    exit_in_use =
                        LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr())) != std::ptr::null_mut();

                    if exit_in_use {
                        self.llvm_emit_br(exit_block.as_mut_ptr());
                        self.blockreg.cur_block = None;
                    }
                }
            }
        }

        if then_block != exit_block {
            self.emit_block(then_block);
            self.emit_body(&if_stmt.then_block);

            if let Some(cur_block) = self.blockreg.cur_block {
                if cur_block.get_terminator().is_none() {
                    self.llvm_emit_br(exit_block.as_mut_ptr());
                }
            }
            self.blockreg.cur_block = None;
        }

        if else_block != exit_block {
            self.emit_block(else_block);
            self.emit_body(if_stmt.else_block.as_ref().unwrap());

            if let Some(cur_block) = &self.blockreg.cur_block {
                if cur_block.get_terminator().is_none() {
                    self.llvm_emit_br(exit_block.as_mut_ptr());
                }
            }
            self.blockreg.cur_block = None;
        }

        let exit_in_use =
            unsafe { LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr())) != std::ptr::null_mut() };

        if exit_in_use {
            self.emit_block(exit_block);
        }
    }

    pub unsafe fn llvm_emit_check_block_branch(&mut self) -> bool {
        let cur = match self.blockreg.cur_block {
            Some(bb) => bb,
            None => return false,
        };

        // if it's the function's first block, never delete it.
        if let Some(first) = self.blockreg.first_block {
            if cur.as_mut_ptr() == first.as_mut_ptr() {
                return true;
            }
        }

        let first_use = unsafe { LLVMGetFirstUse(LLVMBasicBlockAsValue(cur.as_mut_ptr())) };
        if first_use.is_null() {
            unsafe { LLVMDeleteBasicBlock(cur.as_mut_ptr()) };
            self.blockreg.cur_block = None;
            return false;
        }
        true
    }

    pub fn llvm_emit_br(&mut self, next_block: LLVMBasicBlockRef) {
        unsafe {
            if !self.llvm_emit_check_block_branch() {
                return;
            }

            self.blockreg.cur_block = None;
            LLVMBuildBr(self.llvmbuilder.as_mut_ptr(), next_block);
        }
    }

    pub fn llvm_emit_cond_br(
        &mut self,
        cond: LLVMValueRef,
        then_block: LLVMBasicBlockRef,
        else_block: LLVMBasicBlockRef,
    ) {
        unsafe {
            if !self.llvm_emit_check_block_branch() {
                return;
            }
            self.blockreg.cur_block = None;
            LLVMBuildCondBr(self.llvmbuilder.as_mut_ptr(), cond, then_block, else_block);
        }
    }

    pub(crate) fn new_basic_block(&mut self, name: &str) -> BasicBlock<'ll> {
        unsafe {
            let llvm_bb = LLVMCreateBasicBlockInContext(self.llvmctx.as_ctx_ref(), c!(name).as_ptr());
            BasicBlock::new(llvm_bb).unwrap()
        }
    }

    pub(crate) fn emit_block(&mut self, next_block: BasicBlock<'ll>) {
        let cur_fn = self.cur_func.unwrap();
        unsafe {
            if LLVMGetBasicBlockParent(next_block.as_mut_ptr()).is_null() {
                LLVMAppendExistingBasicBlock(cur_fn.as_value_ref(), next_block.as_mut_ptr());
            }
            self.llvmbuilder.position_at_end(next_block);
        }
        self.blockreg.cur_block = Some(next_block);
    }
}

pub unsafe fn llvm_get_current_block_if_in_use(
    current_block: &mut Option<LLVMBasicBlockRef>,
) -> Option<LLVMBasicBlockRef> {
    if let Some(block) = *current_block {
        if unsafe { llvm_basic_block_is_unused(block) } {
            unsafe { LLVMDeleteBasicBlock(block) };
            *current_block = None;
            return None;
        }
        return Some(block);
    }
    None
}

unsafe fn llvm_basic_block_is_unused(block: LLVMBasicBlockRef) -> bool {
    let first_instr = unsafe { LLVMGetFirstInstruction(block) };
    let first_use = unsafe { LLVMGetFirstUse(LLVMBasicBlockAsValue(block)) };

    first_instr.is_null() && first_use.is_null()
}
