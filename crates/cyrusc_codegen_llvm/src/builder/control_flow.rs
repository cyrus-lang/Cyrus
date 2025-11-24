use crate::{
    builder::{builder::IRBuilderCtx, irreg::LocalIRValue},
    llvm::c_str::to_c_str,
};
use cyrusc_cir::{
    CIRBlockStmt, CIRForStmt, CIRIfStmt, CIRSwitchOnEnumPattern, CIRSwitchOnEnumStmt, CIRSwitchStmt, CIRWhileStmt,
    types::CIRTy,
};
use cyrusc_tast::exprs::TypedIdentifier;
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
    values::{AsValueRef, IntValue},
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

pub(crate) enum CFEntry<'ll> {
    Loop(CFLoop<'ll>),
}

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
            if let cyrusc_cir::CIRStmt::Label(label_stmt) = cir_stmt {
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

    pub(crate) fn emit_label(&mut self, label_stmt: &cyrusc_cir::CIRLabelStmt) {
        if let Some(prev_block) = self.blockreg.cur_block {
            if prev_block.get_terminator().is_none() {
                let label_block = self.blockreg.labels.get(&label_stmt.label_id).unwrap();
                self.llvmbuilder.build_unconditional_branch(*label_block).unwrap();
            }
        }

        if let Some(label_basic_block) = self.blockreg.labels.get(&label_stmt.label_id) {
            self.emit_block(*label_basic_block);
        } else {
            panic!("Label block for '{}' not predefined.", label_stmt.name);
        }
    }

    pub(crate) fn emit_goto(&mut self, goto_stmt: &cyrusc_cir::CIRGotoStmt) {
        let target_block = self.blockreg.labels.get(&goto_stmt.label_id).cloned().unwrap();

        let cur_block = self.blockreg.cur_block.unwrap();
        self.emit_block(cur_block);
        if cur_block.get_terminator().is_none() {
            self.llvmbuilder.build_unconditional_branch(target_block).unwrap();
        }
    }

    pub(crate) fn emit_switch_on_enum_export_fields(
        &self,
        payload_alloca: PointerValue<'ll>,
        variant_field_types: Vec<CIRTy>,
        payload_struct_ty: StructType<'ll>,
        exported_fields: &Vec<(TypedIdentifier, CIRTy)>,
    ) {
        let mut irreg = self.irreg.borrow_mut();

        for (idx, (exported_field, _)) in exported_fields.iter().enumerate() {
            let ptr = self
                .llvmbuilder
                .build_struct_gep(
                    payload_struct_ty,
                    payload_alloca,
                    idx.try_into().unwrap(),
                    &format!("export_field.{}", exported_field.name),
                )
                .unwrap();

            let field_ty = &variant_field_types[idx];
            irreg.insert(exported_field.symbol_id, LocalIRValue::LValue(ptr, field_ty.clone()));
        }

        drop(irreg);
    }

    pub(crate) fn emit_switch_on_enum(&mut self, switch_on_enum_stmt: &CIRSwitchOnEnumStmt) {
        let lvalue = self.emit_expr(&switch_on_enum_stmt.value);
        let rvalue = self.load_rvalue(lvalue);
        let enum_ty = rvalue.ty.as_enum().unwrap();
        let enum_struct_ty = self.emit_enum_ty(enum_ty.clone());
        let enum_struct_value = rvalue.as_basic_value().into_struct_value();
        let enum_idx_int_value = self.extract_enum_idx(enum_struct_value);

        let parent_block = self.blockreg.cur_block.unwrap();
        let exit_block = self.new_basic_block("switch_on_enum.exit");

        let else_block = if let Some(block_stmt) = &switch_on_enum_stmt.default {
            let else_block = self.new_basic_block("switch_on_enum.default");
            self.emit_block(else_block);
            self.emit_body(block_stmt);

            let cur_block = self.blockreg.cur_block.unwrap();
            if cur_block.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }

            else_block
        } else {
            exit_block
        };

        let mut cases: Vec<(IntValue<'ll>, BasicBlock<'ll>)> = Vec::new();

        for case in &switch_on_enum_stmt.cases {
            let case_block = self.new_basic_block("switch_on_enum.case");
            self.emit_block(case_block);

            for pattern in &case.patterns {
                let pattern_int_value = self
                    .llvmctx
                    .i32_type()
                    .const_int(pattern.get_variant_idx().try_into().unwrap(), false);

                if let CIRSwitchOnEnumPattern::ExportFields(variant_idx, exported_fields) = pattern {
                    self.emit_block(case_block);

                    let enum_payload = self.extract_enum_payload(enum_struct_value);
                    let payload_field_tys: Vec<BasicTypeEnum<'ll>> = exported_fields
                        .iter()
                        .map(|(_, ty)| self.emit_ty(ty.clone()).try_into().unwrap())
                        .collect();
                    let payload_struct_type = self.llvmctx.struct_type(&payload_field_tys, false);
                    let payload_struct_value = self.intrinsic_copy_buffer_to_struct(enum_payload, enum_struct_ty);

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
                    );
                } else if let CIRSwitchOnEnumPattern::Valued(_, (identifier, expr)) = pattern {
                    self.emit_block(case_block);

                    let lvalue = self.emit_expr(expr);
                    let rvalue = self.load_rvalue(lvalue);
                    let alloca = self
                        .llvmbuilder
                        .build_alloca(rvalue.as_basic_value().get_type(), "alloca")
                        .unwrap();
                    self.llvmbuilder.build_store(alloca, rvalue.as_basic_value()).unwrap();

                    let mut irreg = self.irreg.borrow_mut();
                    irreg.insert(identifier.symbol_id, LocalIRValue::LValue(alloca, rvalue.ty));
                    drop(irreg);
                }

                cases.push((pattern_int_value, case_block));
            }

            self.emit_block(case_block);
            self.emit_body(&case.body);
            let cur_block = self.blockreg.cur_block.unwrap();
            self.llvmbuilder.position_at_end(cur_block);
            if cur_block.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }
        }

        // back to parent block to build switch instruction
        self.llvmbuilder.position_at_end(parent_block);

        self.llvmbuilder
            .build_switch(enum_idx_int_value, else_block, &cases)
            .unwrap();

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

            let cur_block = self.blockreg.cur_block.unwrap();
            if cur_block.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
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

            let cur_block = self.blockreg.cur_block.unwrap();
            if cur_block.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
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
            let lvalue = self.emit_expr(cond_expr);
            let rvalue = self.load_rvalue(lvalue);
            let cond_val = rvalue.as_basic_value().into_int_value();

            if cond_bb.get_terminator().is_none() {
                self.llvmbuilder
                    .build_conditional_branch(cond_val, body_block, exit_block)
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

        if let Some(body_block_now) = self.blockreg.cur_block {
            if body_block_now.get_terminator().is_none() {
                self.emit_block(body_block_now);
                let next_block = inc_block.or(cond_block).unwrap_or(exit_block);
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
        let cur_fn = self.cur_fn.unwrap();

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

        let lvalue = self.emit_expr(&while_stmt.cond);
        let rvalue = self.load_rvalue(lvalue);
        let cond_val = rvalue.as_basic_value().into_int_value();

        let cond_block_now = self.blockreg.cur_block.unwrap();
        if cond_block_now.get_terminator().is_none() {
            self.llvmbuilder
                .build_conditional_branch(cond_val, body_block, exit_block)
                .unwrap();
        }

        self.llvmbuilder.position_at_end(body_block);
        self.blockreg.cur_block = Some(body_block);
        self.emit_body(&while_stmt.body);

        let body_block_now = self.blockreg.cur_block.unwrap();
        if body_block_now.get_terminator().is_none() {
            self.llvmbuilder.build_unconditional_branch(cond_block).unwrap();
            self.blockreg.cur_block = Some(cond_block);
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

    pub(crate) fn emit_break(&mut self) {
        let entry = self.blockreg.control_flow_stack.last().unwrap();

        let CFEntry::Loop(cf_loop) = entry;

        let exit_block = cf_loop.exit_block;
        let cur_block = self.blockreg.cur_block.unwrap();

        if cur_block.get_terminator().is_none() {
            self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
        }
        self.blockreg.cur_block = None;
    }

    pub(crate) fn emit_continue(&mut self) {
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

        let cond_val: IntValue<'ll> = {
            let lvalue = self.emit_expr(&if_stmt.cond);
            let rvalue = self.load_rvalue(lvalue);
            rvalue.as_basic_value().into_int_value()
        };

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
            if LLVMIsAConstantInt(cond_val.as_value_ref()) != std::ptr::null_mut() && then_block != else_block {
                if LLVMConstIntGetZExtValue(cond_val.as_value_ref()) != 0 {
                    self.llvm_emit_br(then_block.as_mut_ptr());
                    else_block = exit_block;
                } else {
                    self.llvm_emit_br(else_block.as_mut_ptr());
                    then_block = exit_block;
                }
                self.blockreg.cur_block = None;
            } else {
                if then_block != else_block {
                    self.llvm_emit_cond_br(
                        cond_val.as_value_ref(),
                        then_block.as_mut_ptr(),
                        else_block.as_mut_ptr(),
                    );
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

            if let Some(cur_block) = self.blockreg.cur_block {
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
        let c_str = to_c_str(name);
        unsafe {
            let llvm_bb = LLVMCreateBasicBlockInContext(self.llvmctx.as_ctx_ref(), c_str.as_ptr());
            BasicBlock::new(llvm_bb).unwrap()
        }
    }

    pub(crate) fn emit_block(&mut self, next_block: BasicBlock<'ll>) {
        let cur_fn = self.cur_fn.unwrap();
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
