use crate::{builder::builder::IRBuilderCtx, llvm::c_str::to_c_str};
use cyrusc_cir::{CIRBlockStmt, CIRForStmt, CIRIfStmt, CIRSwitchStmt, CIRWhileStmt};
use inkwell::llvm_sys::{
    LLVMUse,
    core::{
        LLVMAppendExistingBasicBlock, LLVMBasicBlockAsValue, LLVMCreateBasicBlockInContext, LLVMGetBasicBlockParent,
        LLVMGetFirstUse,
    },
};
use inkwell::{
    basic_block::BasicBlock,
    context::AsContextRef,
    values::{AsValueRef, IntValue},
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
                let label_basic_block = self.emit_new_basic_block(&format!("label.{}", label_stmt.name));
                self.blockreg
                    .labels
                    .insert(label_stmt.label_id.clone(), label_basic_block);
            }
        }
    }

    pub(crate) fn ensure_entry_block(&mut self) {
        let entry_block = self.emit_new_basic_block("entry");
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

    pub(crate) fn emit_switch(&mut self, switch_stmt: &CIRSwitchStmt) {
        let lvalue = self.emit_expr(&switch_stmt.value);
        let rvalue = self.load_rvalue(lvalue);

        let parent_block = self.blockreg.cur_block.unwrap();
        let exit_block = self.emit_new_basic_block("switch.exit");

        let else_block = if let Some(block_stmt) = &switch_stmt.default {
            let else_block = self.emit_new_basic_block("switch.default");
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
            let case_block = self.emit_new_basic_block("switch.case");
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
        self.llvmbuilder.position_at_end(parent_block );

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
        let cond_block = for_stmt.cond.as_ref().map(|_| self.emit_new_basic_block("for.cond"));
        let inc_block = for_stmt
            .increment
            .as_ref()
            .map(|_| self.emit_new_basic_block("for_inc"));
        let body_block = self.emit_new_basic_block("for.body");
        let exit_block = self.emit_new_basic_block("for.exit");

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

        let body_block_now = self.blockreg.cur_block.unwrap();
        if body_block_now.get_terminator().is_none() {
            let next_block = inc_block.or(cond_block).unwrap_or(body_block);
            self.llvmbuilder.build_unconditional_branch(next_block).unwrap();
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
        let cond_val: IntValue<'ll> = {
            let lvalue = self.emit_expr(&if_stmt.cond);
            let rvalue = self.load_rvalue(lvalue);
            rvalue.as_basic_value().into_int_value()
        };

        let exit_block = self.emit_new_basic_block("if.exit");

        let then_has_body = !if_stmt.then_block.stmts.is_empty();
        let else_has_body = if_stmt.else_block.as_ref().map_or(false, |b| !b.stmts.is_empty());

        let mut then_block = if then_has_body {
            self.emit_new_basic_block("if.then")
        } else {
            exit_block
        };

        let mut else_block = if else_has_body {
            self.emit_new_basic_block("if.else")
        } else {
            exit_block
        };

        let mut exit_in_use = false;

        let cur_block = self.blockreg.cur_block.unwrap();
        self.emit_block(cur_block);

        if cond_val.is_const() && then_block != else_block {
            cond_val.get_zero_extended_constant().inspect(|val| {
                if *val == 1 {
                    self.llvmbuilder.build_unconditional_branch(then_block).unwrap();
                    else_block = exit_block;
                } else {
                    self.llvmbuilder.build_unconditional_branch(else_block).unwrap();
                    then_block = exit_block;
                }
            });
            self.blockreg.cur_block = None;
        } else {
            if then_block != else_block {
                self.llvmbuilder
                    .build_conditional_branch(cond_val, then_block, else_block)
                    .unwrap();
                self.blockreg.cur_block = None;
            } else {
                let first_use = unsafe { LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr())) };
                let has_use = !first_use.is_null();
                if has_use {
                    self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                    exit_in_use = true;
                    self.blockreg.cur_block = None;
                } else {
                    self.blockreg.cur_block = None;
                }
            }
        }

        if then_block != exit_block {
            self.emit_block(then_block);
            self.emit_body(&if_stmt.then_block);
            if self.blockreg.cur_block.unwrap().get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                exit_in_use = true;
            }
            self.blockreg.cur_block = None;
        }

        if else_block != exit_block {
            self.emit_block(else_block);
            self.emit_body(&if_stmt.else_block.as_ref().unwrap());
            if self.blockreg.cur_block.unwrap().get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                exit_in_use = true;
            }
            self.blockreg.cur_block = None;
        }

        if exit_in_use {
            self.emit_block(exit_block);
        }
    }

    pub(crate) fn emit_new_basic_block(&mut self, name: &str) -> BasicBlock<'ll> {
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
