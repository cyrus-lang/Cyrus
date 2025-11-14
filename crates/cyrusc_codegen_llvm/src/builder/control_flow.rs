use crate::{builder::builder::IRBuilderCtx, llvm::c_str::to_c_str};
use cyrusc_cir::{CIRForStmt, CIRIfStmt, CIRWhileStmt};
use inkwell::{
    basic_block::BasicBlock,
    context::AsContextRef,
    llvm_sys::{
        LLVMUse,
        core::{
            LLVMAppendBasicBlockInContext, LLVMAppendExistingBasicBlock, LLVMBasicBlockAsValue,
            LLVMCreateBasicBlockInContext, LLVMGetBasicBlockParent, LLVMGetFirstUse,
        },
    },
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

pub(crate) struct CFIf<'ll> {
    pub exit_block: BasicBlock<'ll>,
}

impl<'ll> CFEntry<'ll> {
    pub(crate) fn as_loop(&self) -> &CFLoop<'ll> {
        match self {
            CFEntry::Loop(cf_loop) => cf_loop,
        }
    }
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
    pub(crate) fn emit_for(&mut self, for_stmt: &CIRForStmt) {
        let cur_fn = self.cur_fn.unwrap();

        let cond_block = for_stmt
            .cond
            .as_ref()
            .map(|_| self.llvmctx.append_basic_block(cur_fn, "for_cond"));
        let inc_block = for_stmt
            .increment
            .as_ref()
            .map(|_| self.llvmctx.append_basic_block(cur_fn, "for_inc"));
        let body_block = self.llvmctx.append_basic_block(cur_fn, "for_body");
        let exit_block = self.llvmctx.append_basic_block(cur_fn, "for_exit");

        self.blockreg
            .control_flow_stack
            .push(CFEntry::Loop(CFLoop::new(cond_block, inc_block, exit_block)));

        if let Some(initializer) = &for_stmt.initializer {
            self.emit_var(initializer);
        }

        let cur_block = self.blockreg.cur_block.unwrap();
        self.llvmbuilder.position_at_end(cur_block);
        if cur_block.get_terminator().is_none() {
            let first_block = cond_block.unwrap_or(body_block);
            self.llvmbuilder.build_unconditional_branch(first_block).unwrap();
        }

        if let (Some(cond_expr), Some(cond_bb)) = (&for_stmt.cond, cond_block) {
            self.llvmbuilder.position_at_end(cond_bb);
            self.blockreg.cur_block = Some(cond_bb);
            let lvalue = self.emit_expr(cond_expr);
            let rvalue = self.load_rvalue(lvalue);
            let cond_val = rvalue.as_basic_value().into_int_value();

            let cur_block = self.blockreg.cur_block.unwrap();
            if cur_block.get_terminator().is_none() {
                self.llvmbuilder
                    .build_conditional_branch(cond_val, body_block, exit_block)
                    .unwrap();
            }
        }

        if let (Some(inc_expr), Some(inc_bb)) = (&for_stmt.increment, inc_block) {
            self.llvmbuilder.position_at_end(inc_bb);
            self.blockreg.cur_block = Some(inc_bb);

            self.emit_expr(inc_expr);

            let inc_block_now = self.blockreg.cur_block.unwrap();

            if inc_block_now.get_terminator().is_none() {
                let next_after_inc = cond_block.unwrap_or(body_block);
                self.llvmbuilder.build_unconditional_branch(next_after_inc).unwrap();
            }
        }

        self.llvmbuilder.position_at_end(body_block);
        self.blockreg.cur_block = Some(body_block);
        self.emit_body(&for_stmt.body);

        let cur_block = self.blockreg.cur_block.unwrap();
        if cur_block.get_terminator().is_none() {
            let next_block = inc_block.or(cond_block).unwrap_or(body_block);
            self.llvmbuilder.build_unconditional_branch(next_block).unwrap();
            self.blockreg.cur_block = Some(next_block);
        }

        self.llvmbuilder.position_at_end(exit_block);
        self.blockreg.cur_block = Some(exit_block);

        self.blockreg.control_flow_stack.pop();
    }

    pub(crate) fn emit_while(&mut self, while_stmt: &CIRWhileStmt) {
        let cur_fn = self.cur_fn.unwrap();

        let cond_block = self.llvmctx.append_basic_block(cur_fn, "while_cond");
        let body_block = self.llvmctx.append_basic_block(cur_fn, "while_body");
        let exit_block = self.llvmctx.append_basic_block(cur_fn, "while_exit");

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

        let cur_block = self
            .blockreg
            .cur_block
            .expect("missing current block while emitting while condition");

        if cur_block.get_terminator().is_none() {
            self.llvmbuilder
                .build_conditional_branch(cond_val, body_block, exit_block)
                .unwrap();
        }

        self.llvmbuilder.position_at_end(body_block);
        self.blockreg.cur_block = Some(body_block);
        self.emit_body(&while_stmt.body);

        let cur_block = self.blockreg.cur_block.unwrap();

        if cur_block.get_terminator().is_none() {
            self.llvmbuilder.build_unconditional_branch(cond_block).unwrap();
            self.blockreg.cur_block = Some(cond_block);
        }

        self.llvmbuilder.position_at_end(exit_block);
        self.blockreg.cur_block = Some(exit_block);

        self.blockreg.control_flow_stack.pop().unwrap();
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

        let mut exit_in_use = true;

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
                let first_use: *const LLVMUse =
                    unsafe { LLVMGetFirstUse(LLVMBasicBlockAsValue(exit_block.as_mut_ptr())) };
                exit_in_use = !first_use.is_null();
                if exit_in_use {
                    self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
                    self.blockreg.cur_block = None;
                }
            }
        }

        if then_block != exit_block {
            self.emit_block(then_block);
            self.emit_body(&if_stmt.then_block);
            if self.blockreg.cur_block.unwrap().get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }
            self.blockreg.cur_block = None;
            exit_in_use = true;
        }

        if else_block != exit_block {
            self.emit_block(else_block);
            self.emit_body(&if_stmt.else_block.as_ref().unwrap());
            if self.blockreg.cur_block.unwrap().get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }
            self.blockreg.cur_block = None;
            exit_in_use = true;
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
