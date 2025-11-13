use cyrusc_cir::CIRIfStmt;
use inkwell::{basic_block::BasicBlock, values::IntValue};

use crate::builder::builder::IRBuilderCtx;

pub(crate) enum CFEntry<'ll> {
    Loop(CFLoop<'ll>),
    If(CFIf<'ll>),
}

pub(crate) struct CFLoop<'ll> {
    pub cond_block: BasicBlock<'ll>,
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
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_if(&self) -> &CFIf<'ll> {
        match self {
            CFEntry::If(cf_if) => cf_if,
            _ => unreachable!(),
        }
    }

    pub(crate) fn get_exit_block(&self) -> &BasicBlock<'ll> {
        match self {
            CFEntry::Loop(cf_loop) => &cf_loop.exit_block,
            CFEntry::If(cf_if) => &cf_if.exit_block,
        }
    }
}

impl<'ll> CFLoop<'ll> {
    pub(crate) fn new(
        cond_block: BasicBlock<'ll>,
        exit_block: BasicBlock<'ll>,
        inc_block: Option<BasicBlock<'ll>>,
    ) -> Self {
        Self {
            cond_block,
            inc_block,
            exit_block,
        }
    }
}

impl<'ll> CFIf<'ll> {
    pub(crate) fn new(exit_block: BasicBlock<'ll>) -> Self {
        Self { exit_block }
    }
}

impl<'ll> IRBuilderCtx<'ll> {
    pub(crate) fn emit_if(&mut self, if_stmt: &CIRIfStmt) {
        let cond: IntValue<'ll>;
        {
            let lvalue = self.emit_expr(&if_stmt.cond);
            let rvalue = self.load_rvalue(lvalue);
            cond = rvalue.as_basic_value().into_int_value();
        }

        let cur_fn = self.cur_fn.unwrap();
        let exit_block = self.llvmctx.append_basic_block(cur_fn, "exit_block");

        self.blockreg
            .control_flow_stack
            .push(CFEntry::If(CFIf::new(exit_block)));

        let then_has_body = !if_stmt.then_block.stmts.is_empty();
        let else_has_body = if_stmt
            .else_block
            .as_ref()
            .map_or(false, |block| !block.stmts.is_empty());

        let then_block = if then_has_body {
            self.llvmctx.append_basic_block(cur_fn, "if_then")
        } else {
            exit_block
        };

        let else_block = if else_has_body {
            self.llvmctx.append_basic_block(cur_fn, "if_else")
        } else {
            exit_block
        };

        let cur_block = self.blockreg.cur_block.unwrap();
        self.llvmbuilder.position_at_end(cur_block);

        if then_block != else_block {
            self.llvmbuilder
                .build_conditional_branch(cond, then_block, else_block)
                .unwrap();
        } else {
            self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
        }

        if then_has_body {
            self.llvmbuilder.position_at_end(then_block);
            self.blockreg.cur_block = Some(then_block);
            self.emit_body(&if_stmt.then_block);
            if then_block.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }
        }

        if else_has_body {
            self.llvmbuilder.position_at_end(else_block);
            self.blockreg.cur_block = Some(else_block);
            self.emit_body(&if_stmt.else_block.as_ref().unwrap());
            if else_block.get_terminator().is_none() {
                self.llvmbuilder.build_unconditional_branch(exit_block).unwrap();
            }
        }

        self.llvmbuilder.position_at_end(exit_block);
        self.blockreg.cur_block = Some(exit_block);

        self.blockreg.control_flow_stack.pop();

        if let Some(cf_entry) = self.blockreg.control_flow_stack.last() {
            let parent_exit_block = cf_entry.get_exit_block();
            self.llvmbuilder.build_unconditional_branch(*parent_exit_block).unwrap();
        }
    }
}
