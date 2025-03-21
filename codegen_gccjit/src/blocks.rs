use crate::Compiler;
use gccjit_sys::*;

impl Compiler {
    pub(crate) fn switch_active_block(&mut self, active_block: *mut gcc_jit_block) {
        let mut guard = self.block_func_ref.lock().unwrap();
        guard.block = Some(active_block);
        drop(guard);
    }

    pub(crate) fn active_block(&mut self) -> Option<*mut gccjit_sys::gcc_jit_block> {
        let guard = self.block_func_ref.lock().unwrap();
        return guard.block;
    }

    pub(crate) fn mark_block_terminated(&mut self, block: *mut gcc_jit_block) {
        if !self.block_is_terminated(block) {
            self.terminated_blocks.push(block);
        }
    }

    pub(crate) fn block_is_terminated(&self, block: *mut gcc_jit_block) -> bool {
        self.terminated_blocks.contains(&block)
    }
}
