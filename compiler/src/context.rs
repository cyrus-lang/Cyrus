use gccjit_sys::*;
use crate::Compiler;

impl Compiler {
    pub fn new_master_context() -> *mut gcc_jit_context {
        unsafe { gcc_jit_context_acquire() }
    }

    pub fn new_child_context(master: *mut gcc_jit_context) -> *mut gcc_jit_context {
        unsafe { gcc_jit_context_new_child_context(master) }
    }   
}