// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_internal::compiler_options::CompilerOption_Optimize;
use std::ffi::{CStr, CString, c_char, c_int, c_void};
use std::ptr;

mod tests;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CyrusOptLevel {
    O0 = 0,
    O1 = 1,
    O2 = 2,
    O3 = 3,
    Os = 4,
    Oz = 5,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct CyrusOptimizerConfig {
    pub verify_each: u32,
    pub debug_logging: u32,
    pub allow_loop_unrolling: u32,
    pub allow_slp_vectorization: u32,
    pub allow_loop_vectorization: u32,
    pub allow_loop_interleaving: u32,
    pub allow_merge_functions: u32,
    pub add_discriminators: u32,
    pub verify_input: u32,
    pub verify_output: u32,
    pub disable_code_hoisting: u32,
    pub disable_speculative_execution: u32,
}

#[repr(C)]
pub struct CyrusErrorMessage {
    data: *mut c_char,
    len: usize,
}

#[repr(C)]
pub struct CyrusOptimizerContext {
    _private: [u8; 0],
}

impl Default for CyrusOptimizerConfig {
    fn default() -> Self {
        unsafe { cyrus_optimizer_config_default() }
    }
}

impl CyrusOptimizerConfig {
    pub fn debug() -> Self {
        unsafe { cyrus_optimizer_config_debug() }
    }
}

unsafe extern "C" {
    unsafe fn cyrus_optimizer_create(target_machine: *mut c_void) -> *mut CyrusOptimizerContext;
    unsafe fn cyrus_optimizer_destroy(ctx: *mut CyrusOptimizerContext);
    unsafe fn cyrus_optimizer_set_target_machine(ctx: *mut CyrusOptimizerContext, target_machine: *mut c_void);
    unsafe fn cyrus_optimizer_config_default() -> CyrusOptimizerConfig;
    unsafe fn cyrus_optimizer_config_debug() -> CyrusOptimizerConfig;
    unsafe fn cyrus_module_get_triple(module_ptr: *mut c_void) -> *const c_char;
    unsafe fn cyrus_module_set_triple(module_ptr: *mut c_void, triple: *const c_char);
    unsafe fn cyrus_module_verify(module_ptr: *mut c_void, error: *mut CyrusErrorMessage) -> c_int;
    unsafe fn cyrus_error_message_free(error: *mut CyrusErrorMessage);
    unsafe fn cyrus_optimizer_optimize_simple(
        ctx: *mut CyrusOptimizerContext,
        module_ptr: *mut c_void,
        level: CyrusOptLevel,
        is_debug: c_int,
    ) -> c_int;
    unsafe fn cyrus_optimizer_optimize(
        ctx: *mut CyrusOptimizerContext,
        module_ptr: *mut c_void,
        level: CyrusOptLevel,
        config: *const CyrusOptimizerConfig,
    ) -> c_int;
}

pub struct CyrusLLVMOptimizer {
    ctx: *mut CyrusOptimizerContext,
}

impl CyrusLLVMOptimizer {
    pub fn new(target_machine: Option<&mut inkwell::targets::TargetMachine>) -> Self {
        let tm_ptr = target_machine
            .map(|tm| tm.as_mut_ptr() as *mut c_void)
            .unwrap_or(ptr::null_mut());

        let ctx = unsafe { cyrus_optimizer_create(tm_ptr) };
        Self { ctx }
    }

    pub fn set_target_machine(&mut self, target_machine: &mut inkwell::targets::TargetMachine) {
        unsafe {
            cyrus_optimizer_set_target_machine(self.ctx, target_machine.as_mut_ptr() as *mut c_void);
        }
    }

    pub fn optimize(
        &self,
        module: &inkwell::module::Module,
        level: CyrusOptLevel,
        config: Option<&CyrusOptimizerConfig>,
    ) -> Result<(), String> {
        let module_ptr = module.as_mut_ptr() as *mut c_void;
        let config_ptr = config.map(|c| c as *const CyrusOptimizerConfig).unwrap_or(ptr::null());

        let result = unsafe { cyrus_optimizer_optimize(self.ctx, module_ptr, level, config_ptr) };

        if result != 0 {
            Ok(())
        } else {
            Err("Optimization failed".to_string())
        }
    }

    pub fn optimize_simple(
        &self,
        module: &inkwell::module::Module,
        level: CyrusOptLevel,
        is_debug: bool,
    ) -> Result<(), String> {
        let module_ptr = module.as_mut_ptr() as *mut c_void;
        let result =
            unsafe { cyrus_optimizer_optimize_simple(self.ctx, module_ptr, level, if is_debug { 1 } else { 0 }) };

        if result != 0 {
            Ok(())
        } else {
            Err("Optimization failed".to_string())
        }
    }

    pub fn verify_module(module: &inkwell::module::Module) -> Result<(), String> {
        let module_ptr = module.as_mut_ptr() as *mut c_void;
        let mut error = CyrusErrorMessage {
            data: ptr::null_mut(),
            len: 0,
        };

        let result = unsafe { cyrus_module_verify(module_ptr, &mut error) };

        if result != 0 {
            Ok(())
        } else {
            let error_msg = if error.data.is_null() {
                "Unknown verification error".to_string()
            } else {
                unsafe {
                    let cstr = CStr::from_ptr(error.data);
                    cstr.to_string_lossy().into_owned()
                }
            };

            unsafe { cyrus_error_message_free(&mut error) };
            Err(error_msg)
        }
    }

    pub fn get_module_triple(module: &inkwell::module::Module) -> Option<String> {
        let module_ptr = module.as_mut_ptr() as *mut c_void;
        let triple_ptr = unsafe { cyrus_module_get_triple(module_ptr) };

        if triple_ptr.is_null() {
            None
        } else {
            unsafe {
                let cstr = CStr::from_ptr(triple_ptr);
                Some(cstr.to_string_lossy().into_owned())
            }
        }
    }

    pub fn set_module_triple(module: &inkwell::module::Module, triple: &str) {
        let module_ptr = module.as_mut_ptr() as *mut c_void;
        let triple_cstr = CString::new(triple).unwrap();
        unsafe {
            cyrus_module_set_triple(module_ptr, triple_cstr.as_ptr());
        }
    }
}

impl Drop for CyrusLLVMOptimizer {
    fn drop(&mut self) {
        if !self.ctx.is_null() {
            unsafe { cyrus_optimizer_destroy(self.ctx) };
        }
    }
}

impl From<CompilerOption_Optimize> for CyrusOptLevel {
    fn from(opt: CompilerOption_Optimize) -> Self {
        match opt {
            CompilerOption_Optimize::O0 => CyrusOptLevel::O0,
            CompilerOption_Optimize::O1 => CyrusOptLevel::O1,
            CompilerOption_Optimize::O2 => CyrusOptLevel::O2,
            CompilerOption_Optimize::O3 => CyrusOptLevel::O3,
            CompilerOption_Optimize::Os => CyrusOptLevel::Os,
            CompilerOption_Optimize::Oz => CyrusOptLevel::Oz,
        }
    }
}
