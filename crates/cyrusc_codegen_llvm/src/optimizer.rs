// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_internal::compiler_options::CompilerOption_Optimize;
use inkwell::{
    llvm_sys::transforms::pass_builder::{
        LLVMCreatePassBuilderOptions, LLVMDisposePassBuilderOptions, LLVMPassBuilderOptionsSetDebugLogging,
        LLVMPassBuilderOptionsSetLoopUnrolling, LLVMPassBuilderOptionsSetLoopVectorization,
        LLVMPassBuilderOptionsSetVerifyEach, LLVMRunPasses,
    },
    module::Module,
};
use std::ffi::CString;

// Standard optimization pipelines
const PIPELINE_O0: &str = "default<O0>";
const PIPELINE_O1: &str = "default<O1>";
const PIPELINE_O2: &str = "default<O2>";
const PIPELINE_OZ: &str = "default<Oz>";

const PIPELINE_AGGRESSIVE_CUSTOM: &str = "default<O3>";

const PIPELINE_SIZE: &str = "default<Os>";

fn get_optimization_pipeline(opt_level: CompilerOption_Optimize) -> &'static str {
    match opt_level {
        CompilerOption_Optimize::O0 => PIPELINE_O0,
        CompilerOption_Optimize::O1 => PIPELINE_O1,
        CompilerOption_Optimize::O2 => PIPELINE_O2,
        CompilerOption_Optimize::O3 => PIPELINE_AGGRESSIVE_CUSTOM, // Extra passes on top of O3
        CompilerOption_Optimize::Os => PIPELINE_SIZE,              // Enhanced size optimization
        CompilerOption_Optimize::Oz => PIPELINE_OZ,                // Plain Oz (most aggressive size)
    }
}

pub fn optimize_module_with_custom_passes<'ctx>(
    module: &Module<'ctx>,
    opt_level: CompilerOption_Optimize,
) -> Result<(), String> {
    let module_ptr = module.as_mut_ptr();
    let options = unsafe { LLVMCreatePassBuilderOptions() };

    unsafe {
        LLVMPassBuilderOptionsSetVerifyEach(options, 0);
        LLVMPassBuilderOptionsSetDebugLogging(options, 0);
        LLVMPassBuilderOptionsSetLoopVectorization(options, 1);
        LLVMPassBuilderOptionsSetLoopUnrolling(options, 1);
    }

    let pipeline = get_optimization_pipeline(opt_level);

    let pipeline_cstr = CString::new(pipeline).unwrap();

    let result = unsafe { LLVMRunPasses(module_ptr, pipeline_cstr.as_ptr(), std::ptr::null_mut(), options) };

    unsafe {
        LLVMDisposePassBuilderOptions(options);
    }

    if result == std::ptr::null_mut() {
        Ok(())
    } else {
        Err("optimization failed".to_string())
    }
}
