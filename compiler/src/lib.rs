use std::ffi::CStr;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target;

mod compiler_test;

fn compile_func() ->LLVMModuleRef {
    let module_name = CStr::from_bytes_with_nul(b"hello\0").unwrap();
    let module = unsafe { LLVMModuleCreateWithName(module_name.as_ptr()) };
    let func_type = unsafe {
        let int64 = LLVMInt64Type();
        let mut param_types = [int64, int64];

        LLVMFunctionType(int64, param_types.as_mut_ptr(), param_types.len() as u32, 0)
    };

    let func_name = CStr::from_bytes_with_nul(b"sample_func\0").unwrap();
    let function = unsafe {
        LLVMAddFunction(module, func_name.as_ptr(), func_type)
    };

    let block_name = CStr::from_bytes_with_nul(b"entry\0").unwrap();
    let entry_block = unsafe {
        LLVMAppendBasicBlock(function, block_name.as_ptr())
    };

    let builder = unsafe {
        let b = LLVMCreateBuilder();
        LLVMPositionBuilderAtEnd(b, entry_block);
        b
    };

    // Fill in the body of the function.
    unsafe {
        let a = LLVMGetParam(function, 0);
        let b = LLVMGetParam(function, 1);

        let temp_name = CStr::from_bytes_with_nul(b"temp.1\0").unwrap();
        let res = LLVMBuildAdd(builder, a, b, temp_name.as_ptr());

        LLVMBuildRet(builder, res);
    }

    unsafe { LLVMDisposeBuilder(builder) }

    module
}

fn sdkmsdlkf() {
    
    unsafe {
        if target::LLVM_InitializeNativeTarget() != 0 {
            panic!("Could not initialise target");
        }
        if target::LLVM_InitializeNativeAsmPrinter() != 0 {
            panic!("Could not initialise ASM Printer");
        }
    }  

    let module = compile_func();

    // Dump the LLVM IR to stdout so we can see what we've created
    unsafe { LLVMDumpModule(module) }

    // Clean up the module after we're done with it.
    unsafe { LLVMDisposeModule(module) }
}
