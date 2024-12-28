use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::ffi::CStr;
use std::ffi::CString;

use crate::compiler_error;

pub fn generate_printf(
    module: LLVMModuleRef,
    context: LLVMContextRef,
    builder: LLVMBuilderRef,
    args: &[LLVMValueRef],
) -> LLVMValueRef {
    let printf_name = CString::new("printf").unwrap();
    let printf_type;

    let printf_func = unsafe { LLVMGetNamedFunction(module, printf_name.as_ptr()) };

    if printf_func.is_null() {
        let int32_type = unsafe { LLVMInt32TypeInContext(context) };
        let char_ptr_type = unsafe { LLVMPointerType(LLVMInt8TypeInContext(context), 0) };
        let printf_arg_types = [char_ptr_type];

        printf_type = unsafe {
            LLVMFunctionType(
                int32_type,
                printf_arg_types.as_ptr() as *mut LLVMTypeRef,
                1,
                1, // variadic
            )
        };

        unsafe { LLVMAddFunction(module, printf_name.as_ptr(), printf_type) };
    } else {
        printf_type = unsafe { LLVMTypeOf(printf_func) };
    }

    let printf = unsafe { LLVMGetNamedFunction(module, printf_name.as_ptr()) };

    if args.is_empty() {
        compiler_error!("printf requires at least a format string argument.");
    }

    let fmt = args[0];
    let fmt_name = CString::new("format_str").unwrap();
    let mut printf_args: Vec<LLVMValueRef> = Vec::new();

    let mut fmt_size: usize = 0;
    let fmt_str = unsafe { LLVMGetAsString(fmt, &mut fmt_size) };
    let fmt_str = unsafe { CStr::from_ptr(fmt_str) };
    let fmt_str = fmt_str.to_string_lossy().replace("\\n", "\n");

    let fmt_global = unsafe {
        LLVMBuildGlobalStringPtr(builder, fmt_str.as_ptr() as *const i8, fmt_name.as_ptr())
    };

    printf_args.push(fmt_global);
    printf_args.extend_from_slice(&args[1..]);

    let call_name = CString::new("printf_call").unwrap();

    unsafe {
        LLVMBuildCall2(
            builder,
            printf_type,
            printf,
            printf_args.as_mut_ptr(),
            printf_args.len() as u32,
            call_name.as_ptr(),
        )
    }
}
