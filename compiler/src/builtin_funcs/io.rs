use gccjit_sys::*;
use std::{ffi::CString, ptr::null_mut};

pub fn compile_builtin_printf_func(
    context: *mut gcc_jit_context,
    block: *mut gcc_jit_block,
    mut args: Vec<*mut gcc_jit_rvalue>,
) -> *mut gcc_jit_rvalue {
    let name = CString::new("printf").unwrap();
    let func_def = unsafe { gcc_jit_context_get_builtin_function(context, name.as_ptr()) };

    let rvalue = unsafe {
        gcc_jit_context_new_call(
            context,
            null_mut(),
            func_def,
            args.len().try_into().unwrap(),
            args.as_mut_ptr(),
        )
    };
    unsafe { gcc_jit_block_add_eval(block, null_mut(), rvalue) };

    null_mut()
}
