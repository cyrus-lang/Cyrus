use gccjit_sys::*;
use std::{
    ffi::{c_void, CString},
    mem,
    ptr::null_mut,
};

use crate::Compiler;

extern "C" {
    fn sample_func(fmt: *const i8);
}

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

pub fn compile_builtin_print_func(
    context: *mut gcc_jit_context,
    block: *mut gcc_jit_block,
    mut args: Vec<*mut gcc_jit_rvalue>,
) -> *mut gcc_jit_rvalue {
    let void_type = Compiler::void_type(context);

    let func_ptr = unsafe { mem::transmute::<*mut (), *mut c_void>(sample_func as *mut ()) };
    let func_type = unsafe { gcc_jit_context_new_function_ptr_type(context, null_mut(), void_type, 0, null_mut(), 0) };
    let func = unsafe { gcc_jit_context_new_rvalue_from_ptr(context, func_type, func_ptr) };

    let rvalue = unsafe {
        gcc_jit_context_new_call_through_ptr(
            context,
            null_mut(),
            func,
            args.len().try_into().unwrap(),
            args.as_mut_ptr(),
        )
    };
    unsafe { gcc_jit_block_add_eval(block, null_mut(), rvalue) };

    null_mut()
}

