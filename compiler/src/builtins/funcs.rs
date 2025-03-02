use crate::Compiler;
use gccjit_sys::*;
use utils::compiler_error;
use utils::compile_time_errors::errors::*;
use std::{ffi::CString, ptr::null_mut};

pub fn builtin_func__len(
    file_path: String,
    context: *mut gcc_jit_context,
    args: Vec<*mut gcc_jit_rvalue>,
) -> *mut gcc_jit_rvalue {
    if args.len() != 1 {
        compiler_error!(format!("Expected 1 argument but got {}", args.len()), file_path);
    }
    let rvalue = args[0].clone();
    let rvalue_type = unsafe { gcc_jit_rvalue_get_type(rvalue) };

    let size_t_type = Compiler::size_t_type(context);
    let void_ptr_type = Compiler::void_ptr_type(context);
    let wrapper_func_name = CString::new("__cyrus_builtin_wrapped_func__len").unwrap();
    // TODO  Check param type to be array or string
    let rvalue_param_name = CString::new("rvalue").unwrap();
    let rvalue_param =
        unsafe { gcc_jit_context_new_param(context, null_mut(), void_ptr_type, rvalue_param_name.as_ptr()) };
    let wrapper_func = unsafe {
        gcc_jit_context_new_function(
            context,
            null_mut(),
            gcc_jit_function_kind::GCC_JIT_FUNCTION_INTERNAL,
            size_t_type,
            wrapper_func_name.as_ptr(),
            1,
            [rvalue_param].as_mut_ptr(),
            0,
        )
    };

    let block_name = CString::new("entry").unwrap();
    let block = unsafe { gcc_jit_function_new_block(wrapper_func, block_name.as_ptr()) };

    let sizeof_array = unsafe { gcc_jit_context_new_sizeof(context, rvalue_type) };
    let first_item_of_array = unsafe {
        gcc_jit_context_new_array_access(
            context,
            null_mut(),
            rvalue,
            gcc_jit_context_new_rvalue_from_int(context, Compiler::i32_type(context), 0),
        )
    };

    let len_result = unsafe {
        gcc_jit_context_new_binary_op(
            context,
            null_mut(),
            gcc_jit_binary_op::GCC_JIT_BINARY_OP_DIVIDE,
            size_t_type,
            sizeof_array,
            unsafe {
                gcc_jit_lvalue_as_rvalue(first_item_of_array)
            },
        )
    };

    unsafe { gcc_jit_block_end_with_return(block, null_mut(), len_result) };

    return len_result;
}

pub fn builtin_func__sizeof(file_path: String,
    context: *mut gcc_jit_context,
    args: Vec<*mut gcc_jit_rvalue>,) -> *mut gcc_jit_rvalue {
    let rvalue = args.first().unwrap();
    let rvalue_type = unsafe { gcc_jit_rvalue_get_type(*rvalue) };
    let rvalue_size = unsafe { gcc_jit_context_new_sizeof(context, rvalue_type) };

    return rvalue_size;
}