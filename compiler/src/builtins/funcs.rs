use crate::Compiler;
use gccjit_sys::*;
use std::{ffi::CString, ptr::null_mut};
use utils::compile_time_errors::errors::*;
use utils::compiler_error;

pub fn builtin_func_len(
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
    let wrapper_func_name = CString::new("__cyrus_builtin__len").unwrap();
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

    let sizeof_array = unsafe {
        gcc_jit_context_new_cast(
            context,
            null_mut(),
            gcc_jit_context_new_sizeof(context, rvalue_type),
            size_t_type,
        )
    };

    let sizeof_first_item = unsafe {
        gcc_jit_context_new_cast(
            context,
            null_mut(),
            gcc_jit_context_new_sizeof(
                context,
                gcc_jit_rvalue_get_type(gcc_jit_lvalue_as_rvalue(gcc_jit_context_new_array_access(
                    context,
                    null_mut(),
                    rvalue,
                    gcc_jit_context_new_rvalue_from_int(context, Compiler::i32_type(context), 0),
                ))),
            ),
            size_t_type,
        )
    };

    let mut return_rvalue = unsafe {
        gcc_jit_context_new_binary_op(
            context,
            null_mut(),
            gcc_jit_binary_op::GCC_JIT_BINARY_OP_DIVIDE,
            Compiler::i32_type(context),
            sizeof_array,
            sizeof_first_item,
        )
    };
    return_rvalue = unsafe { gcc_jit_context_new_cast(context, null_mut(), return_rvalue, size_t_type) };

    unsafe { gcc_jit_block_end_with_return(block, null_mut(), return_rvalue) };
    return return_rvalue;
}

pub fn builtin_func_sizeof(
    _: String,
    context: *mut gcc_jit_context,
    args: Vec<*mut gcc_jit_rvalue>,
) -> *mut gcc_jit_rvalue {
    let rvalue = args.first().unwrap();
    let rvalue_type = unsafe { gcc_jit_rvalue_get_type(*rvalue) };
    let rvalue_size = unsafe { gcc_jit_context_new_sizeof(context, rvalue_type) };

    return rvalue_size;
}
