use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::ffi::CStr;
use std::ffi::CString;

use crate::compiler_error;
use crate::types::cstr;
use crate::Compiler;

impl Compiler {
    pub fn builtin_printf_func(&mut self, args: &[LLVMValueRef]) -> LLVMValueRef {
        let printf_name = CString::new("printf").unwrap();
        let printf_type;

        let printf_func = unsafe { LLVMGetNamedFunction(self.module, printf_name.as_ptr()) };

        if printf_func.is_null() {
            let char_ptr_type = unsafe { LLVMPointerType(LLVMInt8TypeInContext(self.context), 0) };
            let printf_arg_types = [char_ptr_type];

            printf_type = unsafe {
                LLVMFunctionType(
                    self.i32_type(),
                    printf_arg_types.as_ptr() as *mut LLVMTypeRef,
                    1,
                    1, // variadic
                )
            };

            unsafe { LLVMAddFunction(self.module, printf_name.as_ptr(), printf_type) };
        } else {
            printf_type = unsafe { LLVMTypeOf(printf_func) };
        }

        let printf = unsafe { LLVMGetNamedFunction(self.module, printf_name.as_ptr()) };

        if args.is_empty() {
            compiler_error!("printf requires at least a format string argument.");
        }

        let final_fmt: LLVMValueRef;
        let kind = unsafe { LLVMGetValueKind(args[0]) };

        match kind {
            llvm_sys::LLVMValueKind::LLVMConstantDataArrayValueKind => {
                let fmt = args[0];
                let fmt_name = CString::new("format_str").unwrap();

                let mut fmt_size: usize = 0;
                let fmt_str = unsafe { LLVMGetAsString(fmt, &mut fmt_size) };

                let fmt_str = unsafe { CStr::from_ptr(fmt_str) };
                let fmt_str = fmt_str.to_string_lossy().replace("\\n", "\n");

                final_fmt = unsafe { LLVMBuildGlobalStringPtr(self.builder, fmt_str.as_ptr() as *const i8, fmt_name.as_ptr()) };
            }
            llvm_sys::LLVMValueKind::LLVMInstructionValueKind | llvm_sys::LLVMValueKind::LLVMGlobalVariableValueKind => final_fmt = args[0],
            _ => compiler_error!("This kind of formating not supported yet for printf func."),
        }

        let mut printf_args: Vec<LLVMValueRef> = Vec::new();
        printf_args.push(final_fmt);
        printf_args.extend_from_slice(&args[1..]);

        unsafe {
            LLVMBuildCall2(
                self.builder,
                printf_type,
                printf,
                printf_args.as_mut_ptr(),
                printf_args.len() as u32,
                cstr(""),
            )
        }
    }
}
