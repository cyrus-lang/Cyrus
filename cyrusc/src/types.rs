use std::ffi::CString;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::Compiler;

impl Compiler {
    pub unsafe fn i1_type(&self) -> LLVMTypeRef {
        LLVMInt1TypeInContext(self.context)
    }

    pub unsafe fn i8_type(&self) -> LLVMTypeRef {
        LLVMInt8TypeInContext(self.context)
    }

    pub unsafe fn i32_type(&self) -> LLVMTypeRef {
        LLVMInt32TypeInContext(self.context)
    }

    pub unsafe fn i64_type(&self) -> LLVMTypeRef {
        LLVMInt64TypeInContext(self.context)
    }

    pub unsafe fn f64_type(&self) -> LLVMTypeRef {
        LLVMDoubleTypeInContext(self.context)
    }

    pub unsafe fn string_type(&self) -> LLVMTypeRef {
        LLVMPointerType(LLVMInt8TypeInContext(self.context), 0)
    }

    pub unsafe fn i1_const(&self, num: bool) -> LLVMValueRef {
        LLVMConstInt(self.i1_type(), if num { 1 } else { 0 }, 0)
    }

    pub unsafe fn i8_const(&self, num: i8) -> LLVMValueRef {
        LLVMConstInt(self.i8_type(), num as u64, 0)
    }

    pub unsafe fn i32_const(&self, num: i32) -> LLVMValueRef {
        LLVMConstInt(self.i32_type(), num as u64, 0)
    }

    pub unsafe fn i64_const(&self, num: i64) -> LLVMValueRef {
        LLVMConstInt(self.i64_type(), num as u64, 0)
    }

    pub unsafe fn u64_const(&self, num: u64) -> LLVMValueRef {
        LLVMConstInt(self.i64_type(), num as u64, 0)
    }

    pub unsafe fn float_const(&self, num: f64) -> LLVMValueRef {
        LLVMConstReal(self.f64_type(), num)
    }

    pub unsafe fn null_const(&self, ltype: LLVMTypeRef) -> LLVMValueRef {
        LLVMConstNull(ltype)
    }

    pub unsafe fn string_const(&self, string: &str) -> LLVMValueRef {
        LLVMBuildGlobalStringPtr(self.builder, cstr(string), cstr("__string"))
    }
}

pub unsafe fn cstr(str: &str) -> *mut i8 {
    CString::new(str).unwrap().into_raw()
}
