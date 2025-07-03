use crate::CodeGenLLVM;
use inkwell::{
    AddressSpace,
    llvm_sys::{
        core::{LLVMAddFunction, LLVMFunctionType},
        prelude::{LLVMTypeRef, LLVMValueRef},
    },
    types::{AnyTypeEnum, AsTypeRef},
    values::FunctionValue,
};
use std::ffi::CString;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn intrinsic_va_start_function(&self) -> FunctionValue<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        self.retrieve_intrinsic_function(
            "llvm.va_start",
            AnyTypeEnum::VoidType(self.context.void_type()),
            &[AnyTypeEnum::PointerType(ptr_type.clone())],
        )
    }

    pub(crate) fn intrinsic_va_end_function(&self) -> FunctionValue<'ctx> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        self.retrieve_intrinsic_function(
            "llvm.va_end",
            AnyTypeEnum::VoidType(self.context.void_type()),
            &[AnyTypeEnum::PointerType(ptr_type.clone())],
        )
    }

    fn retrieve_intrinsic_function(
        &self,
        intrinsic_name: &'ctx str,
        return_type: AnyTypeEnum<'ctx>,
        param_types: &[AnyTypeEnum<'ctx>],
    ) -> FunctionValue<'ctx> {
        let mut param_types = param_types
            .iter()
            .map(|t| t.as_type_ref())
            .collect::<Vec<LLVMTypeRef>>();

        let raw_value = unsafe {
            self.retrieve_intrinsic_function_internal(
                intrinsic_name,
                return_type.as_type_ref(),
                param_types.as_mut_slice(),
            )
        };
        unsafe { FunctionValue::new(raw_value).unwrap() }
    }

    unsafe fn retrieve_intrinsic_function_internal(
        &self,
        intrinsic_name: &'ctx str,
        return_type: LLVMTypeRef,
        param_types: &mut [LLVMTypeRef],
    ) -> LLVMValueRef {
        let module_ref = self.module.borrow_mut().as_mut_ptr();
        let intrinsic_name_c_str = CString::new(intrinsic_name).unwrap();
        let intrinsic_function_type =
            unsafe { LLVMFunctionType(return_type, param_types.as_mut_ptr(), param_types.len() as u32, 0) };
        let intrinsic_func_decl =
            unsafe { LLVMAddFunction(module_ref, intrinsic_name_c_str.as_ptr(), intrinsic_function_type) };
        intrinsic_func_decl
    }
}
