use llvm_sys::core::*;
use llvm_sys::prelude::*;

pub unsafe fn get_alloc_value(
    builder: LLVMBuilderRef,
    alloc: LLVMValueRef,
    ty: LLVMTypeRef,
) -> LLVMValueRef {
    LLVMBuildLoad2(builder, ty, alloc, "loaded_value".as_ptr() as *const i8)
}

pub fn store_string_data(builder: LLVMBuilderRef, stack_ptr: LLVMValueRef, string_data: &str) {
    unsafe {
        let i64_type = LLVMInt64TypeInContext(LLVMGetGlobalContext()); // Use i64 for indices
        let i8_type = LLVMInt8TypeInContext(LLVMGetGlobalContext());
        let zero = LLVMConstInt(i64_type, 0, 0);

        for (i, c) in string_data.chars().enumerate() {
            let mut indices = [zero, LLVMConstInt(i64_type, i as u64, 0)];

            let char_ptr = LLVMBuildGEP2(
                builder,
                LLVMTypeOf(stack_ptr), // Type of the pointer
                stack_ptr,
                indices.as_mut_ptr(),
                indices.len() as u32,
                format!("char_ptr_{}", i).as_ptr() as *const _,
            );

            LLVMBuildStore(builder, LLVMConstInt(i8_type, c as u64, 0), char_ptr);
        }

        let mut indices = [zero, LLVMConstInt(i64_type, string_data.len() as u64, 0)];
        let null_ptr = LLVMBuildGEP2(
            builder,
            LLVMTypeOf(stack_ptr),
            stack_ptr,
            indices.as_mut_ptr(),
            indices.len() as u32,
            "null_ptr".as_ptr() as *const _,
        );
        LLVMBuildStore(builder, LLVMConstInt(i8_type, 0, 0), null_ptr);
    }
}
