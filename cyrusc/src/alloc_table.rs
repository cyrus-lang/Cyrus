use llvm_sys::prelude::{LLVMTypeRef, LLVMValueRef};

#[derive(Debug)]
pub struct AllocTable {
    pub alloc: LLVMValueRef,
    pub ty: LLVMTypeRef,
}
