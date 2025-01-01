#[cfg(test)]
mod tests {
    use std::ffi::{CStr, CString};
    use llvm_sys::{core::{LLVMConstString, LLVMGetAsString, LLVMGetInitializer, LLVMGetValueKind, LLVMIsAConstantDataArray}, prelude::LLVMValueRef, LLVMValueKind};

    use crate::types::cstr;

    fn my_extract_string(value: LLVMValueRef) -> Option<String> {
        unsafe {
            // Check if the value is a global variable
            if LLVMGetValueKind(value) != LLVMValueKind::LLVMGlobalVariableValueKind {
                return None;
            }
    
            // Get the initializer of the global variable
            let initializer = LLVMGetInitializer(value);
            if initializer.is_null() {
                return None;
            }
    
            // Check if the initializer is a constant data array
            if LLVMGetValueKind(initializer) != LLVMValueKind::LLVMConstantDataArrayValueKind {
                return None;
            }
    
            // Check if it is a string
            let mut length = 0;
            let c_string = LLVMGetAsString(initializer, &mut length as *mut usize);
            if c_string.is_null() {
                return None;
            }
    
            // Convert the C string to a Rust String
            let slice = CStr::from_ptr(c_string).to_bytes();
            Some(String::from_utf8_lossy(&slice[..length]).to_string())
        }
    }


    #[test]
    fn extract_string() {
    
        unsafe {
            let str = "Taha";
            let c_str = CString::new(str).unwrap();
    
            let llvmstr = LLVMConstString(cstr("Taha"), str.len() as u32 +1 , 0);

            dbg!(my_extract_string(llvmstr));
        }
    }
}