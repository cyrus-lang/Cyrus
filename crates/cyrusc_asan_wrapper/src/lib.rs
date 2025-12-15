use inkwell::llvm_sys::prelude::LLVMModuleRef;
use inkwell::llvm_sys::target_machine::LLVMTargetMachineRef;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;

mod tests;

#[repr(C)]
pub struct SanitizerOptions {
    pub address_sanitize: bool,
    pub thread_sanitize: bool,
    pub mem_sanitize: bool,
    pub hwaddress_sanitize: bool,
    pub recover: bool,
    pub asan_use_after_scope: bool,
    pub asan_use_after_return: bool,
}

#[link(name = "cyrusc_asan_wrapper", kind = "dylib")]
unsafe extern "C" {
    fn run_sanitizer_passes(
        module_ref: LLVMModuleRef,
        tm_ref: LLVMTargetMachineRef,
        opt_level: i32,
        opts: SanitizerOptions,
    );
}

pub fn run_sanitizers(module: &Module, tm: &TargetMachine, opt_level: i32, opts: SanitizerOptions) {
    unsafe {
        run_sanitizer_passes(module.as_mut_ptr(), tm.as_mut_ptr(), opt_level, opts);
    }
}
