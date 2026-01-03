/* 
 * Copyright (c) 2026 The Cyrus Language
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
