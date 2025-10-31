use crate::{object_file_info::ObjectFileInfo, target_machine_info::TargetMachineInfo};
use cyrusc_cir::CIRProgramTree;
use std::any::Any;

/// The base trait that every code generation backend must implement.
///
/// This trait defines the essential functionality a backend must provide
/// in order to emit object files, describe target machine details, and
/// expose its compilation strategy (unified or separate module compilation).
pub trait CodeGenBackend<'cdg, BackendModule> {
    /// Emits the given module to an object file.
    ///
    /// The backend owns the logic for generating the final object file
    /// from the internal representation of the module.
    ///
    /// # Parameters
    /// - `module`: A reference to the backend-specific module to be saved.
    ///
    /// # Returns
    /// Returns an `ObjectFileInfo` describing the output object file.
    fn save_object_file(&'cdg self, module: &BackendModule) -> ObjectFileInfo;

    /// Returns information about the target machine that this backend is generating code for.
    ///
    /// This includes details like the target triple, CPU name, pointer size,
    /// relocation mode, code model, and optimization level.
    fn get_target_machine_info(&'cdg self) -> TargetMachineInfo;

    /// Returns a human-readable name for the backend.
    ///
    /// Typically something like `"llvm"` or `"cyrusc_jit"`.
    fn name(&'cdg self) -> &'static str;

    /// Allows dynamic downcasting to the concrete backend type.
    ///
    /// Useful when working with trait objects and needing to access
    /// backend-specific functionality.
    fn as_any(&'cdg self) -> &'cdg dyn Any;

    /// Returns a reference to the backend as a unified module compiler, if supported.
    ///
    /// Unified module compilation emits all CIR modules into a single LLVM module
    /// for optimizations like link-time optimization or reduced cross-module overhead.
    fn as_unified(&'cdg self) -> Option<&'cdg dyn UnifiedModuleSupport<'cdg, BackendModule>> {
        None
    }

    /// Returns a reference to the backend as a separate module compiler, if supported.
    ///
    /// Separate module compilation emits each CIR module independently,
    /// allowing parallel compilation and independent object file generation.
    fn as_separate(&'cdg self) -> Option<&'cdg dyn SeparateModuleSupport<'cdg, BackendModule>> {
        None
    }
}

/// Trait implemented by backends that can merge all modules into one unified LLVM module,
/// compiling serially (e.g., for link-time optimization or reduced cross-module overhead).
pub trait UnifiedModuleSupport<'cdg, BackendModule>: CodeGenBackend<'cdg, BackendModule> {
    /// Emits code for all CIR modules into a single shared module.
    fn process_unified(&self, cir_modules: &[Box<CIRProgramTree>]) -> BackendModule;
}

/// Trait implemented by backends that can compile modules separately and in parallel.
pub trait SeparateModuleSupport<'cdg, BackendModule>: CodeGenBackend<'cdg, BackendModule> {
    /// Emits object files independently, allowing parallel compilation.
    fn process_separately(&self, cir_modules: &[Box<CIRProgramTree>]) -> Vec<BackendModule>;
}
