use crate::{object_file_info::ObjectFileInfo, target_machine_info::TargetMachineInfo};
use cyrusc_cir::CIRProgramTree;
use std::any::Any;

/// The base trait every codegen backend must implement.
pub trait CodeGenBackend<BackendModule> {
    fn save_object_file(&self, module: &BackendModule) -> ObjectFileInfo;

    /// Returns the target machine info; backend owns the logic
    fn get_target_machine_info(&self) -> TargetMachineInfo;

    /// Human-readable backend name
    fn name(&self) -> &'static str;

    fn as_any(&self) -> &dyn Any;

    fn as_unified(&self) -> Option<&dyn UnifiedModuleSupport<BackendModule>> {
        None
    }

    fn as_separate(&self) -> Option<&dyn SeparateModuleSupport<BackendModule>> {
        None
    }
}

/// Trait implemented by backends that can merge all modules into one unified LLVM module,
/// compiling serially (e.g., for link-time optimization or reduced cross-module overhead).
pub trait UnifiedModuleSupport<BackendModule>: CodeGenBackend<BackendModule> {
    /// Emits code for all CIR modules into a single shared module.
    fn process_unified(&self, cir_modules: &[Box<CIRProgramTree>]) -> BackendModule;
}

/// Trait implemented by backends that can compile modules separately and in parallel.
pub trait SeparateModuleSupport<BackendModule>: CodeGenBackend<BackendModule> {
    /// Emits object files independently, allowing parallel compilation.
    fn process_separately(&self, cir_modules: &[Box<CIRProgramTree>]) -> Vec<BackendModule>;
}
