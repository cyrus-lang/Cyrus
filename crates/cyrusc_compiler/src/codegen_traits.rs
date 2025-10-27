use crate::{context::CodeGenContext, object_file_info::ObjectFileInfo, target_machine_info::TargetMachineInfo};
use cyrusc_cir::CIRModule;

pub trait CodeGenBackend: Send + Sync {
    /// Takes the fully analyzed modules and emits object file for each module.
    fn process_module(&self, ctx: &CodeGenContext, cir_module: &CIRModule) -> ObjectFileInfo;

    /// Returns the target machine info; backend owns the logic
    fn get_target_machine_info(&self, ctx: &CodeGenContext) -> TargetMachineInfo;

    /// Human-readable backend name
    fn name(&self) -> &'static str;
}
