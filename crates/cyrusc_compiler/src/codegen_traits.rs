use cyrusc_cir::CIRProgramTree;

use crate::{context::CodeGenContext, object_file_info::ObjectFileInfo, target_machine_info::TargetMachineInfo};

pub trait CodeGenBackend: Send + Sync {
    /// Takes the fully analyzed modules and emits object file for each module.
    fn process_module(&self, ctx: &CodeGenContext, cir_program_tree: &CIRProgramTree) -> ObjectFileInfo;

    /// Returns the target machine info; backend owns the logic
    fn get_target_machine_info(&self) -> TargetMachineInfo;

    /// Human-readable backend name
    fn name(&self) -> &'static str;
}
