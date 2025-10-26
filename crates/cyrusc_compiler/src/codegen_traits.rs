use crate::{context::CodeGenContext, target_machine_info::TargetMachineInfo};

pub trait CodeGenBackend: Send + Sync {
    /// Takes the fully analyzed module and emits its object file(s)
    fn process_module(&self, ctx: &CodeGenContext, module_name: &str);

    /// Returns the target machine info; backend owns the logic
    fn get_target_machine_info(&self, ctx: &CodeGenContext) -> TargetMachineInfo;

    /// Human-readable backend name
    fn name(&self) -> &'static str;
}
