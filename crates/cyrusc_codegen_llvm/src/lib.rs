use cyrusc_compiler::codegen_traits::CodeGenBackend;

#[derive(Debug)]
pub struct CodeGenLLVM {}

impl CodeGenLLVM {
    pub fn new() -> Self {
        Self {}
    }
}

impl CodeGenBackend for CodeGenLLVM {
    fn process_module(
        &self,
        ctx: &cyrusc_compiler::context::CodeGenContext,
        cir_module: &cyrusc_cir::CIRProgramTree,
    ) -> cyrusc_compiler::object_file_info::ObjectFileInfo {
        todo!()
    }

    fn get_target_machine_info(
        &self,
        ctx: &cyrusc_compiler::context::CodeGenContext,
    ) -> cyrusc_compiler::target_machine_info::TargetMachineInfo {
        todo!()
    }

    fn name(&self) -> &'static str {
        "llvm"
    }
}
