use cyrusc_compiler::{
    codegen_traits::CodeGenBackend,
    context::CodeGenContext,
    options::{CodeGenOptions, CodeModelOptions, RelocModeOptions},
    target_machine_info::TargetMachineInfo,
};
use cyrusc_diagcentral::display_single_custom_diag;
use inkwell::{
    OptimizationLevel,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
};

#[derive(Debug)]
pub struct CodeGenLLVM {
    opts: CodeGenOptions,
}

impl CodeGenLLVM {
    pub fn new(opts: CodeGenOptions) -> Self {
        Self { opts }
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

    fn get_target_machine_info(&self) -> TargetMachineInfo {
        Target::initialize_all(&InitializationConfig::default());

        let cpu = if let Some(cpu) = &self.opts.cpu {
            cpu.clone()
        } else {
            TargetMachine::get_host_cpu_name().to_string()
        };
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target_triple = if let Some(target_triple_str) = &self.opts.target_triple {
            TargetTriple::create(&target_triple_str)
        } else {
            TargetMachine::get_default_triple()
        };

        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = match target.create_target_machine(
            &target_triple,
            &cpu,
            &features,
            OptimizationLevel::Default,
            llvm_reloc_mode(self.opts.reloc_mode.clone()),
            llvm_code_model(self.opts.code_model.clone()),
        ) {
            Some(target_machine) => target_machine,
            None => {
                display_single_custom_diag!(
                    "Failed to create LLVM Target Machine with given target_triple, cpu, features.".to_string()
                );
            }
        };

        TargetMachineInfo {
            triple: target_machine.get_triple().to_string(),
            cpu_name: target_machine.get_cpu().to_string(),
            data_layout: target_machine
                .get_target_data()
                .get_data_layout()
                .as_str()
                .to_str()
                .unwrap()
                .to_string(),
            pointer_size_bits: target_machine.get_target_data().get_pointer_byte_size(None),
            opt_level: self
                .opts
                .opt_level
                .and_then(|opt| Some(format!("O{}", opt)))
                .unwrap_or("Default".to_string()),
            reloc_mode: self.opts.reloc_mode.to_string(),
            code_model: self.opts.code_model.to_string(),
            link_static: self.opts.linker_options.link_static,
            pie: self.opts.linker_options.pie,
        }
    }

    fn name(&self) -> &'static str {
        "llvm"
    }
}

fn llvm_reloc_mode(reloc_mode: RelocModeOptions) -> RelocMode {
    match reloc_mode {
        RelocModeOptions::Default => RelocMode::Default,
        RelocModeOptions::Static => RelocMode::Static,
        RelocModeOptions::PIC => RelocMode::PIC,
        RelocModeOptions::DynamicNoPic => RelocMode::DynamicNoPic,
    }
}

fn llvm_code_model(code_model: CodeModelOptions) -> CodeModel {
    match code_model {
        CodeModelOptions::Default => CodeModel::Default,
        CodeModelOptions::Tiny => CodeModel::Default,
        CodeModelOptions::Small => CodeModel::Small,
        CodeModelOptions::Kernel => CodeModel::Kernel,
        CodeModelOptions::Medium => CodeModel::Medium,
        CodeModelOptions::Large => CodeModel::Large,
    }
}
