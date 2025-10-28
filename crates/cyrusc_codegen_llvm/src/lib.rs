use cyrusc_cir::CIRProgramTree;
use cyrusc_compiler::{
    codegen_traits::{CodeGenBackend, SeparateModuleSupport, UnifiedModuleSupport},
    object_file_info::ObjectFileInfo,
    options::{CodeGenOptions, CodeModelOptions, RelocModeOptions},
    target_machine_info::TargetMachineInfo,
};
use cyrusc_diagcentral::display_single_custom_diag;
use cyrusc_sema::monomorph::MonomorphRegistry;
use inkwell::{
    OptimizationLevel,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
};
use std::{
    any::Any,
    sync::{Arc, Mutex, RwLock},
};

pub struct CodeGenLLVM {
    opts: CodeGenOptions,
    monomorph_registry: Arc<Mutex<MonomorphRegistry>>,
}

impl CodeGenLLVM {
    pub fn new(opts: CodeGenOptions, monomorph_registry: Arc<Mutex<MonomorphRegistry>>) -> Self {
        Self {
            opts,
            monomorph_registry,
        }
    }

    fn process_module_with_local_context(
        &self,
        cir_module: &CIRProgramTree,
        context: &Context,
        builder: &Builder,
        module: &Arc<RwLock<Module>>,
    ) {
        // Generate LLVM IR here safely
        todo!()
    }
}

/// An owned llvm module
pub struct OwnedModule {
    pub module: Arc<RwLock<Module<'static>>>,
}

impl<'ctx> CodeGenBackend<OwnedModule> for CodeGenLLVM {
    fn save_object_file(&self, module: &OwnedModule) -> ObjectFileInfo {
        
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

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_unified(&self) -> Option<&dyn UnifiedModuleSupport<OwnedModule>> {
        Some(self)
    }

    fn as_separate(&self) -> Option<&dyn SeparateModuleSupport<OwnedModule>> {
        Some(self)
    }
}

impl SeparateModuleSupport<OwnedModule> for CodeGenLLVM {
    fn process_separately(&self, cir_modules: &[Box<CIRProgramTree>]) -> Vec<OwnedModule> {
        let mut modules = Vec::with_capacity(cir_modules.len());

        for program_tree in cir_modules {
            let context = Context::create();
            let builder = context.create_builder();

            let module = unsafe {
                Arc::new(RwLock::new(std::mem::transmute::<Module<'_>, Module<'static>>(
                    context.create_module("module"),
                )))
            };

            self.process_module_with_local_context(program_tree, &context, &builder, &module);

            modules.push(OwnedModule { module });
        }

        modules
    }
}

impl UnifiedModuleSupport<OwnedModule> for CodeGenLLVM {
    fn process_unified(&self, cir_modules: &[Box<CIRProgramTree>]) -> OwnedModule {
        let context = Context::create();
        let builder = context.create_builder();

        // create module and immediately erase its lifetime
        let module = unsafe {
            Arc::new(RwLock::new(std::mem::transmute::<Module<'_>, Module<'static>>(
                context.create_module("unified_module"),
            )))
        };

        for program_tree in cir_modules {
            self.process_module_with_local_context(program_tree, &context, &builder, &module);
        }

        OwnedModule { module }
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
