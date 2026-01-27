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
use crate::{
    asan::enable_asan_for_owned_module,
    builder::builder::IRBuilderCtx,
    llvm::target_machine::{create_target_machine, llvm_code_model, llvm_opt_level, llvm_reloc_mode},
};
use cyrusc_abi::modulename::make_module_name_from_filepath;
use cyrusc_cir::{CIRProgramTree, monomorph::CIRMonomorphRegistry};
use cyrusc_compiler::{
    codegen_traits::{CodeGenBackend, SeparateModuleSupport, UnifiedModuleSupport},
    context::{CodeGenContext, need_to_be_recompiled},
    object_file_info::ObjectFileInfo,
    options::{CodeGenEndianness, CodeGenOptions},
    tm_info::TargetMachineInfo,
};
use cyrusc_diagcentral::display_single_custom_diag;
use cyrusc_fs_utils::ensure_output_dir;
use cyrusc_scaffold_parser::{LLVM_IR_DIR_PATH, OBJECTS_FILENAME};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{ByteOrdering, FileType, InitializationConfig, Target, TargetData, TargetMachine, TargetTriple},
};
use std::{
    any::Any,
    cell::RefCell,
    path::{Path, PathBuf},
    rc::Rc,
    sync::{Arc, Mutex},
};

mod asan;
mod builder;
mod llvm;

pub struct CodeGenLLVM {
    ctx: Rc<CodeGenContext>,
    opts: CodeGenOptions,
    build_dir: String,
    llvmtm: TargetMachine,
    monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
}

impl CodeGenLLVM {
    pub fn new(
        ctx: Rc<CodeGenContext>,
        opts: CodeGenOptions,
        build_dir: String,
        monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
    ) -> Self {
        let llvmtm = create_target_machine(
            opts.cpu.clone(),
            opts.target_triple.clone(),
            llvm_reloc_mode(opts.reloc_mode.clone()),
            llvm_code_model(opts.code_model.clone()),
            llvm_opt_level(opts.opt_level.unwrap_or(0).try_into().unwrap()),
        );

        Self {
            ctx,
            opts,
            build_dir,
            llvmtm,
            monomorph_registry,
        }
    }

    fn set_endianness<'module>(&self, module: &Module<'module>) {
        if let Some(endianness) = &self.opts.endianness {
            let layout = self
                .llvmtm
                .get_target_data()
                .get_data_layout()
                .as_str()
                .to_str()
                .unwrap()
                .to_string();

            let new_layout_str = match endianness {
                CodeGenEndianness::Little => layout.replacen(&layout[0..1], "e", 1),
                CodeGenEndianness::Big => layout.replacen(&layout[0..1], "E", 1),
            };

            let new_layout = TargetData::create(&new_layout_str);
            module.set_data_layout(&new_layout.get_data_layout());
        }
    }

    fn process_module_with_local_context<'ctx>(
        &self,
        owned_module: &'ctx OwnedModule,
        builder: Rc<Builder<'ctx>>,
        cir_program_tree: &'ctx CIRProgramTree,
        monomorph_registry: Arc<Mutex<CIRMonomorphRegistry>>,
    ) {
        {
            let llvmmodule = owned_module.module.borrow();
            llvmmodule.set_triple(&self.llvmtm.get_triple());
            llvmmodule.set_data_layout(&self.llvmtm.get_target_data().get_data_layout());
            self.set_endianness(&llvmmodule);
            enable_asan_for_owned_module(
                &self.opts,
                owned_module,
                &self.llvmtm,
                self.opts.opt_level.unwrap_or_default(),
            );
        }

        let mut ir_builder_ctx = IRBuilderCtx::new(owned_module, &builder, &self.llvmtm, monomorph_registry);
        ir_builder_ctx.emit_program_tree(cir_program_tree);

        {
            let llvmmodule = owned_module.module.borrow();
            if let Err(err) = llvmmodule.verify() {
                eprintln!("LLVM Module Error: {}", err)
            }
        }
    }

    pub fn save_modules_llvm_ir(&self, owned_modules: &Vec<OwnedModule>, output_path: Option<String>) {
        let llvm_ir_dir = output_path
            .map(PathBuf::from)
            .unwrap_or_else(|| Path::new(&self.build_dir).join(LLVM_IR_DIR_PATH));

        ensure_output_dir(llvm_ir_dir.to_str().unwrap().to_string());

        for owned_module in owned_modules {
            let module = owned_module.module.borrow();
            let mut llvm_ir_path = llvm_ir_dir.join(module.get_name().to_str().unwrap());
            llvm_ir_path.set_extension("ll");

            if let Err(llvm_str) = module.print_to_file(llvm_ir_path) {
                display_single_custom_diag!(llvm_str.to_string());
            }
            drop(module);
        }
    }

    fn make_module_name(&self, file_path: Option<String>) -> Option<String> {
        file_path.clone().map(|file_path| {
            let base_path = self.opts.base_path.as_ref().map(|p| Path::new(p));
            let stdlib_path = self.opts.stdlib_path.as_ref().map(|p| Path::new(p));
            make_module_name_from_filepath(file_path, base_path, stdlib_path)
        })
    }
}

/// An owned llvm module
pub struct OwnedModule {
    pub context: &'static Arc<Context>,
    pub module: Rc<RefCell<Module<'static>>>,
    pub file_path: Option<String>,
}

impl CodeGenBackend<'static, OwnedModule> for CodeGenLLVM {
    fn save_object_file(&self, owned_module: &OwnedModule) -> ObjectFileInfo {
        let module_name = owned_module
            .file_path
            .clone()
            .map(|file_path| {
                let base_path = self.opts.base_path.as_ref().map(|p| Path::new(p));
                let stdlib_path = self.opts.stdlib_path.as_ref().map(|p| Path::new(p));
                make_module_name_from_filepath(file_path, base_path, stdlib_path)
            })
            .unwrap();

        let path = Path::new(&self.build_dir)
            .join(OBJECTS_FILENAME)
            .join(format!("{}.o", module_name));

        if let Some(dir) = path.parent() {
            std::fs::create_dir_all(dir).expect("Failed to create directories for object file");
        }

        let module = owned_module.module.borrow();

        self.llvmtm
            .write_to_file(&module, FileType::Object, &path)
            .expect("Failed to write LLVM object file");

        drop(module);

        ObjectFileInfo {
            path: path.to_str().unwrap().to_string(),
            size: std::fs::metadata(&path)
                .map(|m| m.len())
                .unwrap_or_default()
                .try_into()
                .unwrap(),
        }
    }

    fn target_machine_info(&self) -> TargetMachineInfo {
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
            llvm_opt_level(self.opts.opt_level.unwrap_or(0).try_into().unwrap()),
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

        let native_endianness = match target_machine.get_target_data().get_byte_ordering() {
            ByteOrdering::LittleEndian => "Little".to_string(),
            ByteOrdering::BigEndian => "Big".to_string(),
        };
        let endianness = match &self.opts.endianness {
            Some(codegen_endianness) => match codegen_endianness {
                CodeGenEndianness::Little => "Little".to_string(),
                CodeGenEndianness::Big => "Big".to_string(),
            },
            None => native_endianness,
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
            endianness,
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

    fn as_unified(&self) -> Option<&dyn UnifiedModuleSupport<'static, OwnedModule>> {
        Some(self)
    }

    fn as_separate(&self) -> Option<&dyn SeparateModuleSupport<'static, OwnedModule>> {
        Some(self)
    }
}

impl SeparateModuleSupport<'static, OwnedModule> for CodeGenLLVM {
    fn process_separately(&self, cir_modules: &[Box<CIRProgramTree>]) -> Vec<OwnedModule> {
        let mut modules = Vec::with_capacity(cir_modules.len());

        for cir_program_tree in cir_modules {
            if !need_to_be_recompiled(&self.ctx, cir_program_tree.file_path.clone()) {
                continue; // skip recompilation
            }

            let module_name = self.make_module_name(Some(cir_program_tree.file_path.clone())).unwrap();

            let context = OwnedModule::create_context();
            let mut owned_module = OwnedModule::create_owned_module(context, &module_name);
            owned_module.file_path = Some(cir_program_tree.file_path.clone());

            let builder = owned_module.create_builder();

            self.process_module_with_local_context(
                &owned_module,
                builder,
                cir_program_tree,
                self.monomorph_registry.clone(),
            );

            modules.push(owned_module);
        }

        modules
    }
}

impl UnifiedModuleSupport<'static, OwnedModule> for CodeGenLLVM {
    fn process_unified(&self, cir_modules: &[Box<CIRProgramTree>]) -> OwnedModule {
        let context = OwnedModule::create_context();
        let mut owned_module = OwnedModule::create_owned_module(context, "unified_module");

        for cir_program_tree in cir_modules {
            owned_module.file_path = Some(cir_program_tree.file_path.clone());
            let builder = owned_module.create_builder();
            self.process_module_with_local_context(
                &owned_module,
                builder.clone(),
                cir_program_tree,
                self.monomorph_registry.clone(),
            );
        }

        owned_module
    }
}

impl OwnedModule {
    pub fn create_context() -> &'static Arc<Context> {
        Box::leak(Box::new(Arc::new(Context::create())))
    }

    pub fn create_owned_module(context: &'static Arc<Context>, name: &str) -> Self {
        let module: Rc<RefCell<Module<'_>>> = Rc::new(RefCell::new(context.create_module(name)));
        Self {
            context,
            module,
            file_path: None,
        }
    }

    pub fn create_builder(&self) -> Rc<Builder<'_>> {
        Rc::new(self.context.create_builder())
    }
}
