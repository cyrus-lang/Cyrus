// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    asan::enable_asan_for_owned_module,
    builder::builder::CodeGenIRBuilder,
    llvm::{
        debug_info::{DebugContext, emit_debug_module_flags, finalize_debug},
        target_machine::{create_target_machine, llvm_code_model, llvm_opt_level, llvm_reloc_mode},
    },
    optimizer::optimize_module_with_custom_passes,
};
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_compiler::{
    codegen_traits::{CodeGenBackend, SeparateModuleSupport, UnifiedModuleSupport},
    context::CodeGenContext,
    object_file_info::ObjectFileInfo,
    target_machine_info::TargetMachineInfo,
};
use cyrusc_diagcentral::exit_with_msg;
use cyrusc_internal::{
    cir::{cir::CIRModule, typectx::CIRTypeContext},
    compiler_options::CompilerOptions,
    vtable::VTableRegistry,
};
use cyrusc_scaffold_parser::OBJECT_CACHE_DIR_FILENAME;
use cyrusc_source_loc::SourceMap;
use cyrusc_tui_utils::{tui_compiled, tui_skipped};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{ByteOrdering, FileType, InitializationConfig, Target as InkwellTarget, TargetMachine, TargetTriple},
};
use std::{
    cell::RefCell,
    path::{Path, PathBuf},
    rc::Rc,
    sync::{Arc, Mutex},
};

mod asan;
mod builder;
mod llvm;
mod optimizer;

pub struct CodeGenLLVM {
    ctx: Rc<CodeGenContext>,
    opts: CompilerOptions,
    build_dir: PathBuf,
    llvm_target_machine: TargetMachine,
    build_manifest: Arc<Mutex<BuildManifest>>,
    entry_module_file_path: PathBuf,
    source_map: Arc<SourceMap>,
}

impl CodeGenLLVM {
    pub fn new(
        ctx: Rc<CodeGenContext>,
        target: &InkwellTarget,
        target_triple: &TargetTriple,
        opts: CompilerOptions,
        build_dir: PathBuf,
        build_manifest: Arc<Mutex<BuildManifest>>,
        entry_module_file_path: PathBuf,
        source_map: Arc<SourceMap>,
    ) -> Self {
        let llvm_target_machine = create_target_machine(
            target,
            target_triple,
            opts.cpu.clone(),
            llvm_reloc_mode(opts.reloc_mode.clone()),
            llvm_code_model(opts.code_model.clone()),
            llvm_opt_level(opts.opt_level.unwrap_or_default()),
        );

        Self {
            ctx,
            opts,
            build_dir,
            llvm_target_machine,
            build_manifest,
            entry_module_file_path,
            source_map,
        }
    }

    fn process_module_with_local_context<'ctx>(
        &self,
        owned_module: &'ctx OwnedModule,
        builder: Rc<Builder<'ctx>>,
        cir_module: &'ctx CIRModule,
        dctx: DebugContext,
        tctx: Arc<CIRTypeContext>,
        vtable_registry: Arc<VTableRegistry>,
        source_map: Arc<SourceMap>,
    ) {
        let dctx = if self.opts.debuginfo_enabled { Some(dctx) } else { None };

        {
            let llvm_module = owned_module.module.borrow();
            llvm_module.set_triple(&self.llvm_target_machine.get_triple());
            llvm_module.set_data_layout(&self.llvm_target_machine.get_target_data().get_data_layout());

            enable_asan_for_owned_module(&self.opts, owned_module, &self.llvm_target_machine);
        }

        let mut codegen_ir_builder = CodeGenIRBuilder::new(
            owned_module,
            cir_module,
            &self.ctx.target,
            &builder,
            &self.llvm_target_machine,
            dctx.clone(),
            tctx,
            vtable_registry,
            source_map,
            self.opts.profile.clone(),
        );

        codegen_ir_builder.emit_module();

        // run optimizer
        {
            let llvm_module = owned_module.module.borrow();

            let opt_level = self.opts.opt_level.unwrap_or_default();

            optimize_module_with_custom_passes(&llvm_module, opt_level).unwrap();
        }

        if let Some(dctx) = &dctx {
            unsafe { finalize_debug(&dctx) };
            let llvm_module = owned_module.module.borrow();

            unsafe { emit_debug_module_flags(llvm_module.as_mut_ptr()) };

            if let Err(err) = llvm_module.verify() {
                eprintln!("LLVM Module Error: {}", err)
            }
        }

        if !self.opts.quiet {
            tui_compiled(cir_module.file_path.clone());
        }
    }

    pub fn save_modules_as_llvm_ir(&self, owned_modules: &Vec<OwnedModule>, llvm_ir_dir_path: &PathBuf) {
        for owned_module in owned_modules {
            let module = owned_module.module.borrow();
            let mut llvm_ir_path = llvm_ir_dir_path.join(module.get_name().to_str().unwrap());
            llvm_ir_path.set_extension("ll");

            if let Err(llvm_str) = module.print_to_file(llvm_ir_path) {
                exit_with_msg!(llvm_str.to_string());
            }
            drop(module);
        }
    }

    pub fn save_modules_as_bitcode(&self, owned_modules: &Vec<OwnedModule>, bitcode_dir_path: &PathBuf) {
        for owned_module in owned_modules {
            let module = owned_module.module.borrow();
            let mut bitcode_path = bitcode_dir_path.join(module.get_name().to_str().unwrap());
            bitcode_path.set_extension("bc");

            module.write_bitcode_to_path(bitcode_path);
            drop(module);
        }
    }

    pub fn save_modules_as_assembly(&self, owned_modules: &Vec<OwnedModule>, assembly_dir_path: &PathBuf) {
        for owned_module in owned_modules {
            let module = owned_module.module.borrow();
            let mut assembly_path = assembly_dir_path.join(module.get_name().to_str().unwrap());
            assembly_path.set_extension("asm");

            if let Err(llvm_str) = self
                .llvm_target_machine
                .write_to_file(&module, FileType::Assembly, &assembly_path)
            {
                exit_with_msg!(llvm_str.to_string());
            }
            drop(module);
        }
    }

    pub fn save_modules_as_object(&self, owned_modules: &Vec<OwnedModule>, object_dir_path: &PathBuf) {
        for owned_module in owned_modules {
            let module = owned_module.module.borrow();
            let mut object_path = object_dir_path.join(module.get_name().to_str().unwrap());
            object_path.set_extension("o");

            if let Err(llvm_str) = self
                .llvm_target_machine
                .write_to_file(&module, FileType::Object, &object_path)
            {
                exit_with_msg!(llvm_str.to_string());
            }
            drop(module);
        }
    }

    fn store_object_file_cache(&self, module_name: &String, object_path: &PathBuf) {
        {
            let mut build_manifest = self.build_manifest.lock().unwrap();
            if let Err(err) = build_manifest.insert_object(module_name, object_path) {
                exit_with_msg!(err.to_string());
            }
        }
    }

    fn load_object_file_cache(&self, object_name: &String) -> Option<PathBuf> {
        {
            let build_manifest = self.build_manifest.lock().unwrap();
            build_manifest.get_object(object_name).cloned()
        }
    }
}

/// An owned llvm module
pub struct OwnedModule {
    pub context: &'static Arc<Context>,
    pub module: Rc<RefCell<Module<'static>>>,
    pub module_name: String,
    pub module_file_path: PathBuf,
    pub module_merge_mode_is_unified: bool,
}

impl CodeGenBackend<'static, OwnedModule> for CodeGenLLVM {
    fn save_object_file(&self, owned_module: &OwnedModule) -> ObjectFileInfo {
        let object_file_size = |object_path: &PathBuf| -> usize {
            std::fs::metadata(&object_path)
                .map(|m| m.len())
                .unwrap_or_default()
                .try_into()
                .unwrap()
        };

        // use obj-cache
        if !owned_module.module_merge_mode_is_unified
            && !need_to_be_recompiled(&self.ctx, &owned_module.module_file_path)
        {
            if let Some(cached_object_path) = self.load_object_file_cache(&owned_module.module_name) {
                return ObjectFileInfo::new(cached_object_path.clone(), object_file_size(&cached_object_path));
            }
        }

        let object_path = Path::new(&self.build_dir)
            .join(OBJECT_CACHE_DIR_FILENAME)
            .join(format!("{}.o", owned_module.module_name.clone()));

        if let Some(dir) = object_path.parent() {
            std::fs::create_dir_all(dir).expect("Failed to create directories for object file");
        }

        {
            let module = owned_module.module.borrow();

            self.llvm_target_machine
                .write_to_file(&module, FileType::Object, &object_path)
                .expect("Failed to write LLVM object file")
        }

        self.store_object_file_cache(&owned_module.module_name.clone(), &object_path);

        ObjectFileInfo::new(object_path.clone(), object_file_size(&object_path))
    }

    fn target_machine_info(&self) -> TargetMachineInfo {
        InkwellTarget::initialize_all(&InitializationConfig::default());

        let cpu = if let Some(cpu) = &self.opts.cpu {
            cpu.clone()
        } else {
            TargetMachine::get_host_cpu_name().to_string()
        };
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target_triple = self.llvm_target_machine.get_triple();

        let target = InkwellTarget::from_triple(&target_triple).unwrap();
        let target_machine = match target.create_target_machine(
            &target_triple,
            &cpu,
            &features,
            llvm_opt_level(self.opts.opt_level.unwrap_or_default()),
            llvm_reloc_mode(self.opts.reloc_mode.clone()),
            llvm_code_model(self.opts.code_model.clone()),
        ) {
            Some(target_machine) => target_machine,
            None => {
                exit_with_msg!(
                    "Failed to create LLVM Target Machine with given target_triple, cpu, features.".to_string()
                );
            }
        };

        let endianness = match target_machine.get_target_data().get_byte_ordering() {
            ByteOrdering::LittleEndian => "Little".to_string(),
            ByteOrdering::BigEndian => "Big".to_string(),
        };

        {
            let data_layout = target_machine
                .get_target_data()
                .get_data_layout()
                .as_str()
                .to_str()
                .unwrap()
                .to_string();

            let pointer_size_bits = target_machine.get_target_data().get_pointer_byte_size(None);

            let opt_level = self
                .opts
                .opt_level
                .and_then(|opt| Some(opt.to_string()))
                .unwrap_or("Default".to_string());

            TargetMachineInfo {
                triple: target_machine.get_triple().to_string(),
                cpu_name: target_machine.get_cpu().to_string(),
                reloc_mode: self.opts.reloc_mode.to_string(),
                code_model: self.opts.code_model.to_string(),
                link_static: self.opts.linker_options.link_static,
                pie: self.opts.linker_options.pie,
                data_layout,
                endianness,
                pointer_size_bits,
                opt_level,
            }
        }
    }

    fn name(&self) -> &'static str {
        "codegen_llvm"
    }

    fn as_unified(&self) -> Option<&dyn UnifiedModuleSupport<'static, OwnedModule>> {
        Some(self)
    }

    fn as_separate(&self) -> Option<&dyn SeparateModuleSupport<'static, OwnedModule>> {
        Some(self)
    }
}

impl SeparateModuleSupport<'static, OwnedModule> for CodeGenLLVM {
    fn process_separately(&self, cir_modules: &[Box<CIRModule>]) -> Vec<OwnedModule> {
        let mut modules = Vec::with_capacity(cir_modules.len());

        for cir_module in cir_modules {
            let context = OwnedModule::create_context();
            let owned_module =
                OwnedModule::create_owned_module(context, &cir_module.file_path, &cir_module.module_name, false);

            let recompile_forced = need_to_be_recompiled(&self.ctx, &Path::new(&cir_module.file_path).to_path_buf());

            // skip emit module if recompilation is not forced
            if !recompile_forced {
                tui_skipped(cir_module.file_path.clone());
                modules.push(owned_module);
                continue;
            }

            let dctx = {
                let llvmmodule_ref = owned_module.module.borrow().as_mut_ptr();
                unsafe { DebugContext::new(llvmmodule_ref, owned_module.module_file_path.to_str().unwrap(), ".") }
            };

            // emit llvm-ir
            let builder = owned_module.create_builder();
            self.process_module_with_local_context(
                &owned_module,
                builder,
                cir_module,
                dctx,
                self.ctx.tctx.clone(),
                cir_module.vtable_registry.clone(),
                self.source_map.clone(),
            );

            modules.push(owned_module);
        }

        modules
    }
}

impl UnifiedModuleSupport<'static, OwnedModule> for CodeGenLLVM {
    fn process_unified(&self, cir_modules: &[Box<CIRModule>]) -> OwnedModule {
        let context = OwnedModule::create_context();
        let owned_module = OwnedModule::create_owned_module(context, &self.entry_module_file_path, "module", true);

        for cir_module in cir_modules {
            let dctx = {
                let llvmmodule_ref = owned_module.module.borrow().as_mut_ptr();
                unsafe { DebugContext::new(llvmmodule_ref, &cir_module.file_path, ".") }
            };

            let builder = owned_module.create_builder();

            self.process_module_with_local_context(
                &owned_module,
                builder.clone(),
                cir_module,
                dctx,
                self.ctx.tctx.clone(),
                cir_module.vtable_registry.clone(),
                self.source_map.clone(),
            );
        }

        owned_module
    }
}

impl OwnedModule {
    pub fn create_context() -> &'static Arc<Context> {
        Box::leak(Box::new(Arc::new(Context::create())))
    }

    pub fn create_owned_module<P: AsRef<Path> + ?Sized>(
        context: &'static Arc<Context>,
        file_path: &P,
        name: &str,
        module_merge_mode_is_unified: bool,
    ) -> Self {
        let module: Rc<RefCell<Module<'_>>> = Rc::new(RefCell::new(context.create_module(name)));

        Self {
            context,
            module,
            module_name: name.to_string(),
            module_file_path: file_path.as_ref().to_path_buf(),
            module_merge_mode_is_unified,
        }
    }

    pub fn create_builder(&self) -> Rc<Builder<'_>> {
        Rc::new(self.context.create_builder())
    }
}

/// Decides whether to recompile a module.
fn need_to_be_recompiled(ctx: &CodeGenContext, module_file_path: &PathBuf) -> bool {
    let build_manifest = ctx.build_manifest.lock().unwrap();
    let is_source_changed = build_manifest.is_source_changed(&module_file_path.clone()).unwrap();

    is_source_changed || ctx.opts.disable_modulefs_cache || build_manifest.initial_build
}
