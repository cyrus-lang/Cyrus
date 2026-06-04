// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{
    codegen_traits::CodeGenBackend, linker::Linker, object_file_info::{ObjectFileInfo, collect_objects_file_names}, options::canonical_project_name, target_machine_info::TargetMachineInfo
};
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_diagcentral::exit_with_msg;
use cyrusc_internal::{abi::target::ABITarget, cir::cir::CIRModule, compiler_options::{CompilerOption_LinkerOutputKind, CompilerOption_ModuleKind, CompilerOptions}};
use cyrusc_tui_utils::{tui_compile_finished, tui_warning};
use inkwell::targets::{Target as LLVMTarget, TargetTriple};
use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

pub struct CodeGenContext {
    pub opts: CompilerOptions,

    pub target: Arc<ABITarget>,

    pub build_manifest: Arc<Mutex<BuildManifest>>,

    pub master_module_file_path: PathBuf,

    pub linker_output_kind: CompilerOption_LinkerOutputKind,
    pub linker: Linker,

    pub llvm_target: LLVMTarget,
    pub llvm_target_triple: TargetTriple,
}

impl CodeGenContext {
    pub(crate) fn new(
        opts: CompilerOptions,
        target: Arc<ABITarget>,
        llvm_target: LLVMTarget,
        llvm_target_triple: TargetTriple,
        build_manifest: Arc<Mutex<BuildManifest>>,
        master_module_file_path: PathBuf,
        linker_output_kind: CompilerOption_LinkerOutputKind,
        linker: Linker,
    ) -> Self {
        Self {
            opts,
            target,
            llvm_target,
            llvm_target_triple,
            build_manifest,
            master_module_file_path,
            linker_output_kind,
            linker,
        }
    }

    /// Orchestrates compilation and returns collected objects.
    pub fn compile<'cdg, B, M>(&self, backend: &'cdg B, cir_modules: &mut Vec<Box<CIRModule>>) -> Vec<M>
    where
        B: CodeGenBackend<'cdg, M>,
        M: 'cdg,
    {
        if self.opts.display_target_machine {
            println!("{}", self.target_machine_info(backend));
        }

        let modules = match self.opts.module_kind {
            Some(CompilerOption_ModuleKind::Separate) => {
                let separate_backend = backend
                    .as_separate()
                    .expect("backend does not support separate module compilation");

                separate_backend.process_separately(cir_modules)
            }
            Some(CompilerOption_ModuleKind::Unified) | None => {
                let unified_backend = backend
                    .as_unified()
                    .expect("backend does not support unified module compilation");

                vec![unified_backend.process_unified(cir_modules)]
            }
        };

        self.save_cir_modules_source_hash_in_build_manifest(cir_modules);

        tui_compile_finished();
        modules
    }

    #[inline]
    pub fn save_context_build_cache(&self) {
        let build_manifest = self.build_manifest.lock().unwrap();

        if let Err(err) = build_manifest.save_manifest() {
            exit_with_msg!(err.to_string());
        }

        drop(build_manifest);
    }

    fn save_cir_modules_source_hash_in_build_manifest(&self, cir_modules: &[Box<CIRModule>]) {
        {
            let mut build_manifest = self.build_manifest.lock().unwrap();

            for module in cir_modules {
                let source_hash = match build_manifest.hash_source_code(&module.file_path) {
                    Ok(source_hash) => source_hash,
                    Err(err) => {
                        tui_warning(format!(
                            "Couldn't hash source code '{}' for this unknown reason: {}",
                            module.file_path,
                            err.to_string()
                        ));
                        continue;
                    }
                };

                if let Err(err) = build_manifest.update_source_hash(&module.file_path, &source_hash) {
                    tui_warning(format!(
                        "Couldn't save source code hash '{}' for this unknown reason: {}",
                        module.file_path,
                        err.to_string()
                    ));
                    continue;
                }
            }
        }
    }

    /// Fetch the target machine info from the backend
    #[inline]
    pub fn target_machine_info<'cdg, BackendModule>(
        &self,
        backend: &'cdg dyn CodeGenBackend<'cdg, BackendModule>,
    ) -> TargetMachineInfo {
        backend.target_machine_info()
    }

    pub fn trigger_linker(&self, object_files: Vec<ObjectFileInfo>, output_path: &PathBuf) -> Result<(), String> {
        let object_files = collect_objects_file_names(object_files);

        match self.linker_output_kind {
            CompilerOption_LinkerOutputKind::Executable => {
                let output_path_cow = output_path.to_string_lossy();

                self.linker.link_executable(&object_files, &output_path_cow.to_string())
            }
            CompilerOption_LinkerOutputKind::SharedLib => {
                let lib_name = canonical_project_name(&self.opts);

                self.linker
                    .link_shared_library(&object_files, &output_path, lib_name)
                    .map(|_| ())
            }
            CompilerOption_LinkerOutputKind::StaticLib => {
                let lib_name = canonical_project_name(&self.opts);

                self.linker
                    .link_static_library(&object_files, &output_path, lib_name)
                    .map(|_| ())
            }
            CompilerOption_LinkerOutputKind::ObjectFile => Ok(()),
        }
    }
}
