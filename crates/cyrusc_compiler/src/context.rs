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
    codegen_traits::CodeGenBackend,
    linker::Linker,
    object_file_info::{ObjectFileInfo, collect_objects_file_names},
    options::{CodeGenOptions, LinkerOutputKind, ModuleKind},
    tm_info::TargetMachineInfo,
};
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_diagcentral::exit_with_msg;
use cyrusc_internal::{abi::target::ABITarget, cir::cir::CIRModule};
use cyrusc_tui_utils::{tui_compile_finished, tui_warning};
use inkwell::targets::{Target as LLVMTarget, TargetTriple};
use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

pub struct CodeGenContext {
    pub opts: CodeGenOptions,
    pub target: ABITarget,
    pub llvm_target: LLVMTarget,
    pub llvm_target_triple: TargetTriple,
    pub build_manifest: Arc<Mutex<BuildManifest>>,
    pub master_module_file_path: PathBuf,
    pub linker_output_kind: LinkerOutputKind,
    pub linker: Linker,
}

impl CodeGenContext {
    pub(crate) fn new(
        opts: CodeGenOptions,
        target: ABITarget,
        llvm_target: LLVMTarget,
        llvm_target_triple: TargetTriple,
        build_manifest: Arc<Mutex<BuildManifest>>,
        master_module_file_path: PathBuf,
        linker_output_kind: LinkerOutputKind,
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
            Some(ModuleKind::Separate) => {
                let separate_backend = backend
                    .as_separate()
                    .expect("Backend does not support separate module compilation");

                separate_backend.process_separately(cir_modules)
            }
            Some(ModuleKind::Unified) | None => {
                let unified_backend = backend
                    .as_unified()
                    .expect("Backend does not support unified module compilation");

                vec![unified_backend.process_unified(cir_modules)]
            }
        };

        self.save_cir_modules_source_hash_in_build_manifest(cir_modules);

        tui_compile_finished();
        modules
    }

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
    pub fn target_machine_info<'cdg, BackendModule>(
        &self,
        backend: &'cdg dyn CodeGenBackend<'cdg, BackendModule>,
    ) -> TargetMachineInfo {
        backend.target_machine_info()
    }

    pub fn trigger_linker(&self, object_files: Vec<ObjectFileInfo>, output_path: &PathBuf) -> Result<(), String> {
        let object_files = collect_objects_file_names(object_files);

        match self.linker_output_kind {
            LinkerOutputKind::Executable => {
                let output_path_cow = output_path.to_string_lossy();
                self.linker.link_executable(&object_files, &output_path_cow.to_string())
            }
            LinkerOutputKind::SharedLib => {
                let lib_name = self.opts.canonical_project_name();
                self.linker
                    .link_shared_library(&object_files, &output_path, lib_name)
                    .map(|_| ())
            }
            LinkerOutputKind::StaticLib => {
                let lib_name = self.opts.canonical_project_name();
                self.linker
                    .link_static_library(&object_files, &output_path, lib_name)
                    .map(|_| ())
            }
            LinkerOutputKind::ObjectFile => Ok(()),
        }
    }
}
