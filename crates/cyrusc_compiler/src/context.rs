// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use crate::{
    codegen_traits::CodeGenBackend,
    linker::Linker,
    object_file_info::{ObjectFileInfo, get_objects_file_names},
    options::{CodeGenOptions, LinkerOutputKind, ModuleKind},
    tm_info::TargetMachineInfo,
};
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_cir::CIRProgramTree;
use cyrusc_tui_utils::tui_compile_finished;
use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

pub struct CodeGenContext {
    pub opts: CodeGenOptions,
    pub build_manifest: Arc<Mutex<BuildManifest>>,
    pub master_module_file_path: String,
    pub linker_output_kind: LinkerOutputKind,
    pub linker: Linker,
}

impl CodeGenContext {
    pub(crate) fn new(
        opts: CodeGenOptions,
        build_manifest: Arc<Mutex<BuildManifest>>,
        master_module_file_path: String,
        linker_output_kind: LinkerOutputKind,
        linker: Linker,
    ) -> Self {
        Self {
            opts,
            build_manifest,
            master_module_file_path,
            linker_output_kind,
            linker,
        }
    }

    /// Orchestrates compilation and returns collected objects.
    pub fn compile<'cdg, B, M>(&self, backend: &'cdg B, cir_modules: &[Box<CIRProgramTree>]) -> Vec<M>
    where
        B: CodeGenBackend<'cdg, M>,
        M: 'cdg,
    {
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

        tui_compile_finished();
        modules
    }

    /// Fetch the target machine info from the backend
    pub fn target_machine_info<'cdg, BackendModule>(
        &self,
        backend: &'cdg dyn CodeGenBackend<'cdg, BackendModule>,
    ) -> TargetMachineInfo {
        backend.get_target_machine_info()
    }

    pub fn trigger_linker(&self, object_files: Vec<ObjectFileInfo>, output_path: String) -> Result<(), String> {
        match self.linker_output_kind {
            LinkerOutputKind::Executable => self
                .linker
                .link_executable(&get_objects_file_names(object_files), &output_path),
            LinkerOutputKind::SharedLib => {
                let output_dir = PathBuf::from(&output_path);
                let lib_name = self.opts.canonical_project_name();
                self.linker
                    .link_shared_library(&get_objects_file_names(object_files), &output_dir, lib_name)
                    .map(|_| ())
            }
            LinkerOutputKind::StaticLib => {
                let output_dir = PathBuf::from(&output_path);
                let lib_name = self.opts.canonical_project_name();
                self.linker
                    .link_static_library(&get_objects_file_names(object_files), &output_dir, lib_name)
                    .map(|_| ())
            }
            LinkerOutputKind::ObjectFile => Ok(()),
        }
    }
}

/// Decides whether to recompile a module.
pub fn need_to_be_recompiled(ctx: &CodeGenContext, module_file_path: String) -> bool {
    let build_manifest = ctx.build_manifest.lock().unwrap();
    let is_source_changed = build_manifest.is_source_changed(module_file_path.clone()).unwrap();

    is_source_changed || ctx.opts.disable_modulefs_cache || build_manifest.is_first_build
}
