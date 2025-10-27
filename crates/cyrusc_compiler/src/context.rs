use crate::{
    codegen_traits::CodeGenBackend,
    linker::Linker,
    object_file_info::ObjectFileInfo,
    options::{CodeGenOptions, LinkerOutputKind},
    target_machine_info::TargetMachineInfo,
};
use cyrusc_buildmanifest::BuildManifest;
use cyrusc_cir::CIRModule;
use rayon::{
    ThreadPoolBuilder,
    iter::{IntoParallelRefIterator, ParallelIterator},
};
use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

pub struct CodeGenContext {
    pub opts: Box<CodeGenOptions>,
    pub build_manifest: Arc<Mutex<BuildManifest>>,
    pub master_module_file_path: String,
    pub linker_output_kind: LinkerOutputKind,
    pub backend: Arc<dyn CodeGenBackend + Send + Sync>,
    pub linker: Linker,
}

impl CodeGenContext {
    pub fn new(
        opts: Box<CodeGenOptions>,
        build_manifest: Arc<Mutex<BuildManifest>>,
        master_module_file_path: String,
        linker_output_kind: LinkerOutputKind,
        backend: Arc<dyn CodeGenBackend + Send + Sync>,
        linker: Linker,
    ) -> Self {
        Self {
            opts,
            build_manifest,
            master_module_file_path,
            linker_output_kind,
            backend,
            linker,
        }
    }

    /// Orchestrates compilation and returns collected objects.
    pub fn compile(&self, cir_modules: &Vec<CIRModule>) -> Vec<ObjectFileInfo> {
        self.compile_modules_in_parallel(cir_modules)
    }

    fn compile_modules_in_parallel(&self, cir_modules: &Vec<CIRModule>) -> Vec<ObjectFileInfo> {
        let threads = std::cmp::min(num_cpus::get_physical(), self.opts.jobs.unwrap_or(num_cpus::get()));

        let pool = ThreadPoolBuilder::new()
            .num_threads(threads)
            .build()
            .expect("Failed to build thread pool.");

        pool.install(|| {
            cir_modules
                .par_iter()
                .filter(|m| need_to_be_recompiled(self, m.file_path.clone()))
                .map(|m| self.backend.process_module(self, m))
                .collect()
        })
    }

    pub fn trigger_linker(&self, object_files: Vec<String>, output_path: String) -> Result<(), String> {
        match self.linker_output_kind {
            LinkerOutputKind::Executable => self.linker.link_executable(&object_files, &output_path),
            LinkerOutputKind::SharedLib => {
                let output_dir = PathBuf::from(&output_path);
                let lib_name = self.opts.canonical_project_name();
                self.linker
                    .link_shared_library(&object_files, &output_dir, lib_name)
                    .map(|_| ())
            }
            LinkerOutputKind::StaticLib => {
                let output_dir = PathBuf::from(&output_path);
                let lib_name = self.opts.canonical_project_name();
                self.linker
                    .link_static_library(&object_files, &output_dir, lib_name)
                    .map(|_| ())
            }
            LinkerOutputKind::ObjectFile => Ok(()),
        }
    }

    pub fn target_machine_info(&self) -> TargetMachineInfo {
        self.backend.get_target_machine_info(self)
    }
}

/// Decides whether to recompile a module.
fn need_to_be_recompiled(ctx: &CodeGenContext, module_file_path: String) -> bool {
    let build_manifest = ctx.build_manifest.lock().unwrap();
    let is_source_changed = build_manifest.is_source_changed(module_file_path.clone()).unwrap();

    is_source_changed || ctx.opts.disable_modulefs_cache || build_manifest.is_first_build
}
