use crate::{
    codegen_traits::CodeGenBackend,
    linker::Linker,
    object_file_info::ObjectFileInfo,
    options::{CodeGenOptions, LinkerOutputKind},
    target_machine_info::TargetMachineInfo,
};
use cyrusc_buildmanifest::BuildManifest;
use std::{
    path::PathBuf,
    sync::{Arc, Mutex},
};

pub struct CodeGenContext {
    pub opts: Box<CodeGenOptions>,
    pub build_manifest: Arc<Mutex<BuildManifest>>,
    pub compiled_objects: Arc<Mutex<Vec<ObjectFileInfo>>>,
    pub master_module_file_path: String,
    pub linker_output_kind: LinkerOutputKind,
    pub backend: Arc<dyn CodeGenBackend + Send + Sync>,
    pub linker: Linker,
}

impl CodeGenContext {
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
