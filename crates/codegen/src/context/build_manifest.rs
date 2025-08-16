use crate::diagnostics::CodeGenDiagKind;
use diagcentral::{Diag, DiagLevel, display_single_diag};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::Path,
};

const SOURCES_DIR_PATH: &str = "sources";
const OBJECTS_FILENAME: &str = "obj";
const MANIFEST_FILENAME: &str = "manifest.json";
const OUTPUT_FILENAME: &str = "output";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildManifest {
    // The directory where the build-manifest and other artifacts are stored.
    pub build_dir: String,
    pub sources: HashMap<String, String>,
    pub objects: HashMap<String, String>,
}

impl BuildManifest {
    pub fn new(build_dir: String) -> Self {
        Self {
            build_dir,
            sources: HashMap::new(),
            objects: HashMap::new(),
        }
    }
}

impl BuildManifest {
    pub fn check_source_code_changed(&self, input_source_file_path: String) -> bool {
        match self
            .sources
            .iter()
            .find(|(source_file_path, _)| **source_file_path == input_source_file_path)
        {
            Some((source_file)) => {
                todo!();
            },
            None => true,
        }
    }

    pub fn save_manifest(&self) {
        let manifest_filepath = format!("{}/{}", self.build_dir, MANIFEST_FILENAME);

        if Path::new(&manifest_filepath).exists() {
            fs::remove_file(manifest_filepath.clone()).unwrap();
        }
        let mut file = File::create(manifest_filepath).unwrap();
        file.write(serde_json::to_string(&self).unwrap().as_bytes()).unwrap();
    }

    pub fn read_manifest(&self) -> Option<Self> {
        let manifest_filepath = format!("{}/{}", self.build_dir, MANIFEST_FILENAME);
        let file_content = std::fs::read_to_string(manifest_filepath.to_string()).ok()?;

        match serde_json::from_str::<BuildManifest>(&file_content) {
            Ok(manifest) => Some(manifest),
            Err(err) => {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: CodeGenDiagKind::FailedToParseBuildManifest {
                        file_path: manifest_filepath,
                        err: err.to_string()
                    },
                    location: None,
                    hint: None
                });
            }
        }
    }
}
