use crate::diagnostics::CodeGenDiagKind;
use diagcentral::{Diag, DiagLevel, display_single_diag};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
};

const SOURCES_DIR_PATH: &str = "sources";
const OBJECTS_FILENAME: &str = "obj";
const MANIFEST_FILENAME: &str = "manifest.json";
const OUTPUT_FILENAME: &str = "output";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildManifest {
    pub sources: HashMap<String, String>,
    pub objects: HashMap<String, String>,
}

impl Default for BuildManifest {
    fn default() -> Self {
        Self {
            sources: HashMap::new(),
            objects: HashMap::new(),
        }
    }
}

impl BuildManifest {
    fn save_file(&self, build_dir: String) {
        let manifest_filepath = format!("{}/{}", build_dir, MANIFEST_FILENAME);

        fs::remove_file(manifest_filepath.clone()).unwrap();
        let mut file = File::create(manifest_filepath).unwrap();
        file.write(serde_json::to_string(&self).unwrap().as_bytes()).unwrap();
    }

    fn read_file(&self, build_dir: String) -> Self {
        let manifest_filepath = format!("{}/{}", build_dir, MANIFEST_FILENAME);

        match serde_json::from_str::<BuildManifest>(&utils::fs::read_file(manifest_filepath.to_string()).0) {
            Ok(manifest) => manifest,
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
