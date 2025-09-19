use crate::diagnostics::CodeGenDiagKind;
use diagcentral::{Diag, DiagLevel, display_single_custom_diag, display_single_diag};
use project_layout::{MANIFEST_FILENAME, SOURCES_DIR_PATH};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
};
use utils::{fs::read_file, generate_random_hex::generate_random_hex};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildManifest {
    // The directory where the build-manifest and other artifacts are stored.
    pub build_dir: String,
    // The base path of the project, used to resolve relative paths.
    pub base_path: Option<String>,
    // Maps source file paths to their content hashes.
    pub sources: HashMap<String, String>,
    // Maps object file names to their paths.
    // This is used to track which object files were generated from which source files.
    pub objects: HashMap<String, String>,
    pub is_first_build: bool,
}

impl BuildManifest {
    pub fn new(base_path: Option<String>, build_dir: String) -> Self {
        Self {
            build_dir,
            base_path,
            sources: HashMap::new(),
            objects: HashMap::new(),
            is_first_build: true,
        }
    }
}

impl BuildManifest {
    pub fn hash_source_code(&self, source_file_path: String) -> String {
        let file_content = read_file(source_file_path).0;
        blake3::hash(&file_content.as_bytes()).to_hex().to_string()
    }

    pub fn update_source_hash(&mut self, source_file_path: String, new_content_hash: String) {
        if let Some(source_hash_file_path) = self.sources.get_mut(&source_file_path) {
            let path = Path::new(source_hash_file_path);
            std::fs::write(path, new_content_hash).unwrap();
        } else {
            self.add_source_code(source_file_path, new_content_hash);
        }
    }

    pub fn add_source_code(&mut self, source_file_path: String, content_hash: String) {
        let source_hash_file_name = generate_random_hex();
        let source_hash_file_path = Path::new(&self.base_path.clone().unwrap_or(String::new()))
            .join(&self.build_dir)
            .join(SOURCES_DIR_PATH)
            .join(format!("{}", source_hash_file_name));

        std::fs::write(&source_hash_file_path, content_hash).unwrap();

        self.sources.insert(
            source_file_path,
            fs::canonicalize(&source_hash_file_path)
                .unwrap_or(source_hash_file_path)
                .to_str()
                .unwrap()
                .to_string(),
        );
    }

    pub fn check_source_code_hash_exists(&self, source_file_path: String) -> bool {
        match self
            .sources
            .iter()
            .find(|(file_path, _)| **file_path == source_file_path)
        {
            Some(..) => true,
            None => false,
        }
    }

    pub fn check_source_code_changed(&self, input_source_file_path: String) -> bool {
        match self
            .sources
            .iter()
            .find(|(source_file_path, _)| **source_file_path == input_source_file_path)
        {
            Some((_, source_hash_file_path)) => {
                let source_hash_file_content = match std::fs::read_to_string(source_hash_file_path.clone()).ok() {
                    Some(file_content) => file_content,
                    None => return true,
                };

                let new_source_hash = self.hash_source_code(input_source_file_path.clone());
                new_source_hash.to_string() != source_hash_file_content
            }
            None => true,
        }
    }

    pub fn save_manifest(&self) {
        let manifest_filepath = Path::new("")
            .join(self.base_path.clone().unwrap_or(String::new()))
            .join(self.build_dir.clone())
            .join(MANIFEST_FILENAME);

        if manifest_filepath.exists() {
            fs::remove_file(manifest_filepath.clone()).unwrap();
        }

        let mut file = File::create(manifest_filepath).unwrap();
        file.write(serde_json::to_string(&self).unwrap().as_bytes()).unwrap();
    }

    pub fn read_manifest(&self) -> Option<Self> {
        let mut manifest_filepath = match &self.base_path {
            Some(path_str) => PathBuf::from(path_str),
            None => PathBuf::from("."),
        };

        manifest_filepath.push(&self.build_dir);
        manifest_filepath.push(MANIFEST_FILENAME);

        let file_content = match fs::read_to_string(&manifest_filepath) {
            Ok(content) => content,
            Err(err) => {
                if err.kind() != std::io::ErrorKind::NotFound {
                    display_single_custom_diag!("Failed to read 'build_manifest.json'.".to_string());
                }
                return None;
            }
        };

        match serde_json::from_str::<BuildManifest>(&file_content) {
            Ok(manifest) => Some(manifest),
            Err(err) => {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: CodeGenDiagKind::FailedToParseBuildManifest {
                        file_path: manifest_filepath.display().to_string(),
                        err: err.to_string()
                    },
                    location: None,
                    hint: None
                });
            }
        }
    }
}
