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
use cyrusc_diagcentral::display_single_custom_diag;
use cyrusc_scaffold_parser::{MANIFEST_FILENAME, SOURCES_DIR_PATH};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fs::{self, File},
    io::{self, Read},
    path::{Path, PathBuf},
};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BuildManifest {
    /// Manifest schema version, for forward compatibility.
    pub version: u32,
    /// Directory where build artifacts are stored.
    pub build_dir: PathBuf,
    /// Base project path, used to resolve relative files.
    pub base_path: Option<PathBuf>,
    /// Maps source file paths to their stored hash file paths.
    pub sources: HashMap<PathBuf, PathBuf>,
    /// Maps object file names to their object file paths.
    pub objects: HashMap<PathBuf, PathBuf>,
    pub is_first_build: bool,
}

impl BuildManifest {
    pub fn new(base_path: Option<PathBuf>, build_dir: PathBuf) -> Self {
        Self {
            version: 1,
            build_dir,
            base_path,
            sources: HashMap::new(),
            objects: HashMap::new(),
            is_first_build: true,
        }
    }

    fn manifest_path(&self) -> PathBuf {
        self.base_path
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("."))
            .join(&self.build_dir)
            .join(MANIFEST_FILENAME)
    }

    fn sources_dir(&self) -> PathBuf {
        self.base_path
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("."))
            .join(&self.build_dir)
            .join(SOURCES_DIR_PATH)
    }

    pub fn hash_source_code<P: AsRef<Path>>(&self, path: P) -> io::Result<String> {
        let mut file = File::open(&path)?;
        let mut hasher = blake3::Hasher::new();
        let mut buf = [0u8; 8192];
        loop {
            let n = file.read(&mut buf)?;
            if n == 0 {
                break;
            }
            hasher.update(&buf[..n]);
        }
        Ok(hasher.finalize().to_hex().to_string())
    }

    pub fn update_source_hash<P: AsRef<Path>>(&mut self, source_file: P, new_hash: &str) -> io::Result<()> {
        let path = source_file.as_ref();
        if let Some(hash_path) = self.sources.get_mut(path) {
            fs::write(hash_path, new_hash)?;
        } else {
            self.add_source_code(path, new_hash)?;
        }
        Ok(())
    }

    pub fn add_source_code<P: AsRef<Path>>(&mut self, source_file_path: P, content_hash: &str) -> io::Result<()> {
        let unique_name = Uuid::new_v4().to_string();
        let hash_file_path = self.sources_dir().join(format!("{}.hash", unique_name));

        fs::create_dir_all(hash_file_path.parent().unwrap())?;
        fs::write(&hash_file_path, content_hash)?;

        let canonical = fs::canonicalize(&hash_file_path).unwrap_or(hash_file_path);
        self.sources.insert(source_file_path.as_ref().to_path_buf(), canonical);
        Ok(())
    }

    pub fn has_source<P: AsRef<Path>>(&self, source_path: P) -> bool {
        self.sources.contains_key(source_path.as_ref())
    }

    pub fn is_source_changed<P: AsRef<Path>>(&self, source_path: P) -> io::Result<bool> {
        let source_path = source_path.as_ref();
        if let Some(hash_path) = self.sources.get(source_path) {
            let old_hash = fs::read_to_string(hash_path).unwrap_or_default();
            let new_hash = self.hash_source_code(source_path)?;
            Ok(new_hash != old_hash)
        } else {
            Ok(true)
        }
    }

    pub fn save_manifest(&self) -> io::Result<()> {
        let manifest_path = self.manifest_path();
        fs::create_dir_all(manifest_path.parent().unwrap())?;

        let file = File::create(&manifest_path)?;
        serde_json::to_writer_pretty(file, &self)?;
        Ok(())
    }

    pub fn read_manifest(base_path: Option<PathBuf>, build_dir: PathBuf) -> Option<Self> {
        let manifest_path = base_path
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("."))
            .join(&build_dir)
            .join(MANIFEST_FILENAME);

        let file_content = match fs::read_to_string(&manifest_path) {
            Ok(c) => c,
            Err(err) => {
                if err.kind() != io::ErrorKind::NotFound {
                    display_single_custom_diag!(format!("Failed to read '{}'.", MANIFEST_FILENAME));
                }
                return None;
            }
        };

        match serde_json::from_str::<BuildManifest>(&file_content) {
            Ok(manifest) => Some(manifest),
            Err(err) => {
                let file_path = manifest_path.display().to_string();
                display_single_custom_diag!(format!("Failed to parse '{}': {}", file_path, err));
            }
        }
    }
}
