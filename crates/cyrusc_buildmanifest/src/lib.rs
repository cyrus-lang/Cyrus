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
use cyrusc_diagcentral::exit_with_single_diag;
use cyrusc_scaffold_parser::{MANIFEST_FILENAME, SRC_CACHE_DIR_PATH};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fs::{self, File},
    io::{self, Read},
    path::{Path, PathBuf},
};
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct BuildManifest {
    /// Directory where build artifacts are stored.
    build_dir: PathBuf,
    /// Base project path, used to resolve relative files.
    base_path: PathBuf,
    /// Maps source file paths to their stored hash file paths.
    sources: HashMap<PathBuf, PathBuf>,
    /// Maps object file names to their object file paths.
    objects: HashMap<PathBuf, PathBuf>,
    /// Used to disable cache, if is initial build.
    pub initial_build: bool,
}

impl BuildManifest {
    fn new(base_path: PathBuf, build_dir: PathBuf) -> Self {
        Self {
            build_dir,
            base_path,
            sources: HashMap::new(),
            objects: HashMap::new(),
            initial_build: true,
        }
    }

    pub fn mark_initial_build_complete(&mut self) {
        self.initial_build = false;
    }

    pub fn initial_build(&mut self) {
        self.initial_build = true;
    }

    pub fn get_object<P: AsRef<Path>>(&self, object_name: P) -> Option<&PathBuf> {
        self.objects.get_key_value(object_name.as_ref()).map(|(_, path)| path)
    }

    pub fn insert_object<P: AsRef<Path> + ?Sized, Q: AsRef<Path> + ?Sized>(
        &mut self,
        object_name: &P,
        object_path: &Q,
    ) -> io::Result<()> {
        let object_path = object_path.as_ref();

        // ensure the object file exists
        if !object_path.exists() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("Object file not found: {}", object_path.display()),
            ));
        }

        // canonicalize the path for consistency
        let canonical_path = fs::canonicalize(object_path).unwrap_or_else(|_| object_path.to_path_buf());

        self.objects.insert(object_name.as_ref().to_path_buf(), canonical_path);
        Ok(())
    }

    fn manifest_path(&self) -> PathBuf {
        self.base_path.join(&self.build_dir).join(MANIFEST_FILENAME)
    }

    fn sources_dir(&self) -> PathBuf {
        self.base_path.join(&self.build_dir).join(SRC_CACHE_DIR_PATH)
    }

    pub fn hash_source_code<P: AsRef<Path> + ?Sized>(&self, path: &P) -> io::Result<String> {
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
            self.insert_source_code(path, new_hash)?;
        }
        Ok(())
    }

    fn insert_source_code<P: AsRef<Path> + ?Sized>(
        &mut self,
        source_file_path: &P,
        content_hash: &str,
    ) -> io::Result<()> {
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

    pub fn is_source_changed<P: AsRef<Path> + ?Sized>(&self, source_path: &P) -> io::Result<bool> {
        if let Some(hash_path) = self.sources.get(source_path.as_ref()) {
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
        if cfg!(debug_assertions) {
            serde_json::to_writer_pretty(file, &self)?;
        } else {
            serde_json::to_writer(file, &self)?;
        }
        Ok(())
    }

    pub fn load_manifest_or_make_new(base_path: &PathBuf, build_dir: &PathBuf) -> BuildManifest {
        let manifest_path = base_path.join(build_dir).join(MANIFEST_FILENAME);

        match fs::read_to_string(&manifest_path) {
            Ok(file_content) => match serde_json::from_str::<BuildManifest>(&file_content) {
                Ok(mut build_manifest) => {
                    build_manifest.mark_initial_build_complete();
                    return build_manifest;
                }
                Err(err) => {
                    let file_path = manifest_path.display().to_string();
                    exit_with_single_diag!(format!("Failed to parse '{}': {}", file_path, err));
                }
            },
            _ => {}
        };

        let initial_build_manifest = BuildManifest::new(base_path.clone(), build_dir.clone());
        if let Err(err) = initial_build_manifest.save_manifest() {
            exit_with_single_diag!(format!("Error while saving build manifest: {}", err.to_string()));
        }
        return initial_build_manifest;
    }
}
