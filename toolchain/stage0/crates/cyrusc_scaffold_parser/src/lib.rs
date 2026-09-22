// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language
use serde::Deserialize;
use std::path::Path;
use thiserror::Error;

pub const MANIFEST_FILENAME: &str = "manifest.json";
pub const PROJECT_FILE_PATH: &str = "Project.toml";
pub const SRC_CACHE_DIR_PATH: &str = "src-cache";
pub const OBJECT_CACHE_DIR_FILENAME: &str = "obj-cache";
pub const OBJECT_DIR_FILENAME: &str = "object";
pub const OUTPUT_DIR_FILENAME: &str = "output";
pub const LLVM_IR_DIR_PATH: &str = "llvm-ir";
pub const CIR_DUMP_DIR_PATH: &str = "cir";
pub const BITCODE_DIR_PATH: &str = "bitcode";
pub const ASSEMBLY_DIR_PATH: &str = "assembly";
pub const SHARED_LIB_DIR_PATH: &str = "shared-lib";
pub const STATIC_LIB_DIR_PATH: &str = "static-lib";

#[derive(Debug, Error)]
pub enum ScaffoldParseError {
    #[error("IO error reading '{0}': {1}")]
    IO(String, #[source] std::io::Error),

    #[error("TOML deserialization error: {0}")]
    TOML(#[from] toml::de::Error),

    #[error("Unexpected format: {0}")]
    Format(String),
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct ScaffoldConfig {
    #[serde(default)]
    pub project: Option<ProjectSection>,

    #[serde(default)]
    pub compiler: Option<CompilerSection>,

    #[serde(default)]
    pub dependencies: Option<DependenciesSection>,
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct ProjectSection {
    pub name: Option<String>,
    pub version: Option<String>,
    #[serde(default)]
    pub sources: Vec<String>,
    pub project_type: Option<String>, // e.g. "executable", "library"
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct CompilerSection {
    pub sources: Vec<String>,
    pub optimize: Option<String>, // e.g. "o1", "o2", "o3", "none"
    pub build_dir: Option<String>,
    pub version: Option<String>,
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct DependenciesSection {
    #[serde(default)]
    pub library_path: Vec<String>,

    #[serde(default)]
    pub libraries: Vec<String>,
}

/// Parse a Project.toml file and return the parsed `ScaffoldConfig`.
pub fn parse_project_toml<P: AsRef<Path>>(path: P) -> Result<ScaffoldConfig, ScaffoldParseError> {
    let path = path.as_ref();
    let s = std::fs::read_to_string(path).map_err(|e| ScaffoldParseError::IO(path.to_string_lossy().to_string(), e))?;
    let scaffold_config: ScaffoldConfig = toml::from_str(&s)?;

    Ok(scaffold_config)
}
