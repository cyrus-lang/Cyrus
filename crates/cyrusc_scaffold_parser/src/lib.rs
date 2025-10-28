use serde::Deserialize;
use std::path::Path;
use thiserror::Error;

pub const SOURCES_DIR_PATH: &str = "sources";
pub const OBJECTS_FILENAME: &str = "obj";
pub const MANIFEST_FILENAME: &str = "manifest.json";
pub const OUTPUT_FILENAME: &str = "output";
pub const PROJECT_FILE_PATH: &str = "Project.toml";
pub const LLVM_IR_DIR_PATH: &str = "llvmir";

#[derive(Debug, Error)]
pub enum ParseError {
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
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct CompilerSection {
    pub optimize: Option<String>, // e.g. "o1", "o2", "o3", "none"
    pub build_dir: Option<String>,
}

#[derive(Debug, Deserialize, Default, Clone)]
pub struct DependenciesSection {
    #[serde(default)]
    pub library_path: Vec<String>,

    #[serde(default)]
    pub libraries: Vec<String>,
}

/// Parse a Project.toml file and return the parsed `ScaffoldConfig`.
pub fn parse_project_toml<P: AsRef<Path>>(path: P) -> Result<ScaffoldConfig, ParseError> {
    let path = path.as_ref();
    let s = std::fs::read_to_string(path).map_err(|e| ParseError::IO(path.to_string_lossy().to_string(), e))?;
    let cfg: ScaffoldConfig = toml::from_str(&s)?;
    Ok(cfg)
}
