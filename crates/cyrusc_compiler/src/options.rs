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
use core::fmt;
use cyrusc_scaffold_parser::ScaffoldConfig;

#[derive(Debug, Clone)]
pub struct CodeGenOptions {
    pub module_kind: Option<ModuleKind>,
    pub jobs: Option<usize>,
    pub sanitizer: Vec<CodeGenSanitizer>,
    pub linker: Option<String>,
    pub linker_options: CodeGenLinkerOptions,
    pub linker_flags: Vec<String>,
    pub base_path: Option<String>,
    pub project_type: Option<String>,
    pub project_name: Option<String>,
    pub project_version: Option<String>,
    pub cyrus_version: Option<String>,
    pub authors: Option<Vec<String>>,
    pub opt_level: Option<i32>,
    pub library_paths: Vec<String>,
    pub libraries: Vec<String>,
    pub build_dir: BuildDir,
    pub quiet: bool,
    pub verbose: bool,
    pub stdlib_path: Option<String>,
    pub display_target_machine: bool,
    pub reloc_mode: RelocModeOptions,
    pub code_model: CodeModelOptions,
    pub cpu: Option<String>,
    pub target_triple: Option<String>,
    pub disable_modulefs_cache: bool,
    pub disable_warnings: bool,
    pub endianness: Option<CodeGenEndianness>,
    pub abi: Option<CodeGenABI>,
    
    // TODO: Consider to use Path/PathBuf here and do not used never ever String to store paths.
    pub source_dirs: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum CodeGenABI {
    Cyrus,
    C,
}

#[derive(Debug, Clone)]
pub enum ModuleKind {
    Separate,
    Unified,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkerOutputKind {
    Executable,
    SharedLib,
    StaticLib,
    ObjectFile,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuildDir {
    Default,
    Provided(String),
}

impl BuildDir {
    pub fn build_dir_provided(&self) -> bool {
        matches!(self, BuildDir::Provided(_))
    }
}

impl Default for CodeGenOptions {
    fn default() -> Self {
        Self {
            module_kind: None,
            jobs: None,
            linker: None,
            base_path: None,
            project_type: None,
            project_name: None,
            authors: None,
            opt_level: None,
            library_paths: Vec::new(),
            libraries: Vec::new(),
            build_dir: BuildDir::Default,
            cyrus_version: None,
            project_version: None,
            source_dirs: vec!["./".to_string()],
            quiet: false,
            verbose: false,
            stdlib_path: None,
            display_target_machine: false,
            reloc_mode: RelocModeOptions::Default,
            code_model: CodeModelOptions::Default,
            target_triple: None,
            cpu: None,
            disable_modulefs_cache: false,
            disable_warnings: false,
            linker_options: CodeGenLinkerOptions::default(),
            linker_flags: Vec::new(),
            sanitizer: Vec::new(),
            endianness: None,
            abi: None,
        }
    }
}

impl CodeGenOptions {
    pub fn from_scaffold(scaffold: &ScaffoldConfig) -> Self {
        let mut options = CodeGenOptions::default();

        if let Some(deps) = &scaffold.dependencies {
            options.library_paths = deps.library_path.clone();
            options.libraries = deps.libraries.clone();
        }

        if let Some(project) = &scaffold.project {
            if let Some(name) = &project.name {
                options.project_name = Some(name.clone());
            }
            if !project.sources.is_empty() {
                options.source_dirs = project.sources.clone();
            }
            if let Some(v) = &project.version {
                options.project_version = Some(v.clone());
            }
        }

        if let Some(compiler) = &scaffold.compiler {
            if let Some(opt) = &compiler.optimize {
                options.opt_level = match opt.as_str() {
                    "none" => Some(0),
                    "o1" => Some(1),
                    "o2" => Some(2),
                    "o3" => Some(3),
                    other => {
                        if let Some(num) = other.strip_prefix('o') {
                            num.parse().ok()
                        } else {
                            None
                        }
                    }
                }
            }
            if let Some(build_dir) = &compiler.build_dir {
                options.build_dir = BuildDir::Provided(build_dir.clone());
            }
        }

        options
    }

    pub fn canonical_project_name(&self) -> &str {
        self.project_name.as_deref().unwrap_or("library")
    }
}

#[derive(Debug, Clone)]
pub enum CodeGenEndianness {
    Little,
    Big,
}

#[derive(Debug, Clone)]
pub enum RelocModeOptions {
    Default,
    Static,
    PIC,
    DynamicNoPic,
}

#[derive(Debug, Clone)]
pub enum CodeModelOptions {
    Default,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
}

impl fmt::Display for RelocModeOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RelocModeOptions::Default => write!(f, "Default"),
            RelocModeOptions::Static => write!(f, "Static"),
            RelocModeOptions::PIC => write!(f, "PIE"),
            RelocModeOptions::DynamicNoPic => write!(f, "DynamicNoPIC"),
        }
    }
}

impl fmt::Display for CodeModelOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeModelOptions::Default => write!(f, "Default"),
            CodeModelOptions::Tiny => write!(f, "Tiny"),
            CodeModelOptions::Small => write!(f, "Small"),
            CodeModelOptions::Kernel => write!(f, "Kernel"),
            CodeModelOptions::Medium => write!(f, "Medium"),
            CodeModelOptions::Large => write!(f, "Large"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CodeGenLinkerOptions {
    pub link_static: bool,
    pub pie: bool,
    pub no_pie: bool,
}

impl Default for CodeGenLinkerOptions {
    fn default() -> Self {
        Self {
            link_static: false,
            pie: true,
            no_pie: false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CodeGenSanitizer {
    Address,
    Memory,
    Thread,
    HWAddress,
}

impl fmt::Display for CodeGenSanitizer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeGenSanitizer::Address => write!(f, "address"),
            CodeGenSanitizer::Memory => write!(f, "memory"),
            CodeGenSanitizer::Thread => write!(f, "thread"),
            CodeGenSanitizer::HWAddress => write!(f, "hwaddress"),
        }
    }
}

impl ModuleKind {
    pub fn is_unified(&self) -> bool {
        matches!(self, ModuleKind::Unified)
    }
}

impl Default for CodeGenABI {
    fn default() -> Self {
        Self::Cyrus
    }
}
