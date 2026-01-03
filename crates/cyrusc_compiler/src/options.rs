// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use core::fmt;
use cyrusc_scaffold_parser::ScaffoldConfig;
use std::collections::HashSet;

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
    pub library_path: Vec<String>,
    pub libraries: Vec<String>,
    pub source_dirs: Vec<String>,
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
    ObjectFile, // single .o file
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
            library_path: Vec::new(),
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
    pub fn merge_preferring(&self, overrides: &Self) -> Self {
        Self {
            abi: overrides.abi.clone().or(self.abi.clone()),
            endianness: overrides.endianness.clone().or(self.endianness.clone()),
            module_kind: overrides.module_kind.clone().or(self.module_kind.clone()),
            jobs: overrides.jobs.or(self.jobs.clone()),
            linker: overrides.linker.clone().or_else(|| self.linker.clone()),
            project_type: overrides.project_type.clone().or_else(|| self.project_type.clone()),
            project_name: overrides.project_name.clone().or_else(|| self.project_name.clone()),
            project_version: overrides
                .project_version
                .clone()
                .or_else(|| self.project_version.clone()),
            cyrus_version: overrides.cyrus_version.clone().or_else(|| self.cyrus_version.clone()),
            authors: overrides.authors.clone().or_else(|| self.authors.clone()),
            opt_level: overrides.opt_level.or(self.opt_level),
            cpu: overrides.cpu.clone().or_else(|| self.cpu.clone()),
            library_path: merge_vec_prepend_unique(&overrides.library_path, &self.library_path),
            libraries: merge_vec_prepend_unique(&overrides.libraries, &self.libraries),
            build_dir: match &overrides.build_dir {
                BuildDir::Provided(s) => BuildDir::Provided(s.clone()),
                BuildDir::Default => self.build_dir.clone(),
            },
            source_dirs: merge_vec_prepend_unique(&overrides.source_dirs, &self.source_dirs),
            linker_flags: merge_vec_prepend_unique(&overrides.linker_flags, &self.linker_flags),
            sanitizer: {
                // additive sanitize flags
                let out = merge_vec_prepend_unique(
                    &overrides.sanitizer.iter().map(|s| s.to_string()).collect::<Vec<_>>(),
                    &self.sanitizer.iter().map(|s| s.to_string()).collect::<Vec<_>>(),
                );

                // convert back to enum
                out.into_iter()
                    .filter_map(|s| match s.as_str() {
                        "address" => Some(CodeGenSanitizer::Address),
                        "memory" => Some(CodeGenSanitizer::Memory),
                        "thread" => Some(CodeGenSanitizer::Thread),
                        _ => None,
                    })
                    .collect()
            },
            quiet: overrides.quiet || self.quiet,
            verbose: overrides.verbose || self.verbose,
            disable_modulefs_cache: overrides.disable_modulefs_cache || self.disable_modulefs_cache,
            stdlib_path: overrides.stdlib_path.clone().or_else(|| self.stdlib_path.clone()),
            display_target_machine: overrides.display_target_machine || self.display_target_machine,
            reloc_mode: match &overrides.reloc_mode {
                RelocModeOptions::Default => self.reloc_mode.clone(),
                other => other.clone(),
            },
            code_model: match &overrides.code_model {
                CodeModelOptions::Default => self.code_model.clone(),
                other => other.clone(),
            },
            target_triple: overrides.target_triple.clone().or_else(|| self.target_triple.clone()),
            base_path: overrides.base_path.clone().or_else(|| self.base_path.clone()),
            disable_warnings: overrides.disable_warnings || self.disable_warnings,
            linker_options: overrides.linker_options.clone(),
        }
    }

    pub fn from_scaffold(scaffold: &ScaffoldConfig) -> Self {
        let mut options = CodeGenOptions::default();

        if let Some(deps) = &scaffold.dependencies {
            options.library_path = deps.library_path.clone();
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

fn merge_vec_prepend_unique(overrides: &[String], base: &[String]) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut out = Vec::with_capacity(overrides.len() + base.len());
    for v in overrides.iter().chain(base.iter()) {
        if seen.insert(v) {
            out.push(v.clone());
        }
    }
    out
}

impl Default for CodeGenABI {
    fn default() -> Self {
        Self::Cyrus
    }
}
