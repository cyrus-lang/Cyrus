// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#![allow(nonstandard_style)]

use std::fmt;

#[derive(Debug, Clone)]
pub struct CompilerOptions {
    pub profile: CompilerOption_Profile,
    pub project_type: Option<CompilerOption_ProjectType>,
    pub module_kind: Option<CompilerOption_ModuleKind>,
    pub sanitizer: Vec<CompilerOption_Sanitizer>,
    pub build_dir: CompilerOption_BuildDir,
    pub reloc_mode: CompilerOption_RelocMode,
    pub code_model: CompilerOption_CodeModel,
    pub abi: Option<CompilerOption_CodeGenABI>,

    pub linker: Option<String>,
    pub linker_options: CompilerOption_Linker,
    pub linker_flags: Vec<String>,
    pub base_path: Option<String>,

    pub stdlib_path: Option<String>,
    pub project_name: Option<String>,
    pub project_version: Option<String>,
    pub cyrus_version: Option<String>,
    pub target: Option<String>,
    pub cpu: Option<String>,
    pub libraries: Vec<String>,
    pub library_paths: Vec<String>,
    pub source_dirs: Vec<String>,

    pub opt_level: Option<i32>,
    pub jobs: Option<usize>,

    pub display_target_machine: bool,
    pub disable_modulefs_cache: bool,
    pub disable_warnings: bool,
    pub debuginfo_enabled: bool,
    pub quiet: bool,
    pub verbose: bool,
}

#[derive(Debug, Clone)]
pub enum CompilerOption_ProjectType {
    Library,
    Executable,
}

#[derive(Debug, Clone)]
pub enum CompilerOption_CodeGenABI {
    Cyrus,
    C,
}

#[derive(Debug, Clone)]
pub enum CompilerOption_ModuleKind {
    Separate,
    Unified,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilerOption_LinkerOutputKind {
    Executable,
    SharedLib,
    StaticLib,
    ObjectFile,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompilerOption_BuildDir {
    Default,
    Provided(String),
}

#[derive(Debug, Clone)]
pub enum CompilerOption_Endianness {
    Little,
    Big,
}

#[derive(Debug, Clone)]
pub enum CompilerOption_RelocMode {
    Default,
    Static,
    PIC,
    DynamicNoPic,
}

#[derive(Debug, Clone)]
pub enum CompilerOption_CodeModel {
    Default,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
}

#[derive(Debug, Clone)]
pub enum CompilerOption_Profile {
    Debug,
    Release,
}

#[derive(Debug, Clone)]
pub struct CompilerOption_Linker {
    pub link_static: bool,
    pub pie: bool,
    pub no_pie: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompilerOption_Sanitizer {
    Address,
    Memory,
    Thread,
    HWAddress,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            reloc_mode: CompilerOption_RelocMode::Default,
            code_model: CompilerOption_CodeModel::Default,
            build_dir: CompilerOption_BuildDir::Default,
            profile: CompilerOption_Profile::default(),
            linker_options: CompilerOption_Linker::default(),

            stdlib_path: None,
            target: None,
            cpu: None,
            module_kind: None,
            jobs: None,
            linker: None,
            base_path: None,
            project_type: None,
            project_name: None,
            opt_level: None,
            cyrus_version: None,
            project_version: None,
            abi: None,

            library_paths: Vec::new(),
            libraries: Vec::new(),
            source_dirs: Vec::new(),
            linker_flags: Vec::new(),
            sanitizer: Vec::new(),

            display_target_machine: false,
            disable_modulefs_cache: false,
            disable_warnings: false,
            debuginfo_enabled: false,
            quiet: false,
            verbose: false,
        }
    }
}

impl CompilerOption_BuildDir {
    #[inline]
    pub fn build_dir_provided(&self) -> bool {
        matches!(self, CompilerOption_BuildDir::Provided(_))
    }
}

impl CompilerOption_ModuleKind {
    pub fn is_unified(&self) -> bool {
        matches!(self, CompilerOption_ModuleKind::Unified)
    }
}

impl CompilerOption_Profile {
    #[inline]
    pub fn is_debug(&self) -> bool {
        matches!(self, Self::Debug)
    }

    #[inline]
    pub fn is_release(&self) -> bool {
        matches!(self, Self::Release)
    }
}

// default

impl Default for CompilerOption_Linker {
    fn default() -> Self {
        Self {
            link_static: false,
            pie: true,
            no_pie: false,
        }
    }
}

impl Default for CompilerOption_CodeGenABI {
    fn default() -> Self {
        Self::Cyrus
    }
}

impl Default for CompilerOption_ProjectType {
    fn default() -> Self {
        CompilerOption_ProjectType::Executable
    }
}

impl Default for CompilerOption_Profile {
    fn default() -> Self {
        Self::Debug
    }
}

// display

impl fmt::Display for CompilerOption_RelocMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerOption_RelocMode::Default => write!(f, "Default"),
            CompilerOption_RelocMode::Static => write!(f, "Static"),
            CompilerOption_RelocMode::PIC => write!(f, "PIE"),
            CompilerOption_RelocMode::DynamicNoPic => write!(f, "DynamicNoPIC"),
        }
    }
}

impl fmt::Display for CompilerOption_CodeModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerOption_CodeModel::Default => write!(f, "Default"),
            CompilerOption_CodeModel::Tiny => write!(f, "Tiny"),
            CompilerOption_CodeModel::Small => write!(f, "Small"),
            CompilerOption_CodeModel::Kernel => write!(f, "Kernel"),
            CompilerOption_CodeModel::Medium => write!(f, "Medium"),
            CompilerOption_CodeModel::Large => write!(f, "Large"),
        }
    }
}

impl fmt::Display for CompilerOption_Sanitizer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerOption_Sanitizer::Address => write!(f, "address"),
            CompilerOption_Sanitizer::Memory => write!(f, "memory"),
            CompilerOption_Sanitizer::Thread => write!(f, "thread"),
            CompilerOption_Sanitizer::HWAddress => write!(f, "hwaddress"),
        }
    }
}
