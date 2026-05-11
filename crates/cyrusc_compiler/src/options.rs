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

#![allow(nonstandard_style)]

use core::fmt;
use cyrusc_scaffold_parser::ScaffoldConfig;
use cyrusc_tui_utils::tui_error;
use std::{
    fs,
    path::{Path, PathBuf},
};

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

impl CompilerOptions {
    /// Validates and normalizes all filesystem paths in the compiler options.
    ///
    /// Existing paths are canonicalized and checked for validity, while paths that
    /// may not exist yet (e.g. the build directory) are normalized into absolute
    /// form. Collected errors are reported together to avoid partial failures.
    pub fn validate_paths(&mut self) -> Result<(), ()> {
        let mut errors = Vec::new();

        // helper function to validate and canonicalize a single path
        fn validate_single_path(path_str: &str, section_name: &str, must_exist: bool) -> Result<PathBuf, String> {
            let path = Path::new(path_str);

            // check if path exists if required
            if must_exist && !path.exists() {
                return Err(format!("Path '{}' of '{}' does not exist.", path_str, section_name));
            }

            match fs::canonicalize(path) {
                Ok(canonical_path) => Ok(canonical_path),
                Err(err) => {
                    if path_str.trim().is_empty() {
                        Err(format!("Empty path provided for '{}'.", section_name))
                    } else {
                        Err(format!(
                            "Failed to canonicalize path '{}' for field '{}': {}",
                            path_str, section_name, err
                        ))
                    }
                }
            }
        }

        /// helper function to validate and canonicalize a path that might not exist yet.
        fn validate_future_path(path_str: &str) -> Result<PathBuf, String> {
            let path = Path::new(path_str);

            // clean up the path components

            let mut clean_path = PathBuf::new();

            for component in path.components() {
                match component {
                    std::path::Component::Prefix(_) => clean_path.push(component.as_os_str()),
                    std::path::Component::RootDir => clean_path.push(component.as_os_str()),
                    std::path::Component::CurDir => {
                        // skip . components
                    }
                    std::path::Component::ParentDir => {
                        // try to go up if possible
                        if !clean_path.pop() {
                            // if we can't go up, keep it as part of the path
                            clean_path.push("..");
                        }
                    }
                    std::path::Component::Normal(c) => clean_path.push(c),
                }
            }

            // convert to absolute path if relative

            let absolute_path = if clean_path.is_relative() {
                match std::env::current_dir() {
                    Ok(current_dir) => current_dir.join(clean_path),
                    Err(e) => return Err(format!("Failed to get current directory: {}", e)),
                }
            } else {
                clean_path
            };

            Ok(absolute_path)
        }

        // validate base path (must exist)

        if let Some(base_path_str) = &self.base_path {
            if !base_path_str.trim().is_empty() {
                match validate_single_path(base_path_str, "base path", true) {
                    Ok(canonical_path) => {
                        // Convert back to string for storage
                        match canonical_path.to_str() {
                            Some(path_str) => {
                                self.base_path = Some(path_str.to_string());
                            }
                            None => {
                                errors.push("Invalid UTF-8 in base path.".to_string());
                            }
                        }
                    }
                    Err(e) => errors.push(e),
                }
            }
        }

        // validate library paths (must exist)

        let mut validated_library_paths = Vec::new();

        for (i, lib_path) in self.library_paths.iter().enumerate() {
            if lib_path.trim().is_empty() {
                errors.push(format!("Empty library path at index {}", i));
                continue;
            }

            match validate_single_path(lib_path, &format!("library_paths[{}]", i), true) {
                Ok(canonical_path) => match canonical_path.to_str() {
                    Some(path_str) => {
                        validated_library_paths.push(path_str.to_string());
                    }
                    None => {
                        errors.push(format!("Invalid UTF-8 in library path at index {}", i));
                    }
                },
                Err(e) => errors.push(e),
            }
        }

        self.library_paths = validated_library_paths;

        // validate build_dir if provided (doesn't need to exist yet)
        match &self.build_dir {
            CompilerOption_BuildDir::Provided(build_dir_str) => {
                if build_dir_str.trim().is_empty() {
                    errors.push("Build directory path is empty.".to_string());
                } else {
                    match validate_future_path(build_dir_str) {
                        Ok(canonical_path) => match canonical_path.to_str() {
                            Some(path_str) => {
                                self.build_dir = CompilerOption_BuildDir::Provided(path_str.to_string());
                            }
                            None => {
                                errors.push("Invalid UTF-8 in build directory path.".to_string());
                            }
                        },
                        Err(e) => errors.push(e),
                    }
                }
            }
            CompilerOption_BuildDir::Default => {}
        }

        // validate stdlib_path (must exist)
        if let Some(stdlib_path_str) = &self.stdlib_path {
            if !stdlib_path_str.trim().is_empty() {
                match validate_single_path(stdlib_path_str, "stdlib path", true) {
                    Ok(canonical_path) => match canonical_path.to_str() {
                        Some(path_str) => {
                            self.stdlib_path = Some(path_str.to_string());
                        }
                        None => {
                            errors.push("Invalid UTF-8 in stdlib path".to_string());
                        }
                    },
                    Err(e) => errors.push(e),
                }
            } else {
                errors.push("Stdlib path path is empty.".to_string());
            }
        }

        // validate source_dirs (must exist)
        let mut validated_source_dirs = Vec::new();

        for (i, source_dir) in self.source_dirs.iter().enumerate() {
            if source_dir.trim().is_empty() {
                errors.push(format!("Empty source directory at index {}.", i));
                continue;
            }

            match validate_single_path(source_dir, "source dirs", true) {
                Ok(canonical_path) => match canonical_path.to_str() {
                    Some(path_str) => {
                        validated_source_dirs.push(path_str.to_string());
                    }
                    None => {
                        errors.push(format!("Invalid UTF-8 in source directory at index {}", i));
                    }
                },
                Err(e) => errors.push(e),
            }
        }

        // remove duplicates from source_dirs
        validated_source_dirs.sort();
        validated_source_dirs.dedup();
        self.source_dirs = validated_source_dirs;

        // check for circular dependencies or weird paths
        self.validate_path_relationships(&mut errors);

        // return results
        if errors.is_empty() {
            Ok(())
        } else {
            // display all errors using tui_error
            for error in &errors {
                tui_error(error.clone());
            }

            return Err(());
        }
    }

    /// Merges two `CompilerOptions` structures into a new configuration.
    ///
    /// Fields missing in `self` are filled from `other`. Collection fields are
    /// combined with deduplication, and boolean flags are merged conservatively.
    pub fn merge(&self, other: &CompilerOptions) -> Self {
        let mut merged = self.clone();

        // Helper macro to merge Option fields
        macro_rules! merge_option {
            ($field:ident) => {
                merged.$field = merged.$field.clone().or_else(|| other.$field.clone());
            };
        }

        // Helper macro to merge Vec fields with deduplication
        macro_rules! merge_vec {
            ($field:ident) => {
                let mut vec = merged.$field.clone();
                for item in &other.$field {
                    if !vec.contains(item) {
                        vec.push(item.clone());
                    }
                }
                merged.$field = vec;
            };
        }

        merge_option!(module_kind);
        merge_option!(jobs);
        merge_option!(linker);
        merge_option!(base_path);
        merge_option!(project_type);
        merge_option!(project_name);
        merge_option!(project_version);
        merge_option!(cyrus_version);
        merge_option!(opt_level);
        merge_option!(stdlib_path);
        merge_option!(target);
        merge_option!(cpu);
        merge_option!(abi);
        merge_option!(cyrus_version);

        merge_vec!(library_paths);
        merge_vec!(libraries);
        merge_vec!(source_dirs);
        merge_vec!(linker_flags);
        merge_vec!(sanitizer);

        merged.quiet = merged.quiet || other.quiet;
        merged.verbose = merged.verbose || other.verbose;
        merged.display_target_machine = merged.display_target_machine || other.display_target_machine;
        merged.disable_modulefs_cache = merged.disable_modulefs_cache || other.disable_modulefs_cache;
        merged.disable_warnings = merged.disable_warnings || other.disable_warnings;

        merged.build_dir = match (&self.build_dir, &other.build_dir) {
            (CompilerOption_BuildDir::Provided(_), _) => self.build_dir.clone(),
            (_, CompilerOption_BuildDir::Provided(_)) => other.build_dir.clone(),
            _ => self.build_dir.clone(),
        };

        merged.linker_options.link_static = self.linker_options.link_static || other.linker_options.link_static;
        merged.linker_options.pie = self.linker_options.pie || other.linker_options.pie;
        merged.linker_options.no_pie = self.linker_options.no_pie || other.linker_options.no_pie;

        if matches!(self.reloc_mode, CompilerOption_RelocMode::Default) {
            merged.reloc_mode = other.reloc_mode.clone();
        }

        if matches!(self.code_model, CompilerOption_CodeModel::Default) {
            merged.code_model = other.code_model.clone();
        }

        merged
    }

    /// Constructs `CompilerOptions` from a project scaffold configuration.
    ///
    /// Extracts project metadata, dependency paths, and compiler settings,
    /// applying sensible defaults where fields are absent.
    pub fn from_scaffold(scaffold: &ScaffoldConfig) -> Self {
        let mut opts = CompilerOptions::default();

        if let Some(deps) = &scaffold.dependencies {
            opts.library_paths = deps.library_path.clone();
            opts.libraries = deps.libraries.clone();
        }

        if let Some(project) = &scaffold.project {
            if let Some(name) = &project.name {
                opts.project_name = Some(name.clone());
            }
            if !project.sources.is_empty() {
                opts.source_dirs = project.sources.clone();
            }
            if let Some(v) = &project.version {
                opts.project_version = Some(v.clone());
            }
        }

        if let Some(compiler) = &scaffold.compiler {
            if let Some(opt) = &compiler.optimize {
                // parse the optimize flag in a case-insensitive way
                opts.opt_level = match opt.to_lowercase().as_str() {
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

            if let Some(version) = &compiler.version {
                opts.cyrus_version = Some(version.clone());
            }

            if let Some(build_dir) = &compiler.build_dir {
                opts.build_dir = CompilerOption_BuildDir::Provided(build_dir.clone());
            }

            for path in &compiler.sources {
                opts.source_dirs.push(path.clone());
            }
        }

        opts
    }

    /// Returns the canonical project name, falling back to `"unknown"`
    /// if no explicit project name was provided.
    #[inline]
    pub fn canonical_project_name(&self) -> &str {
        self.project_name.as_deref().unwrap_or("unknown")
    }

    /// Performs additional sanity checks between configured paths.
    ///
    /// Ensures source directories do not contain each other and verifies that
    /// all source directories reside under `base_path` when it is set.
    fn validate_path_relationships(&self, errors: &mut Vec<String>) {
        // check that source_dirs don't contain each other
        'check: for (i, dir1) in self.source_dirs.iter().enumerate() {
            for (j, dir2) in self.source_dirs.iter().enumerate() {
                if i != j {
                    let path1 = Path::new(dir1);
                    let path2 = Path::new(dir2);

                    if let (Ok(canon1), Ok(canon2)) = (fs::canonicalize(path1), fs::canonicalize(path2)) {
                        if canon1.starts_with(&canon2) || canon2.starts_with(&canon1) {
                            errors.push(format!(
                                "Source directories '{}' and '{}' have a containment relationship. This may cause issues.",
                                dir1, dir2
                            ));
                            break 'check;
                        }
                    }
                }
            }
        }

        // check that base_path contains source_dirs if base_path is set
        if let Some(base_path_str) = &self.base_path {
            let base_path = Path::new(base_path_str);

            if let Ok(canon_base) = fs::canonicalize(base_path) {
                for source_dir in &self.source_dirs {
                    let source_path = Path::new(source_dir);
                    if let Ok(canon_source) = fs::canonicalize(source_path) {
                        if !canon_source.starts_with(&canon_base) {
                            errors.push(format!(
                                "Source directory '{}' is not under base path '{}'.",
                                source_dir, base_path_str
                            ));
                        }
                    }
                }
            }
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
