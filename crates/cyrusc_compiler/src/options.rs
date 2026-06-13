// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_diagcentral::exit_with_msg;
use cyrusc_internal::compiler_options::{
    CompilerOption_BuildDir, CompilerOption_CodeModel, CompilerOption_Optimize, CompilerOption_Profile,
    CompilerOption_RelocMode, CompilerOption_Sanitizer, CompilerOptions,
};
use cyrusc_scaffold_parser::ScaffoldConfig;
use cyrusc_tui_utils::{tui_error, tui_note, tui_warning};
use std::{
    fs,
    path::{Path, PathBuf},
};

/// Validates and normalizes all filesystem paths in the compiler options.
///
/// Existing paths are canonicalized and checked for validity, while paths that
/// may not exist yet (e.g. the build directory) are normalized into absolute
/// form. Collected errors are reported together to avoid partial failures.
pub fn validate_compiler_options_paths(opts: &mut CompilerOptions) -> Result<(), ()> {
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

    if let Some(base_path_str) = &opts.base_path {
        if !base_path_str.trim().is_empty() {
            match validate_single_path(base_path_str, "base path", true) {
                Ok(canonical_path) => {
                    // Convert back to string for storage
                    match canonical_path.to_str() {
                        Some(path_str) => {
                            opts.base_path = Some(path_str.to_string());
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

    for (i, lib_path) in opts.library_paths.iter().enumerate() {
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

    opts.library_paths = validated_library_paths;

    // validate build_dir if provided (doesn't need to exist yet)
    match &opts.build_dir {
        CompilerOption_BuildDir::Provided(build_dir_str) => {
            if build_dir_str.trim().is_empty() {
                errors.push("Build directory path is empty.".to_string());
            } else {
                match validate_future_path(build_dir_str) {
                    Ok(canonical_path) => match canonical_path.to_str() {
                        Some(path_str) => {
                            opts.build_dir = CompilerOption_BuildDir::Provided(path_str.to_string());
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
    if let Some(stdlib_path_str) = &opts.stdlib_path {
        if !stdlib_path_str.trim().is_empty() {
            match validate_single_path(stdlib_path_str, "stdlib path", true) {
                Ok(canonical_path) => match canonical_path.to_str() {
                    Some(path_str) => {
                        opts.stdlib_path = Some(path_str.to_string());
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

    for (i, source_dir) in opts.source_dirs.iter().enumerate() {
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
    opts.source_dirs = validated_source_dirs;

    // check for circular dependencies or weird paths
    validate_compiler_options_path_relationships(opts, &mut errors);

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
pub fn merge_compiler_options(opts: &CompilerOptions, other: &CompilerOptions) -> CompilerOptions {
    let mut merged = opts.clone();

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

    merged.build_dir = match (&opts.build_dir, &other.build_dir) {
        (CompilerOption_BuildDir::Provided(_), _) => opts.build_dir.clone(),
        (_, CompilerOption_BuildDir::Provided(_)) => other.build_dir.clone(),
        _ => opts.build_dir.clone(),
    };

    merged.linker_options.link_static = opts.linker_options.link_static || other.linker_options.link_static;
    merged.linker_options.pie = opts.linker_options.pie || other.linker_options.pie;
    merged.linker_options.no_pie = opts.linker_options.no_pie || other.linker_options.no_pie;

    if matches!(opts.reloc_mode, CompilerOption_RelocMode::Default) {
        merged.reloc_mode = other.reloc_mode.clone();
    }

    if matches!(opts.code_model, CompilerOption_CodeModel::Default) {
        merged.code_model = other.code_model.clone();
    }

    merged
}

/// Constructs `CompilerOptions` from a project scaffold configuration.
///
/// Extracts project metadata, dependency paths, and compiler settings,
/// applying sensible defaults where fields are absent.
pub fn compiler_options_from_scaffold(scaffold: &ScaffoldConfig) -> CompilerOptions {
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
                "none" => Some(CompilerOption_Optimize::O1),
                "o1" => Some(CompilerOption_Optimize::O1),
                "o2" => Some(CompilerOption_Optimize::O2),
                "o3" => Some(CompilerOption_Optimize::O3),
                "os" => Some(CompilerOption_Optimize::Os),
                "oz" => Some(CompilerOption_Optimize::Oz),

                _ => None,
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
pub fn canonical_project_name(opts: &CompilerOptions) -> &str {
    opts.project_name.as_deref().unwrap_or("unknown")
}

/// Performs additional sanity checks between configured paths.
///
/// Ensures source directories do not contain each other and verifies that
/// all source directories reside under `base_path` when it is set.
fn validate_compiler_options_path_relationships(opts: &CompilerOptions, errors: &mut Vec<String>) {
    // check that source_dirs don't contain each other
    'check: for (i, dir1) in opts.source_dirs.iter().enumerate() {
        for (j, dir2) in opts.source_dirs.iter().enumerate() {
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
    if let Some(base_path_str) = &opts.base_path {
        let base_path = Path::new(base_path_str);

        if let Ok(canon_base) = fs::canonicalize(base_path) {
            for source_dir in &opts.source_dirs {
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

pub fn validate_compiler_options(opts: &CompilerOptions) {
    if opts.debuginfo_enabled && opts.opt_level != Some(CompilerOption_Optimize::O0) {
        exit_with_msg!("Debug info emission '-g' can only be used with optimization level O0".to_string());
    }

    if !opts.sanitizer.is_empty() {
        if !opts.debuginfo_enabled {
            tui_warning(
                "Sanitizers work better with debug info '-g'. Consider enabling debug info for better error messages."
                    .to_string(),
            );
        }

        // sanitizers don't work well with high optimization levels
        if let Some(opt_level) = opts.opt_level {
            match opt_level {
                CompilerOption_Optimize::O0 | CompilerOption_Optimize::O1 => {}
                CompilerOption_Optimize::O2 | CompilerOption_Optimize::O3 => {
                    tui_warning("Sanitizers with --optimize=O2/--optimize=O3 may miss some errors. Consider using --optimize=O1 for better sanitizer coverage".to_string());
                }
                CompilerOption_Optimize::Os | CompilerOption_Optimize::Oz => {
                    tui_warning(
                        "Sanitizers with size optimizations may produce false negatives. Consider using --optimize=O1 for better sanitizer results".to_string(),
                    );
                }
            }
        }

        // can't use multiple incompatible sanitizers
        let has_address = opts.sanitizer.contains(&CompilerOption_Sanitizer::Address);
        let has_memory = opts.sanitizer.contains(&CompilerOption_Sanitizer::Memory);
        let has_thread = opts.sanitizer.contains(&CompilerOption_Sanitizer::Thread);

        if has_address && has_memory {
            exit_with_msg!("AddressSanitizer and MemorySanitizer cannot be used together".to_string());
        }
        if has_address && has_thread {
            exit_with_msg!("AddressSanitizer and ThreadSanitizer cannot be used together".to_string());
        }
        if has_memory && has_thread {
            exit_with_msg!("MemorySanitizer and ThreadSanitizer cannot be used together".to_string());
        }
    }

    // linker validation
    if opts.linker_options.link_static && opts.linker_options.pie {
        tui_warning(
            "Static linking with PIE may not be supported on all targets. Consider using '--static' without '--pie' or vice versa."
                .to_string(),
        );
    }

    if opts.linker_options.no_pie && opts.linker_options.pie {
        exit_with_msg!("Cannot specify both '--pie' and '--no-pie'.".to_string());
    }

    // profile validation
    match opts.profile {
        CompilerOption_Profile::Debug => {
            if let Some(opt_level) = opts.opt_level {
                match opt_level {
                    CompilerOption_Optimize::O0 => {}
                    CompilerOption_Optimize::O1
                    | CompilerOption_Optimize::O2
                    | CompilerOption_Optimize::O3
                    | CompilerOption_Optimize::Os
                    | CompilerOption_Optimize::Oz => {
                        tui_note("Debug profile with optimization may complicate debugging. consider using --optimize=O0 for debug builds.".to_string());
                    }
                }
            }
        }
        CompilerOption_Profile::Release => {
            if let Some(opt_level) = opts.opt_level {
                match opt_level {
                    CompilerOption_Optimize::O0 => {
                        tui_warning(
                            "Release profile with --optimize=O0 will produce unoptimized code. Consider using --optimize=O2 or --optimize=O3 for release builds."
                                .to_string(),
                        );
                    }
                    _ => {}
                }
            } else {
                tui_note("Release profile defaulting to --optimize=O1 optimization.".to_string());
            }
        }
    }

    // 5. Code model validation
    match opts.code_model {
        CompilerOption_CodeModel::Tiny => {
            if opts.reloc_mode != CompilerOption_RelocMode::Static {
                tui_warning(
                    "Tiny code model works best with static relocation forcing static relocation mode.".to_string(),
                );
            }
        }
        CompilerOption_CodeModel::Large => {
            if opts.reloc_mode == CompilerOption_RelocMode::PIC {
                tui_warning("Large code model with PIC may produce inefficient code".to_string());
            }
        }
        _ => {}
    }

    // job count validation
    if let Some(jobs) = opts.jobs {
        if jobs == 0 {
            exit_with_msg!("Job count must be at least 1.".to_string());
        }
        if jobs > num_cpus::get() {
            tui_warning(format!(
                "Job count ({}) exceeds available CPU cores ({}). performance may degrade.",
                jobs,
                num_cpus::get(),
            ));
        }
    }
}

mod num_cpus {
    pub fn get() -> usize {
        std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1)
    }
}
