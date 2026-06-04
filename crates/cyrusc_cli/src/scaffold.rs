// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_compiler::{
    options::{compiler_options_from_scaffold, merge_compiler_options, validate_compiler_options_paths},
    vercheck::validate_compiler_version,
};
use cyrusc_diagcentral::exit_with_msg;
use cyrusc_internal::compiler_options::CompilerOptions;
use cyrusc_scaffold::version::CYRUS_COMPILER_VERSION;
use cyrusc_scaffold_parser::{PROJECT_FILE_PATH, ScaffoldConfig, parse_project_toml};
use cyrusc_tui_utils::tui_error;
use std::{
    env,
    path::{Path, PathBuf},
    process::exit,
};

pub(crate) fn project_file_required() {
    if !std::path::Path::new(PROJECT_FILE_PATH).exists() {
        exit_with_msg!(format!("'{}' not found in current directory.", PROJECT_FILE_PATH));
    }
}

pub(crate) fn compiler_option_from_scaffold_parser(base_path: Option<String>) -> Option<ScaffoldConfig> {
    let base_path = base_path.map(|path| Path::new(&path).to_path_buf()).unwrap_or_default();

    // resolve project file path
    let project_file = env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(&base_path)
        .join(PROJECT_FILE_PATH);

    if !project_file.exists() {
        return None;
    }

    match parse_project_toml(project_file) {
        Ok(scaffold_config) => Some(scaffold_config),
        Err(err) => {
            exit_with_msg!(format!("Scaffold Parse Error: {}", err.to_string()));
        }
    }
}

pub(crate) fn merge_and_validate_scaffold_config_with_codegen_options(
    opts: &mut CompilerOptions,
    scaffold_config_opt: &Option<ScaffoldConfig>,
) {
    let Some(scaffold_config) = scaffold_config_opt else {
        return;
    };

    let scaffold_compiler_options = compiler_options_from_scaffold(scaffold_config);

    *opts = merge_compiler_options(&opts, &scaffold_compiler_options);

    if let Err(err) = validate_compiler_version(CYRUS_COMPILER_VERSION.trim(), scaffold_compiler_options.cyrus_version)
    {
        tui_error(err);
        exit(1);
    }

    if validate_compiler_options_paths(opts).is_err() {
        exit(1);
    }
}
