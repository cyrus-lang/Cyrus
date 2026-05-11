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

use crate::{
    enums::{
        CliABIOption, CliCodeModelOption, CliModuleMergeModeOption, CliOptimizeLevelOption, CliProfileOption,
        CliRelocModeOption, CliSanitizerOption,
    },
    options::{CliCompilerOptions, CliLinkerOptions, get_current_dir_as_base_path},
};
use cyrusc_compiler::linker::default_linker;
use cyrusc_internal::compiler_options::*;

impl CliCompilerOptions {
    pub fn as_compiler_options(&self) -> CompilerOptions {
        let linker = self.linker.clone().unwrap_or(default_linker().to_string());
        let base_path = Some(self.base_path.clone().or(Some(get_current_dir_as_base_path())).unwrap());

        CompilerOptions {
            profile: self.profile.convert(),
            abi: Some(self.abi.convert()),
            module_kind: self
                .module_merge_mode
                .and_then(|module_merge_mode| match module_merge_mode {
                    CliModuleMergeModeOption::Unified => Some(CompilerOption_ModuleKind::Unified),
                    CliModuleMergeModeOption::Separate => Some(CompilerOption_ModuleKind::Separate),
                }),
            jobs: self.jobs,
            sanitizer: self.sanitizer.iter().map(|s| s.convert()).collect(),
            linker_flags: self.linkerflags.clone(),
            linker_options: CompilerOption_Linker::default(),
            linker: Some(linker),
            disable_modulefs_cache: self.disable_modulefs_cache,
            base_path,
            opt_level: match self.optimize {
                CliOptimizeLevelOption::None => None,
                CliOptimizeLevelOption::O1 => Some(1),
                CliOptimizeLevelOption::O2 => Some(2),
                CliOptimizeLevelOption::O3 => Some(3),
            },
            library_paths: self.library_path.clone(),
            libraries: self.libraries.clone(),
            source_dirs: self.source_dirs.clone(),
            project_name: None,
            project_version: None,
            cyrus_version: None,
            project_type: None,
            build_dir: {
                match self.build_dir.clone() {
                    Some(path) => CompilerOption_BuildDir::Provided(path),
                    None => CompilerOption_BuildDir::Default,
                }
            },
            debuginfo_enabled: self.debuginfo_enabled,
            quiet: self.quiet,
            verbose: self.verbose,
            stdlib_path: self.stdlib.clone(),
            display_target_machine: self.display_target_machine,
            disable_warnings: self.disable_warnings,
            reloc_mode: self.reloc_mode.convert(),
            code_model: self.code_model.convert(),
            target: if self.target.trim().is_empty() {
                None
            } else {
                Some(self.target.clone())
            },
            cpu: if self.cpu.trim().is_empty() {
                None
            } else {
                Some(self.cpu.clone())
            },
        }
    }
}

impl CliProfileOption {
    #[inline]
    pub fn convert(&self) -> CompilerOption_Profile {
        match self {
            CliProfileOption::Debug => CompilerOption_Profile::Debug,
            CliProfileOption::Release => CompilerOption_Profile::Release,
        }
    }
}

impl CliCodeModelOption {
    #[inline]
    pub fn convert(&self) -> CompilerOption_CodeModel {
        match self {
            CliCodeModelOption::Default => CompilerOption_CodeModel::Default,
            CliCodeModelOption::Tiny => CompilerOption_CodeModel::Tiny,
            CliCodeModelOption::Small => CompilerOption_CodeModel::Small,
            CliCodeModelOption::Kernel => CompilerOption_CodeModel::Kernel,
            CliCodeModelOption::Medium => CompilerOption_CodeModel::Medium,
            CliCodeModelOption::Large => CompilerOption_CodeModel::Large,
        }
    }
}

impl CliRelocModeOption {
    pub fn convert(&self) -> CompilerOption_RelocMode {
        match self {
            CliRelocModeOption::Default => CompilerOption_RelocMode::Default,
            CliRelocModeOption::Static => CompilerOption_RelocMode::Static,
            CliRelocModeOption::PIC => CompilerOption_RelocMode::PIC,
            CliRelocModeOption::DynamicNoPic => CompilerOption_RelocMode::DynamicNoPic,
        }
    }
}

impl CliLinkerOptions {
    #[inline]
    pub fn convert(&self) -> CompilerOption_Linker {
        CompilerOption_Linker {
            link_static: self.r#static,
            pie: self.pie,
            no_pie: self.no_pie,
        }
    }
}

impl CliSanitizerOption {
    #[inline]
    pub fn convert(&self) -> CompilerOption_Sanitizer {
        match self {
            CliSanitizerOption::Address => CompilerOption_Sanitizer::Address,
            CliSanitizerOption::Memory => CompilerOption_Sanitizer::Memory,
            CliSanitizerOption::Thread => CompilerOption_Sanitizer::Thread,
            CliSanitizerOption::HWAddress => CompilerOption_Sanitizer::HWAddress,
        }
    }
}

impl CliABIOption {
    #[inline]
    pub fn convert(&self) -> CompilerOption_CodeGenABI {
        match self {
            CliABIOption::Cyrus => CompilerOption_CodeGenABI::Cyrus,
            CliABIOption::C => CompilerOption_CodeGenABI::C,
        }
    }
}
