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
use cyrusc_compiler::options::{CompilerOption_CodeModel, CompilerOption_RelocMode};
use cyrusc_tui_utils::tui_error;
use inkwell::{
    OptimizationLevel,
    targets::{CodeModel, InitializationConfig, RelocMode, Target as InkwellTarget, TargetMachine, TargetTriple},
};
use std::process::exit;

/// Get an LLVM TargetMachine based on optional CPU, features, reloc, code model.
/// Defaults to host if CPU/target_triple is None.
pub(crate) fn create_target_machine(
    target: &InkwellTarget,
    target_triple: &TargetTriple,
    cpu: Option<String>,
    reloc: RelocMode,
    code_model: CodeModel,
    opt_level: OptimizationLevel,
) -> TargetMachine {
    InkwellTarget::initialize_all(&InitializationConfig::default());

    let cpu_name = cpu.unwrap_or(TargetMachine::get_host_cpu_name().to_str().unwrap().to_string());
    let features = TargetMachine::get_host_cpu_features().to_str().unwrap().to_string();

    match target.create_target_machine(target_triple, &cpu_name, &features, opt_level, reloc, code_model) {
        Some(target_machine) => target_machine,
        None => {
            tui_error("Failed to create LLVM Target Machine.".to_string());
            exit(1);
        }
    }
}

pub(crate) fn llvm_reloc_mode(reloc_mode: CompilerOption_RelocMode) -> RelocMode {
    match reloc_mode {
        CompilerOption_RelocMode::Default => RelocMode::Default,
        CompilerOption_RelocMode::Static => RelocMode::Static,
        CompilerOption_RelocMode::PIC => RelocMode::PIC,
        CompilerOption_RelocMode::DynamicNoPic => RelocMode::DynamicNoPic,
    }
}

pub(crate) fn llvm_code_model(code_model: CompilerOption_CodeModel) -> CodeModel {
    match code_model {
        CompilerOption_CodeModel::Default => CodeModel::Default,
        CompilerOption_CodeModel::Tiny => CodeModel::Default,
        CompilerOption_CodeModel::Small => CodeModel::Small,
        CompilerOption_CodeModel::Kernel => CodeModel::Kernel,
        CompilerOption_CodeModel::Medium => CodeModel::Medium,
        CompilerOption_CodeModel::Large => CodeModel::Large,
    }
}

pub(crate) fn llvm_opt_level(opt_level: u32) -> OptimizationLevel {
    match opt_level {
        0 => OptimizationLevel::Default,
        1 => OptimizationLevel::None,
        2 => OptimizationLevel::Less,
        3 => OptimizationLevel::Aggressive,
        _ => panic!("Unknown llvm optimization level."),
    }
}
