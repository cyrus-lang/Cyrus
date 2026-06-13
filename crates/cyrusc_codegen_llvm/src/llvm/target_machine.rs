// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_internal::compiler_options::{CompilerOption_CodeModel, CompilerOption_Optimize, CompilerOption_RelocMode};
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

pub(crate) fn llvm_opt_level(opt_level: CompilerOption_Optimize) -> OptimizationLevel {
    match opt_level {
        CompilerOption_Optimize::O0 => OptimizationLevel::None,    // O0
        CompilerOption_Optimize::O1 => OptimizationLevel::Less,    // O1
        CompilerOption_Optimize::O2 => OptimizationLevel::Default, // O2
        CompilerOption_Optimize::O3 => OptimizationLevel::Aggressive, // O3
        CompilerOption_Optimize::Os => OptimizationLevel::Default, // O2 with size optimization (handled via pipeline)
        CompilerOption_Optimize::Oz => OptimizationLevel::Default, // O2 with aggressive size optimization
    }
}
