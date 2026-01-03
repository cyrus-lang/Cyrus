// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
use cyrusc_compiler::options::{CodeModelOptions, RelocModeOptions};
use inkwell::{
    OptimizationLevel,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
};

/// Get an LLVM TargetMachine based on optional CPU, features, reloc, code model.
/// Defaults to host if CPU/target_triple is None.
pub(crate) fn create_target_machine(
    cpu: Option<String>,
    target_triple: Option<String>,
    reloc: RelocMode,
    code_model: CodeModel,
    opt_level: OptimizationLevel,
) -> TargetMachine {
    Target::initialize_all(&InitializationConfig::default());

    let triple = match target_triple {
        Some(t) => TargetTriple::create(&t),
        None => TargetMachine::get_default_triple(),
    };

    let target = Target::from_triple(&triple).expect("Failed to get LLVM Target from triple.");

    let cpu_name = cpu.unwrap_or(TargetMachine::get_host_cpu_name().to_str().unwrap().to_string());
    let features = TargetMachine::get_host_cpu_features().to_str().unwrap().to_string();

    target
        .create_target_machine(&triple, &cpu_name, &features, opt_level, reloc, code_model)
        .expect("Failed to create TargetMachine")
}

pub(crate) fn llvm_reloc_mode(reloc_mode: RelocModeOptions) -> RelocMode {
    match reloc_mode {
        RelocModeOptions::Default => RelocMode::Default,
        RelocModeOptions::Static => RelocMode::Static,
        RelocModeOptions::PIC => RelocMode::PIC,
        RelocModeOptions::DynamicNoPic => RelocMode::DynamicNoPic,
    }
}

pub(crate) fn llvm_code_model(code_model: CodeModelOptions) -> CodeModel {
    match code_model {
        CodeModelOptions::Default => CodeModel::Default,
        CodeModelOptions::Tiny => CodeModel::Default,
        CodeModelOptions::Small => CodeModel::Small,
        CodeModelOptions::Kernel => CodeModel::Kernel,
        CodeModelOptions::Medium => CodeModel::Medium,
        CodeModelOptions::Large => CodeModel::Large,
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
