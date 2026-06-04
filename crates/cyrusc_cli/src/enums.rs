// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use clap::ValueEnum;
use serde::Deserialize;

#[derive(Copy, Clone, PartialEq, Eq, Debug, ValueEnum)]
pub(crate) enum CliEndiannessOption {
    Little,
    Big,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, ValueEnum)]
pub(crate) enum CliModuleMergeModeOption {
    Unified,
    Separate,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub(crate) enum CliSanitizerOption {
    Address,
    Memory,
    Thread,
    HWAddress,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub(crate) enum CliRelocModeOption {
    Default,
    Static,
    PIC,
    DynamicNoPic,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub(crate) enum CliOptimizeLevelOption {
    None,
    O1,
    O2,
    O3,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum CliProfileOption {
    Debug,
    Release,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum CliCodeModelOption {
    Default,
    Tiny,
    Small,
    Kernel,
    Medium,
    Large,
}

#[derive(Deserialize, Debug, Clone, ValueEnum)]
pub enum CliABIOption {
    Cyrus,
    C,
}

// default

impl Default for CliRelocModeOption {
    fn default() -> Self {
        Self::PIC
    }
}

// display

impl std::fmt::Display for CliOptimizeLevelOption {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CliOptimizeLevelOption::None => write!(f, "none"),
            CliOptimizeLevelOption::O1 => write!(f, "o1"),
            CliOptimizeLevelOption::O2 => write!(f, "o2"),
            CliOptimizeLevelOption::O3 => write!(f, "o3"),
        }
    }
}
