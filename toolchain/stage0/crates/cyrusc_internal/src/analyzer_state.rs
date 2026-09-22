// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

/// Minimal interface exposed to subsystems such as const-eval.
pub trait AnalyzerState {
    fn func_name(&self) -> String;
    fn method_name(&self) -> String;
    fn module_name(&self) -> String;
    fn file_name(&self) -> String;
}
