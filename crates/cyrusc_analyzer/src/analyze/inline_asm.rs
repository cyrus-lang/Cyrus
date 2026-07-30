// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    exprs::{TypedInlineAsm, ValueCategory},
    types::{PlainType, SemaType},
};

impl<'a> AnalysisContext<'a> {

    pub(crate) fn analyze_inline_asm_stmt(&mut self, asm: &mut TypedInlineAsm) {
        self.validate_asm_template(asm);
        self.validate_asm_outputs(asm);
        self.validate_asm_inputs(asm);
    }

    pub(crate) fn analyze_inline_asm_expr(
        &mut self,
        asm: &mut TypedInlineAsm,
    ) -> Option<SemaType> {
        self.validate_asm_template(asm);
        self.validate_asm_outputs(asm);
        self.validate_asm_inputs(asm);

        match asm.outputs.len() {
            0 => Some(SemaType::Plain(PlainType::Void)),
            1 => asm.outputs[0].expr.ty.clone(),
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AsmMultipleOutputsNotSupported),
                    loc: Some(asm.loc),
                    hint: Some(
                        "Multiple output operands are not supported when @asm is used." //will be back to it 
                            .to_string(),
                    ),
                });
                None
            }
        }
    }

    fn validate_asm_template(&self, asm: &TypedInlineAsm) {
        if asm.template.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::AsmEmptyTemplate),
                loc: Some(asm.loc),
                hint: None,
            });
        }
    }

    fn validate_asm_outputs(&mut self, asm: &mut TypedInlineAsm) {
        for operand in &mut asm.outputs {
            
            self.analyze_expr_non_terminal(&mut operand.expr, None);

            if !matches!(operand.expr.val_cat, ValueCategory::LValue(_)) {
                self.reporter.report(Diag {
                    level: DiagLevel::Warning,
                    kind: Box::new(AnalyzerDiagKind::AsmOutputNotLValue),
                    loc: Some(operand.loc),
                    hint: Some(
                        "Output operand should be an lvalue."
                            .to_string(),
                    ),
                });
            }

            let c = operand.constraint.trim_matches(|ch| ch == '{' || ch == '}');
            if !c.starts_with('=') && !c.starts_with('+') {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AsmInvalidOutputConstraint {
                        constraint: operand.constraint.clone(),
                    }),
                    loc: Some(operand.loc),
                    hint: Some(
                        "Output constraints must start with '=' (write-only) or '+' (read-write)."
                            .to_string(),
                    ),
                });
            }
        }
    }

    fn validate_asm_inputs(&mut self, asm: &mut TypedInlineAsm) {
        for operand in &mut asm.inputs {
            
            self.analyze_expr_non_terminal(&mut operand.expr, None);

            let c = operand.constraint.trim_matches(|ch| ch == '{' || ch == '}');
            if c.starts_with('=') || c.starts_with('+') {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AsmInvalidInputConstraint {
                        constraint: operand.constraint.clone(),
                    }),
                    loc: Some(operand.loc),
                    hint: Some(
                        "Input constraints must not start with '=' or '+'. Those are only for outputs."
                            .to_string(),
                    ),
                });
            }
        }
    }
}