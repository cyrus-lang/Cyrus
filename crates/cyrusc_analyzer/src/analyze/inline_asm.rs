// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_const_eval::value::is_expr_const_evaluable;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    exprs::{TypedInlineAsm, ValueCategory},
    types::{PlainType, SemaType, TypedTupleType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_inline_asm_stmt(&mut self, asm: &mut TypedInlineAsm) {
        self.validate_asm_template(asm);
        self.validate_asm_outputs(asm);
        self.validate_asm_inputs(asm);
        self.validate_asm_clobbers(asm);
        self.validate_asm_template_indices(asm);
        self.validate_asm_constraint_types(asm);
    }

    pub(crate) fn analyze_inline_asm_expr(&mut self, asm: &mut TypedInlineAsm) -> Option<SemaType> {
        self.validate_asm_template(asm);
        self.validate_asm_outputs(asm);
        self.validate_asm_inputs(asm);
        self.validate_asm_clobbers(asm);
        self.validate_asm_template_indices(asm);
        self.validate_asm_constraint_types(asm);

        match asm.outputs.len() {
            0 => Some(SemaType::Plain(PlainType::Void)),
            1 => asm.outputs[0].expr.ty.clone(),
            _ => {
                let elements = asm
                    .outputs
                    .iter()
                    .filter_map(|op| op.expr.ty.clone().map(|ty| (ty, op.loc)))
                    .collect::<Vec<_>>();

                if elements.len() != asm.outputs.len() {
                    return None;
                }
                Some(SemaType::Tuple(TypedTupleType { elements, loc: asm.loc }))
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
                    hint: Some("Output operand should be an lvalue.".to_string()),
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
                    hint: Some("Output constraints must start with '=' (write-only) or '+' (read-write).".to_string()),
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
                        "Input constraints must not start with '=' or '+'. Those are only for outputs.".to_string(),
                    ),
                });
            }
        }
    }

    fn validate_asm_clobbers(&self, asm: &TypedInlineAsm) {
        let mut seen: Vec<String> = Vec::new();
        for clobber in &asm.clobbers {
            let name = clobber.name.to_lowercase();
            if seen.contains(&name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Warning,
                    kind: Box::new(AnalyzerDiagKind::AsmDuplicateClobber {
                        name: clobber.name.clone(),
                    }),
                    loc: Some(clobber.loc),
                    hint: Some("Remove the duplicate clobber entry.".to_string()),
                });
            } else {
                seen.push(name);
            }
        }
    }

    fn validate_asm_template_indices(&self, asm: &TypedInlineAsm) {
        let total_operands = asm.outputs.len() + asm.inputs.len();

        for line in &asm.template {
            let mut chars = line.char_indices().peekable();
            while let Some((i, ch)) = chars.next() {
                if ch != '%' {
                    continue;
                }

                let mut num_str = String::new();
                while let Some(&(_, d)) = chars.peek() {
                    if d.is_ascii_digit() {
                        num_str.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if num_str.is_empty() {
                    continue;
                }
                if let Ok(index) = num_str.parse::<usize>() {
                    if index >= total_operands {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::AsmOperandIndexOutOfBounds {
                                index,
                                max: total_operands,
                            }),
                            loc: Some(asm.loc),
                            hint: Some(format!(
                                "You have {} output(s) and {} input(s), so valid indices are 0..{}.",
                                asm.outputs.len(),
                                asm.inputs.len(),
                                total_operands,
                            )),
                        });
                    }
                }
                let _ = i;
            }
        }
    }

    fn validate_asm_constraint_types(&self, asm: &TypedInlineAsm) {
        let all_operands = asm.outputs.iter().chain(asm.inputs.iter());

        for operand in all_operands {
            let raw = operand.constraint.trim_matches(|ch| ch == '{' || ch == '}');
            let bare = raw
                .trim_start_matches('=')
                .trim_start_matches('+')
                .trim_matches(|ch| ch == '{' || ch == '}');

            let ty = match &operand.expr.ty {
                Some(t) => t,
                None => continue,
            };

            let ok = match bare {
                "r" | "g" => is_integer_bool_or_pointer(ty),

                "m" => matches!(ty, SemaType::Pointer(_)),

                "i" => is_integer_bool_or_pointer(ty) && is_expr_const_evaluable(&operand.expr.kind),

                _ => true,
            };

            if !ok {
                let found = format!("{:?}", ty);
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AsmConstraintTypeMismatch {
                        constraint: bare.to_string(),
                        found,
                    }),
                    loc: Some(operand.loc),
                    hint: Some(match bare {
                        "r" | "g" => "Constraint 'r'/'g' requires an integer, bool, or pointer.".to_string(),
                        "m" => "Constraint 'm' requires a pointer.".to_string(),
                        "i" => "Constraint 'i' requires a compile-time constant integer.".to_string(),
                        _ => String::new(),
                    }),
                });
            }
        }
    }
}

fn is_integer_bool_or_pointer(ty: &SemaType) -> bool {
    match ty {
        SemaType::Pointer(_) => true,
        SemaType::Plain(p) => matches!(
            p,
            PlainType::Bool
                | PlainType::UIntPtr
                | PlainType::IntPtr
                | PlainType::ISize
                | PlainType::USize
                | PlainType::Int8
                | PlainType::Int16
                | PlainType::Int32
                | PlainType::Int64
                | PlainType::Int128
                | PlainType::UInt8
                | PlainType::UInt16
                | PlainType::UInt32
                | PlainType::UInt64
                | PlainType::UInt128
        ),
        SemaType::Const(inner) => is_integer_bool_or_pointer(inner),
        _ => false,
    }
}
