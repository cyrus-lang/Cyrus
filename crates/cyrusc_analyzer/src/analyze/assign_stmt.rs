// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::AssignKind;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{exprs::TypedAssignExpr, format::format_sema_type};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_assign(&mut self, assign: &mut TypedAssignExpr) {
        let lhs_type = match self.analyze_expr(&mut assign.lhs, None) {
            Some(sema_type) => sema_type,
            None => return,
        };

        let rhs_type = match self.analyze_expr(&mut assign.rhs, Some(lhs_type.clone())) {
            Some(sema_type) => sema_type,
            None => return,
        };

        let is_const = self.is_const_qualified_lvalue(&assign.lhs);

        if is_const {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotAssignToConstLValue),
                loc: Some(assign.loc),
                hint: None,
            });
        }

        assert!(assign.kind == AssignKind::Default);

        if !self.is_assignable_to(rhs_type.clone(), lhs_type.clone(), assign.loc) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                    lhs_type: format_sema_type(lhs_type, self.formatter),
                    rhs_type: format_sema_type(rhs_type, self.formatter),
                }),
                loc: Some(assign.loc),
                hint: None,
            });
        }

        if !assign.lhs.is_lvalue() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotAssignToNonValue),
                loc: Some(assign.loc),
                hint: None,
            });
        }
    }
}
