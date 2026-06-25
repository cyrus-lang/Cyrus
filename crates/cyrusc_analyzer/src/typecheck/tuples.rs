// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    exprs::{TypedTupleAccessExpr, TypedTupleExpr},
    types::{SemaType, TypedTupleType},
};

impl<'a> AnalysisContext<'a> {
    /// Analyzes tuple value expressions, inferring types from elements.
    ///
    /// Type-checks tuple literals by analyzing each element expression and
    /// constructing a tuple type from the element types. Uses expected type
    /// context to guide element type inference when available.
    pub(crate) fn analyze_tuple_value(
        &mut self,
        tuple_value: &mut TypedTupleExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let mut elements = Vec::new();

        let tuple_type_opt = match expected_type {
            Some(sema_type) => sema_type.as_tuple_type().cloned(),
            None => None,
        };

        for (i, expr) in &mut tuple_value.elements.iter_mut().enumerate() {
            let mut expected_type: Option<SemaType> = None;

            if let Some(tuple_type) = &tuple_type_opt {
                expected_type = tuple_type.elements.get(i).cloned().map(|(ty, _)| ty);
            }

            match self.analyze_expr(expr, expected_type) {
                Some(ty) => elements.push((ty, expr.loc)),
                None => continue,
            }
        }

        Some(SemaType::Tuple(TypedTupleType {
            elements,
            loc: tuple_value.loc,
        }))
    }

    /// Analyzes tuple member access expressions (e.g., `tuple.0`, `tuple.1`).
    ///
    /// Type-checks tuple indexing operations by verifying the operand is a tuple type
    /// and the index is within bounds for that tuple. Returns the type of the accessed
    /// element if the access is valid.
    pub(crate) fn analyze_tuple_access(
        &mut self,
        tuple_member_access: &mut TypedTupleAccessExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let operand_type = self.analyze_expr(&mut tuple_member_access.operand, expected_type)?;

        if !operand_type.const_inner().as_tuple_type().is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                loc: Some(tuple_member_access.loc),
                hint: None,
            });
            return None;
        }

        let tuple_type = operand_type.as_tuple_type().unwrap();

        // inbounds check for tuple type

        if tuple_type.elements.len() == 0 {
             self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MemberAccessOnEmptyTuple),
                loc: Some(tuple_member_access.loc),
                hint: None,
            });
            return None;
        }

        if tuple_member_access.index > (tuple_type.elements.len() - 1) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleIndexOutOfRange {
                    index: tuple_member_access.index.try_into().unwrap(),
                    length: tuple_type.elements.len(),
                }),
                loc: Some(tuple_member_access.loc),
                hint: None,
            });
            return None;
        }

        let (element_type, _) = tuple_type.elements.get(tuple_member_access.index).unwrap();

        Some(element_type.clone())
    }
}
