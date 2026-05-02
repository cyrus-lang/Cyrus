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
        let mut elements: Vec<SemaType> = Vec::new();

        let tuple_type_opt = match expected_type {
            Some(sema_type) => sema_type.as_tuple_type().cloned(),
            None => None,
        };

        for (i, expr) in &mut tuple_value.elements.iter_mut().enumerate() {
            let mut expected_type: Option<SemaType> = None;

            if let Some(tuple_type) = &tuple_type_opt {
                expected_type = tuple_type.elements.get(i).cloned();
            }

            match self.analyze_expr(expr, expected_type) {
                Some(sema_type) => elements.push(sema_type),
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

        let element_type = tuple_type.elements.get(tuple_member_access.index).unwrap();

        Some(element_type.clone())
    }
}
