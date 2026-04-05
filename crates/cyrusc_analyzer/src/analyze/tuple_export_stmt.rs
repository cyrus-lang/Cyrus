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
use cyrusc_ast::Mutability;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::VarDeclID,
    exprs::{TypedExprKind, TypedExprStmt, TypedTupleAccessExpr},
    format::format_sema_type,
    stmts::{TypedExportPattern, TypedExportPatternKind, TypedExportTupleStmt},
    types::{SemanticType, TypedTupleType, UnresolvedType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_export_tuple_values(&mut self, export_tuple: &mut TypedExportTupleStmt) -> Option<()> {
        let rhs = match export_tuple.rhs.as_mut() {
            Some(rhs) => rhs,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::TupleDestructionWithNoRhs),
                    loc: Some(export_tuple.loc),
                    hint: None,
                });
                return None;
            }
        };

        let expected_type = self.build_expected_tuple_type_from_pattern(&export_tuple.pattern);

        self.analyze_expr(rhs, expected_type)?;

        let Some(tuple_type) = rhs.sema_type.as_ref()?.as_tuple_type() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                loc: Some(export_tuple.loc),
                hint: None,
            });
            return None;
        };

        let patterns = match &export_tuple.pattern.kind {
            TypedExportPatternKind::Tuple(p) => p,
            _ => unreachable!(), // handled in parser
        };

        if patterns.len() != tuple_type.elements.len() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleExportedValuesAndTupleElementsCountMismatch),
                loc: Some(export_tuple.loc),
                hint: None,
            });
            return None;
        }

        let mut path = Vec::new();

        for (i, (pattern, sema_type)) in patterns.iter().zip(tuple_type.elements.iter()).enumerate() {
            path.clear();
            path.push(i);

            self.analyze_tuple_pattern(
                pattern,
                sema_type,
                rhs,
                export_tuple.is_const,
                export_tuple.loc,
                &mut path,
            );
        }

        Some(())
    }

    fn analyze_tuple_ident_pattern(
        &mut self,
        var_decl_id: VarDeclID,
        sema_type: &SemanticType,
        rhs: &TypedExprStmt,
        is_const: bool,
    ) {
        let ty = sema_type.clone();

        self.decl_tables.with_var_decl_mut(var_decl_id, |var_decl| {
            var_decl.ty = Some(ty);
            var_decl.rhs = Some(rhs.clone());
            var_decl.is_const = is_const;
        });
    }

    fn analyze_tuple_pattern(
        &mut self,
        pattern: &TypedExportPattern,
        sema_type: &SemanticType,
        root_expr: &TypedExprStmt,
        stmt_is_const: bool,
        loc: Loc,
        path: &mut Vec<usize>,
    ) {
        if let Some(explicit_type) = &pattern.ty {
            if !self.is_assignable_to(sema_type.clone(), explicit_type.clone(), loc) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                        lhs_type: format_sema_type(explicit_type.clone(), self.formatter),
                        rhs_type: format_sema_type(sema_type.clone(), self.formatter),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
                return;
            }
        }

        match &pattern.kind {
            TypedExportPatternKind::Ident(symbol_id) => {
                let rhs = self.tuple_access_expr(root_expr, path, loc);

                let Some(var_decl_id) = self.query.get_var(*symbol_id) else {
                    return;
                };

                let is_const = pattern
                    .mutability
                    .map(|mutability| match mutability {
                        Mutability::Const => true,
                        Mutability::Var => false,
                    })
                    .unwrap_or(stmt_is_const);

                self.analyze_tuple_ident_pattern(var_decl_id, sema_type, &rhs, is_const);
            }

            TypedExportPatternKind::Tuple(patterns) => {
                let Some(tuple_ty) = sema_type.as_tuple_type() else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                        loc: Some(loc),
                        hint: None,
                    });
                    return;
                };

                if patterns.len() != tuple_ty.elements.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::TupleExportedValuesAndTupleElementsCountMismatch),
                        loc: Some(loc),
                        hint: None,
                    });
                    return;
                }

                for (i, (sub_pattern, sub_ty)) in patterns.iter().zip(&tuple_ty.elements).enumerate() {
                    path.push(i);
                    self.analyze_tuple_pattern(sub_pattern, sub_ty, root_expr, stmt_is_const, loc, path);
                    path.pop();
                }
            }

            TypedExportPatternKind::Ignore => {}
        }
    }

    fn build_expected_tuple_type_from_pattern(&self, pattern: &TypedExportPattern) -> Option<SemanticType> {
        let TypedExportPatternKind::Tuple(elements) = &pattern.kind else {
            return None;
        };

        let mut result_type = Vec::with_capacity(elements.len());
        let mut has_explicit_type = false;

        for element in elements {
            if let Some(ty) = &element.ty {
                result_type.push(ty.clone());
                has_explicit_type = true;
                continue;
            }

            match &element.kind {
                TypedExportPatternKind::Tuple(_) => {
                    if let Some(inner) = self.build_expected_tuple_type_from_pattern(element) {
                        result_type.push(inner);
                        has_explicit_type = true;
                    } else {
                        result_type.push(SemanticType::Unresolved(UnresolvedType::Infer));
                    }
                }
                _ => {
                    result_type.push(SemanticType::Unresolved(UnresolvedType::Infer));
                }
            }
        }

        if !has_explicit_type {
            return None;
        }

        Some(SemanticType::Tuple(TypedTupleType {
            elements: result_type,
            loc: pattern.loc,
        }))
    }

    fn tuple_access_expr(&self, base_expr: &TypedExprStmt, access_path: &[usize], loc: Loc) -> TypedExprStmt {
        let mut expr = base_expr.clone();
        let mut current_type = expr.sema_type.clone();
        let mloc = expr.mloc;

        for &index in access_path {
            let Some(ty) = &current_type else {
                break;
            };

            let Some(tuple_ty) = ty.as_tuple_type() else {
                break;
            };

            let element_type = tuple_ty.elements.get(index).cloned();

            expr = TypedExprStmt {
                kind: TypedExprKind::TupleAccess(TypedTupleAccessExpr {
                    operand: Box::new(expr),
                    index,
                    loc,
                }),
                sema_type: element_type.clone(),
                mloc,
                loc,
            };

            current_type = element_type;
        }

        expr
    }
}
