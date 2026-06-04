// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::Mutability;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::VarDeclID,
    exprs::{TypedExpr, TypedExprKind, TypedTupleAccessExpr},
    format::format_sema_type,
    stmts::{TypedTupleExportPattern, TypedTupleExportPatternKind, TypedTupleExportStmt},
    types::{SemaType, TypedTupleType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_export_tuple_values(&mut self, export_tuple: &mut TypedTupleExportStmt) -> Option<()> {
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

        let Some(tuple_type) = rhs.ty.as_ref()?.as_tuple_type() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                loc: Some(export_tuple.loc),
                hint: None,
            });
            return None;
        };

        let TypedTupleExportPatternKind::Tuple(patterns) = &export_tuple.pattern.kind else {
            unreachable!() // handled in parser
        };

        if patterns.len() != tuple_type.elements.len() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleExportedValuesAndTupleElementsCountMismatch {
                    expected: tuple_type.elements.len(),
                    provided: patterns.len(),
                }),
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
        sema_type: &SemaType,
        rhs: &TypedExpr,
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
        pattern: &TypedTupleExportPattern,
        sema_type: &SemaType,
        root_expr: &TypedExpr,
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
            TypedTupleExportPatternKind::Ident(var_decl_id) => {
                let rhs = self.tuple_access_expr(root_expr, path, loc);

                let is_const_opt = pattern.mutability.map(|mutability| match mutability {
                    Mutability::Const => true,
                    Mutability::Var => false,
                });

                if let Some(is_const) = is_const_opt
                    && !(is_const ^ stmt_is_const)
                {
                    let mutability = if is_const { "const" } else { "var" };

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::InvalidNestedExportTuple {
                            mutability: mutability.to_string(),
                        }),
                        loc: Some(loc),
                        hint: None,
                    });
                }

                self.analyze_tuple_ident_pattern(*var_decl_id, sema_type, &rhs, is_const_opt.unwrap_or(stmt_is_const));
            }

            TypedTupleExportPatternKind::Tuple(patterns) => {
                let Some(tuple_type) = sema_type.as_tuple_type() else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                        loc: Some(loc),
                        hint: None,
                    });
                    return;
                };

                if patterns.len() != tuple_type.elements.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::TupleExportedValuesAndTupleElementsCountMismatch {
                            expected: tuple_type.elements.len(),
                            provided: patterns.len(),
                        }),
                        loc: Some(loc),
                        hint: None,
                    });
                    return;
                }

                for (i, (sub_pattern, sub_ty)) in patterns.iter().zip(&tuple_type.elements).enumerate() {
                    path.push(i);
                    self.analyze_tuple_pattern(sub_pattern, sub_ty, root_expr, stmt_is_const, loc, path);
                    path.pop();
                }
            }

            TypedTupleExportPatternKind::Ignore => {}
        }
    }

    fn build_expected_tuple_type_from_pattern(&mut self, pattern: &TypedTupleExportPattern) -> Option<SemaType> {
        let TypedTupleExportPatternKind::Tuple(elements) = &pattern.kind else {
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
                TypedTupleExportPatternKind::Tuple(_) => {
                    if let Some(inner) = self.build_expected_tuple_type_from_pattern(element) {
                        result_type.push(inner);
                        has_explicit_type = true;
                    } else {
                        result_type.push(self.func_env.infer.as_mut().unwrap().new_var());
                    }
                }
                _ => {
                    result_type.push(self.func_env.infer.as_mut().unwrap().new_var());
                }
            }
        }

        if !has_explicit_type {
            return None;
        }

        Some(SemaType::Tuple(TypedTupleType {
            elements: result_type,
            loc: pattern.loc,
        }))
    }

    fn tuple_access_expr(&self, base_expr: &TypedExpr, access_path: &[usize], loc: Loc) -> TypedExpr {
        let mut expr = base_expr.clone();
        let mut current_type = expr.ty.clone();
        let val_cat = expr.val_cat;

        for &index in access_path {
            let Some(ty) = &current_type else {
                break;
            };

            let Some(tuple_type) = ty.as_tuple_type() else {
                break;
            };

            let element_type = tuple_type.elements.get(index).cloned().unwrap();

            expr = TypedExpr {
                kind: TypedExprKind::TupleAccess(TypedTupleAccessExpr {
                    operand: Box::new(expr),
                    index,
                    loc,
                }),
                ty: Some(element_type.clone()),
                val_cat,
                loc,
            };

            current_type = Some(element_type);
        }

        expr
    }
}
