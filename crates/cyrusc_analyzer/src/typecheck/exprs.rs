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
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::DeclID,
    exprs::{TypedExpr, TypedExprKind, TypedSymbolExpr},
    format::format_sema_type,
    types::{PlainType, SemaType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_expr(&mut self, expr: &mut TypedExpr, expected_type: Option<SemaType>) -> Option<SemaType> {
        match &mut expr.kind {
            TypedExprKind::Symbol(symbol_expr) => {
                let decl_id_opt = self.analyze_symbol_expr(symbol_expr);

                if let Some(decl_id) = decl_id_opt {
                    symbol_expr.to_resolved_decl_id(decl_id);

                    if !decl_id.is_var_or_global_var() && !decl_id.is_func() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::UnknownSymbol {
                                symbol_name: self.formatter.format_decl(decl_id),
                            }),
                            loc: Some(symbol_expr.loc()),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            _ => {}
        };

        self.analyze_expr_non_terminal(expr, expected_type)
    }

    pub(crate) fn analyze_expr_non_terminal(
        &mut self,
        expr: &mut TypedExpr,
        mut expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        if let Some(ty) = expected_type {
            expected_type = Some(ty.const_inner().clone());
        }

        self.lower_expr_pre_analysis(expr, expected_type.clone());

        let expr_type = match &mut expr.kind {
            TypedExprKind::Symbol(symbol_expr) => {
                let decl_id_opt = self.analyze_symbol_expr(symbol_expr);

                if let Some(decl_id) = decl_id_opt {
                    symbol_expr.to_resolved_decl_id(decl_id);

                    self.resolve_symbol_type(decl_id, symbol_expr.loc())
                } else {
                    None
                }
            }

            TypedExprKind::Assign(assign) => {
                self.analyze_assign(assign);
                assign.rhs.ty.clone()
            }
            TypedExprKind::Literal(literal) => self.analyze_literal(literal, expected_type.clone()),
            TypedExprKind::Prefix(prefix) => self.analyze_prefix(prefix, expected_type.clone()),
            TypedExprKind::Infix(infix) => self.analyze_infix(infix, expected_type.clone()),
            TypedExprKind::Unary(unary) => self.analyze_unary(unary),
            TypedExprKind::AddrOf(addr_of) => self.analyze_addr_of(addr_of),
            TypedExprKind::Deref(deref) => self.analyze_deref(deref),
            TypedExprKind::Array(array) => self.analyze_array(array, expected_type.clone()),
            TypedExprKind::ArrayIndex(array_index) => self.analyze_array_index(array_index),
            TypedExprKind::Dynamic(dynamic) => self.analyze_dynamic(dynamic, expected_type.clone()),
            TypedExprKind::MethodCall(method_call) => self.analyze_method_call(method_call),
            TypedExprKind::FieldAccess(field_access) => self.analyze_field_access(field_access),
            TypedExprKind::FuncCall(func_call) => self.analyze_func_call(func_call),
            TypedExprKind::Lambda(lambda) => self.analyze_lambda(lambda),
            TypedExprKind::Tuple(tuple) => self.analyze_tuple_value(tuple, expected_type.clone()),
            TypedExprKind::TupleAccess(tuple_access) => self.analyze_tuple_access(tuple_access, expected_type.clone()),
            TypedExprKind::EnumInit(enum_init) => self.analyze_enum_init(enum_init, expected_type.clone()),
            TypedExprKind::StructInit(struct_init) => self.analyze_struct_init(struct_init, expected_type.clone()),
            TypedExprKind::UnionInit(union_init) => self.analyze_union_init(union_init, expected_type.clone()),
            TypedExprKind::UnnamedStructValue(struct_value) => {
                self.analyze_unnamed_struct_value(struct_value, expected_type.clone())
            }
            TypedExprKind::UnnamedUnionValue(union_value) => {
                self.analyze_unnamed_union_value(union_value, expected_type.clone())
            }
            TypedExprKind::UnnamedEnumValue(enum_value) => {
                self.analyze_unnamed_enum_value(enum_value, expected_type.clone())
            }
            TypedExprKind::EnumStructVariantInit(struct_variant_init) => {
                self.analyze_enum_struct_variant_init(struct_variant_init)
            }

            // TODO
            TypedExprKind::Builtin(_typed_builtin) => todo!(),

            TypedExprKind::SemaType(ty) => return self.normalize_sema_type(ty.clone(), expr.loc),
            TypedExprKind::Poisoned => return None,
        };

        let expr_type = expr_type?;

        let normalized_type = self.normalize_and_check_type_formation(expr_type, expr.loc);

        expr.ty = Some(normalized_type.clone()?);

        // debug
        if cfg!(debug_assertions) {
            if let Some(ty) = expr.ty.clone() {
                assert!(!ty.is_unresolved());
            }
            if expr.ty.is_none() {
                panic!("expr.sema_type is empty!");
            }
        }

        self.fold_const_expr(expr);

        self.lower_expr_post_analysis(expr);

        normalized_type
    }

    pub(crate) fn analyze_symbol_expr(&self, symbol_expr: &TypedSymbolExpr) -> Option<DeclID> {
        symbol_expr
            .as_symbol_id()
            .and_then(|symbol_id| self.query.lookup_symbol_as_decl_id(symbol_id))
            .or(symbol_expr.as_decl_id())
    }

    pub(crate) fn analyze_cond_expr(&mut self, cond: &mut TypedExpr) {
        if let Some(sema_type) = self.analyze_expr(cond, Some(SemaType::Plain(PlainType::Bool))) {
            self.report_if_not_cond_expr(sema_type, cond.loc);
        }
    }

    pub(crate) fn analyze_field_assign(
        &mut self,
        field_expr: &mut TypedExpr,
        mut expected_type: SemaType,
        loc: Loc,
    ) -> Option<()> {
        expected_type = self.normalize_sema_type(expected_type.clone(), loc)?;

        expected_type = self.substitute_type(&expected_type);

        let Some(mut value_type) = self.analyze_expr(field_expr, Some(expected_type.clone())) else {
            return Some(());
        };

        value_type = self.substitute_type(&value_type);

        if let Some(infer) = &mut self.func_env.infer {
            infer.unify(&expected_type, &value_type);

            expected_type = infer.resolve(&expected_type);
            value_type = infer.resolve(&value_type);
        }

        expected_type = self.substitute_type(&expected_type);

        if !self.is_assignable_to(value_type.clone(), expected_type.clone(), loc) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                    lhs_type: format_sema_type(expected_type.clone(), self.formatter),
                    rhs_type: format_sema_type(value_type.clone(), self.formatter),
                }),
                loc: Some(loc),
                hint: None,
            });
        }

        Some(())
    }
}
