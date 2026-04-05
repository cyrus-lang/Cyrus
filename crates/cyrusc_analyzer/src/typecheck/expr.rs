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
use cyrusc_internal::symbols::symbols::SymbolEntryKind;
use cyrusc_typed_ast::{
    exprs::{TypedExprKind, TypedExprStmt},
    types::{PlainType, SemanticType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_expr(
        &mut self,
        expr: &mut TypedExprStmt,
        mut expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        match &expr.kind {
            TypedExprKind::Symbol(symbol_expr) => {
                let symbol_entry = self.query.lookup_symbol_entry(symbol_expr.symbol_id).unwrap();

                debug_assert!(!matches!(symbol_entry.kind, SymbolEntryKind::Unresolved));

                if !symbol_entry.is_var_or_global_var() && !symbol_entry.is_func() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnknownSymbol {
                            symbol_name: self.formatter.format_symbol_name(symbol_expr.symbol_id),
                        }),
                        loc: Some(symbol_expr.loc),
                        hint: None,
                    });
                    return None;
                }
            }
            _ => {}
        };

        // if the expected type is a generic parameter with a default, use the default type
        if let Some(sema_type) = &expected_type {
            if let Some(generic_param) = sema_type.as_generic_param() {
                expected_type = generic_param.default.clone().map(|sema_type| *sema_type);
            }
        }

        self.analyze_expr_non_terminal(expr, expected_type)
    }

    pub(crate) fn analyze_expr_non_terminal(
        &mut self,
        expr: &mut TypedExprStmt,
        mut expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        if let Some(sema_type) = expected_type {
            expected_type = Some(sema_type.const_inner().clone());
        }

        self.lower_expr_pre_analysis(expr, expected_type.clone());

        let ty_opt = match &mut expr.kind {
            TypedExprKind::Symbol(symbol_expr) => self.resolve_symbol_type(symbol_expr.symbol_id, symbol_expr.loc),
            TypedExprKind::Assign(assign) => {
                self.analyze_assign(assign);
                assign.rhs.sema_type.clone()
            }
            TypedExprKind::Literal(literal) => self.analyze_literal(literal, expected_type),
            TypedExprKind::Prefix(prefix) => self.analyze_prefix(prefix, expected_type),
            TypedExprKind::Infix(infix) => self.analyze_infix(infix, expected_type),
            TypedExprKind::Unary(unary) => self.analyze_unary(unary),
            TypedExprKind::AddrOf(addr_of) => self.analyze_addr_of(addr_of),
            TypedExprKind::Deref(deref) => self.analyze_deref(deref),
            TypedExprKind::Array(array) => self.analyze_array(array, expected_type),
            TypedExprKind::ArrayIndex(array_index) => self.analyze_array_index(array_index),
            TypedExprKind::StructInit(struct_init) => self.analyze_struct_init(struct_init),

            TypedExprKind::UnnamedStructValue(unnamed_struct_value) => todo!(),
            TypedExprKind::UnnamedUnionValue(unnamed_union_value) => todo!(),
            TypedExprKind::UnnamedEnumValue(unnamed_enum_value) => todo!(),
            TypedExprKind::EnumStructVariantInit(struct_variant_init) => todo!(),
            TypedExprKind::MethodCall(typed_method_call) => todo!(),
            TypedExprKind::FieldAccess(typed_field_access) => todo!(),
            TypedExprKind::Dynamic(typed_dynamic_expr) => todo!(),

            TypedExprKind::FuncCall(func_call) => self.analyze_func_call(func_call),
            TypedExprKind::Lambda(lambda) => self.analyze_lambda(lambda),
            TypedExprKind::Tuple(tuple) => self.analyze_tuple_value(tuple, expected_type),
            TypedExprKind::TupleAccess(tuple_access) => self.analyze_tuple_access(tuple_access, expected_type),

            TypedExprKind::Builtin(_typed_builtin) => todo!(), // TODO

            // invalid expressions
            TypedExprKind::SemanticType(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidUsageOfTheSemanticType),
                    loc: Some(expr.loc),
                    hint: None,
                });
                return None;
            }
        };

        let normalized_type = self.normalize_sema_type(ty_opt.clone()?, expr.loc);
        expr.sema_type = Some(normalized_type.clone()?);

        if cfg!(debug_assertions) {
            if let Some(sema_type) = expr.sema_type.clone() {
                assert!(!sema_type.is_unresolved());
            }

            if expr.sema_type.is_none() {
                panic!("expr.sema_type is empty!");
            }
        }

        self.fold_const_expr(expr);

        self.lower_expr_post_analysis(expr);

        normalized_type
    }

    pub(crate) fn analyze_cond_expr(&mut self, cond: &mut TypedExprStmt) {
        if let Some(sema_type) = self.analyze_expr(cond, Some(SemanticType::Plain(PlainType::Bool))) {
            self.report_if_not_cond_expr(sema_type, cond.loc);
        }
    }
}
