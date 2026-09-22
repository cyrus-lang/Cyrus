// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, lower::lower_assign::lower_assign_to_infix_expr};
use cyrusc_ast::{AssignKind, operators::PrefixOperator};
use cyrusc_typed_ast::{
    exprs::{TypedExpr, TypedExprKind},
    types::SemaType,
};

pub(crate) mod lower_assign;
pub(crate) mod lower_enum_init;
pub(crate) mod lower_prefix_not;
pub(crate) mod lower_struct_init;
pub(crate) mod lower_union_init;

impl<'a> AnalysisContext<'a> {
    /// Rewrites special expression forms into their canonical AST representation.
    pub(crate) fn lower_expr_pre_analysis(&mut self, typed_expr: &mut TypedExpr, expected_type: Option<SemaType>) {
        match &mut typed_expr.kind {
            TypedExprKind::Assign(assign) => {
                if assign.kind != AssignKind::Default {
                    typed_expr.kind = lower_assign_to_infix_expr(assign);
                }
            }
            TypedExprKind::Prefix(prefix) => {
                if let PrefixOperator::Bang = prefix.op {
                    if let Some(lowered_typed_expr) = self.lower_prefix_not_pointer(expected_type.clone(), prefix) {
                        *typed_expr = lowered_typed_expr;
                    }
                };
            }
            TypedExprKind::UnnamedStructValue(unnamed_struct_value) => {
                if let Some(expr) = self.lower_unnamed_struct_value_as_unnamed_struct_type(unnamed_struct_value.clone())
                {
                    *typed_expr = expr;
                }
            }
            TypedExprKind::UnnamedUnionValue(unnamed_union_value) => {
                if let Some(expr) = self.lower_unnamed_union_value_as_unnamed_union_type(unnamed_union_value.clone()) {
                    *typed_expr = expr;
                }
            }

            _ => {
                self.lower_field_access_as_enum_init(typed_expr);
                self.lower_method_call_as_enum_init(typed_expr);
                self.lower_struct_init_as_union_init(typed_expr);
            }
        };
    }

    pub(crate) fn lower_expr_post_analysis(&mut self, typed_expr: &mut TypedExpr) {
        self.lower_unnamed_struct_value_as_struct_init(typed_expr);
        self.lower_unnamed_enum_value_as_enum_init(typed_expr);
        self.lower_unnamed_union_value_as_union_init(typed_expr);
        self.lower_enum_struct_variant_init_as_enum_init(typed_expr);
    }
}
