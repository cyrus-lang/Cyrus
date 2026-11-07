// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::context::AnalysisContext;
use cyrusc_typed_ast::exprs::{TypedExpr, TypedExprKind, ValueCategory};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_field_access_as_object_method(&mut self, expr: &mut TypedExpr) {
        let field_access = match &mut expr.kind {
            TypedExprKind::FieldAccess(field_access) if !field_access.is_thin_arrow => field_access,
            _ => return,
        };

        if field_access.operand.kind.as_type_expr().is_some() {
            let Some((function_address, _)) = self.analyze_field_access_as_object_method(field_access) else {
                return;
            };

            *expr = TypedExpr {
                kind: TypedExprKind::FunctionAddress(function_address),
                ty: None,
                val_cat: ValueCategory::RValue,
                analyzed: false,
                loc: field_access.loc,
            };
        }
    }
}
