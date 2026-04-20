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
    exprs::{TypedExpr, TypedExprKind, TypedFieldInit, TypedUnionInitExpr, ValueCategory},
    stmts::TypedTypeArgs,
    types::{NamedType, SemaType, TypeDeclID},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_struct_init_as_union_init(&mut self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::StructInit(struct_init) = &typed_expr.kind else {
            return;
        };

        let operand = self.expand_sema_type(struct_init.operand.clone(), struct_init.loc);

        if operand.as_union().is_none() {
            return;
        }

        if struct_init.fields.len() != 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnionInitMustContainExactlyOneField),
                loc: Some(struct_init.loc),
                hint: None,
            });
            typed_expr.kind = TypedExprKind::Poisoned;
            return;
        }

        let field = struct_init.fields.first().unwrap();

        let union_init = TypedUnionInitExpr {
            operand,
            field: Box::new(field.clone()),
            loc: struct_init.loc,
        };

        *typed_expr = TypedExpr {
            kind: TypedExprKind::UnionInit(union_init),
            sema_type: None,
            val_cat: ValueCategory::RValue,
            loc: struct_init.loc,
        };
    }

    pub(crate) fn lower_unnamed_union_value_as_union_init(&self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::UnnamedUnionValue(union_value) = &typed_expr.kind else {
            return;
        };

        let field = TypedFieldInit {
            name: union_value.name.as_string(),
            value: *union_value.value.clone(),
            loc: union_value.loc,
        };

        *typed_expr = TypedExpr {
            kind: TypedExprKind::UnionInit(TypedUnionInitExpr {
                operand: SemaType::Named(NamedType {
                    type_decl_id: TypeDeclID::Union(union_value.union_decl_id.unwrap()),
                    type_args: TypedTypeArgs::new(),
                }),
                field: Box::new(field),
                loc: union_value.loc,
            }),
            sema_type: typed_expr.sema_type.clone(),
            val_cat: typed_expr.val_cat,
            loc: typed_expr.loc,
        };
    }
}
