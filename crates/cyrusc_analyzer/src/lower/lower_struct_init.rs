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

use crate::context::AnalysisContext;
use cyrusc_typed_ast::{
    decls::DeclID,
    exprs::{TypedExprKind, TypedExprStmt, TypedFieldInit, TypedStructInitExpr},
    stmts::TypedTypeArgs,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_unnamed_struct_value_as_struct_init(&self, typed_expr: &mut TypedExprStmt) {
        let TypedExprKind::UnnamedStructValue(struct_value) = &typed_expr.kind else {
            return;
        };

        let fields = struct_value
            .fields
            .iter()
            .map(|field| TypedFieldInit {
                name: field.name.clone(),
                value: *field.value.clone(),
                loc: field.loc,
            })
            .collect();

        *typed_expr = TypedExprStmt {
            kind: TypedExprKind::StructInit(TypedStructInitExpr {
                decl_id: DeclID::Struct(struct_value.struct_decl_id.unwrap()),
                type_args: TypedTypeArgs::new(),
                fields,
                loc: struct_value.loc,
            }),
            sema_type: typed_expr.sema_type.clone(),
            mloc: typed_expr.mloc,
            loc: typed_expr.loc,
        };
    }
}
