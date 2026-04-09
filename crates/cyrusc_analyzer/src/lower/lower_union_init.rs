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
use cyrusc_typed_ast::exprs::{MemoryLocation, TypedExprKind, TypedExprStmt, TypedUnionInitExpr};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_struct_init_as_union_init(&mut self, typed_expr: &mut TypedExprStmt) {
        let TypedExprKind::StructInit(struct_init) = &typed_expr.kind else {
            return;
        };

        let Some(symbol_id) = struct_init.symbol_id else {
            return;
        };

        let Some(init_type) = self.resolve_symbol_type_expanded(symbol_id, struct_init.loc) else {
            return;
        };

        if init_type.as_union().is_none() {
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
            symbol_id: Some(symbol_id),
            type_args: struct_init.type_args.clone(),
            field: Box::new(field.clone()),
            loc: struct_init.loc,
        };

        *typed_expr = TypedExprStmt {
            kind: TypedExprKind::UnionInit(union_init),
            sema_type: None,
            mloc: MemoryLocation::RValue,
            loc: struct_init.loc,
        };
    }
}
