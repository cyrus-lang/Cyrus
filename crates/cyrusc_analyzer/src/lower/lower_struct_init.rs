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
use cyrusc_ast::{abi::Visibility, modifiers::StructModifiers};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    decls::{MethodDecls, StructDecl},
    exprs::{TypedExpr, TypedExprKind, TypedFieldInit, TypedStructInitExpr, TypedUnnamedStructValue, ValueCategory},
    stmts::{TypedGenericParams, TypedStructField, TypedTypeArgs},
    types::{NamedType, SemaType, TypeDeclID},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_unnamed_struct_value_as_struct_init(&self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::UnnamedStructValue(struct_value) = &typed_expr.kind else {
            return;
        };

        let Some(operand) = &typed_expr.ty else {
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

        let struct_init = TypedStructInitExpr {
            operand: operand.clone(),
            fields,
            loc: struct_value.loc,
        };

        *typed_expr = TypedExpr {
            kind: TypedExprKind::StructInit(struct_init),
            ty: typed_expr.ty.clone(),
            val_cat: ValueCategory::RValue,
            loc: typed_expr.loc,
        };
    }

    pub(crate) fn lower_unnamed_struct_value_as_unnamed_struct_type(
        &mut self,
        mut struct_value: TypedUnnamedStructValue,
    ) -> Option<TypedExpr> {
        for field in &mut struct_value.fields {
            self.analyze_expr_non_terminal(&mut field.value, None);
        }

        let only_type_expr = struct_value
            .fields
            .iter_mut()
            .all(|field| field.value.kind.as_type_expr().is_some());

        if !only_type_expr {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MixedUnionFieldKinds),
                loc: Some(struct_value.loc),
                hint: None,
            });

            return Some(TypedExpr {
                kind: TypedExprKind::Poisoned,
                ty: None,
                val_cat: ValueCategory::Unknown,
                loc: struct_value.loc,
            });
        }

        let fields: Vec<TypedStructField> = struct_value
            .fields
            .iter_mut()
            .map(|field| {
                let ty = field.value.ty.clone().unwrap();

                TypedStructField {
                    name: field.name.clone(),
                    ty,
                    vis: Visibility::Public,
                    loc: field.loc,
                }
            })
            .collect();

        let struct_decl = StructDecl {
            name: None,
            fields,
            impls: Vec::new(),
            methods: MethodDecls::new(),
            generic_params: TypedGenericParams::new(),
            modifiers: StructModifiers {
                repr_attr: struct_value.repr_attr.clone(),
                vis: Visibility::Public,
            },
            align: struct_value.align,
            loc: struct_value.loc,
            is_normalized: false,
        };

        let struct_decl_id = self.decl_tables.insert_struct(struct_decl);

        let ty = SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: TypedTypeArgs::new(),
        });

        Some(TypedExpr {
            kind: TypedExprKind::SemaType(ty.clone()),
            ty: Some(ty),
            val_cat: ValueCategory::Unknown,
            loc: struct_value.loc,
        })
    }
}
