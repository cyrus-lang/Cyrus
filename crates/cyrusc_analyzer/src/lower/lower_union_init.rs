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
use cyrusc_ast::{abi::Visibility, modifiers::UnionModifiers};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    decls::{MethodDecls, UnionDecl},
    exprs::{TypedExpr, TypedExprKind, TypedFieldInit, TypedUnionInitExpr, TypedUnnamedUnionValue, ValueCategory},
    stmts::{TypedGenericParams, TypedTypeArgs, TypedUnionField},
    types::{NamedType, SemaType, TypeDeclID},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_struct_init_as_union_init(&mut self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::StructInit(struct_init) = &typed_expr.kind else {
            return;
        };

        let Some(mut operand_type) = self.normalize_sema_type(struct_init.operand.clone(), struct_init.loc) else {
            return;
        };

        operand_type = self.expand_sema_type(operand_type, struct_init.loc);

        if operand_type.as_union().is_none() {
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
            operand: operand_type,
            field: Box::new(field.clone()),
            loc: struct_init.loc,
        };

        *typed_expr = TypedExpr {
            kind: TypedExprKind::UnionInit(union_init),
            ty: None,
            val_cat: ValueCategory::RValue,
            loc: struct_init.loc,
        };
    }

    pub(crate) fn lower_unnamed_union_value_as_union_init(&self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::UnnamedUnionValue(union_value) = &typed_expr.kind else {
            return;
        };

        let Some(operand) = &typed_expr.ty else {
            return;
        };

        if union_value.fields.first().is_none() || union_value.fields.len() != 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnionInitMustContainExactlyOneField),
                loc: Some(union_value.loc),
                hint: None,
            });
            return;
        }

        let union_field = union_value.fields.first().unwrap();

        let field = TypedFieldInit {
            name: union_field.name.clone(),
            value: *union_field.value.clone(),
            loc: union_field.loc,
        };

        let union_init = TypedUnionInitExpr {
            operand: operand.clone(),
            field: Box::new(field),
            loc: union_value.loc,
        };

        *typed_expr = TypedExpr {
            kind: TypedExprKind::UnionInit(union_init),
            ty: typed_expr.ty.clone(),
            val_cat: ValueCategory::RValue,
            loc: typed_expr.loc,
        };
    }

    pub(crate) fn lower_unnamed_union_value_as_unnamed_union_type(
        &mut self,
        mut union_value: TypedUnnamedUnionValue,
    ) -> Option<TypedExpr> {
        for field in &mut union_value.fields {
            self.analyze_expr_non_terminal(&mut field.value, None);
        }

        let all_type_fields = union_value
            .fields
            .iter()
            .all(|field| field.value.kind.as_type_expr().is_some());

        let all_expr_fields = union_value
            .fields
            .iter()
            .all(|field| field.value.kind.as_type_expr().is_none());

        if all_expr_fields {
            // normal struct literal expression
            // skip lowering
            return None;
        }

        if !all_type_fields {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MixedUnionFieldKinds),
                loc: Some(union_value.loc),
                hint: None,
            });

            return Some(TypedExpr {
                kind: TypedExprKind::Poisoned,
                ty: None,
                val_cat: ValueCategory::Unknown,
                loc: union_value.loc,
            });
        }

        let fields = union_value
            .fields
            .iter()
            .map(|field| {
                let ty = field.value.kind.as_type_expr().unwrap();

                TypedUnionField {
                    name: field.name.clone(),
                    ty,
                    loc: field.loc,
                }
            })
            .collect();

        let union_decl = UnionDecl {
            name: None,
            fields,
            impls: Vec::new(),
            methods: MethodDecls::new(),
            generic_params: TypedGenericParams::new(),
            modifiers: UnionModifiers {
                vis: Visibility::Public,
                repr_attr: None,
            },
            align: None,
            loc: union_value.loc,
            is_normalized: false,
        };

        let union_decl_id = self.decl_tables.insert_union(union_decl);

        let ty = SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Union(union_decl_id),
            type_args: TypedTypeArgs::new(),
        });

        Some(TypedExpr {
            kind: TypedExprKind::SemaType {
                ty: ty.clone(),
                loc: union_value.loc,
            },
            ty: None,
            val_cat: ValueCategory::Unknown,
            loc: union_value.loc,
        })
    }
}
