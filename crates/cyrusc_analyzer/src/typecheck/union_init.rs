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
    decls::{MethodDecls, UnionDecl, UnionDeclID},
    exprs::TypedUnnamedUnionValue,
    format::format_union_decl,
    stmts::TypedUnionField,
    types::{NamedType, SemanticType, TypeDeclID, UnresolvedType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_unnamed_union_value(
        &mut self,
        union_value: &mut TypedUnnamedUnionValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        // case 1: expected type is a union
        if let Some((union_decl_id, union_decl)) = self.infer_union_decl_from_expected_type(expected_type) {
            // look up the expected field
            if let Some(union_field) = union_decl.lookup_field(&union_value.name.value) {
                let expected_ty = Some(union_field.ty.clone());

                // analyze expression with expected type
                self.analyze_expr(&mut union_value.value, expected_ty);
            } else {
                // field does not exist in this union type
                let union_name = format_union_decl(&union_decl, self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: union_name,
                        field_name: union_value.name.as_string(),
                    }),
                    loc: Some(union_value.loc),
                    hint: None,
                });

                // still analyze expression with no expectation to avoid cascading errors
                self.analyze_expr(&mut union_value.value, None);
            }

            return Some(SemanticType::Named(NamedType {
                decl_id: TypeDeclID::Union(union_decl_id),
                type_args: None,
            }));
        }

        // case 2: no expected type: (create a new union decl)

        let mut union_decl = self.create_union_decl_from_unnamed_union_value(union_value);
        let union_decl_id = self.decl_tables.insert_union(union_decl.clone());

        self.analyze_expr(&mut union_value.value, None);

        if let Some(rhs_type) = &union_value.value.sema_type {
            let union_field = union_decl
                .lookup_field_mut(&union_value.name.value)
                .expect("synthesized union must contain the field");

            union_field.ty = rhs_type.clone();
        }

        // write updated decl back
        self.decl_tables.with_union_decl_mut(union_decl_id, |_decl| {
            *_decl = union_decl.clone();
        });

        Some(SemanticType::Named(NamedType {
            decl_id: TypeDeclID::Union(union_decl_id),
            type_args: None,
        }))
    }

    fn infer_union_decl_from_expected_type(
        &self,
        expected_type: Option<SemanticType>,
    ) -> Option<(UnionDeclID, UnionDecl)> {
        expected_type.and_then(|sema_type| {
            sema_type.as_named_type().and_then(|named_type| {
                named_type
                    .decl_id
                    .as_union()
                    .map(|id| (id, self.decl_tables.union_decl(id)))
            })
        })
    }

    fn create_union_decl_from_unnamed_union_value(&mut self, union_value: &TypedUnnamedUnionValue) -> UnionDecl {
        let fields = vec![TypedUnionField {
            name: union_value.name.as_string(),
            ty: SemanticType::Unresolved(UnresolvedType::Infer),
            loc: union_value.loc,
        }];

        UnionDecl {
            symbol_id: None,
            name: None,
            fields,
            impls: Vec::new(),
            methods: MethodDecls::new(),
            generic_params: None,
            modifiers: UnionModifiers {
                vis: Visibility::Public,
                repr_attr: None,
            },
            align: None,
            loc: union_value.loc,
        }
    }
}
