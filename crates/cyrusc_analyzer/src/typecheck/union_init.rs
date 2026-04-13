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
    exprs::{TypedUnionInitExpr, TypedUnnamedUnionValue},
    format::format_union_decl,
    stmts::{TypedGenericParams, TypedTypeArgs, TypedUnionField},
    types::{NamedType, SemaType, TypeDeclID},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_union_init(&mut self, union_init: &mut TypedUnionInitExpr) -> Option<SemaType> {
        let decl_id = union_init.decl_id.unwrap();

        let init_type = self.resolve_symbol_type_expanded(decl_id, union_init.loc)?;

        let Some(named_type) = init_type.as_named_type() else {
            self.report_non_union_symbol(decl_id, union_init.loc);
            return None;
        };

        let Some(union_decl_id) = named_type.decl_id.as_union() else {
            self.report_non_union_symbol(decl_id, union_init.loc);
            return None;
        };

        if union_init.type_args.is_empty() {
            union_init.type_args = named_type.type_args.clone();
        }

        let union_decl = self.decl_tables.union_decl(union_decl_id);

        let union_name = format_union_decl(&union_decl, self.formatter);

        self.normalize_type_args(&mut union_init.type_args);

        let generic_env = self.create_inference_generic_env(
            &union_name,
            union_decl.generic_params.clone(),
            &union_init.type_args,
            union_init.loc,
        )?;

        self.with_generic_env(generic_env, |this| {
            let Some(expected_type) = union_decl
                .lookup_field(&union_init.field.name)
                .map(|field| this.substitute_type(&field.ty))
            else {
                todo!();
            };

            this.analyze_field_assign(&mut union_init.field.value, expected_type, union_init.field.loc);

            this.apply_generic_defaults(union_decl.generic_params.clone());

            let final_type_args = this.collect_instantiated_type_args(union_decl.generic_params);

            let final_type = SemaType::Named(NamedType {
                decl_id: TypeDeclID::Union(union_decl_id),
                type_args: final_type_args,
            });

            Some(this.func_env.infer.as_mut().unwrap().resolve(&final_type))
        })
    }

    pub(crate) fn analyze_unnamed_union_value(
        &mut self,
        union_value: &mut TypedUnnamedUnionValue,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        if let Some(union_decl) = self.infer_union_decl_from_expected_type(expected_type.clone()) {
            let type_args = if let Some(SemaType::Named(named)) = &expected_type {
                named.type_args.clone()
            } else {
                TypedTypeArgs::new()
            };

            let union_name = format_union_decl(&union_decl, self.formatter);

            let generic_env = self.create_inference_generic_env(
                &union_name,
                union_decl.generic_params.clone(),
                &type_args,
                union_value.loc,
            )?;

            return self.with_generic_env(generic_env, |this| {
                if let Some(union_field) = union_decl.lookup_field(&union_value.name.value) {
                    let expected_type = this.substitute_type(&union_field.ty);

                    this.analyze_field_assign(&mut union_value.value, expected_type, union_value.loc);
                } else {
                    let union_name = format_union_decl(&union_decl, this.formatter);

                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                            object_name: union_name,
                            field_name: union_value.name.as_string(),
                        }),
                        loc: Some(union_value.loc),
                        hint: None,
                    });

                    this.analyze_expr(&mut union_value.value, None);
                }

                Some(expected_type.unwrap())
            });
        }

        let mut union_decl = self.create_union_decl_from_unnamed_union_value(union_value);
        let union_decl_id = self.decl_tables.insert_union(union_decl.clone());

        self.analyze_expr(&mut union_value.value, None);

        if let Some(rhs_type) = &union_value.value.sema_type {
            let union_field = union_decl.lookup_field_mut(&union_value.name.value).unwrap();
            union_field.ty = rhs_type.clone();
        }

        self.decl_tables.with_union_decl_mut(union_decl_id, |_decl| {
            *_decl = union_decl.clone();
        });

        Some(SemaType::Named(NamedType {
            decl_id: TypeDeclID::Union(union_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    fn infer_union_decl_from_expected_type(&self, expected_type: Option<SemaType>) -> Option<UnionDecl> {
        expected_type.and_then(|sema_type| {
            sema_type
                .as_union()
                .map(|union_decl_id| self.decl_tables.union_decl(union_decl_id))
        })
    }

    fn create_union_decl_from_unnamed_union_value(&mut self, union_value: &TypedUnnamedUnionValue) -> UnionDecl {
        let fields = vec![TypedUnionField {
            name: union_value.name.as_string(),
            ty: SemaType::Placeholder,
            loc: union_value.loc,
        }];

        UnionDecl {
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
        }
    }
}
