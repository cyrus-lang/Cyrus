// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::{abi::Visibility, modifiers::UnionModifiers};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    decls::{MethodDecls, UnionDecl, UnionDeclID},
    exprs::{TypedUnionInitExpr, TypedUnnamedUnionValue, TypedUnnamedUnionValueField},
    format::{format_sema_type, format_union_decl},
    stmts::{TypedGenericParams, TypedTypeArgs, TypedUnionField},
    types::{NamedType, SemaType, TypeDeclID},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_union_init(
        &mut self,
        union_init: &mut TypedUnionInitExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let mut operand = self.normalize_and_check_type_formation(union_init.operand.clone(), union_init.loc, 0)?;

        // expand operand type
        operand = self.expand_sema_type(operand, union_init.loc);

        let Some(named_type) = operand.as_named_type() else {
            let symbol_name = format_sema_type(operand, self.formatter);
            self.report_non_union_symbol(symbol_name, union_init.loc);
            return None;
        };

        let Some(union_decl_id) = named_type.type_decl_id.as_union() else {
            let symbol_name = format_sema_type(operand, self.formatter);
            self.report_non_union_symbol(symbol_name, union_init.loc);
            return None;
        };

        let union_decl = self.decl_tables.union_decl(union_decl_id);

        let union_name = format_union_decl(&union_decl, self.formatter);

        let generic_env = self.create_inference_generic_env(
            &union_name,
            union_decl.generic_params.clone(),
            &named_type.type_args,
            union_init.loc,
        )?;

        self.with_generic_env(generic_env, |this| {
            let Some(expected_field_type) = union_decl
                .lookup_field(&union_init.field.name)
                .map(|field| this.substitute_type(&field.ty))
            else {
                this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        object_name: union_name.clone(),
                        field_name: union_init.field.name.clone(),
                    }),
                    loc: Some(union_init.field.loc),
                    hint: None,
                });
                return None;
            };

            this.analyze_field_assign(&mut union_init.field.value, expected_field_type, union_init.field.loc);

            let inferred_type_args = this.collect_instantiated_type_args(union_decl.generic_params.clone());

            this.unify_with_expected_type(
                SemaType::Named(NamedType {
                    type_decl_id: TypeDeclID::Union(union_decl_id),
                    type_args: inferred_type_args,
                }),
                &expected_type,
            );

            this.apply_generic_defaults(union_decl.generic_params.clone());

            let final_type_args = this.collect_instantiated_type_args(union_decl.generic_params);

            let final_type = SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Union(union_decl_id),
                type_args: final_type_args,
            });

            union_init.operand = final_type.clone();

            Some(final_type)
        })
    }

    pub(crate) fn analyze_unnamed_union_value(
        &mut self,
        union_value: &mut TypedUnnamedUnionValue,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        if union_value.fields.first().is_none() || union_value.fields.len() != 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnionInitMustContainExactlyOneField),
                loc: Some(union_value.loc),
                hint: None,
            });
            return None;
        }

        let union_field_value = union_value.fields.first_mut().unwrap();

        if let Some((union_decl_id, union_decl, type_args)) =
            self.infer_union_decl_from_expected_type(expected_type.clone())
        {
            let union_name = format_union_decl(&union_decl, self.formatter);

            let generic_env = self.create_inference_generic_env(
                &union_name,
                union_decl.generic_params.clone(),
                &type_args,
                union_value.loc,
            )?;

            return self.with_generic_env(generic_env, |this| {
                if let Some(union_field) = union_decl.lookup_field(&union_field_value.name) {
                    let expected_type = this.substitute_type(&union_field.ty);

                    this.analyze_field_assign(&mut union_field_value.value, expected_type, union_value.loc);
                } else {
                    let union_name = format_union_decl(&union_decl, this.formatter);

                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                            object_name: union_name,
                            field_name: union_field_value.name.clone(),
                        }),
                        loc: Some(union_value.loc),
                        hint: None,
                    });

                    this.analyze_expr(&mut union_field_value.value, None);
                }

                union_value.union_decl_id = Some(union_decl_id);

                Some(expected_type.unwrap())
            });
        }

        let mut union_decl = self.create_union_decl_from_unnamed_union_value(union_field_value);
        let union_decl_id = self.decl_tables.insert_union(union_decl.clone());

        self.analyze_expr(&mut union_field_value.value, None);

        if let Some(rhs_type) = &union_field_value.value.ty {
            let union_field = union_decl.lookup_field_mut(&union_field_value.name).unwrap();

            union_field.ty = rhs_type.clone();
        }

        self.decl_tables.with_union_decl_mut(union_decl_id, |_decl| {
            *_decl = union_decl.clone();
        });

        union_value.union_decl_id = Some(union_decl_id);

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Union(union_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    fn infer_union_decl_from_expected_type(
        &self,
        expected_type: Option<SemaType>,
    ) -> Option<(UnionDeclID, UnionDecl, TypedTypeArgs)> {
        expected_type.and_then(|ty| {
            ty.as_named_type().and_then(|named_type| {
                named_type.type_decl_id.as_union().map(|union_decl_id| {
                    (
                        union_decl_id,
                        self.decl_tables.union_decl(union_decl_id),
                        named_type.type_args.clone(),
                    )
                })
            })
        })
    }

    fn create_union_decl_from_unnamed_union_value(
        &mut self,
        union_field_value: &TypedUnnamedUnionValueField,
    ) -> UnionDecl {
        let fields = vec![TypedUnionField {
            name: union_field_value.name.clone(),
            ty: SemaType::Placeholder,
            loc: union_field_value.loc,
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
            loc: union_field_value.loc,

            is_normalized: false,
        }
    }
}
