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
    decls::{MethodDecls, StructDecl, StructDeclID},
    exprs::{TypedFieldInit, TypedStructInitExpr, TypedUnnamedStructValue},
    format::{format_missing_fields, format_struct_decl},
    stmts::{TypedGenericParams, TypedStructField, TypedTypeArgs},
    types::{NamedType, SemaType, TypeDeclID},
};
use fx_hash::FxHashSet;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_struct_init(&mut self, struct_init: &mut TypedStructInitExpr) -> Option<SemaType> {
        let init_type = self.resolve_symbol_type_expanded(struct_init.decl_id, struct_init.loc)?;

        let Some(named_type) = init_type.as_named_type() else {
            self.report_non_struct_symbol(struct_init.decl_id, struct_init.loc);
            return None;
        };

        let Some(struct_decl_id) = named_type.type_decl_id.as_struct() else {
            self.report_non_struct_symbol(struct_init.decl_id, struct_init.loc);
            return None;
        };

        // overwrite type args with expanded ones if none explicitly provided
        if struct_init.type_args.is_empty() {
            struct_init.type_args = named_type.type_args.clone();
        }

        let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

        let struct_name = format_struct_decl(&struct_decl, self.formatter);

        self.normalize_type_args(&mut struct_init.type_args);

        let generic_env = self.create_inference_generic_env(
            &struct_name,
            struct_decl.generic_params.clone(),
            &struct_init.type_args,
            struct_init.loc,
        )?;

        self.with_generic_env(generic_env, |this| {
            for field in &mut struct_init.fields {
                let Some(struct_field) = struct_decl.lookup_field(&field.name) else {
                    // unknown field, reported later
                    continue;
                };

                let expected_field_type = this.substitute_type(&struct_field.ty);

                this.analyze_field_assign(&mut field.value, expected_field_type, field.loc);
            }

            this.check_duplicate_struct_field_init(&struct_init.fields);
            this.check_missing_fields(&struct_decl, struct_init);
            this.check_invalid_fields(&struct_decl, struct_init);

            this.apply_generic_defaults(struct_decl.generic_params.clone());

            let final_type_args = this.collect_instantiated_type_args(struct_decl.generic_params);

            struct_init.type_args = final_type_args.clone();

            Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Struct(struct_decl_id),
                type_args: final_type_args,
            }))
        })
    }

    pub(crate) fn analyze_unnamed_struct_value(
        &mut self,
        struct_value: &mut TypedUnnamedStructValue,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        // case 1: if expected type is a struct, then use it's declaration
        // and analyze fields with expected type.
        if let Some((struct_decl_id, struct_decl)) = self.infer_struct_decl_from_expected_type(expected_type.clone()) {
            let type_args = if let Some(SemaType::Named(named)) = &expected_type {
                named.type_args.clone()
            } else {
                TypedTypeArgs::new()
            };

            let struct_name = format_struct_decl(&struct_decl, self.formatter);

            let generic_env = self.create_inference_generic_env(
                &struct_name,
                struct_decl.generic_params.clone(),
                &type_args,
                struct_value.loc,
            )?;

            return self.with_generic_env(generic_env, |this| {
                for struct_value_field in &mut struct_value.fields {
                    let Some(struct_field) = struct_decl.lookup_field(&struct_value_field.name) else {
                        // unknown field, reported later
                        continue;
                    };

                    let expected_field_type = this.substitute_type(&struct_field.ty);

                    this.analyze_field_assign(
                        &mut struct_value_field.value,
                        expected_field_type,
                        struct_value_field.loc,
                    );
                }

                this.check_invalid_fields_for_unnamed_struct_value(&struct_decl, struct_value);
                this.check_missing_fields_for_unnamed_struct_value(&struct_decl, struct_value);
                this.check_duplicate_fields_for_unnamed_struct_value(struct_value);

                struct_value.struct_decl_id = Some(struct_decl_id);

                Some(expected_type.unwrap())
            });
        }

        // case 2: no expected type (create a new struct decl)
        let mut struct_decl = self.create_struct_decl_from_unnamed_struct_value(struct_value);
        let struct_decl_id = self.decl_tables.insert_struct(struct_decl.clone());

        for struct_value_field in &mut struct_value.fields {
            let struct_field = struct_decl.lookup_field_mut(&struct_value_field.name).unwrap();

            self.analyze_expr(&mut struct_value_field.value, None);

            if let Some(rhs_type) = &struct_value_field.value.sema_type {
                struct_field.ty = rhs_type.clone();
            }
        }

        self.check_duplicate_fields_for_unnamed_struct_value(struct_value);

        // write back field types in struct decl
        self.decl_tables.with_struct_decl_mut(struct_decl_id, |_struct_decl| {
            *_struct_decl = struct_decl.clone();
        });

        struct_value.struct_decl_id = Some(struct_decl_id);

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    fn infer_struct_decl_from_expected_type(
        &self,
        expected_type: Option<SemaType>,
    ) -> Option<(StructDeclID, StructDecl)> {
        expected_type.and_then(|sema_type| {
            sema_type
                .as_struct()
                .map(|struct_decl_id| (struct_decl_id, self.decl_tables.struct_decl(struct_decl_id)))
        })
    }

    fn create_struct_decl_from_unnamed_struct_value(&mut self, struct_value: &TypedUnnamedStructValue) -> StructDecl {
        let fields = struct_value
            .fields
            .iter()
            .map(|field| TypedStructField {
                name: field.name.clone(),
                ty: SemaType::Placeholder,
                vis: Visibility::Public,
                loc: field.loc,
            })
            .collect();

        StructDecl {
            name: None,
            fields,
            impls: Vec::new(),
            methods: MethodDecls::new(),
            generic_params: TypedGenericParams::new(),
            modifiers: StructModifiers {
                vis: Visibility::Public,
                repr_attr: struct_value.repr_attr.clone(),
            },
            align: struct_value.align,
            loc: struct_value.loc,
        }
    }

    fn check_invalid_fields_for_unnamed_struct_value(
        &self,
        struct_decl: &StructDecl,
        struct_value: &TypedUnnamedStructValue,
    ) {
        let mut decl_field_names: FxHashSet<&str> = FxHashSet::default();

        for decl_field in &struct_decl.fields {
            decl_field_names.insert(decl_field.name.as_str());
        }

        let struct_name = format_struct_decl(struct_decl, self.formatter);

        for field in &struct_value.fields {
            let field_name = field.name.as_str();

            if !decl_field_names.contains(field_name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        object_name: struct_name.clone(),
                        field_name: field.name.clone(),
                    }),
                    loc: Some(field.loc),
                    hint: None,
                });
            }
        }
    }

    fn check_missing_fields_for_unnamed_struct_value(
        &self,
        struct_decl: &StructDecl,
        struct_value: &TypedUnnamedStructValue,
    ) {
        let mut decl_field_names: FxHashSet<&str> = FxHashSet::default();
        for decl_field in &struct_decl.fields {
            decl_field_names.insert(decl_field.name.as_str());
        }

        let mut initialized_field_names: FxHashSet<&str> = FxHashSet::default();
        for field in &struct_value.fields {
            initialized_field_names.insert(field.name.as_str());
        }

        let mut missing_fields: Vec<&str> = Vec::new();
        for name in decl_field_names {
            if !initialized_field_names.contains(name) {
                missing_fields.push(name);
            }
        }

        if !missing_fields.is_empty() {
            let struct_name = format_struct_decl(struct_decl, self.formatter);
            let missing_field_names = format_missing_fields(&missing_fields);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::StructMissingFields {
                    struct_name,
                    missing_field_names,
                }),
                loc: Some(struct_value.loc),
                hint: None,
            });
        }
    }

    fn check_duplicate_fields_for_unnamed_struct_value(&self, struct_value: &TypedUnnamedStructValue) {
        let mut field_names: FxHashSet<&str> = FxHashSet::default();

        for field in &struct_value.fields {
            if !field_names.insert(field.name.as_str()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldInitializer {
                        field_name: field.name.clone(),
                    }),
                    loc: Some(field.loc),
                    hint: None,
                });
            }
        }
    }

    fn check_invalid_fields(&self, struct_decl: &StructDecl, struct_init: &TypedStructInitExpr) {
        let mut decl_field_names: FxHashSet<&str> = FxHashSet::default();

        for decl_field in &struct_decl.fields {
            decl_field_names.insert(decl_field.name.as_str());
        }

        let struct_name = format_struct_decl(struct_decl, self.formatter);

        for init in &struct_init.fields {
            let field_name = init.name.as_str();

            if !decl_field_names.contains(field_name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        object_name: struct_name.clone(),
                        field_name: field_name.to_string(),
                    }),
                    loc: Some(init.loc),
                    hint: None,
                });
            }
        }
    }

    fn check_missing_fields(&self, struct_decl: &StructDecl, struct_init: &TypedStructInitExpr) {
        let mut decl_field_names: FxHashSet<&str> = FxHashSet::default();
        for decl_field in &struct_decl.fields {
            decl_field_names.insert(decl_field.name.as_str());
        }

        let mut initialized_field_names: FxHashSet<&str> = FxHashSet::default();
        for init in &struct_init.fields {
            initialized_field_names.insert(init.name.as_str());
        }

        let mut missing_fields: Vec<&str> = Vec::new();
        for name in decl_field_names {
            if !initialized_field_names.contains(name) {
                missing_fields.push(name);
            }
        }

        if !missing_fields.is_empty() {
            let struct_name = format_struct_decl(struct_decl, self.formatter);
            let missing_field_names = format_missing_fields(&missing_fields);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::StructMissingFields {
                    struct_name,
                    missing_field_names,
                }),
                loc: Some(struct_init.loc),
                hint: None,
            });
        }
    }

    fn check_duplicate_struct_field_init(&self, fields: &[TypedFieldInit]) {
        let mut field_names: FxHashSet<&str> = FxHashSet::default();

        for field in fields {
            if !field_names.insert(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldInitializer {
                        field_name: field.name.clone(),
                    }),
                    loc: Some(field.loc),
                    hint: None,
                });
            }
        }
    }
}
