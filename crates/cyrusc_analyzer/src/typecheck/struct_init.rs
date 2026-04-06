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
    exprs::{TypedStructFieldInit, TypedStructInitExpr, TypedUnnamedStructValue},
    format::{format_missing_fields, format_sema_type, format_struct_decl},
    stmts::TypedStructField,
    types::{NamedType, SemanticType, TypeDeclID, UnresolvedType},
};
use fx_hash::FxHashSet;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_struct_init(&mut self, struct_init: &mut TypedStructInitExpr) -> Option<SemanticType> {
        let symbol_id = struct_init.symbol_id.unwrap();

        let Some(struct_decl_id) = self.query.get_struct(symbol_id) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonStructSymbol {
                    symbol_name: self.formatter.format_symbol_name(symbol_id),
                }),
                loc: Some(struct_init.loc),
                hint: None,
            });
            return None;
        };

        let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

        for field in &mut struct_init.fields {
            let expected_type = struct_decl.lookup_field(&field.name).map(|field| field.ty.clone());

            self.analyze_expr(&mut field.value, expected_type);
        }

        self.check_duplicate_struct_field_init(&struct_init.fields);
        self.check_missing_fields(&struct_decl, struct_init);
        self.check_invalid_fields(&struct_decl, struct_init);

        Some(SemanticType::Named(NamedType {
            decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: struct_init.type_args.clone(),
        }))
    }

    pub(crate) fn analyze_unnamed_struct_value(
        &mut self,
        struct_value: &mut TypedUnnamedStructValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        // case 1: if expected type is a struct, then use it's declaration
        // and analyze fields with expected type.
        if let Some((struct_decl_id, struct_decl)) = self.infer_struct_decl_from_expected_type(expected_type) {
            for struct_value_field in &mut struct_value.fields {
                if struct_value_field.ty.is_some() {
                    todo!();
                }

                if let Some(struct_field) = struct_decl.lookup_field(&struct_value_field.name) {
                    let expected_type = Some(struct_field.ty.clone());

                    self.analyze_expr(&mut struct_value_field.value, expected_type);
                } else {
                    // field does not exist in expected type
                    let struct_name = format_struct_decl(&struct_decl, self.formatter);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                            object_name: struct_name,
                            field_name: struct_value_field.name.clone(),
                        }),
                        loc: Some(struct_value_field.loc),
                        hint: None,
                    });
                }
            }

            self.check_invalid_fields_for_unnamed_struct_value(&struct_decl, struct_value);
            self.check_missing_fields_for_unnamed_struct_value(&struct_decl, struct_value);
            self.check_duplicate_fields_for_unnamed_struct_value(struct_value);

            return Some(SemanticType::Named(NamedType {
                decl_id: TypeDeclID::Struct(struct_decl_id),
                type_args: None,
            }));
        }

        // case 2: no expected type (create a new struct decl)

        let mut struct_decl = self.create_struct_decl_from_unnamed_struct_value(struct_value);
        let struct_decl_id = self.decl_tables.insert_struct(struct_decl.clone());

        for struct_value_field in &mut struct_value.fields {
            let struct_field = struct_decl.lookup_field_mut(&struct_value_field.name).unwrap();

            self.analyze_expr(&mut struct_value_field.value, None);

            if let Some(rhs_type) = &struct_value_field.value.sema_type {
                if let Some(explicit_type) = &struct_value_field.ty {
                    if !self.is_assignable_to(rhs_type.clone(), explicit_type.clone(), struct_value_field.loc) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                                lhs_type: format_sema_type(explicit_type.clone(), self.formatter),
                                rhs_type: format_sema_type(rhs_type.clone(), self.formatter),
                            }),
                            loc: Some(struct_value_field.loc),
                            hint: None,
                        });
                    }
                }

                struct_field.ty = rhs_type.clone();
            }
        }

        self.check_duplicate_fields_for_unnamed_struct_value(struct_value);

        // write back field types in struct decl
        self.decl_tables.with_struct_decl_mut(struct_decl_id, |_struct_decl| {
            *_struct_decl = struct_decl.clone();
        });

        Some(SemanticType::Named(NamedType {
            decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: None,
        }))
    }

    fn infer_struct_decl_from_expected_type(
        &self,
        expected_type: Option<SemanticType>,
    ) -> Option<(StructDeclID, StructDecl)> {
        expected_type.and_then(|sema_type| {
            sema_type.as_named_type().and_then(|named_type| {
                let id_opt = named_type.decl_id.as_struct();

                id_opt.map(|struct_decl_id| (struct_decl_id, self.decl_tables.struct_decl(struct_decl_id)))
            })
        })
    }

    fn create_struct_decl_from_unnamed_struct_value(&mut self, struct_value: &TypedUnnamedStructValue) -> StructDecl {
        let fields = struct_value
            .fields
            .iter()
            .map(|field| {
                TypedStructField {
                    name: field.name.clone(),
                    ty: SemanticType::Unresolved(UnresolvedType::Infer), // placeholder
                    vis: Visibility::Public,
                    loc: field.loc,
                }
            })
            .collect();

        StructDecl {
            symbol_id: None,
            name: None,
            fields,
            impls: Vec::new(),
            methods: MethodDecls::new(),
            generic_params: None,
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

    fn check_duplicate_struct_field_init(&self, fields: &[TypedStructFieldInit]) {
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
