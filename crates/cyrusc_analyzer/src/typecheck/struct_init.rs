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
    decls::StructDecl,
    exprs::{TypedStructFieldInit, TypedStructInitExpr},
    format::{format_missing_fields, format_struct_decl},
    types::{NamedType, SemanticType, TypeDeclID},
};
use fx_hash::FxHashSet;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_struct_init(&mut self, struct_init: &mut TypedStructInitExpr) -> Option<SemanticType> {
        let struct_decl_id = struct_init.struct_decl_id.unwrap();
        let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

        self.check_duplicate_struct_field_init(&struct_init.fields);
        self.check_missing_fields(&struct_decl, struct_init);
        self.check_invalid_fields(&struct_decl, struct_init);

        Some(SemanticType::Named(NamedType {
            decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: struct_init.type_args.clone(),
        }))
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
                        struct_name: struct_name.clone(),
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
