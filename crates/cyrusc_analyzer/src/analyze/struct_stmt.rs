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
use cyrusc_ast::abi::{ReprAttr, ReprKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::{StructDecl, StructDeclID},
    format::{format_sema_type, format_struct_decl},
    stmts::{TypedStructField, TypedStructStmt, TypedTypeArgs},
    types::{NamedType, SemaType, TypeDeclID},
};
use fx_hash::FxHashSet;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_struct_stmt(&mut self, struct_stmt: &mut TypedStructStmt) {
        let Some(struct_decl_id) = self.query.get_struct(struct_stmt.symbol_id) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonStructSymbol {
                    symbol_name: self.formatter.format_symbol_name(struct_stmt.symbol_id),
                }),
                loc: Some(struct_stmt.loc),
                hint: None,
            });
            return;
        };

        let mut struct_decl = self.decl_tables.struct_decl(struct_decl_id);

        self.analyze_struct_decl(struct_decl_id, &mut struct_decl);

        struct_stmt.fields = struct_decl.fields.clone();

        self.decl_tables.with_struct_decl_mut(struct_decl_id, |_struct_decl| {
            *_struct_decl = struct_decl;
        });
    }

    pub(crate) fn analyze_struct_decl(&mut self, struct_decl_id: StructDeclID, struct_decl: &mut StructDecl) {
        self.validate_align(&struct_decl.align, struct_decl.loc);
        self.validate_struct_repr_attr(
            &struct_decl.modifiers.repr_attr,
            struct_decl.fields.len(),
            struct_decl.loc,
        );

        if let Some(struct_name) = &struct_decl.name {
            self.nameconv_check_struct_name(struct_name, struct_decl.loc);
        }

        let object_name = format_struct_decl(struct_decl, self.formatter);

        self.check_duplicate_struct_field(&object_name, &struct_decl.fields);

        self.analyze_struct_fields(struct_decl_id, &mut struct_decl.fields);

        self.analyze_object_implements_interfaces(&object_name, &struct_decl.impls, &struct_decl.methods);

        self.analyze_object_methods(&struct_decl.methods);
    }

    fn analyze_struct_fields(&mut self, struct_decl_id: StructDeclID, struct_fields: &mut [TypedStructField]) {
        for field in struct_fields {
            field.ty = match self.normalize_and_check_type_formation(field.ty.clone(), field.loc) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            if !field.ty.contains_generic_param() {
                self.validate_struct_field_type(struct_decl_id, &field.ty, field.loc);
            }
        }
    }

    fn check_duplicate_struct_field(&self, object_name: &String, fields: &[TypedStructField]) {
        let mut field_names: FxHashSet<&str> = FxHashSet::default();

        for field in fields {
            if !field_names.insert(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: object_name.clone(),
                        field_name: field.name.clone(),
                    }),
                    loc: Some(field.loc),
                    hint: None,
                });
            }
        }
    }

    fn validate_struct_repr_attr(&mut self, repr_attr: &Option<ReprAttr>, fields_count: usize, loc: Loc) {
        let Some(repr_attr) = repr_attr else {
            return;
        };

        if let Some(kind) = repr_attr.kind() {
            match kind {
                ReprKind::C | ReprKind::Cyrus => { /* valid */ }
                ReprKind::Transparent => {
                    // transparent is allowed on structs and requires exactly one field
                    if fields_count != 1 {
                        self.reporter.report(Diag {
                            kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                                err: "Repr 'transparent' structs must have exactly one field.".to_string(),
                            }),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some(
                                "Add or remove fields to have exactly one field, or remove the 'transparent' attribute."
                                    .to_string(),
                            ),
                        });
                    }

                    if repr_attr.is_packed() {
                        self.reporter.report(Diag {
                            kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                                err: "Cannot combine 'packed' with repr 'transparent' on structs.".to_string(),
                            }),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some("Remove either 'packed' or 'transparent'.".to_string()),
                        });
                    }
                }
            }
        }
    }

    pub(crate) fn validate_struct_field_type(
        &mut self,
        struct_decl_id: StructDeclID,
        sema_type: &SemaType,
        loc: Loc,
    ) {
        let sema_type = sema_type.const_inner();

        if sema_type.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VoidFieldType),
                loc: Some(loc),
                hint: None,
            });
        }

        let ty = NamedType {
            decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: TypedTypeArgs::new(),
        };

        if self.sema_type_contains_self_by_value(sema_type, ty) {
            let type_name = format_sema_type(sema_type.clone(), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InfiniteSizeRecursiveType { type_name }),
                loc: Some(loc),
                hint: None,
            });
            return;
        }

        self.check_type_arity(sema_type.clone(), loc);
    }
}
