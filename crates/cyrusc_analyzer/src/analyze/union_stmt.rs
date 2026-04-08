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
    decls::{UnionDecl, UnionDeclID},
    format::{format_sema_type, format_union_decl},
    stmts::{TypedTypeArgs, TypedUnionField, TypedUnionStmt},
    types::{NamedType, SemanticType, TypeDeclID},
};
use fx_hash::FxHashSet;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_union_stmt(&mut self, union_stmt: &mut TypedUnionStmt) {
        let Some(union_decl_id) = self.query.get_union(union_stmt.symbol_id) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonUnionSymbol {
                    symbol_name: self.formatter.format_symbol_name(union_stmt.symbol_id),
                }),
                loc: Some(union_stmt.loc),
                hint: None,
            });
            return;
        };

        let mut union_decl = self.decl_tables.union_decl(union_decl_id);

        self.analyze_union_decl(union_decl_id, &mut union_decl);

        union_stmt.fields = union_decl.fields.clone();

        self.decl_tables.with_union_decl_mut(union_decl_id, |_union_decl| {
            *_union_decl = union_decl;
        });
    }

    pub(crate) fn analyze_union_decl(&mut self, union_decl_id: UnionDeclID, union_decl: &mut UnionDecl) {
        self.validate_align(&union_decl.align, union_decl.loc);
        self.validate_union_repr_attr(&union_decl.modifiers.repr_attr, union_decl.fields.len(), union_decl.loc);

        if let Some(union_name) = &union_decl.name {
            self.nameconv_check_union_name(union_name, union_decl.loc);
        }

        let object_name = format_union_decl(union_decl, self.formatter);

        self.check_duplicate_union_field(&object_name, &union_decl.fields);

        self.analyze_union_fields(union_decl_id, &mut union_decl.fields);

        self.analyze_object_implements_interfaces(&object_name, &union_decl.impls, &union_decl.methods);

        self.analyze_object_methods(&union_decl.methods);
    }

    fn analyze_union_fields(&mut self, union_decl_id: UnionDeclID, union_fields: &mut [TypedUnionField]) {
        for field in union_fields {
            field.ty = match self.normalize_sema_type(field.ty.clone(), field.loc) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            self.validate_union_field_type(union_decl_id, &field.ty, field.loc);
        }
    }

    fn check_duplicate_union_field(&self, object_name: &String, fields: &[TypedUnionField]) {
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

    pub(crate) fn validate_union_repr_attr(&mut self, repr_attr: &Option<ReprAttr>, fields_count: usize, loc: Loc) {
        let Some(repr_attr) = repr_attr else {
            return;
        };

        // packed is not allowed on unions
        if repr_attr.is_packed() {
            self.reporter.report(Diag {
                kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                    err: "Packed layout is not supported for unions.".to_string(),
                }),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some("If you need explicit control over union layout, consider using 'repr(C)' with manual padding or a packed struct wrapper.".to_string()),
            });
            return;
        }

        if let Some(kind) = repr_attr.kind() {
            match kind {
                ReprKind::C | ReprKind::Cyrus => { /* valid */ }
                ReprKind::Transparent => {
                    // transparent unions require exactly one field
                    if fields_count != 1 {
                        self.reporter.report(Diag {
                            kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                                err: "Repr 'transparent' unions must have exactly one field.".to_string(),
                            }),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some("Add or remove fields to have exactly one field, or remove the 'transparent' attribute.".to_string()),
                        });
                        return;
                    }
                }
            }
        }
    }

    fn validate_union_field_type(&mut self, union_decl_id: UnionDeclID, sema_type: &SemanticType, loc: Loc) {
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
            decl_id: TypeDeclID::Union(union_decl_id),
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

        self.check_sema_ty(sema_type.clone(), loc);
    }
}
