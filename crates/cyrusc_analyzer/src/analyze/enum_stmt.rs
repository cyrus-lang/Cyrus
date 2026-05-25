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
    decls::{EnumDecl, EnumDeclID},
    format::{format_enum_decl, format_sema_type},
    stmts::{TypedEnumStmt, TypedEnumVariant, TypedTypeArgs},
    types::{NamedType, SemaType, TypeDeclID},
};
use fx_hash::{FxHashSet, FxHashSetExt};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_enum_stmt(&mut self, enum_stmt: &mut TypedEnumStmt) {
        let mut enum_decl = self.decl_tables.enum_decl(enum_stmt.enum_decl_id);

        if enum_stmt.is_generic() {
            let object_type = SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Enum(enum_stmt.enum_decl_id),
                type_args: TypedTypeArgs::new(),
            });

            self.with_object(Some(object_type), |this| {
                this.analyze_enum_decl(enum_stmt.enum_decl_id, &mut enum_decl);
            })
        } else {
            self.analyze_enum_decl(enum_stmt.enum_decl_id, &mut enum_decl);
        }

        enum_stmt.variants = enum_decl.variants.clone();
        enum_stmt.tag_type = enum_decl.tag_type.clone();

        self.decl_tables
            .with_enum_decl_mut(enum_stmt.enum_decl_id, |_enum_decl| {
                *_enum_decl = enum_decl;
            });
    }

    pub(crate) fn analyze_enum_decl(&mut self, enum_decl_id: EnumDeclID, enum_decl: &mut EnumDecl) {
        let is_repr_c = enum_decl.is_repr_c();

        self.validate_align(&enum_decl.align, enum_decl.loc);
        self.validate_enum_repr_attr(&enum_decl.modifiers.repr_attr, enum_decl.align.is_some(), enum_decl.loc);
        self.validate_enum_tag_type(&enum_decl.tag_type, enum_decl.loc);

        if let Some(enum_name) = &enum_decl.name {
            self.nameconv_check_enum_name(enum_name, enum_decl.loc);
        }

        let object_name = format_enum_decl(enum_decl, self.formatter);

        self.check_duplicate_enum_variants(&object_name, &mut enum_decl.variants);

        self.analyze_enum_variants(enum_decl_id, &mut enum_decl.variants, &enum_decl.tag_type, is_repr_c);

        self.analyze_generic_bounds(&enum_decl.generic_params);

        self.analyze_object_implements_interfaces(
            &object_name,
            enum_decl.is_generic(),
            &enum_decl.impls,
            &enum_decl.methods,
        );

        let object_type_decl_id = TypeDeclID::Enum(enum_decl_id);

        if !enum_decl.is_generic() {
            self.analyze_object_methods(object_type_decl_id, &enum_decl.methods);
        }

        self.decl_tables.with_enum_decl_mut(enum_decl_id, |_enum_decl| {
            _enum_decl.variants = enum_decl.variants.clone();
        });
    }

    #[inline]
    fn analyze_enum_variants(
        &mut self,
        enum_decl_id: EnumDeclID,
        variants: &mut [TypedEnumVariant],
        tag_type_opt: &Option<SemaType>,
        is_repr_c: bool,
    ) {
        for variant in variants {
            self.analyze_enum_variant(enum_decl_id, variant, tag_type_opt, is_repr_c);
        }
    }

    pub(crate) fn analyze_enum_variant(
        &mut self,
        enum_decl_id: EnumDeclID,
        variant: &mut TypedEnumVariant,
        tag_type_opt: &Option<SemaType>,
        is_repr_c: bool,
    ) {
        match variant {
            TypedEnumVariant::Unit(_) => { /* skip */ }
            TypedEnumVariant::Valued { ident, value } => {
                self.analyze_expr(value, tag_type_opt.clone());

                if let Some(value_type) = &value.ty {
                    if is_repr_c && !value_type.is_integer() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                            loc: Some(ident.loc),
                            hint: None,
                        });
                    }
                }

                if let (Some(value_type), Some(tag_type)) = (&value.ty, tag_type_opt) {
                    if !self.is_assignable_to(value_type.clone(), tag_type.clone(), value.loc) {
                        let got_type = format_sema_type(value_type.clone(), self.formatter);
                        let expected_type = format_sema_type(tag_type.clone(), self.formatter);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::InvalidEnumVariantValueType {
                                got_type,
                                expected_type,
                            }),
                            loc: Some(value.loc),
                            hint: None,
                        });
                    }
                }
            }
            TypedEnumVariant::Tuple { ident, fields } => {
                if is_repr_c {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                        loc: Some(ident.loc),
                        hint: None,
                    });
                }

                for tuple_field in fields {
                    if !tuple_field.ty.contains_generic_param() {
                        tuple_field.ty =
                            match self.normalize_and_check_type_formation(tuple_field.ty.clone(), tuple_field.loc) {
                                Some(ty) => ty,
                                None => continue,
                            };

                        self.validate_enum_variant_field_type(enum_decl_id, &tuple_field.ty, tuple_field.loc);
                    }
                }
            }
            TypedEnumVariant::Struct { ident, fields } => {
                if is_repr_c {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                        loc: Some(ident.loc),
                        hint: None,
                    });
                }

                for struct_field in fields {
                    if !struct_field.ty.contains_generic_param() {
                        struct_field.ty =
                            match self.normalize_and_check_type_formation(struct_field.ty.clone(), struct_field.loc) {
                                Some(ty) => ty,
                                None => continue,
                            };

                        self.validate_enum_variant_field_type(enum_decl_id, &struct_field.ty, struct_field.loc);
                    }
                }
            }
        }
    }

    fn check_duplicate_enum_variants(&mut self, object_name: &String, variants: &[TypedEnumVariant]) {
        let mut variant_names: FxHashSet<&str> = FxHashSet::new();

        for variant in variants {
            let ident = variant.ident();
            let (name, loc) = (&ident.value, ident.loc);

            if !variant_names.insert(name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantName {
                        enum_name: object_name.clone(),
                        variant_name: name.clone(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
            }
        }
    }

    fn validate_enum_repr_attr(&mut self, repr_attr: &Option<ReprAttr>, has_align: bool, loc: Loc) {
        let Some(repr_attr) = repr_attr else {
            return;
        };

        // packed is not allowed on enums
        if repr_attr.is_packed() {
            self.reporter.report(Diag {
                kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                    err: "Packed layout is not supported for enums.".to_string(),
                }),
                level: DiagLevel::Error,
                loc: Some(loc),
                hint: Some(
                    "If you need packed enum-like behavior, consider using a manually packed struct with a tag field."
                        .to_string(),
                ),
            });
            return;
        }

        if let Some(kind) = repr_attr.kind() {
            match kind {
                ReprKind::C | ReprKind::Cyrus => {
                    if has_align {
                        self.reporter.report(Diag {
                            kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                                err: "Cannot specify alignment with 'c' or 'cyrus' enum layout. Alignment is determined by the target ABI.".to_string(),
                            }),
                            level: DiagLevel::Error,
                            loc: Some(loc),
                            hint: Some("Remove the alignment specifier.".to_string()),
                        });
                        return;
                    }
                }
                ReprKind::Transparent => {
                    self.reporter.report(Diag {
                        kind: Box::new(AnalyzerDiagKind::InvalidReprAttr {
                            err: "Repr 'transparent' cannot be applied to enums. Enums only support 'c' and 'cyrus' layouts.".to_string(),
                        }),
                        level: DiagLevel::Error,
                        loc: Some(loc),
                        hint: None,
                    });
                    return;
                }
            }
        }
    }

    fn validate_enum_tag_type(&mut self, tag_type: &Option<SemaType>, loc: Loc) {
        if let Some(tag_type) = tag_type {
            let tag_type = tag_type.const_inner();
            let valid = tag_type.is_integer() || tag_type.is_char() || tag_type.is_bool();

            if !valid {
                let got = format_sema_type(tag_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    kind: Box::new(AnalyzerDiagKind::InvalidEnumTagType { got }),
                    level: DiagLevel::Error,
                    loc: Some(loc),
                    hint: None,
                });
            }
        }
    }

    pub(crate) fn validate_enum_variant_field_type(
        &mut self,
        enum_decl_id: EnumDeclID,
        sema_type: &SemaType,
        loc: Loc,
    ) {
        let sema_type = sema_type.const_inner();

        if sema_type.is_void() {}

        let ty = NamedType {
            type_decl_id: TypeDeclID::Enum(enum_decl_id),
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
