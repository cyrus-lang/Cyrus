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
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::{EnumDecl, EnumDeclID},
    exprs::{
        TypedEnumInit, TypedEnumInitArgs, TypedEnumStructVariantFieldInit, TypedEnumStructVariantInit,
        TypedUnnamedEnumValue, TypedUnnamedEnumValueKind,
    },
    format::{format_enum_decl, format_sema_type, format_typed_expr},
    stmts::{TypedEnumVariant, TypedEnumVariantStructField, TypedTypeArgs},
    types::{NamedType, SemanticType, TypeDeclID},
};
use fx_hash::{FxHashMap, FxHashSet};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_enum_init(&mut self, enum_init: &mut TypedEnumInit) -> Option<SemanticType> {
        let enum_decl = self.decl_tables.enum_decl(enum_init.enum_decl_id);

        let Some(variant) = enum_decl.lookup_variant(&enum_init.name) else {
            let enum_name = format_enum_decl(&enum_decl, self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                    enum_name,
                    variant_name: enum_init.name.clone(),
                }),
                loc: Some(enum_init.loc),
                hint: None,
            });
            return None;
        };

        match (&mut enum_init.args, variant) {
            // .Variant
            (TypedEnumInitArgs::Unit, TypedEnumVariant::Unit(_)) => { /* skip */ }

            // .Variant(a, b)
            (TypedEnumInitArgs::Tuple(elements), TypedEnumVariant::Tuple { fields, .. }) => {
                if elements.len() != fields.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantArgCountMismatch {
                            variant_name: enum_init.name.clone(),
                            expected: fields.len() as u32,
                            provided: elements.len() as u32,
                        }),
                        loc: Some(enum_init.loc),
                        hint: None,
                    });
                    return None;
                }

                for (element, field) in elements.iter_mut().zip(fields) {
                    let value_type = match self.analyze_expr(element, Some(field.ty.clone())) {
                        Some(sema_type) => sema_type,
                        None => continue,
                    };

                    if !self.is_assignable_to(value_type.clone(), field.ty.clone(), element.loc) {
                        let got_type = format_sema_type(value_type.clone(), self.formatter);
                        let expected_type = format_sema_type(field.ty.clone(), self.formatter);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::InvalidEnumVariantFieldValueType {
                                got_type,
                                expected_type,
                            }),
                            loc: Some(element.loc),
                            hint: None,
                        });
                    }
                }
            }

            // .Variant { a: expr1, b: expr2 }
            (TypedEnumInitArgs::Struct(field_inits), TypedEnumVariant::Struct { fields, .. }) => {
                self.analyze_enum_struct_variant_fields(&enum_init.name, field_inits, fields, enum_init.loc);
            }

            _ => match variant {
                TypedEnumVariant::Unit(_) | TypedEnumVariant::Valued { .. } => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                            variant_name: enum_init.name.clone(),
                        }),
                        loc: Some(enum_init.loc),
                        hint: None,
                    });
                    return None;
                }
                TypedEnumVariant::Tuple { .. } | TypedEnumVariant::Struct { .. } => {
                    let provided_kind = match &enum_init.args {
                        TypedEnumInitArgs::Unit => "unit",
                        TypedEnumInitArgs::Tuple(_) => "tuple",
                        TypedEnumInitArgs::Struct(_) => "struct",
                    };

                    let expected_kind = match variant {
                        TypedEnumVariant::Tuple { .. } => "tuple",
                        TypedEnumVariant::Struct { .. } => "struct",
                        _ => unreachable!(),
                    };

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantKindMismatch {
                            variant_name: enum_init.name.clone(),
                            expected_kind: expected_kind.to_string(),
                            provided_kind: provided_kind.to_string(),
                        }),
                        loc: Some(enum_init.loc),
                        hint: Some(self.invalid_enum_variant_construction_hint(variant)),
                    });

                    return None;
                }
            },
        }

        Some(SemanticType::Named(NamedType {
            decl_id: TypeDeclID::Enum(enum_init.enum_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    pub(crate) fn analyze_unnamed_enum_value(
        &mut self,
        enum_value: &mut TypedUnnamedEnumValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let Some((enum_decl_id, enum_decl)) = self.infer_enum_decl_from_expected_type(expected_type) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotInferEnumForUnnamedVariant {
                    variant_name: enum_value.ident.as_string(),
                }),
                loc: Some(enum_value.loc),
                hint: None,
            });
            return None;
        };

        let Some(variant) = enum_decl.lookup_variant(&enum_value.ident.value) else {
            let enum_name = format_enum_decl(&enum_decl, self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                    enum_name,
                    variant_name: enum_value.ident.as_string(),
                }),
                loc: Some(enum_value.loc),
                hint: None,
            });
            return None;
        };

        match (&mut enum_value.kind, variant) {
            // .Variant
            (TypedUnnamedEnumValueKind::Unit, TypedEnumVariant::Unit(_)) => { /* skip */ }

            // .Variant(a, b)
            (TypedUnnamedEnumValueKind::Tuple(elements), TypedEnumVariant::Tuple { fields, .. }) => {
                if elements.len() != fields.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantArgCountMismatch {
                            variant_name: enum_value.ident.as_string(),
                            expected: fields.len() as u32,
                            provided: elements.len() as u32,
                        }),
                        loc: Some(enum_value.loc),
                        hint: None,
                    });
                    return None;
                }

                for (element, field) in elements.iter_mut().zip(fields) {
                    let value_type = match self.analyze_expr(element, Some(field.ty.clone())) {
                        Some(sema_type) => sema_type,
                        None => continue,
                    };

                    if !self.is_assignable_to(value_type.clone(), field.ty.clone(), element.loc) {
                        let got_type = format_sema_type(value_type.clone(), self.formatter);
                        let expected_type = format_sema_type(field.ty.clone(), self.formatter);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::InvalidEnumVariantFieldValueType {
                                got_type,
                                expected_type,
                            }),
                            loc: Some(element.loc),
                            hint: None,
                        });
                    }
                }
            }

            // .Variant { a: expr1, b: expr2 }
            (TypedUnnamedEnumValueKind::Struct(field_inits), TypedEnumVariant::Struct { fields, .. }) => {
                self.analyze_enum_struct_variant_fields(&enum_value.ident.value, field_inits, fields, enum_value.loc);
            }

            _ => match variant {
                TypedEnumVariant::Unit(_) | TypedEnumVariant::Valued { .. } => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                            variant_name: enum_value.ident.as_string(),
                        }),
                        loc: Some(enum_value.loc),
                        hint: None,
                    });
                    return None;
                }
                TypedEnumVariant::Tuple { .. } | TypedEnumVariant::Struct { .. } => {
                    let provided_kind = match &enum_value.kind {
                        TypedUnnamedEnumValueKind::Unit => "unit",
                        TypedUnnamedEnumValueKind::Tuple(_) => "tuple",
                        TypedUnnamedEnumValueKind::Struct(_) => "struct",
                    };

                    let expected_kind = match variant {
                        TypedEnumVariant::Tuple { .. } => "tuple",
                        TypedEnumVariant::Struct { .. } => "struct",
                        _ => unreachable!(),
                    };

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantKindMismatch {
                            variant_name: enum_value.ident.as_string(),
                            expected_kind: expected_kind.to_string(),
                            provided_kind: provided_kind.to_string(),
                        }),
                        loc: Some(enum_value.loc),
                        hint: Some(self.invalid_enum_variant_construction_hint(variant)),
                    });

                    return None;
                }
            },
        }

        enum_value.enum_decl_id = Some(enum_decl_id);

        Some(SemanticType::Named(NamedType {
            decl_id: TypeDeclID::Enum(enum_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    pub(crate) fn analyze_enum_struct_variant_init(
        &mut self,
        struct_variant_init: &mut TypedEnumStructVariantInit,
    ) -> Option<SemanticType> {
        let Some(symbol_id) = struct_variant_init.operand.kind.as_symbol_id() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidEnumConstructorTarget {
                    expr: format_typed_expr(&struct_variant_init.operand, self.formatter),
                }),
                loc: Some(struct_variant_init.loc),
                hint: None,
            });
            return None;
        };

        let Some(enum_decl_id) = self.query.get_enum(symbol_id) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonEnumSymbol {
                    symbol_name: self.formatter.format_symbol_name(symbol_id),
                }),
                loc: Some(struct_variant_init.loc),
                hint: None,
            });
            return None;
        };

        let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        let Some(variant) = enum_decl.lookup_variant(&struct_variant_init.ident.value) else {
            let enum_name = self.formatter.format_symbol_name(symbol_id);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                    enum_name,
                    variant_name: struct_variant_init.ident.as_string(),
                }),
                loc: Some(struct_variant_init.loc),
                hint: None,
            });

            return None;
        };

        match &variant {
            TypedEnumVariant::Struct { fields, .. } => {
                self.analyze_enum_struct_variant_fields(
                    &struct_variant_init.ident.value,
                    &mut struct_variant_init.field_inits,
                    fields,
                    struct_variant_init.loc,
                );
            }

            // kind mismatch errors
            _ => {
                let variant_kind = match variant {
                    TypedEnumVariant::Struct { .. } => unreachable!(),

                    TypedEnumVariant::Unit(_) => "unit",
                    TypedEnumVariant::Valued { .. } => "valued",
                    TypedEnumVariant::Tuple { .. } => "tuple",
                };

                let hint = self.invalid_enum_variant_construction_hint(variant);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::EnumVariantKindMismatch {
                        variant_name: struct_variant_init.ident.as_string(),
                        expected_kind: "struct".to_string(),
                        provided_kind: variant_kind.to_string(),
                    }),
                    loc: Some(struct_variant_init.loc),
                    hint: Some(hint),
                });
                return None;
            }
        }

        struct_variant_init.enum_decl_id = Some(enum_decl_id);

        Some(SemanticType::Named(NamedType {
            decl_id: TypeDeclID::Enum(enum_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    fn infer_enum_decl_from_expected_type(
        &self,
        expected_type: Option<SemanticType>,
    ) -> Option<(EnumDeclID, EnumDecl)> {
        expected_type.and_then(|sema_type| {
            sema_type
                .as_enum()
                .map(|enum_decl_id| (enum_decl_id, self.decl_tables.enum_decl(enum_decl_id)))
        })
    }

    fn analyze_enum_struct_variant_fields(
        &mut self,
        variant_name: &String,
        field_inits: &mut [TypedEnumStructVariantFieldInit],
        fields: &[TypedEnumVariantStructField],
        loc: Loc,
    ) {
        let mut initialized_fields = FxHashSet::default();

        // build lookup for declared fields
        let declared: FxHashMap<_, _> = fields.iter().map(|field| (field.name.as_string(), field)).collect();

        for field_init in field_inits {
            let field_name = &field_init.name.value;

            // duplicate field
            if !initialized_fields.insert(field_name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldInitializer {
                        field_name: field_name.to_string(),
                    }),
                    loc: Some(field_init.loc),
                    hint: None,
                });
                continue;
            }

            // invalid field
            let Some(declared_field) = declared.get(field_name) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidEnumVariantField {
                        variant_name: variant_name.clone(),
                        field_name: field_name.to_string(),
                    }),
                    loc: Some(field_init.loc),
                    hint: None,
                });
                continue;
            };

            // analyze expression with expected type
            let value_type = match self.analyze_expr(&mut field_init.value, Some(declared_field.ty.clone())) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            if !self.is_assignable_to(value_type.clone(), declared_field.ty.clone(), field_init.loc) {
                let got_type = format_sema_type(value_type.clone(), self.formatter);
                let expected_type = format_sema_type(declared_field.ty.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidEnumVariantFieldValueType {
                        got_type,
                        expected_type,
                    }),
                    loc: Some(field_init.loc),
                    hint: None,
                });
            }
        }

        // missing fields
        for declared_field in fields {
            if !initialized_fields.contains(&declared_field.name.value) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::MissingEnumVariantField {
                        field_name: declared_field.name.as_string(),
                        variant_name: variant_name.clone(),
                    }),
                    loc: Some(loc),
                    hint: None,
                });
            }
        }
    }

    #[inline]
    fn invalid_enum_variant_construction_hint(&self, variant: &TypedEnumVariant) -> String {
        match variant {
            TypedEnumVariant::Unit(_) => "Construct it without arguments: '.Variant'.".into(),
            TypedEnumVariant::Tuple { .. } => "Use tuple-style construction: '.Variant(value1, value2)'.".into(),
            TypedEnumVariant::Struct { .. } => "Use struct-style construction: '.Variant { field: value }.'".into(),
            TypedEnumVariant::Valued { .. } => "Provide the required value argument.".into(),
        }
    }
}
