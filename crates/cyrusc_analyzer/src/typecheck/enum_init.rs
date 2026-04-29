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
    types::{NamedType, SemaType, TypeDeclID},
};
use fx_hash::{FxHashMap, FxHashSet, FxHashSetExt};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_enum_init(
        &mut self,
        enum_init: &mut TypedEnumInit,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let mut operand = self.normalize_and_check_type_formation(enum_init.operand.clone(), enum_init.loc)?;

        operand = self.expand_sema_type(operand, enum_init.loc);

        let Some(named_type) = operand.as_named_type() else {
            let symbol_name = format_sema_type(operand, self.formatter);
            self.report_non_enum_symbol(symbol_name, enum_init.loc);
            return None;
        };

        let Some(enum_decl_id) = named_type.type_decl_id.as_enum() else {
            let symbol_name = format_sema_type(operand, self.formatter);
            self.report_non_enum_symbol(symbol_name, enum_init.loc);
            return None;
        };

        let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        let enum_name = format_enum_decl(&enum_decl, self.formatter);

        let Some(variant) = enum_decl.lookup_variant(&enum_init.name).cloned() else {
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

        let generic_env = self.create_inference_generic_env(
            &enum_name,
            enum_decl.generic_params.clone(),
            &named_type.type_args,
            enum_decl.loc,
        )?;

        self.with_generic_env(generic_env, |this| {
            match (&mut enum_init.arg, &variant) {
                // .Unit
                (TypedEnumInitArgs::Unit, TypedEnumVariant::Unit(_)) => { /* skip */ }

                // .ValuedVariant
                (TypedEnumInitArgs::Unit, TypedEnumVariant::Valued { .. }) => { /* valid */ }

                // .Variant(a, b)
                (TypedEnumInitArgs::Tuple(elements), TypedEnumVariant::Tuple { fields, .. }) => {
                    if elements.len() != fields.len() {
                        this.reporter.report(Diag {
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
                        this.analyze_field_assign(element, field.ty.clone(), enum_init.loc);
                    }
                }

                // .Variant { a: expr1, b: expr2 }
                (TypedEnumInitArgs::Struct(field_inits), TypedEnumVariant::Struct { fields, .. }) => {
                    this.analyze_enum_struct_variant_fields(&enum_init.name, field_inits, &fields, enum_init.loc);
                }

                _ => match variant {
                    TypedEnumVariant::Unit(_) | TypedEnumVariant::Valued { .. } => {
                        this.reporter.report(Diag {
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
                        let provided_kind = match &enum_init.arg {
                            TypedEnumInitArgs::Unit => "unit",
                            TypedEnumInitArgs::Tuple(_) => "tuple",
                            TypedEnumInitArgs::Struct(_) => "struct",
                        };

                        let expected_kind = match variant {
                            TypedEnumVariant::Tuple { .. } => "tuple",
                            TypedEnumVariant::Struct { .. } => "struct",
                            _ => unreachable!(),
                        };

                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::EnumVariantKindMismatch {
                                variant_name: enum_init.name.clone(),
                                expected_kind: expected_kind.to_string(),
                                provided_kind: provided_kind.to_string(),
                            }),
                            loc: Some(enum_init.loc),
                            hint: Some(this.invalid_enum_variant_construction_hint(&variant)),
                        });
                        return None;
                    }
                },
            }

            let inferred_type_args = this.collect_instantiated_type_args(enum_decl.generic_params.clone());

            this.unify_with_expected_type(
                SemaType::Named(NamedType {
                    type_decl_id: TypeDeclID::Enum(enum_decl_id),
                    type_args: inferred_type_args,
                }),
                &expected_type,
            );

            this.apply_generic_defaults(enum_decl.generic_params.clone());

            let final_type_args = this.collect_instantiated_type_args(enum_decl.generic_params);

            let final_type = SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Enum(enum_decl_id),
                type_args: final_type_args,
            });

            enum_init.operand = final_type.clone();

            Some(final_type)
        })
    }

    pub(crate) fn analyze_unnamed_enum_value(
        &mut self,
        enum_value: &mut TypedUnnamedEnumValue,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let Some((enum_decl_id, enum_decl)) = self.infer_enum_decl_from_expected_type(expected_type.clone()) else {
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

            // .Variant
            (TypedUnnamedEnumValueKind::Unit, TypedEnumVariant::Valued { .. }) => { /* valid */ }

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

        Some(expected_type.unwrap())
    }

    pub(crate) fn analyze_enum_struct_variant_init(
        &mut self,
        struct_variant_init: &mut TypedEnumStructVariantInit,
    ) -> Option<SemaType> {
        let Some(decl_id) = struct_variant_init.operand.kind.as_decl_id() else {
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

        let Some(enum_decl_id) = decl_id.as_enum() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonEnumSymbol {
                    symbol_name: self.formatter.format_decl(decl_id),
                }),
                loc: Some(struct_variant_init.loc),
                hint: None,
            });
            return None;
        };

        let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        let Some(variant) = enum_decl.lookup_variant(&struct_variant_init.ident.value) else {
            let enum_name = self.formatter.format_decl(decl_id);

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

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Enum(enum_decl_id),
            type_args: TypedTypeArgs::new(),
        }))
    }

    fn infer_enum_decl_from_expected_type(&self, expected_type: Option<SemaType>) -> Option<(EnumDeclID, EnumDecl)> {
        expected_type.and_then(|ty| {
            ty.as_enum()
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
        let mut initialized_fields = FxHashSet::new();

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

            self.analyze_field_assign(&mut field_init.value, declared_field.ty.clone(), declared_field.loc);
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
