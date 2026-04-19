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
    decls::{DeclID, EnumDeclID},
    exprs::{
        ValueCategory, TypedEnumInit, TypedEnumInitArgs, TypedExprKind, TypedExpr, TypedUnnamedEnumValueKind,
    },
    format::format_enum_decl,
    stmts::{TypedTypeArg, TypedTypeArgs},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_field_access_as_enum_init(&mut self, expr: &mut TypedExpr) {
        let field_access = match &expr.kind {
            TypedExprKind::FieldAccess(field_access) if !field_access.is_thin_arrow => field_access,
            _ => return,
        };

        let Some((enum_decl_id, type_args)) =
            self.extract_enum_decl_id_of_expr_kind(&field_access.operand.kind, expr.loc)
        else {
            return;
        };

        let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        if enum_decl.lookup_variant(&field_access.name).is_none() {
            let enum_name = format_enum_decl(&enum_decl, self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                    enum_name,
                    variant_name: field_access.name.clone(),
                }),
                loc: Some(field_access.loc),
                hint: None,
            });

            expr.kind = TypedExprKind::Poisoned;
            return;
        }

        let enum_init = TypedEnumInit {
            decl_id: DeclID::Enum(enum_decl_id),
            name: field_access.name.clone(),
            args: TypedEnumInitArgs::Unit,
            type_args: type_args,
            loc: field_access.loc,
        };

        *expr = TypedExpr {
            kind: TypedExprKind::EnumInit(enum_init),
            sema_type: None,
            val_cat: ValueCategory::RValue,
            loc: field_access.loc,
        };
    }

    pub(crate) fn lower_method_call_as_enum_init(&mut self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::MethodCall(method_call) = &typed_expr.kind else {
            return;
        };

        if method_call.is_thin_arrow {
            return;
        }

        let Some((enum_decl_id, type_args)) =
            self.extract_enum_decl_id_of_expr_kind(&method_call.operand.kind, method_call.loc)
        else {
            return;
        };

        if !method_call.type_args.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TypeArgsMustBeSuppliedToEnumTypeNotVariant),
                loc: Some(method_call.loc),
                hint: None,
            });
        }

        let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        if enum_decl.lookup_variant(&method_call.name).is_some() {
            let args = TypedEnumInitArgs::Tuple(method_call.args.clone());

            let enum_init = TypedEnumInit {
                decl_id: DeclID::Enum(enum_decl_id),
                name: method_call.name.clone(),
                type_args,
                args,
                loc: method_call.loc,
            };

            *typed_expr = TypedExpr {
                kind: TypedExprKind::EnumInit(enum_init),
                sema_type: None,
                val_cat: ValueCategory::RValue,
                loc: method_call.loc,
            };
        }
    }

    pub(crate) fn lower_unnamed_enum_value_as_enum_init(&self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::UnnamedEnumValue(enum_value) = &typed_expr.kind else {
            return;
        };

        let enum_init_args = match &enum_value.kind {
            TypedUnnamedEnumValueKind::Unit => TypedEnumInitArgs::Unit,
            TypedUnnamedEnumValueKind::Tuple(elements) => TypedEnumInitArgs::Tuple(elements.clone()),
            TypedUnnamedEnumValueKind::Struct(field_inits) => TypedEnumInitArgs::Struct(field_inits.clone()),
        };

        *typed_expr = TypedExpr {
            kind: TypedExprKind::EnumInit(TypedEnumInit {
                decl_id: DeclID::Enum(enum_value.enum_decl_id.unwrap()),
                name: enum_value.ident.as_string(),
                type_args: TypedTypeArgs::new(),
                args: enum_init_args,
                loc: enum_value.loc,
            }),
            sema_type: typed_expr.sema_type.clone(),
            val_cat: typed_expr.val_cat,
            loc: typed_expr.loc,
        };
    }

    pub(crate) fn lower_enum_struct_variant_init_as_enum_init(&self, typed_expr: &mut TypedExpr) {
        let TypedExprKind::EnumStructVariantInit(struct_variant_init) = &typed_expr.kind else {
            return;
        };

        let args = TypedEnumInitArgs::Struct(struct_variant_init.field_inits.clone());

        *typed_expr = TypedExpr {
            kind: TypedExprKind::EnumInit(TypedEnumInit {
                decl_id: DeclID::Enum(struct_variant_init.enum_decl_id.unwrap()),
                name: struct_variant_init.ident.as_string(),
                type_args: TypedTypeArgs::new(),
                args,
                loc: struct_variant_init.loc,
            }),
            sema_type: typed_expr.sema_type.clone(),
            val_cat: typed_expr.val_cat,
            loc: typed_expr.loc,
        };
    }
}

// Helpers.
impl<'a> AnalysisContext<'a> {
    fn extract_enum_decl_id_of_expr_kind(
        &mut self,
        expr_kind: &TypedExprKind,
        loc: cyrusc_source_loc::Loc,
    ) -> Option<(EnumDeclID, TypedTypeArgs)> {
        let enum_decl_id;
        let type_args;

        match expr_kind {
            TypedExprKind::Symbol(symbol_expr) => {
                let Some(_enum_decl_id) = symbol_expr.decl_id.as_enum() else {
                    return None;
                };

                enum_decl_id = _enum_decl_id;
                type_args = TypedTypeArgs::new();
            }
            TypedExprKind::SemaType(sema_type) => {
                let ty = match self.normalize_sema_type(sema_type.clone(), loc) {
                    Some(ty) => ty,
                    None => return None,
                };

                let Some(named_type) = ty.as_named_type() else {
                    return None;
                };

                let Some(_enum_decl_id) = named_type.type_decl_id.as_enum() else {
                    return None;
                };

                enum_decl_id = _enum_decl_id;
                type_args = named_type.type_args.clone();
            }
            _ => return None,
        }

        Some((enum_decl_id, type_args))
    }
}
