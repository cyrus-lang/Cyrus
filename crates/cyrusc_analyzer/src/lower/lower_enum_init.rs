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
use cyrusc_internal::symbols::symbols::SymbolEntryKind;
use cyrusc_typed_ast::{
    exprs::{
        MemoryLocation, TypedEnumInit, TypedEnumInitArgs, TypedExprKind, TypedExprStmt, TypedUnnamedEnumValueKind,
    },
    format::format_enum_decl,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn lower_field_access_as_enum_init(&self, typed_expr: &mut TypedExprStmt) {
        let TypedExprKind::FieldAccess(field_access) = &typed_expr.kind else {
            return;
        };

        if field_access.is_fat_arrow {
            return;
        }

        if let Some(symbol_id) = field_access.operand.kind.as_symbol_id() {
            let symbol_entry = self.query.lookup_symbol_entry(symbol_id).unwrap();

            if let SymbolEntryKind::Enum(enum_decl_id) = symbol_entry.kind {
                let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

                // variant exists?
                if enum_decl.lookup_variant(&field_access.name).is_some() {
                    let args = TypedEnumInitArgs::Unit;

                    let enum_init = TypedEnumInit {
                        enum_decl_id,
                        name: field_access.name.clone(),
                        args,
                        loc: field_access.loc,
                    };

                    *typed_expr = TypedExprStmt {
                        kind: TypedExprKind::EnumInit(enum_init),
                        sema_type: None,
                        mloc: MemoryLocation::RValue,
                        loc: field_access.loc,
                    };
                } else {
                    // no such variant on enum decl
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

                    typed_expr.kind = TypedExprKind::Poisoned;
                }
            }
        }
    }

    pub(crate) fn lower_method_call_as_enum_init(&self, typed_expr: &mut TypedExprStmt) {
        let TypedExprKind::MethodCall(method_call) = &typed_expr.kind else {
            return;
        };

        if method_call.is_fat_arrow {
            return;
        }

        if !method_call.type_args.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs {
                    type_name: method_call.name.clone(),
                }),
                loc: Some(method_call.loc),
                hint: None,
            });
        }

        if let Some(symbol_id) = method_call.operand.kind.as_symbol_id() {
            let symbol_entry = self.query.lookup_symbol_entry(symbol_id).unwrap();

            if let SymbolEntryKind::Enum(enum_decl_id) = symbol_entry.kind {
                let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

                // lower only if variant exists
                if enum_decl.lookup_variant(&method_call.name).is_some() {
                    let args = TypedEnumInitArgs::Tuple(method_call.args.clone());

                    let enum_init = TypedEnumInit {
                        enum_decl_id,
                        name: method_call.name.clone(),
                        args,
                        loc: method_call.loc,
                    };

                    *typed_expr = TypedExprStmt {
                        kind: TypedExprKind::EnumInit(enum_init),
                        sema_type: None,
                        mloc: MemoryLocation::RValue,
                        loc: method_call.loc,
                    };
                }
            }
        }
    }

    pub(crate) fn lower_unnamed_enum_value_as_enum_init(&self, typed_expr: &mut TypedExprStmt) {
        let TypedExprKind::UnnamedEnumValue(enum_value) = &typed_expr.kind else {
            return;
        };

        let enum_init_args = match &enum_value.kind {
            TypedUnnamedEnumValueKind::Unit => TypedEnumInitArgs::Unit,
            TypedUnnamedEnumValueKind::Tuple(elements) => TypedEnumInitArgs::Tuple(elements.clone()),
            TypedUnnamedEnumValueKind::Struct(field_inits) => TypedEnumInitArgs::Struct(field_inits.clone()),
        };

        *typed_expr = TypedExprStmt {
            kind: TypedExprKind::EnumInit(TypedEnumInit {
                enum_decl_id: enum_value.enum_decl_id.unwrap(),
                name: enum_value.ident.as_string(),
                args: enum_init_args,
                loc: enum_value.loc,
            }),
            sema_type: typed_expr.sema_type.clone(),
            mloc: typed_expr.mloc,
            loc: typed_expr.loc,
        };
    }

    pub(crate) fn lower_enum_struct_variant_init_as_enum_init(&self, typed_expr: &mut TypedExprStmt) {
        let TypedExprKind::EnumStructVariantInit(struct_variant_init) = &typed_expr.kind else {
            return;
        };

        let args = TypedEnumInitArgs::Struct(struct_variant_init.field_inits.clone());

        *typed_expr = TypedExprStmt {
            kind: TypedExprKind::EnumInit(TypedEnumInit {
                enum_decl_id: struct_variant_init.enum_decl_id.unwrap(),
                name: struct_variant_init.ident.as_string(),
                args,
                loc: struct_variant_init.loc,
            }),
            sema_type: typed_expr.sema_type.clone(),
            mloc: typed_expr.mloc,
            loc: typed_expr.loc,
        };
    }
}
