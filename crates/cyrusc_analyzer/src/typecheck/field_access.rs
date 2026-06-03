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
use cyrusc_ast::abi::Visibility;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    decls::MethodDecls,
    exprs::{TypedFieldAccess, TypedFieldAccessDispatch},
    format::{format_sema_type, format_struct_decl, format_union_decl},
    substitute::{instantiate_struct_decl_with_type_args, instantiate_union_decl_with_type_args},
    types::SemaType,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_field_access(&mut self, field_access: &mut TypedFieldAccess) -> Option<SemaType> {
        let operand_type = self.analyze_expr(&mut field_access.operand, None)?;

        let pure_operand_type = operand_type.const_inner().pointer_inner().clone();

        let Some(type_args) = &pure_operand_type
            .as_named_type()
            .map(|named_type| named_type.type_args.clone())
        else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields {
                    operand_type: format_sema_type(operand_type, self.formatter),
                }),
                loc: Some(field_access.loc),
                hint: None,
            });
            return None;
        };

        if let Some(struct_decl_id) = pure_operand_type.as_struct() {
            let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

            let inst_struct_decl = instantiate_struct_decl_with_type_args(&struct_decl, type_args);

            let struct_name = format_struct_decl(&inst_struct_decl, self.formatter);

            let Some(field) = inst_struct_decl.lookup_field(&field_access.name) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        object_name: struct_name,
                        field_name: field_access.name.clone(),
                    }),
                    loc: Some(field_access.loc),
                    hint: None,
                });
                return None;
            };

            self.validate_field_access(&field_access, field.vis, &inst_struct_decl.methods, &struct_name);

            let field_type = self.normalize_sema_type(field.ty.clone(), field_access.loc)?;
            let field_index = inst_struct_decl
                .fields
                .iter()
                .position(|_field| _field.name == field.name)
                .unwrap();

            field_access.ty = Some(field_type.clone());
            field_access.dispatch = TypedFieldAccessDispatch::Struct {
                struct_decl_id,
                index: field_index,
            };

            Some(field_type)
        } else if let Some(union_decl_id) = pure_operand_type.as_union() {
            let union_decl = self.decl_tables.union_decl(union_decl_id);

            let inst_union_decl = instantiate_union_decl_with_type_args(&union_decl, type_args);

            let union_name = format_union_decl(&inst_union_decl, self.formatter);

            let Some(field) = inst_union_decl.lookup_field(&field_access.name) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        object_name: union_name,
                        field_name: field_access.name.clone(),
                    }),
                    loc: Some(field_access.loc),
                    hint: None,
                });
                return None;
            };

            // unions never involved with visibility violation
            let vis = Visibility::Public;

            self.validate_field_access(&field_access, vis, &inst_union_decl.methods, &union_name);

            let field_type = self.normalize_sema_type(field.ty.clone(), field_access.loc)?;

            field_access.ty = Some(field_type.clone());
            field_access.dispatch = TypedFieldAccessDispatch::Union { union_decl_id };

            Some(field_type)
        } else {
            let operand_type = field_access.operand.ty.clone().unwrap();

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields {
                    operand_type: format_sema_type(operand_type, self.formatter),
                }),
                loc: Some(field_access.loc),
                hint: None,
            });
            None
        }
    }

    /// Validates field access syntax, visibility, and pointer semantics.
    fn validate_field_access(
        &mut self,
        field_access: &TypedFieldAccess,
        field_vis: Visibility,
        method_decls: &MethodDecls,
        object_name: &str,
    ) {
        let access_violation = if let Some(method_decl_id) = self.func_env.current_method {
            if method_decls.contains_method_id(method_decl_id) {
                false
            } else {
                !field_vis.is_public()
            }
        } else {
            !field_vis.is_public()
        };

        if access_violation {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InternalFieldAccess {
                    field_name: field_access.name.clone(),
                    object_name: object_name.to_string(),
                }),
                loc: Some(field_access.loc),
                hint: None,
            });
        }

        let base_type = field_access.operand.ty.as_ref().unwrap().const_inner();

        let is_pointer = base_type.is_pointer();
        let is_object = base_type.is_struct() || base_type.is_union();

        if field_access.is_thin_arrow {
            if !is_pointer {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                    loc: Some(field_access.loc),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
            }
        } else {
            if !is_object {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                    loc: Some(field_access.loc),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
            }
        }
    }
}
