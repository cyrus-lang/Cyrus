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
use cyrusc_typed_ast::{decls::MethodDecls, exprs::TypedDynamicExpr, format::format_sema_type, types::SemaType};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_dynamic(
        &mut self,
        dynamic: &mut TypedDynamicExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let operand_type = self.analyze_expr(&mut dynamic.operand, None)?;

        if dynamic.operand.kind.is_dynamic() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidMultipleDynamicType),
                loc: Some(dynamic.loc),
                hint: None,
            });
            return None;
        }

        let named_type = operand_type.as_named_type().unwrap();
        let object_generic_params = self.decl_tables.type_decl_generic_params(named_type.type_decl_id);
        let object_impls = self.implement_interfaces_of_named_type(named_type).unwrap();
        let object_name = self.formatter.format_type_decl(named_type.type_decl_id);
        let method_decls = self.decl_tables.methods_decl_of_named_type(named_type).unwrap();

        let Some(interface_decl_id) = expected_type.clone().and_then(|sema_ty| sema_ty.as_interface()) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotInferDynamicInterfaceType),
                loc: Some(dynamic.loc),
                hint: None,
            });
            return None;
        };

        let interface_decl = self.decl_tables.interface_decl(interface_decl_id);

        let object_impls_interface = object_impls
            .iter()
            .find(|implement_interface| {
                if let Some(ty) = self.normalize_sema_type(implement_interface.ty.clone(), implement_interface.loc) {
                    if ty.as_interface() == Some(interface_decl_id) {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            })
            .is_some();

        if !object_impls_interface {
            let concrete_type = format_sema_type(operand_type.clone(), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::DynamicConversionMissingInterface {
                    interface_type: interface_decl.name.clone(),
                    concrete_type,
                }),
                loc: Some(dynamic.loc),
                hint: None,
            });
            return None;
        }

        let interface_type_args = expected_type
            .as_ref()
            .unwrap()
            .as_named_type()
            .cloned()
            .unwrap()
            .type_args;

        let object_generic_env =
            self.create_inference_generic_env(&object_name, object_generic_params, &named_type.type_args, dynamic.loc)?;

        self.with_generic_env(object_generic_env, |this| {
            this.analyze_object_implements_monomorphized_interfaces(
                &interface_decl,
                interface_type_args.clone(),
                &object_name,
                &operand_type,
                &object_impls,
                &method_decls,
                dynamic.loc,
            );
        });

        let object_methods = self.decl_tables.methods_decl_of_named_type(named_type).unwrap();

        let interface_methods = MethodDecls(
            interface_decl
                .methods
                .iter()
                .map(|(method_name, _)| {
                    let method_decl_id = object_methods.get(method_name).unwrap();
                    (method_name.clone(), method_decl_id)
                })
                .collect(),
        );

        self.vtable_registry.register(
            operand_type.clone(),
            interface_decl_id,
            interface_type_args,
            interface_methods,
            interface_decl.is_generic(),
            interface_decl.loc,
        );

        dynamic.concrete_type = Some(operand_type);
        dynamic.ty = Some(expected_type.clone().unwrap());

        Some(expected_type.unwrap())
    }
}
