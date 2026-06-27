// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    VTableID,
    decls::MethodDecls,
    exprs::TypedDynamicExpr,
    format::format_sema_type,
    types::{InterfaceObjectType, NamedType, SemaType, TypeDeclID},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_dynamic(
        &mut self,
        dynamic: &mut TypedDynamicExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let mut operand_type = self.analyze_expr(&mut dynamic.operand, None)?;

        // expand operand type
        operand_type = self.expand_sema_type(operand_type, dynamic.loc);

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
        let method_decls = self.decl_tables.method_decls_of_named_type(named_type).unwrap();

        let Some(interface_decl_id) = expected_type.clone().and_then(|sema_ty| sema_ty.as_named_interface()) else {
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
                if let Some(ty) = self.normalize_sema_type(implement_interface.ty.clone(), implement_interface.loc, 0) {
                    if ty.as_named_interface() == Some(interface_decl_id) {
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

        let object_methods = self.decl_tables.method_decls_of_named_type(named_type).unwrap();

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

        let vtable_id = self.vtable_registry.get_or_create_vtable(
            operand_type.clone(),
            interface_decl_id,
            interface_type_args.clone(),
            interface_methods.clone(),
            interface_decl.is_generic(),
            dynamic.loc,
        );

        self.monomorphize_interface_object_methods(
            vtable_id,
            &interface_methods,
            interface_decl.is_generic(),
            &operand_type,
            dynamic.loc,
        );

        let interface_named_type = NamedType {
            type_decl_id: TypeDeclID::Interface(interface_decl_id),
            type_args: interface_type_args,
        };

        let interface_object_type = InterfaceObjectType {
            interface_type: interface_named_type,
            concrete_type: Box::new(operand_type.clone()),
            vtable_id,
            loc: dynamic.loc,
        };

        dynamic.concrete_type = Some(operand_type.clone());
        dynamic.interface_object_type = Some(interface_object_type.clone());

        Some(SemaType::InterfaceObject(interface_object_type))
    }

    fn monomorphize_interface_object_methods(
        &mut self,
        vtable_id: VTableID,
        methods: &MethodDecls,
        is_interface_generic: bool,
        concrete_type: &SemaType,
        loc: Loc,
    ) {
        // checking object is generic or not is necessary here
        // because `is_interface_generic` is not enough solely.
        // and we prevent of monomorphization of concrete methods.
        let is_object_generic = !self
            .decl_tables
            .type_decl_generic_params(concrete_type.as_named_type().unwrap().type_decl_id)
            .is_empty();

        for (method_index, (method_name, method_decl_id)) in methods.iter().enumerate() {
            if is_interface_generic && is_object_generic {
                // monomorphize object method
                let concrete_named_type = concrete_type.as_named_type().unwrap();

                let object_generic_params = self
                    .decl_tables
                    .type_decl_generic_params(concrete_named_type.type_decl_id);

                let mut method_decl = self.decl_tables.method_decl(*method_decl_id);

                let Some(object_method_generic_env) = self.create_inference_generic_env(
                    &method_name,
                    object_generic_params.clone(),
                    &concrete_named_type.type_args,
                    loc,
                ) else {
                    continue;
                };

                self.with_generic_env(object_method_generic_env, |this| {
                    method_decl.func_decl.params = this.substitute_func_params(method_decl.func_decl.params.clone());

                    method_decl.func_decl.ret_type = this.substitute_type(&method_decl.func_decl.ret_type);

                    let self_modifier = method_decl.func_decl.params.get_self_modifier_mut().unwrap();

                    self_modifier.ty = this.substitute_self_type(self_modifier.ty.clone(), &concrete_type);

                    let func_type = method_decl.func_decl.as_func_type();

                    let parent_infer_ctx = this.func_env.infer.clone();
                    let func_env = this.create_method_env(*method_decl_id, func_type, parent_infer_ctx);

                    let final_generics_params = method_decl.func_decl.generic_params.extend(object_generic_params);
                    let final_type_args = this.collect_instantiated_type_args(final_generics_params);

                    this.with_func_env(func_env, |this| {
                        this.func_env.current_object = Some(concrete_type.clone());

                        let Some(monomorph_id) = this.monomorphize_generic_method_call(
                            concrete_type.clone(),
                            *method_decl_id,
                            method_decl,
                            final_type_args,
                            true,
                            loc,
                        ) else {
                            return;
                        };

                        this.vtable_registry.with_vtable_info_mut(vtable_id, |_vtable_info| {
                            let slot = _vtable_info
                                .monomorphized_methods
                                .get_mut(method_index)
                                .expect("vtable method index out of bounds");

                            *slot = Some(monomorph_id);
                        });
                    });
                });
            }
        }
    }
}
