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
use cyrusc_const_eval::resolver::ConstResolver;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::monomorph::MonomorphizableTemplateID;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::{DeclID, FuncDecl, FuncDeclID, MethodDecl, MethodDeclID, MethodDecls, MonomorphID},
    exprs::{TypedExpr, TypedExprKind, TypedFuncCall, TypedFuncCallDispatch, TypedMethodCall, TypedMethodCallDispatch},
    format::{format_func_type, format_loc, format_sema_type},
    stmts::{TypedFuncTypeVariadicParam, TypedFuncVariadicParam, TypedGenericParams, TypedTypeArgs},
    substitute::instantiate_struct_decl_with_type_args,
    types::{NamedType, SemaType, TypeDeclID, TypedFuncType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_func_call(&mut self, func_call: &mut TypedFuncCall) -> Option<SemaType> {
        let func_decl_id_opt = func_call
            .operand
            .kind
            .as_decl_id()
            .and_then(|decl_id| decl_id.as_func());

        let operand_type = self.analyze_expr_non_terminal(&mut func_call.operand, None)?;

        self.normalize_type_args(&mut func_call.type_args);

        let Some(mut func_type) = operand_type.as_func_type().cloned() else {
            let symbol_name = format_sema_type(operand_type, self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonFunctionSymbol { symbol_name }),
                loc: Some(func_call.loc),
                hint: None,
            });
            return None;
        };

        // named func call
        if let Some(func_decl_id) = func_decl_id_opt {
            let mut func_decl = self.decl_tables.func_decl(func_decl_id);

            // generic function
            if func_decl.is_generic() {
                let generic_env = self.create_inference_generic_env(
                    &self.formatter.format_decl(DeclID::Func(func_decl_id)),
                    func_decl.generic_params.clone(),
                    &func_call.type_args,
                    func_call.loc,
                )?;

                self.with_generic_env(generic_env, |this| {
                    this.normalize_func_params(&mut func_decl.params);

                    if !this.analyze_call(&mut func_decl, &mut func_call.args, func_call.loc, false) {
                        return None;
                    }

                    this.apply_generic_defaults(func_decl.generic_params.clone());

                    let final_type_args = this.collect_instantiated_type_args(func_decl.generic_params.clone());

                    let ret_type = this.monomorphize_generic_func_call(
                        &operand_type,
                        func_call,
                        func_decl_id,
                        &mut func_decl,
                        final_type_args,
                    )?;

                    Some(ret_type)
                })
            }
            // normal function
            else {
                self.normalize_func_params(&mut func_decl.params);

                if !self.analyze_call(&mut func_decl, &mut func_call.args, func_call.loc, false) {
                    return None;
                }

                func_decl.ret_type = func_decl.ret_type.clone();

                func_call.dispatch = TypedFuncCallDispatch::Normal {
                    func_decl_id: func_decl_id,
                };

                Some(func_decl.ret_type)
            }
        }
        // function pointer call
        else {
            if !func_call.type_args.is_empty() {
                let type_name = format_sema_type(operand_type, self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs { type_name }),
                    loc: Some(func_call.loc),
                    hint: Some("Lambdas never accept type args.".to_string()),
                });
            }

            self.normalize_func_type_params(&mut func_type.params, func_call.loc);

            func_type.ret_type =
                Box::new(self.normalize_and_check_type_formation(*func_type.ret_type.clone(), func_call.loc)?);

            let ret_type = self.check_func_type_call(&mut func_type, &mut func_call.args, func_call.loc)?;

            func_call.dispatch = TypedFuncCallDispatch::FunctionPointer {
                func_type: func_type.clone(),
            };

            Some(ret_type)
        }
    }
}

// Methods.
impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_method_call(&mut self, method_call: &mut TypedMethodCall) -> Option<SemaType> {
        self.analyze_expr_non_terminal(&mut method_call.operand, None);

        self.normalize_type_args(&mut method_call.type_args);

        let mut is_operand_instance = true;

        // `method_call.operand.sema_type` is only present when operand is a generic inst;
        // we need to extract the decl_id manually if method call used without type args,
        // which means `method_call.operand.kind` would be a `TypedSymbolExpr`.
        let mut operand_type = match {
            if let Some(decl_id) = method_call.operand.kind.as_decl_id() {
                if let Some(type_decl_id) = decl_id.as_type_decl_id() {
                    // type_decl means it's operand of a static method call.
                    is_operand_instance = false;

                    Some(SemaType::Named(NamedType {
                        type_decl_id,
                        type_args: TypedTypeArgs::new(),
                    }))
                } else {
                    is_operand_instance = true;

                    if decl_id.is_var_or_global_var() {
                        // extract variable type as use it as operand type for instance method calls.
                        Some(self.resolve_symbol_type(decl_id, method_call.loc)?)
                    } else {
                        None
                    }
                }
            } else if let Some(sema_type) = method_call.operand.kind.as_type_expr() {
                is_operand_instance = false;

                // normalized later
                Some(sema_type)
            } else {
                None
            }
        } {
            Some(ty) => ty,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                    loc: Some(method_call.loc),
                    hint: None,
                });
                return None;
            }
        };

        operand_type = self.normalize_sema_type(operand_type, method_call.loc)?;

        if is_operand_instance {
            self.analyze_instance_method_call(method_call, &operand_type)
        } else {
            self.analyze_static_method_call(method_call, &operand_type)
        }
    }

    fn analyze_instance_method_call(
        &mut self,
        method_call: &mut TypedMethodCall,
        operand_type: &SemaType,
    ) -> Option<SemaType> {
        let pure_operand_type = operand_type.const_inner().pointer_inner().clone();

        let named_type = pure_operand_type.as_named_type();

        if named_type
            .map(|named_type| named_type.type_decl_id.is_interface())
            .unwrap_or(false)
        {
            return self.analyze_interface_method_call(method_call, operand_type);
        }

        let Some((type_decl_id, method_decls)) = named_type.and_then(|named_type| {
            self.decl_tables
                .methods_decl_of_named_type(named_type)
                .map(|method_decls| (named_type.type_decl_id, method_decls))
        }) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                loc: Some(method_call.loc),
                hint: None,
            });
            return None;
        };

        self.analyze_expr(&mut method_call.operand, None);

        self.analyze_method_call_internal(method_call, &method_decls, type_decl_id, operand_type.clone(), true)
    }

    fn analyze_static_method_call(
        &mut self,
        method_call: &mut TypedMethodCall,
        operand_type: &SemaType,
    ) -> Option<SemaType> {
        let pure_operand_type = operand_type.const_inner().pointer_inner().clone();

        let Some((type_decl_id, method_decls)) = pure_operand_type.as_named_type().and_then(|named_type| {
            self.decl_tables
                .methods_decl_of_named_type(named_type)
                .map(|method_decls| (named_type.type_decl_id, method_decls))
        }) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                loc: Some(method_call.loc),
                hint: None,
            });
            return None;
        };

        if !method_call.type_args.is_empty() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::GenericStaticMethodWrongTypeArgs),
                loc: Some(method_call.loc),
                hint: None,
            });
            return None;
        }

        self.analyze_method_call_internal(method_call, &method_decls, type_decl_id, operand_type.clone(), false)
    }

    fn analyze_interface_method_call(
        &mut self,
        method_call: &mut TypedMethodCall,
        operand_type: &SemaType,
    ) -> Option<SemaType> {
        let decl_id = method_call.operand.kind.as_decl_id().unwrap();
        let expr = self.resolve_symbol_expr(decl_id).unwrap();

        let Some(concrete_type) = expr.extract_dynamic_expr_concrete_type().cloned() else {
            let operand_type = format_sema_type(operand_type.clone(), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InterfaceMethodCallOnNonDynamicObject {
                    method_name: method_call.name.clone(),
                    expected_interface: operand_type,
                }),
                loc: Some(method_call.loc),
                hint: None,
            });
            return None;
        };

        let pure_operand_type = operand_type.const_inner().pointer_inner().clone();

        let interface_decl_id = pure_operand_type.as_interface().unwrap();
        let interface_decl = self.decl_tables.interface_decl(interface_decl_id);
        let is_interface_generic = interface_decl.is_generic();

        let named_type = pure_operand_type.as_named_type().unwrap();
        let interface_type_args = named_type.type_args.clone();

        let Some(interface_method_decl_id) = interface_decl.methods.get(&method_call.name) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectMethodNotDefined {
                    object_name: interface_decl.name.clone(),
                    method_name: method_call.name.clone(),
                }),
                loc: Some(method_call.loc),
                hint: None,
            });
            return None;
        };

        let mut interface_method_decl = self.decl_tables.method_decl(interface_method_decl_id);

        let generic_params = interface_decl.generic_params.clone();
        let generic_env = self.create_inference_generic_env(
            &interface_decl.name,
            generic_params,
            &interface_type_args,
            method_call.loc,
        )?;

        self.with_generic_env(generic_env, |this| {
            this.normalize_func_params(&mut interface_method_decl.func_decl.params);

            interface_method_decl.func_decl.params =
                this.substitute_func_params(interface_method_decl.func_decl.params);

            interface_method_decl.func_decl.ret_type = this.substitute_type(&interface_method_decl.func_decl.ret_type);

            if !this.analyze_call(
                &mut interface_method_decl.func_decl,
                &mut method_call.args,
                method_call.loc,
                true, // always instance call
            ) {
                return None;
            }

            debug_assert!(
                interface_method_decl
                    .func_decl
                    .params
                    .get_self_modifier()
                    .unwrap()
                    .kind
                    .is_referenced()
            );

            interface_method_decl.func_decl.ret_type = this.substitute_type(&interface_method_decl.func_decl.ret_type);

            let ret_type = interface_method_decl.func_decl.ret_type.clone();

            let method_self_type = SemaType::Pointer(Box::new(concrete_type.clone()));

            let method_index = interface_decl.method_index(&method_call.name).unwrap();

            let vtable_id = this
                .vtable_registry
                .get(&concrete_type, (interface_decl_id, interface_type_args.clone()));

            // checking object is generic or not is necessary here
            // because `is_interface_generic` is not enough solely.
            // and we prevent of monomorphization of concrete methods.
            let is_object_generic = !this
                .decl_tables
                .type_decl_generic_params(concrete_type.as_named_type().unwrap().type_decl_id)
                .is_empty();

            if is_interface_generic && is_object_generic {
                // monomorphize object method
                let concrete_named_type = concrete_type.as_named_type().unwrap();

                let object_methods = this
                    .decl_tables
                    .methods_decl_of_named_type(concrete_named_type)
                    .unwrap();

                let object_generic_params = this
                    .decl_tables
                    .type_decl_generic_params(concrete_named_type.type_decl_id);

                let object_method_decl_id = object_methods.get(&method_call.name).unwrap();
                let mut object_method_decl = this.decl_tables.method_decl(object_method_decl_id);

                let object_method_generic_env = this.create_inference_generic_env(
                    &method_call.name,
                    object_generic_params,
                    &concrete_named_type.type_args,
                    method_call.loc,
                )?;

                this.with_generic_env(object_method_generic_env, |this| {
                    object_method_decl.func_decl.params =
                        this.substitute_func_params(object_method_decl.func_decl.params.clone());

                    object_method_decl.func_decl.ret_type =
                        this.substitute_type(&object_method_decl.func_decl.ret_type);

                    let self_modifier = object_method_decl.func_decl.params.get_self_modifier_mut().unwrap();

                    self_modifier.ty = this.substitute_self_type(self_modifier.ty.clone(), &concrete_type);

                    let func_type = object_method_decl.func_decl.as_func_type();

                    let parent_infer_ctx = this.func_env.infer.clone();
                    let func_env = this.create_method_env(object_method_decl_id, func_type, parent_infer_ctx);

                    this.with_func_env(func_env, |this| {
                        this.func_env.current_object = Some(concrete_type.clone());

                        let Some((monomorph_id, _)) = this.monomorphize_generic_method_call(
                            concrete_type.clone(),
                            object_method_decl_id,
                            object_method_decl,
                            true,
                            method_call.loc,
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

            method_call.operand.ty = Some(operand_type.clone());

            method_call.dispatch = TypedMethodCallDispatch::Interface {
                vtable_id,
                interface_decl_id,
                index: method_index,
                method_self_type,
            };

            Some(ret_type)
        })
    }

    fn analyze_method_call_internal(
        &mut self,
        method_call: &mut TypedMethodCall,
        method_decls: &MethodDecls,
        type_decl_id: TypeDeclID,
        mut operand_type: SemaType,
        is_instance_method_call: bool,
    ) -> Option<SemaType> {
        let pure_operand_type = operand_type.const_inner().pointer_inner().clone();

        let object_name = self.formatter.format_type_decl(type_decl_id);

        let Some(method_decl_id) = method_decls.get(&method_call.name) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectMethodNotDefined {
                    object_name,
                    method_name: method_call.name.clone(),
                }),
                loc: Some(method_call.loc),
                hint: None,
            });
            return None;
        };

        let (operand_generic_params, operand_type_args) = pure_operand_type
            .as_named_type()
            .map(|named_type| {
                (
                    self.decl_tables.type_decl_generic_params(named_type.type_decl_id),
                    named_type.type_args.clone(),
                )
            })
            .unwrap_or((TypedGenericParams::new(), TypedTypeArgs::new()));

        let operand_generic_env = self.create_inference_generic_env(
            &format_sema_type(operand_type.clone(), self.formatter),
            operand_generic_params.clone(),
            &operand_type_args,
            method_call.loc,
        )?;

        let mut method_decl = self.decl_tables.method_decl(method_decl_id);

        if method_decl.is_instance_method() && !is_instance_method_call {
            let method_name = method_decl.func_decl.name.clone();

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InstanceMethodCallOnType { method_name }),
                loc: Some(method_call.loc),
                hint: Some("Use an object to call this method, e.g. 'obj.method(...)'.".to_string()),
            });

            return None;
        }

        if !method_decl.is_instance_method() && is_instance_method_call {
            let method_name = method_decl.func_decl.name.clone();

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::StaticMethodCallOnInstance { method_name }),
                loc: Some(method_call.loc),
                hint: Some("Call static methods on the type, e.g. 'TypeName.method(...)'.".to_string()),
            });

            return None;
        }

        let method_generic_params = method_decl.func_decl.generic_params.clone();

        let method_generic_env = self.create_inference_generic_env(
            &method_call.name,
            method_generic_params.clone(),
            &method_call.type_args,
            method_call.loc,
        )?;

        let generic_env = operand_generic_env.merge(method_generic_env);

        let is_generic_method_call = !generic_env.params.is_empty();

        self.with_generic_env(generic_env, |this| {
            this.normalize_func_params(&mut method_decl.func_decl.params);

            // analyze call arguments
            if !this.analyze_call(
                &mut method_decl.func_decl,
                &mut method_call.args,
                method_call.loc,
                is_instance_method_call,
            ) {
                return None;
            }

            this.validate_method_call(
                &operand_type,
                &method_decl,
                method_decls,
                &object_name,
                method_call.is_thin_arrow,
                false,
                method_call.loc,
            );

            // apply defaults for method generics
            this.apply_generic_defaults(method_decl.func_decl.generic_params.clone());

            // instantiate operand type with collected type args
            operand_type = this.substitute_type(&operand_type);

            if let Some(named_type) = operand_type.const_inner_mut().pointer_inner_mut().as_named_type_mut() {
                let inferred_operand_type_args = this.collect_instantiated_type_args(operand_generic_params);

                named_type.type_args = inferred_operand_type_args;
            }

            let inferred_method_call_type_args = this.collect_instantiated_type_args(method_generic_params.clone());
            method_call.type_args = inferred_method_call_type_args;

            // generic static method
            if is_generic_method_call {
                let func_type = method_decl.func_decl.as_func_type();

                let parent_infer_ctx = this.func_env.infer.clone();
                let func_env = this.create_method_env(method_decl_id, func_type, parent_infer_ctx);

                return this.with_func_env(func_env, |this| {
                    this.func_env.current_object = Some(pure_operand_type.clone());

                    method_decl.func_decl.params = this.substitute_func_params(method_decl.func_decl.params.clone());

                    method_decl.func_decl.ret_type = this.substitute_type(&method_decl.func_decl.ret_type);

                    let self_modifier = method_decl.func_decl.params.get_self_modifier_mut().unwrap();

                    self_modifier.ty = this.substitute_self_type(self_modifier.ty.clone(), &operand_type);

                    let ret_type = method_decl.func_decl.ret_type.clone();

                    let (monomorph_id, final_type_args) = this.monomorphize_generic_method_call(
                        operand_type.clone(),
                        method_decl_id,
                        method_decl,
                        false,
                        method_call.loc,
                    )?;

                    method_call.type_args = final_type_args;
                    method_call.operand.ty = Some(operand_type.clone());
                    method_call.dispatch = TypedMethodCallDispatch::Monomorph {
                        monomorph_id,
                        is_instance_method: is_instance_method_call,
                    };

                    Some(ret_type)
                });
            }
            // normal static method
            else {
                method_call.operand.ty = Some(operand_type.clone());
                method_call.dispatch = TypedMethodCallDispatch::Normal { method_decl_id };

                Some(method_decl.func_decl.ret_type.clone())
            }
        })
    }

    fn monomorphize_generic_method_call(
        &mut self,
        operand_type: SemaType,
        method_decl_id: MethodDeclID,
        mut method_decl: MethodDecl,
        is_generic_interface_method_call: bool,
        loc: Loc,
    ) -> Option<(MonomorphID, TypedTypeArgs)> {
        let pure_operand_type = operand_type.const_inner().pointer_inner().clone();

        if !is_generic_interface_method_call {
            // substitute Self inside params, ret_type, variables
            self.apply_self_type_in_method_decl_and_variable(&mut method_decl, &pure_operand_type);
        }

        method_decl.func_decl.params = self.substitute_func_params(method_decl.func_decl.params.clone());
        method_decl.func_decl.ret_type = self.substitute_type(&method_decl.func_decl.ret_type);

        self.check_type_arity(method_decl.func_decl.ret_type.clone(), loc)?;

        let final_type_args = self.collect_instantiated_type_args(method_decl.func_decl.generic_params.clone());

        // get or create monomorph instance
        let monomorph_id = self.monomorph_registry.get_or_create(
            MonomorphizableTemplateID::Method(method_decl_id),
            final_type_args.clone(),
            method_decl.func_decl.params.clone(),
            method_decl.func_decl.ret_type.clone(),
        );

        let monomorph_instance = self.monomorph_registry.get(monomorph_id);

        // analyze monomorphized body if not analyzed yet
        if !monomorph_instance.analyzed {
            let body_id = method_decl.body.unwrap();
            let template_body = self.decl_tables.body(body_id);

            let mut specialized_body = self.specialize_func_body(&template_body, &mut method_decl.func_decl.params);

            let diag_len = self.reporter.len();

            self.analyze_func_body(&mut specialized_body, &method_decl.func_decl.ret_type);

            let monomorph_body_id = self.monomorph_registry.insert_monomorph_body(specialized_body);

            let diag_origin = format_loc(&self.source_map, loc);

            if self.reporter.len() > diag_len {
                self.apply_error_originated_from_on_diag_range(diag_len..=diag_len, |diag| {
                    diag.hint = Some(format!("Error originates from this method call at {}.", diag_origin));
                });
            }

            self.monomorph_registry.update(monomorph_id, |inst| {
                inst.analyzed = true;
                inst.body = Some(monomorph_body_id);

                // ensure unique var_decl_id remapping per monomorph instance
                inst.params = method_decl.func_decl.params.clone();
            });
        }

        Some((monomorph_id, final_type_args))
    }

    fn monomorphize_generic_func_call(
        &mut self,
        operand_type: &SemaType,
        func_call: &mut TypedFuncCall,
        func_decl_id: FuncDeclID,
        func_decl: &mut FuncDecl,
        final_type_args: TypedTypeArgs,
    ) -> Option<SemaType> {
        func_call.operand.ty = Some(self.substitute_type(&operand_type));
        func_decl.params = self.substitute_func_params(func_decl.params.clone());
        func_decl.ret_type = self.substitute_type(&func_decl.ret_type);

        // get or get monomorph instance
        let monomorph_id = self.monomorph_registry.get_or_create(
            MonomorphizableTemplateID::Func(func_decl_id),
            final_type_args.clone(),
            func_decl.params.clone(),   // specialized params
            func_decl.ret_type.clone(), // specialized ret
        );

        let monomorph_instance = self.monomorph_registry.get(monomorph_id);

        // analyze monomorphized body if not analyzed yet
        if !monomorph_instance.analyzed {
            let func_env = self.create_func_def_env(func_decl.as_func_type());

            self.with_func_env(func_env, |this| {
                let body_id = func_decl.body.unwrap();
                let template_body = this.decl_tables.body(body_id);

                let mut specialized_body = this.specialize_func_body(&template_body, &mut func_decl.params);

                let diag_len = this.reporter.len();

                this.analyze_func_body(&mut specialized_body, &func_decl.ret_type);

                let monomorph_body_id = this.monomorph_registry.insert_monomorph_body(specialized_body);

                let diag_origin = format_loc(&this.source_map, func_call.loc);

                if this.reporter.len() > diag_len {
                    this.apply_error_originated_from_on_diag_range(diag_len..=diag_len, |diag| {
                        diag.hint = Some(format!("Error originates from this function call at {}.", diag_origin));
                    });
                }

                this.monomorph_registry.update(monomorph_id, |inst| {
                    inst.analyzed = true;
                    inst.body = Some(monomorph_body_id);

                    // ensure unique var_decl_id remapping per monomorph instance
                    inst.params = func_decl.params.clone();
                });
            });
        }

        func_call.dispatch = TypedFuncCallDispatch::Monomorph { monomorph_id };

        Some(func_decl.ret_type.clone())
    }
}

impl<'a> AnalysisContext<'a> {
    /// Validates method call visibility, accessibility, and pointer semantics.
    fn validate_method_call(
        &mut self,
        operand_type: &SemaType,
        method_decl: &MethodDecl,
        method_decls: &MethodDecls,
        object_name: &str,
        is_thin_arrow: bool,
        is_interface_method_call: bool,
        loc: Loc,
    ) {
        let access_violation = if let Some(current_method_id) = self.func_env.current_method {
            if method_decls.contains_method_id(current_method_id) {
                false
            } else {
                !method_decl.func_decl.modifiers.vis.is_public() && !is_interface_method_call
            }
        } else {
            !method_decl.func_decl.modifiers.vis.is_public() && !is_interface_method_call
        };

        if access_violation {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InternalMethodCall {
                    method_name: method_decl.func_decl.name.to_string(),
                    object_name: object_name.to_string(),
                }),
                loc: Some(loc),
                hint: None,
            });
        }

        let base_type = operand_type.const_inner();

        let is_pointer = base_type.is_pointer();

        let is_object =
            base_type.is_struct() || base_type.is_union() || base_type.is_enum() || is_interface_method_call;

        if is_thin_arrow {
            if !is_pointer {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                    loc: Some(loc),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
            }
        } else {
            if !is_object {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                    loc: Some(loc),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
            }
        }
    }
}

impl<'a> AnalysisContext<'a> {
    fn analyze_argument(&mut self, arg: &mut TypedExpr, mut expected_type: SemaType, loc: Loc) -> Option<SemaType> {
        expected_type = self.substitute_type(&expected_type);

        let Some(mut arg_type) = self.analyze_expr(arg, Some(expected_type.clone())) else {
            return None;
        };

        arg_type = self.substitute_type(&arg_type);

        if let Some(infer) = &mut self.func_env.infer {
            infer.unify(&expected_type, &arg_type);

            expected_type = infer.resolve(&expected_type);
            arg_type = infer.resolve(&arg_type);
        }

        expected_type = self.substitute_type(&expected_type);

        if !self.is_assignable_to(arg_type.clone(), expected_type.clone(), loc) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                    param_type: format_sema_type(expected_type.clone(), self.formatter),
                    argument_type: format_sema_type(arg_type.clone(), self.formatter),
                    argument_idx: 0,
                }),
                loc: Some(arg.loc),
                hint: None,
            });
        }

        Some(expected_type)
    }

    /// Validates function calls against their signature, checking argument counts and types.
    ///
    /// Performs comprehensive validation of function calls including argument count checking,
    /// type compatibility validation, and variadic argument handling. Supports both regular
    /// functions and instance methods (with self parameter adjustment).
    fn analyze_call(
        &mut self,
        func_decl: &mut FuncDecl,
        args: &mut Vec<TypedExpr>,
        loc: Loc,
        instance_method_call: bool,
    ) -> bool {
        let is_variadic = func_decl.params.variadic.is_some();
        let mut expected_args_len = func_decl.params.list.len();

        // if this is an instance method call, self modifier will be pushed later
        if instance_method_call && !func_decl.params.list.is_empty() {
            expected_args_len = expected_args_len.saturating_sub(1);
        }

        // check argument count
        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name: func_decl.name.clone(),
                }),
                loc: Some(loc),
                hint: None,
            });
            return false;
        }

        // handle variadic arguments
        if is_variadic {
            self.check_func_variadic_arguments(&func_decl, args, loc);
        }

        // analyze static arguments
        let start_idx = if instance_method_call { 1 } else { 0 };

        for (param, arg) in func_decl.params.list.iter_mut().skip(start_idx).zip(args.iter_mut()) {
            let Some(param_type) = self.normalize_and_check_type_formation(param.param_type(), param.loc()) else {
                continue;
            };

            if self.analyze_argument(arg, param_type.clone(), arg.loc).is_none() {
                continue;
            }
        }

        true
    }

    /// Validates variadic arguments in function calls, ensuring type compatibility and proper analysis.
    ///
    /// Performs type checking and semantic analysis for variadic arguments in function calls,
    /// handling both typed variadic parameters (with explicit type requirements) and untyped
    /// C-style variadic parameters. This function processes arguments beyond the static
    /// parameter list according to the function's variadic specification.
    fn check_func_variadic_arguments(&mut self, func_decl: &FuncDecl, args: &mut Vec<TypedExpr>, loc: Loc) {
        let static_params_len = func_decl.params.list.len();
        let variadic_args = &mut args[static_params_len..];

        if let Some(variadic) = &func_decl.params.variadic {
            match &variadic {
                TypedFuncVariadicParam::Typed { ty, .. } => {
                    for (i, arg) in variadic_args.iter_mut().enumerate() {
                        if let Some(arg_type) = self.analyze_expr(arg, arg.ty.clone()) {
                            if !self.is_assignable_to(arg_type.clone(), ty.clone(), loc) {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                        param_type: format_sema_type(ty.clone(), self.formatter),
                                        argument_type: format_sema_type(arg_type, self.formatter),
                                        argument_idx: (i + static_params_len) as u32,
                                    }),
                                    loc: Some(loc),
                                    hint: None,
                                });
                            }
                        }
                    }
                }
                TypedFuncVariadicParam::UntypedCStyle => {
                    for arg in variadic_args.iter_mut() {
                        let Some(arg_type) = self.analyze_expr(arg, arg.ty.clone()) else {
                            continue;
                        };

                        if arg_type.is_void() {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: Box::new(AnalyzerDiagKind::VoidArgumentInVariadicParam),
                                loc: Some(loc),
                                hint: None,
                            });
                        }
                    }
                }
            }
        }
    }

    /// Validates calls to function type values (function pointers, lambdas).
    ///
    /// Type-checks calls to first-class function values (function types) rather than
    /// named functions. Similar to `check_func_call` but works with function type
    /// definitions rather than function signatures.
    fn check_func_type_call(
        &mut self,
        func_type: &mut TypedFuncType,
        args: &mut Vec<TypedExpr>,
        loc: Loc,
    ) -> Option<SemaType> {
        let is_variadic = func_type.params.variadic.is_some();
        let expected_args_len = func_type.params.list.len();
        let func_name = format_func_type(func_type, self.formatter);

        // check argument count

        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name,
                }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        }

        // analyze static arguments

        let start_idx = 0;

        for (param_idx, (param, arg)) in func_type
            .params
            .list
            .iter_mut()
            .skip(start_idx)
            .zip(args.iter_mut())
            .enumerate()
        {
            let param_type = self.normalize_and_check_type_formation(param.clone(), loc).unwrap();

            let arg_type = match self.analyze_expr(arg, Some(param_type.clone())) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            if !self.is_assignable_to(arg_type.clone(), param_type.clone(), arg.loc) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_sema_type(param_type.clone(), self.formatter),
                        argument_type: format_sema_type(arg_type, self.formatter),
                        argument_idx: param_idx as u32,
                    }),
                    loc: Some(arg.loc),
                    hint: None,
                });
            }
        }

        // handle variadic arguments

        if is_variadic {
            let static_params_len = func_type.params.list.len();
            let variadic_args = &mut args[static_params_len..];

            if let Some(variadic) = &func_type.params.variadic {
                match *variadic.clone() {
                    TypedFuncTypeVariadicParam::Typed(variadic_param_type) => {
                        for (i, arg) in variadic_args.iter_mut().enumerate() {
                            if let Some(arg_type) = self.analyze_expr(arg, arg.ty.clone()) {
                                if !self.is_assignable_to(arg_type.clone(), variadic_param_type.clone(), loc) {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: Box::new(AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                            param_type: format_sema_type(variadic_param_type.clone(), self.formatter),
                                            argument_type: format_sema_type(arg_type, self.formatter),
                                            argument_idx: (i + static_params_len) as u32,
                                        }),
                                        loc: Some(loc),
                                        hint: None,
                                    });
                                }
                            }
                        }
                    }
                    TypedFuncTypeVariadicParam::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            self.analyze_expr(arg, arg.ty.clone());
                        }
                    }
                }
            }
        }

        Some(*func_type.ret_type.clone())
    }
}
