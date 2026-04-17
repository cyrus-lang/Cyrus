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
use cyrusc_internal::monomorph::CallableTemplateID;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    decls::{DeclID, FuncDecl, MethodDecl, MethodDeclID, MethodDecls},
    exprs::{
        TypedExprKind, TypedExprStmt, TypedFuncCall, TypedFuncCallDispatch, TypedMethodCall, TypedMethodCallDispatch,
    },
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

                    func_call.operand.sema_type = Some(this.substitute_type(&operand_type));
                    func_decl.params = this.substitute_func_params(func_decl.params.clone());
                    func_decl.ret_type = this.substitute_ret_type(&func_decl.ret_type);

                    this.apply_generic_defaults(func_decl.generic_params.clone());

                    let final_type_args = this.collect_instantiated_type_args(func_decl.generic_params.clone());

                    let monomorph_id = this.monomorph_registry.get_or_create(
                        CallableTemplateID::Func(func_decl_id),
                        final_type_args,
                        func_decl.params.clone(),   // specialized params
                        func_decl.ret_type.clone(), // specialized ret type
                    );

                    let monomorph_instance = this.monomorph_registry.get(monomorph_id);

                    // analyze specialized function body
                    if !monomorph_instance.analyzed {
                        let func_env = this.create_func_def_env(func_decl.as_func_type());

                        this.with_func_env(func_env, |this| {
                            let body_id = func_decl.body.unwrap();

                            let template_body = this.decl_tables.body(body_id);

                            // create per‑monomorph locals and remap decl ids
                            let mut specialized_body = this.specialize_func_body(&template_body, &mut func_decl.params);

                            let diag_len = this.reporter.len();

                            // analyze body
                            this.analyze_func_body(&mut specialized_body, &func_decl.ret_type);

                            // insert monomorphized body
                            let monomorph_body_id = this.monomorph_registry.insert_monomorph_body(specialized_body);

                            let diag_originated_from = format_loc(&this.source_map, func_call.loc);

                            if this.reporter.len() > diag_len {
                                this.apply_error_originated_from_on_diag_range(diag_len..=diag_len, |diag| {
                                    diag.hint = Some(format!(
                                        "Error originates from this function call at {}.",
                                        diag_originated_from,
                                    ));
                                });
                            }

                            this.monomorph_registry.update(monomorph_id, |_monomorph_instance| {
                                _monomorph_instance.analyzed = true;
                                _monomorph_instance.body = Some(monomorph_body_id);

                                // params must be cloned after specialization so that each monomorph
                                // owns updated var_decl_ids; avoids aliasing the template params and ensures
                                // decl_id mappings are consistent across monomorph instances.
                                _monomorph_instance.params = func_decl.params.clone();
                            });
                        });
                    }

                    func_call.dispatch = TypedFuncCallDispatch::Monomorph { monomorph_id };

                    Some(func_decl.ret_type)
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
        let is_operand_instance = self.is_method_call_operand_instance(&method_call.operand);

        self.normalize_type_args(&mut method_call.type_args);

        if is_operand_instance {
            self.analyze_instance_method_call(method_call)
        } else {
            self.analyze_static_method_call(method_call)
        }
    }

    fn analyze_instance_method_call(&mut self, method_call: &mut TypedMethodCall) -> Option<SemaType> {
        let operand_type = self.analyze_expr(&mut method_call.operand, None)?;

        let Some((type_decl_id, method_decls)) = self
            .normalize_sema_type(method_call.operand.kind.as_type_expr().unwrap(), method_call.loc)
            .and_then(|sema_type| {
                sema_type.as_named_type().and_then(|named_type| {
                    self.methods_decl_of_named_type(named_type)
                        .map(|method_decls| (named_type.type_decl_id, method_decls))
                })
            })
        else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields),
                loc: Some(method_call.loc),
                hint: None,
            });
            return None;
        };

        // insert instance value to args
        method_call.args.insert(0, *method_call.operand.clone());

        self.analyze_method_call_internal(method_call, &method_decls, type_decl_id, operand_type, true)
    }

    fn analyze_static_method_call(&mut self, method_call: &mut TypedMethodCall) -> Option<SemaType> {
        let operand_type = self.normalize_sema_type(method_call.operand.sema_type.clone().unwrap(), method_call.loc)?;

        let Some((type_decl_id, method_decls)) = self
            .normalize_sema_type(method_call.operand.kind.as_type_expr().unwrap(), method_call.loc)
            .and_then(|sema_type| {
                sema_type.as_named_type().and_then(|named_type| {
                    self.methods_decl_of_named_type(named_type)
                        .map(|method_decls| (named_type.type_decl_id, method_decls))
                })
            })
        else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields),
                loc: Some(method_call.loc),
                hint: None,
            });
            return None;
        };

        if !method_call.type_args.is_empty() {
            // FIXME
            // self.reporter.report(Diag {
            //     level: DiagLevel::Error,
            //     kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
            //     loc: Some(method_call.loc),
            //     hint: None,
            // });
            return None;
        }

        // REVIEW:
        // validate_type_arity is required for operand_type before we get dive into method's own generic env??

        self.analyze_method_call_internal(method_call, &method_decls, type_decl_id, operand_type, false)
    }

    fn analyze_method_call_internal(
        &mut self,
        method_call: &mut TypedMethodCall,
        method_decls: &MethodDecls,
        type_decl_id: TypeDeclID,
        mut operand_type: SemaType,
        is_instance_method_call: bool,
    ) -> Option<SemaType> {
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

        let (operand_generic_params, operand_type_args) = operand_type
            .as_named_type()
            .map(|named_type| {
                (
                    self.type_decl_generic_params(named_type.type_decl_id),
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

        let method_generic_env = self.create_inference_generic_env(
            &method_call.name,
            method_decl.func_decl.generic_params.clone(),
            &method_call.type_args,
            method_call.loc,
        )?;

        let generic_env = operand_generic_env.merge(method_generic_env);

        let is_static_generic_method_call = !generic_env.params.is_empty();

        self.with_generic_env(generic_env, |this| {
            this.normalize_func_params(&mut method_decl.func_decl.params);

            operand_type = this.substitute_type(&operand_type);
            method_decl.func_decl.params = this.substitute_func_params(method_decl.func_decl.params.clone());
            method_decl.func_decl.ret_type = this.substitute_ret_type(&method_decl.func_decl.ret_type);

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
                method_call.is_fat_arrow,
                false,
                method_call.loc,
            );

            // generic static method
            if is_static_generic_method_call {
                let func_type = method_decl.func_decl.as_func_type();
                let func_env = this.create_method_env(method_decl_id, func_type);

                this.with_func_env(func_env, |this| {
                    // set self type
                    this.func_env.current_object = Some(operand_type.clone());

                    // apply defaults for method generics
                    this.apply_generic_defaults(method_decl.func_decl.generic_params.clone());

                    operand_type = this.substitute_type(&operand_type);
                    method_decl.func_decl.params = this.substitute_func_params(method_decl.func_decl.params.clone());
                    method_decl.func_decl.ret_type = this.substitute_ret_type(&method_decl.func_decl.ret_type);

                    let final_type_args =
                        this.collect_instantiated_type_args(method_decl.func_decl.generic_params.clone());

                    // emit monomorphized function
                    let monomorph_id = this.monomorph_registry.get_or_create(
                        CallableTemplateID::Method(method_decl_id),
                        final_type_args.clone(),
                        method_decl.func_decl.params.clone(),   // specialized params
                        method_decl.func_decl.ret_type.clone(), // specialized ret type
                    );

                    let monomorph_instance = this.monomorph_registry.get(monomorph_id);

                    // analyze specialized function body
                    if !monomorph_instance.analyzed {
                        let body_id = method_decl.body.unwrap();

                        let template_body = this.decl_tables.body(body_id);

                        // create per‑monomorph locals and remap decl ids
                        let mut specialized_body =
                            this.specialize_func_body(&template_body, &mut method_decl.func_decl.params);

                        let diag_len = this.reporter.len();

                        // analyze body
                        this.analyze_func_body(&mut specialized_body, &method_decl.func_decl.ret_type);

                        // insert monomorphized body
                        let monomorph_body_id = this.monomorph_registry.insert_monomorph_body(specialized_body);

                        let diag_originated_from = format_loc(&this.source_map, method_decl.func_decl.loc);

                        if this.reporter.len() > diag_len {
                            this.apply_error_originated_from_on_diag_range(diag_len..=diag_len, |diag| {
                                diag.hint = Some(format!(
                                    "Error originates from this function call at {}.",
                                    diag_originated_from,
                                ));
                            });
                        }

                        this.monomorph_registry.update(monomorph_id, |_monomorph_instance| {
                            _monomorph_instance.analyzed = true;
                            _monomorph_instance.body = Some(monomorph_body_id);

                            // params must be cloned after specialization so that each monomorph
                            // owns updated var_decl_ids; avoids aliasing the template params and ensures
                            // decl_id mappings are consistent across monomorph instances.
                            _monomorph_instance.params = method_decl.func_decl.params.clone();
                        });
                    }

                    method_call.type_args = final_type_args;
                    method_call.operand.sema_type = Some(operand_type.clone());

                    method_call.dispatch = TypedMethodCallDispatch::Monomorph {
                        method_decl_id,
                        monomorph_id,
                        self_type: operand_type.clone(),
                    };

                    Some(method_decl.func_decl.ret_type.clone())
                })
            }
            // normal static method
            else {
                method_call.operand.sema_type = Some(operand_type.clone());
                method_call.dispatch = TypedMethodCallDispatch::Normal {
                    method_decl_id,
                    self_type: operand_type.clone(),
                };

                Some(method_decl.func_decl.ret_type.clone())
            }
        })
    }
}

impl<'a> AnalysisContext<'a> {
    /// Returns true if the operand to a method call is a value (instance), not a type.
    fn is_method_call_operand_instance(&self, operand: &TypedExprStmt) -> bool {
        operand
            .kind
            .as_type_expr()
            .and_then(|ty| {
                ty.as_unresolved_base_decl_id()
                    .map(|decl_id| decl_id.is_var_or_global_var())
            })
            .unwrap_or(false)
    }

    fn methods_decl_of_named_type(&self, named_type: &NamedType) -> Option<MethodDecls> {
        match named_type.type_decl_id {
            TypeDeclID::Struct(struct_decl_id) => {
                let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

                Some(struct_decl.methods)
            }
            TypeDeclID::Enum(enum_decl_id) => {
                let enum_decl = self.decl_tables.enum_decl(enum_decl_id);

                Some(enum_decl.methods)
            }
            TypeDeclID::Union(union_decl_id) => {
                let union_decl = self.decl_tables.union_decl(union_decl_id);

                Some(union_decl.methods)
            }
            // FIXME
            TypeDeclID::Interface(_interface_decl_id) => todo!(),
            TypeDeclID::Typedef(_) => None,
        }
    }

    /// Validates method call visibility, accessibility, and pointer semantics.
    fn validate_method_call(
        &mut self,
        operand_type: &SemaType,
        method_decl: &MethodDecl,
        method_decls: &MethodDecls,
        object_name: &str,
        is_fat_arrow: bool,
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

        if is_fat_arrow {
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
    fn analyze_argument(&mut self, arg: &mut TypedExprStmt, mut expected_type: SemaType, loc: Loc) -> Option<SemaType> {
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
        args: &mut Vec<TypedExprStmt>,
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
    fn check_func_variadic_arguments(&mut self, func_decl: &FuncDecl, args: &mut Vec<TypedExprStmt>, loc: Loc) {
        let static_params_len = func_decl.params.list.len();
        let variadic_args = &mut args[static_params_len..];

        if let Some(variadic) = &func_decl.params.variadic {
            match &variadic {
                TypedFuncVariadicParam::Typed { ty, .. } => {
                    for (i, arg) in variadic_args.iter_mut().enumerate() {
                        if let Some(arg_type) = self.analyze_expr(arg, arg.sema_type.clone()) {
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
                        self.analyze_expr(arg, arg.sema_type.clone());
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
        args: &mut Vec<TypedExprStmt>,
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
                            if let Some(arg_type) = self.analyze_expr(arg, arg.sema_type.clone()) {
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
                            self.analyze_expr(arg, arg.sema_type.clone());
                        }
                    }
                }
            }
        }

        Some(*func_type.ret_type.clone())
    }
}
