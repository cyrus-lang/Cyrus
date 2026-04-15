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
    decls::{DeclID, FuncDecl, MethodDecls},
    exprs::{TypedExprStmt, TypedFuncCall, TypedFuncCallDispatch, TypedMethodCall},
    format::{format_func_type, format_loc, format_sema_type},
    stmts::{TypedFuncTypeVariadicParam, TypedFuncVariadicParam},
    types::{SemaType, TypedFuncType},
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
                    func_decl.ret_type = {
                        let ret_type = this.substitute_type(&func_decl.ret_type);

                        if let Some(infer) = &mut this.func_env.infer {
                            infer.resolve(&ret_type)
                        } else {
                            ret_type
                        }
                    };

                    this.apply_generic_defaults(func_decl.generic_params.clone());

                    let final_type_args = this.collect_instantiated_type_args(func_decl.generic_params.clone());

                    let monomorph_id = this.monomorph_registry.get_or_create(
                        func_decl_id,
                        final_type_args,
                        func_decl.params.clone(),   // specialized params
                        func_decl.ret_type.clone(), // specialized ret type
                    );

                    let monomorph_instance = this.monomorph_registry.get(monomorph_id);

                    // analyze specialized function body
                    if !monomorph_instance.analyzed {
                        let monomorph_func_env = this.create_monomorph_func_env(func_decl.as_func_type());

                        this.with_func_env(monomorph_func_env, |this| {
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

    pub(crate) fn analyze_method_call(&mut self, method_call: &mut TypedMethodCall) -> Option<SemaType> {
        let operand_type = self.analyze_expr_non_terminal(&mut method_call.operand, None)?;

        self.normalize_type_args(&mut method_call.type_args);

        todo!();
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

    /// Validates method call visibility, accessibility, and pointer semantics.
    fn validate_method_call(
        &mut self,
        operand_ty: &SemaType,
        method_decl: &FuncDecl,
        method_decls: &MethodDecls,
        method_name: &str,
        object_name: &str,
        is_fat_arrow: bool,
        method_call_on_interface: bool,
        loc: Loc,
    ) {
        // ------------------------------------------------------------
        // 1️⃣ Visibility check (modern style — mirrors field access)
        // ------------------------------------------------------------

        let access_violation = if let Some(current_method_id) = self.func_env.current_method {
            if method_decls.contains_method_id(current_method_id) {
                false
            } else {
                !method_decl.modifiers.vis.is_public() && !method_call_on_interface
            }
        } else {
            !method_decl.modifiers.vis.is_public() && !method_call_on_interface
        };

        if access_violation {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InternalMethodCall {
                    method_name: method_name.to_string(),
                    object_name: object_name.to_string(),
                }),
                loc: Some(loc),
                hint: None,
            });
        }

        let base_type = operand_ty.const_inner();

        let is_pointer = base_type.is_pointer();

        let is_object =
            base_type.is_struct() || base_type.is_union() || base_type.is_enum() || method_call_on_interface;

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
