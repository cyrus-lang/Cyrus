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
    SymbolID,
    decls::FuncDecl,
    exprs::{TypedExprStmt, TypedFuncCall, TypedFuncCallDispatch},
    format::{format_func_type, format_sema_type},
    stmts::{TypedFuncParamKind, TypedFuncTypeVariadicParams, TypedFuncVariadicParam},
    types::{SemanticType, TypedFuncType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_func_call(&mut self, func_call: &mut TypedFuncCall) -> Option<SemanticType> {
        let operand_type = self.analyze_expr_non_terminal(&mut func_call.operand, None)?;

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

        if let Some(symbol_id) = func_type.symbol_id {
            // named func call

            let Some(func_decl_id) = self.query.get_func(symbol_id) else {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::NonFunctionSymbol {
                        symbol_name: self.formatter.format_symbol_name(symbol_id),
                    }),
                    loc: Some(func_call.loc),
                    hint: None,
                });
                return None;
            };

            let mut func_decl = self.decl_tables.func_decl(func_decl_id);

            self.normalize_func_params(&mut func_decl.params, func_call.loc);
            func_decl.ret_type = self.normalize_sema_type(func_decl.ret_type.clone(), func_call.loc)?;

            let ret_type = self.check_func_call(&mut func_decl, &mut func_call.args, func_call.loc, false)?;

            func_call.dispatch = TypedFuncCallDispatch::Direct {
                func_decl_id: func_decl_id,
            };

            Some(ret_type)
        } else {
            // function pointer call

            if !func_call.type_args.is_empty() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                    loc: Some(func_call.loc),
                    hint: Some("Lambdas never accept type args.".to_string()),
                });
                return None;
            }

            self.normalize_func_type_params(&mut func_type.params, func_call.loc);
            func_type.ret_type = Box::new(self.normalize_sema_type(*func_type.ret_type.clone(), func_call.loc)?);

            let ret_type = self.check_func_type_call(&mut func_type, &mut func_call.args, func_call.loc)?;

            func_call.dispatch = TypedFuncCallDispatch::FunctionPointer {
                func_type: func_type.clone(),
            };

            Some(ret_type)
        }
    }

    /// Validates function calls against their signature, checking argument counts and types.
    ///
    /// Performs comprehensive validation of function calls including argument count checking,
    /// type compatibility validation, and variadic argument handling. Supports both regular
    /// functions and instance methods (with self parameter adjustment).
    fn check_func_call(
        &mut self,
        func_decl: &mut FuncDecl,
        args: &mut Vec<TypedExprStmt>,
        loc: Loc,
        instance_method_call: bool,
    ) -> Option<SemanticType> {
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
            return None;
        }

        // handle variadic arguments
        if is_variadic {
            self.check_func_variadic_arguments(&func_decl, args, loc);
        }

        // analyze static arguments
        let start_idx = if instance_method_call { 1 } else { 0 };

        for (param_idx, (param, arg)) in func_decl
            .params
            .list
            .iter_mut()
            .skip(start_idx)
            .zip(args.iter_mut())
            .enumerate()
        {
            let mut param_type = param.param_type().unwrap();

            let arg_type = match self.analyze_expr(arg, Some(param_type.clone())) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            param_type = match self.normalize_and_check_sema_ty(param_type, param.loc()) {
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
                    loc: Some(loc),
                    hint: None,
                });
            }
        }

        self.normalize_sema_type(func_decl.ret_type.clone(), loc)
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

        if let Some(var_param) = &func_decl.params.variadic {
            match var_param.clone() {
                TypedFuncVariadicParam::Typed(_, variadic_param_type) => {
                    for (i, arg) in variadic_args.iter_mut().enumerate() {
                        if let Some(arg_type) = self.analyze_expr(arg, arg.sema_type.clone()) {
                            if !self.is_assignable_to(arg_type.clone(), variadic_param_type.clone(), arg.loc) {
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
    ) -> Option<SemanticType> {
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
            let param_type = self.normalize_sema_type(param.clone(), loc).unwrap();

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
                    loc: Some(loc),
                    hint: None,
                });
            }
        }

        // handle variadic arguments

        if is_variadic {
            let static_params_len = func_type.params.list.len();
            let variadic_args = &mut args[static_params_len..];

            if let Some(var_param) = &func_type.params.variadic {
                match *var_param.clone() {
                    TypedFuncTypeVariadicParams::Typed(variadic_param_type) => {
                        for (i, arg) in variadic_args.iter_mut().enumerate() {
                            if let Some(arg_type) = self.analyze_expr(arg, arg.sema_type.clone()) {
                                if !self.is_assignable_to(arg_type.clone(), variadic_param_type.clone(), arg.loc) {
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
                    TypedFuncTypeVariadicParams::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            self.analyze_expr(arg, arg.sema_type.clone());
                        }
                    }
                }
            }
        }

        Some(*func_type.ret_type.clone())
    }

    // /// Validates a method call's accessibility, syntax, and mutability constraints.
    // fn validate_method_call(
    //     &mut self,
    //     instance_symbol_id: SymbolID,
    //     method_name: &String,
    //     method_call_operand_ty: SemanticType,
    //     is_fat_arrow: bool,
    //     method_call_on_interface: bool,
    //     first_param_opt: Option<&TypedFuncParamKind>,
    //     object_methods_opt: Option<HashMap<String, SymbolID>>,
    //     object_name: String,
    //     func_decl: &FuncDecl,
    //     loc: Loc,
    // ) -> bool {
    //     let mut result = true;
    //     let vis = &func_decl.modifiers.vis;

    //     let access_violation = if let Some(current_method_symbol_id) = self.fenv.current_method_symbol_id {
    //         let object_contains_method = {
    //             if let Some(object_methods) = object_methods_opt {
    //                 object_methods
    //                     .values()
    //                     .cloned()
    //                     .collect::<Vec<SymbolID>>()
    //                     .contains(&current_method_symbol_id)
    //             } else {
    //                 true
    //             }
    //         };

    //         if object_contains_method {
    //             false
    //         } else {
    //             !vis.is_public() && !method_call_on_interface
    //         }
    //     } else {
    //         !vis.is_public() && !method_call_on_interface
    //     };

    //     if access_violation {
    //         self.reporter.report(Diag {
    //             level: DiagLevel::Error,
    //             kind: Box::new(AnalyzerDiagKind::InternalMethodCall {
    //                 method_name: func_decl.name.clone(),
    //                 object_name,
    //             }),
    //             loc: Some(loc),
    //             hint: None,
    //         });
    //         result = false;
    //     }

    //     let is_pointer = method_call_operand_ty.const_inner().is_pointer();
    //     let is_operand_const = method_call_operand_ty.is_const();
    //     let is_object = method_call_operand_ty.const_inner().is_resolved_symbol()
    //         || method_call_operand_ty.as_generic_type().is_some()
    //         || method_call_on_interface;

    //     if is_fat_arrow {
    //         if !is_pointer {
    //             self.reporter.report(Diag {
    //                 level: DiagLevel::Error,
    //                 kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
    //                 loc: Some(loc),
    //                 hint: Some("Use '.' instead of '->'.".to_string()),
    //             });
    //             result = false;
    //         }
    //     } else {
    //         if !is_object {
    //             self.reporter.report(Diag {
    //                 level: DiagLevel::Error,
    //                 kind: Box::new(AnalyzerDiagKind::UseThinArrow),
    //                 loc: Some(loc),
    //                 hint: Some("Use '->' when accessing through a pointer.".to_string()),
    //             });
    //             result = false;
    //         }
    //     }

    //     // TODO: Extend it after implement const self modifier for methods:
    //     //
    //     // struct Foo {
    //     //   ...
    //     //
    //     //   fn bar1(&const self, x: int) int { ... }
    //     //   fn bar2(&const self, const x: int) int { ... }
    //     //   fn bar3(&self, x: int) int { ... }
    //     // }
    //     if let Some(first_param) = first_param_opt {
    //         if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = first_param {
    //             if typed_self_modifier.kind == SelfModifierKind::Referenced && is_operand_const {
    //                 let instance_name = self.formatter.format_symbol_name(instance_symbol_id);

    //                 self.reporter.report(Diag {
    //                     level: DiagLevel::Error,
    //                     kind: Box::new(AnalyzerDiagKind::MutationPossibleMethodCallOnConstInstance {
    //                         method_name: method_name.clone(),
    //                         instance_name: instance_name.clone(),
    //                     }),
    //                     loc: Some(loc),
    //                     hint: Some(format!(
    //                         "Instance '{}' is declared as 'const' and cannot be modified.",
    //                         instance_name
    //                     )),
    //                 });
    //                 result = false;
    //             }
    //         }
    //     }

    //     result
    // }
}
