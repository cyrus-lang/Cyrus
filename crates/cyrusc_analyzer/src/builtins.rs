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
use cyrusc_internal::flow_state::FlowState;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    builtins::{
        TypedBuiltin, TypedBuiltinBlock, TypedBuiltinForm, TypedBuiltinFunc, TypedBuiltinKind, TypedBuiltinSpec,
        builtin_spec_of, lookup_builtin,
    },
    exprs::TypedExprKind,
    format::{format_sema_type, format_struct_decl},
    stmts::TypedStmt,
    types::{PlainType, SemaType},
};

// Builtins entry point.
impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_builtin(&mut self, typed_stmt: &mut TypedStmt, is_toplevel: bool) -> FlowState {
        let TypedStmt::Builtin(builtin) = typed_stmt else {
            unreachable!()
        };

        match builtin {
            TypedBuiltin::BuiltinFunc(builtin_func) => {
                let Some(builtin_kind) = lookup_builtin(&builtin_func.name.value) else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::BuiltinNotDefined {
                            name: builtin_func.name.as_string(),
                        }),
                        loc: Some(builtin_func.loc),
                        hint: None,
                    });
                    return FlowState::Reachable;
                };

                let builtin_spec = builtin_spec_of(builtin_kind);

                self.analyze_builtin_expr(builtin_func);

                if builtin_spec.unreachable {
                    FlowState::Unreachable
                } else {
                    FlowState::Reachable
                }
            }

            TypedBuiltin::BuiltinBlock(block) => self.analyze_builtin_block(block, is_toplevel),
        }
    }

    pub(crate) fn analyze_builtin_expr(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let Some(builtin_kind) = lookup_builtin(&builtin_func.name.value) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::BuiltinNotDefined {
                    name: builtin_func.name.as_string(),
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });
            return None;
        };

        let builtin_spec = builtin_spec_of(builtin_kind);

        if !self.validate_builtin_form(builtin_spec, TypedBuiltinForm::Expr, builtin_func.loc) {
            return None;
        }

        if !self.validate_builtin_arg_count(builtin_spec, builtin_func.args.len(), builtin_func.loc) {
            return None;
        }

        self.analyze_builtin_func_semantics(&builtin_kind, builtin_func)
    }

    fn validate_builtin_form(&self, builtin_spec: &TypedBuiltinSpec, actual: TypedBuiltinForm, loc: Loc) -> bool {
        if builtin_spec.form != actual {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidBuiltinForm {
                    name: builtin_spec.name.to_string(),
                    expected: builtin_spec.form,
                    found: actual,
                }),
                loc: Some(loc),
                hint: None,
            });
            return false;
        }

        true
    }

    fn validate_builtin_arg_count(&self, spec: &TypedBuiltinSpec, count: usize, loc: Loc) -> bool {
        if count < spec.min_args {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::BuiltinTooFewArgs {
                    name: spec.name.to_string(),
                    expected: spec.min_args,
                    found: count,
                }),
                loc: Some(loc),
                hint: None,
            });

            return false;
        }

        if let Some(max) = spec.max_args {
            if count > max {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::BuiltinTooManyArgs {
                        name: spec.name.to_string(),
                        expected: max,
                        found: count,
                    }),
                    loc: Some(loc),
                    hint: None,
                });

                return false;
            }
        }

        true
    }
}

// Builtin Statements
impl<'a> AnalysisContext<'a> {
    fn analyze_builtin_block(&mut self, builtin_block: &mut TypedBuiltinBlock, is_toplevel: bool) -> FlowState {
        let Some(builtin_kind) = lookup_builtin(&builtin_block.name.value) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::BuiltinNotDefined {
                    name: builtin_block.name.as_string(),
                }),
                loc: Some(builtin_block.loc),
                hint: None,
            });
            return FlowState::Reachable;
        };

        let builtin_spec = builtin_spec_of(builtin_kind);

        if !self.validate_builtin_form(builtin_spec, TypedBuiltinForm::Stmt, builtin_block.loc) {
            return FlowState::Reachable;
        }

        builtin_block.is_toplevel = Some(is_toplevel);

        if is_toplevel {
            for stmt in &mut builtin_block.block.stmts {
                self.analyze_toplevel_stmt(stmt);
            }

            FlowState::Reachable
        } else {
            self.analyze_block_stmt(&mut builtin_block.block)
        }
    }
}

// BuiltinFunc (Expr)
impl<'a> AnalysisContext<'a> {
    fn analyze_builtin_func_semantics(
        &mut self,
        builtin_kind: &TypedBuiltinKind,
        builtin_func: &mut TypedBuiltinFunc,
    ) -> Option<SemaType> {
        match builtin_kind {
            TypedBuiltinKind::FuncName => self.analyze_builtin_func_name(builtin_func),
            TypedBuiltinKind::MethodName => self.analyze_builtin_func_name(builtin_func),
            TypedBuiltinKind::ModuleName => self.analyze_builtin_module_name(builtin_func),
            TypedBuiltinKind::FileName => self.analyze_builtin_file_name(builtin_func),
            TypedBuiltinKind::Line => self.analyze_builtin_line(builtin_func),
            TypedBuiltinKind::Column => self.analyze_builtin_column(builtin_func),
            TypedBuiltinKind::SizeOf => self.analyze_builtin_sizeof(builtin_func),
            TypedBuiltinKind::AlignOf => self.analyze_builtin_alignof(builtin_func),
            TypedBuiltinKind::OffsetOf => self.analyze_builtin_offsetof(builtin_func),
            TypedBuiltinKind::TypeOf => self.analyze_builtin_typeof(builtin_func),
            TypedBuiltinKind::Memcpy => self.analyze_builtin_memcpy(builtin_func),
            TypedBuiltinKind::Memset => self.analyze_builtin_memset(builtin_func),
            TypedBuiltinKind::Cast => self.analyze_builtin_cast(builtin_func),
            TypedBuiltinKind::Assert => self.analyze_builtin_assert(builtin_func),
            TypedBuiltinKind::Panic => self.analyze_builtin_panic(builtin_func),
            TypedBuiltinKind::Todo => self.analyze_builtin_panic(builtin_func),
            TypedBuiltinKind::Unimplemented => self.analyze_builtin_unimplemented(builtin_func),
            TypedBuiltinKind::Unreachable => self.analyze_builtin_unreachable(builtin_func),

            _ => {
                unreachable!()
            }
        }
    }

    fn analyze_builtin_func_name(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let ret_type = SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char)));

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_module_name(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let ret_type = SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char)));

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_file_name(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let ret_type = SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char)));

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_line(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let ret_type = SemaType::Plain(PlainType::USize);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_column(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let ret_type = SemaType::Plain(PlainType::USize);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_alignof(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let operand = builtin_func.args.first_mut().unwrap();

        self.analyze_expr_non_terminal(operand, None);

        let ret_type = SemaType::Plain(PlainType::USize);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_sizeof(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let operand = builtin_func.args.first_mut().unwrap();

        self.analyze_expr_non_terminal(operand, None);

        let ret_type = SemaType::Plain(PlainType::USize);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_offsetof(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        for arg in &mut builtin_func.args {
            self.analyze_expr_non_terminal(arg, None);
        }

        let type_expr = &builtin_func.args[0];
        let field_name_expr = &builtin_func.args[1];

        let Some(ty) = type_expr.ty.clone() else { return None };

        if !ty.is_struct() {
            let arg_type = format_sema_type(ty.clone(), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidOffsetOfOperand { arg_type }),
                loc: Some(builtin_func.loc),
                hint: None,
            });
            return None;
        }

        let field_name = match field_name_expr.literal_const_string_value() {
            Some(name) => name,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::OffsetOfFieldMustBeString),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        };

        let struct_decl_id = match ty.as_struct() {
            Some(decl) => decl,
            None => return None,
        };

        let struct_decl = self.decl_tables.struct_decl(struct_decl_id);

        let struct_name = format_struct_decl(&struct_decl, self.formatter);

        let field_exists = struct_decl.fields.iter().any(|field| field.name == field_name);

        if !field_exists {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                    object_name: struct_name,
                    field_name: field_name.to_string(),
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });
            return None;
        }

        let ret_type = SemaType::Plain(PlainType::USize);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_typeof(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let operand = builtin_func.args.first_mut().unwrap();

        self.analyze_expr_non_terminal(operand, None);

        operand.ty.clone()
    }

    fn analyze_builtin_cast(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        self.analyze_expr_non_terminal(&mut builtin_func.args[0], None);
        self.analyze_expr(&mut builtin_func.args[1], None);

        let type_expr = &builtin_func.args[0];
        let value_expr = &builtin_func.args[1];

        let Some(mut target_type) = type_expr.kind.as_type_expr() else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::BuiltinCastRequiresTypeArgument),
                loc: Some(builtin_func.loc),
                hint: None,
            });
            return None;
        };

        target_type = self.expand_sema_type(target_type, builtin_func.loc);

        let value_type = value_expr.ty.as_ref()?;

        if !self.is_explicit_cast_allowed(value_type.clone(), target_type.clone()) {
            let value_type = format_sema_type(value_type.clone(), self.formatter);
            let target_type = format_sema_type(target_type, self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotCast {
                    value_type,
                    target_type,
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });
            return None;
        }

        // IMPORTANT: update analyzed type argument
        let type_expr = builtin_func.args.first_mut().unwrap();
        type_expr.kind = TypedExprKind::SemaType {
            ty: target_type.clone(),
            loc: builtin_func.loc,
        };
        type_expr.ty = Some(target_type.clone());

        builtin_func.ret_type = Some(target_type.clone());

        Some(target_type)
    }

    fn analyze_builtin_memset(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let param_types = [
            SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void))), // dest
            SemaType::Plain(PlainType::Int8),                              // value
            SemaType::Plain(PlainType::USize),                             // size
        ];

        for (arg, expected_type) in builtin_func.args.iter_mut().zip(param_types.iter()) {
            self.analyze_expr_non_terminal(arg, Some(expected_type.clone()));
        }

        for (idx, (arg, expected_type)) in builtin_func.args.iter().zip(param_types.iter()).enumerate() {
            let Some(arg_type) = arg.ty.clone() else { return None };

            if !self.is_assignable_to(arg_type.clone(), expected_type.clone(), builtin_func.loc) {
                let argument_type = format_sema_type(arg_type, self.formatter);

                let param_type = format_sema_type(expected_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type,
                        argument_type,
                        argument_idx: idx as u32,
                    }),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        }

        let ret_type = SemaType::Plain(PlainType::Void);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_memcpy(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let param_types = [
            SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void))), // dest
            SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void))), // src
            SemaType::Plain(PlainType::USize),                             // size
        ];

        for (arg, expected_type) in builtin_func.args.iter_mut().zip(param_types.iter()) {
            self.analyze_expr_non_terminal(arg, Some(expected_type.clone()));
        }

        for (idx, (arg, expected_type)) in builtin_func.args.iter().zip(param_types.iter()).enumerate() {
            let Some(arg_type) = arg.ty.clone() else { return None };

            if !self.is_assignable_to(arg_type.clone(), expected_type.clone(), builtin_func.loc) {
                let argument_type = format_sema_type(arg_type, self.formatter);
                let param_type = format_sema_type(expected_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type,
                        argument_type,
                        argument_idx: idx as u32,
                    }),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        }

        let ret_type = SemaType::Plain(PlainType::Void);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_panic(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let param_types = [
            SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char))), // msg? (char*)
        ];

        for (arg, expected_type) in builtin_func.args.iter_mut().zip(param_types.iter()) {
            self.analyze_expr_non_terminal(arg, Some(expected_type.clone()));
        }

        for (idx, (arg, expected_type)) in builtin_func.args.iter().zip(param_types.iter()).enumerate() {
            let Some(arg_type) = arg.ty.clone() else { return None };

            if !self.is_assignable_to(arg_type.clone(), expected_type.clone(), builtin_func.loc) {
                let argument_type = format_sema_type(arg_type, self.formatter);
                let param_type = format_sema_type(expected_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type,
                        argument_type,
                        argument_idx: idx as u32,
                    }),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        }

        let ret_type = SemaType::Plain(PlainType::Void);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_todo(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let param_types = [
            SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char))), // msg? (char*)
        ];

        for (arg, expected_type) in builtin_func.args.iter_mut().zip(param_types.iter()) {
            self.analyze_expr_non_terminal(arg, Some(expected_type.clone()));
        }

        for (idx, (arg, expected_type)) in builtin_func.args.iter().zip(param_types.iter()).enumerate() {
            let Some(arg_type) = arg.ty.clone() else { return None };

            if !self.is_assignable_to(arg_type.clone(), expected_type.clone(), builtin_func.loc) {
                let argument_type = format_sema_type(arg_type, self.formatter);
                let param_type = format_sema_type(expected_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type,
                        argument_type,
                        argument_idx: idx as u32,
                    }),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        }

        let ret_type = SemaType::Plain(PlainType::Void);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_unimplemented(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let param_types = [
            SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char))), // msg? (char*)
        ];

        for (arg, expected_type) in builtin_func.args.iter_mut().zip(param_types.iter()) {
            self.analyze_expr_non_terminal(arg, Some(expected_type.clone()));
        }

        for (idx, (arg, expected_type)) in builtin_func.args.iter().zip(param_types.iter()).enumerate() {
            let Some(arg_type) = arg.ty.clone() else { return None };

            if !self.is_assignable_to(arg_type.clone(), expected_type.clone(), builtin_func.loc) {
                let argument_type = format_sema_type(arg_type, self.formatter);
                let param_type = format_sema_type(expected_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type,
                        argument_type,
                        argument_idx: idx as u32,
                    }),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        }

        let ret_type = SemaType::Plain(PlainType::Void);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_unreachable(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let param_types = [
            SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char))), // msg? (char*)
        ];

        for (arg, expected_type) in builtin_func.args.iter_mut().zip(param_types.iter()) {
            self.analyze_expr_non_terminal(arg, Some(expected_type.clone()));
        }

        for (idx, (arg, expected_type)) in builtin_func.args.iter().zip(param_types.iter()).enumerate() {
            let Some(arg_type) = arg.ty.clone() else { return None };

            if !self.is_assignable_to(arg_type.clone(), expected_type.clone(), builtin_func.loc) {
                let argument_type = format_sema_type(arg_type, self.formatter);
                let param_type = format_sema_type(expected_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type,
                        argument_type,
                        argument_idx: idx as u32,
                    }),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        }

        let ret_type = SemaType::Plain(PlainType::Void);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }

    fn analyze_builtin_assert(&mut self, builtin_func: &mut TypedBuiltinFunc) -> Option<SemaType> {
        let param_types = [
            SemaType::Plain(PlainType::Bool),                              // cond (bool)
            SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Char))), // msg? (char*)
        ];

        for (arg, expected_type) in builtin_func.args.iter_mut().zip(param_types.iter()) {
            self.analyze_expr_non_terminal(arg, Some(expected_type.clone()));
        }

        for (idx, (arg, expected_type)) in builtin_func.args.iter().zip(param_types.iter()).enumerate() {
            let Some(arg_type) = arg.ty.clone() else { return None };

            if !self.is_assignable_to(arg_type.clone(), expected_type.clone(), builtin_func.loc) {
                let argument_type = format_sema_type(arg_type, self.formatter);
                let param_type = format_sema_type(expected_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type,
                        argument_type,
                        argument_idx: idx as u32,
                    }),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        }

        if let Some(msg) = builtin_func.args.get(1) {
            if msg.literal_const_string_value().is_none() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AssertMessageMustBeConstStringLiteral),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return None;
            }
        }

        let ret_type = SemaType::Plain(PlainType::Void);

        builtin_func.ret_type = Some(ret_type.clone());

        Some(ret_type)
    }
}
