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
    builtins::{
        TypedBuiltinForm, TypedBuiltinFunc, TypedBuiltinKind, TypedBuiltinSpec, builtin_spec_of, lookup_builtin,
    },
    format::format_sema_type,
    types::{PlainType, SemaType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_builtin_func(&self, builtin_func: &TypedBuiltinFunc) -> Option<SemaType> {
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

        self.validate_builtin_func_semantics(builtin_kind, builtin_func)
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

    fn validate_builtin_func_semantics(&self, kind: TypedBuiltinKind, builtin: &TypedBuiltinFunc) -> Option<SemaType> {
        match kind {
            TypedBuiltinKind::SizeOf => self.validate_builtin_sizeof(builtin),
            TypedBuiltinKind::AlignOf => self.validate_builtin_alignof(builtin),
            TypedBuiltinKind::Memcpy => self.validate_builtin_memcpy(builtin),
            TypedBuiltinKind::Memset => self.validate_builtin_memset(builtin),
            _ => {
                unreachable!()
            }
        }
    }
}

impl<'a> AnalysisContext<'a> {
    fn validate_builtin_alignof(&self, _builtin_func: &TypedBuiltinFunc) -> Option<SemaType> {
        Some(SemaType::Plain(PlainType::USize))
    }

    fn validate_builtin_sizeof(&self, _builtin_func: &TypedBuiltinFunc) -> Option<SemaType> {
        Some(SemaType::Plain(PlainType::USize))
    }

    fn validate_builtin_memset(&self, builtin_func: &TypedBuiltinFunc) -> Option<SemaType> {
        let ptr = &builtin_func.args[0];
        let val = &builtin_func.args[1];
        let size = &builtin_func.args[2];

        if !ptr.ty.as_ref()?.is_pointer() {
            let argument_type = format_sema_type(ptr.ty.clone().unwrap(), self.formatter);
            let param_type = format_sema_type(
                SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void))),
                self.formatter,
            );

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                    param_type,
                    argument_type,
                    argument_idx: 0,
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });
            return None;
        }

        if !val.ty.as_ref()?.is_integer() {
            let argument_type = format_sema_type(ptr.ty.clone().unwrap(), self.formatter);
            let param_type = format_sema_type(SemaType::Plain(PlainType::USize), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                    param_type,
                    argument_type,
                    argument_idx: 1,
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });
            return None;
        }

        if !size.ty.as_ref()?.is_integer() {
            let argument_type = format_sema_type(ptr.ty.clone().unwrap(), self.formatter);
            let param_type = format_sema_type(SemaType::Plain(PlainType::USize), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                    param_type,
                    argument_type,
                    argument_idx: 0,
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });
            return None;
        }

        Some(SemaType::Plain(PlainType::Void))
    }

    fn validate_builtin_memcpy(&self, builtin_func: &TypedBuiltinFunc) -> Option<SemaType> {
        let dst = &builtin_func.args[0];
        let src = &builtin_func.args[1];
        let size = &builtin_func.args[2];

        if !dst.ty.as_ref()?.is_pointer() {
            let argument_type = format_sema_type(dst.ty.clone().unwrap(), self.formatter);
            let param_type = format_sema_type(
                SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void))),
                self.formatter,
            );

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                    param_type,
                    argument_type,
                    argument_idx: 0,
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });

            return None;
        }

        if !src.ty.as_ref()?.is_pointer() {
            let argument_type = format_sema_type(src.ty.clone().unwrap(), self.formatter);
            let param_type = format_sema_type(
                SemaType::Pointer(Box::new(SemaType::Plain(PlainType::Void))),
                self.formatter,
            );

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                    param_type,
                    argument_type,
                    argument_idx: 1,
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });

            return None;
        }

        if !size.ty.as_ref()?.is_integer() {
            let argument_type = format_sema_type(size.ty.clone().unwrap(), self.formatter);
            let param_type = format_sema_type(SemaType::Plain(PlainType::USize), self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                    param_type,
                    argument_type,
                    argument_idx: 2,
                }),
                loc: Some(builtin_func.loc),
                hint: None,
            });

            return None;
        }

        Some(SemaType::Plain(PlainType::Void))
    }
}
