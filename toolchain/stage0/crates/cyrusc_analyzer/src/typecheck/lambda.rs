// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::abi::Callconv;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::monomorph::MonomorphizableTemplateID;
use cyrusc_typed_ast::{
    exprs::{TypedFieldAccess, TypedFunctionAddress, TypedFunctionAddressDispatch, TypedLambdaExpr},
    format::format_sema_type,
    types::{SemaType, TypedFuncType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_lambda(&mut self, lambda: &mut TypedLambdaExpr) -> Option<SemaType> {
        self.normalize_func_params(&mut lambda.params, true, 0);

        let params = lambda.params.as_func_type_params();

        lambda.ret_type = self.normalize_and_check_type_formation(lambda.ret_type.clone(), lambda.loc, 0)?;

        let mut func_type = TypedFuncType {
            params,
            ret_type: Box::new(lambda.ret_type.clone()),
            callconv: Callconv::default(),
            is_public: true,
            loc: lambda.loc,
        };

        func_type = self
            .expand_sema_type(SemaType::FuncType(func_type), lambda.loc)
            .as_func_type()
            .unwrap()
            .clone();

        let func_name = "<unnamed>";

        let lambda_env = self.create_func_def_env(func_name, func_type.clone(), None);

        self.with_func_env(lambda_env, |this| {
            this.analyze_block_stmt(&mut lambda.body);
        });

        Some(SemaType::FuncType(func_type))
    }

    pub(crate) fn analyze_field_access_as_object_method(
        &mut self,
        field_access: &mut TypedFieldAccess,
    ) -> Option<(TypedFunctionAddress, SemaType)> {
        let mut operand_type = self.analyze_expr_non_terminal(&mut field_access.operand, None)?;

        // expand operand type
        operand_type = self.expand_sema_type(operand_type, field_access.loc);

        // IMPORTANT: Prevent cascading failures by staying silent when the
        // operand is already poisoned by a previously reported error.
        if operand_type.contains_error() {
            let err_ty = SemaType::Err(field_access.loc);
            field_access.ty = Some(err_ty.clone());
            return None;
        }

        if field_access.is_thin_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                loc: Some(field_access.loc),
                hint: None,
            });
            return None;
        }

        let pure_operand_type = operand_type.const_inner().pointer_inner().clone();

        if !pure_operand_type
            .as_named_type()
            .map(|named_type| named_type.type_args.is_empty())
            .unwrap_or(true)
        {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields {
                    operand_type: format_sema_type(operand_type, self.formatter),
                }),
                loc: Some(field_access.loc),
                hint: None,
            });
            return None;
        }

        let operand_type = field_access.operand.ty.clone().unwrap();

        if let Some(named_type) = operand_type.as_named_type() {
            if let Some(methods) = self.decl_tables.method_decls_of_named_type(named_type) {
                if let Some(method_decl_id) = methods.get(&field_access.name) {
                    let method_decl = self.decl_tables.method_decl(method_decl_id);

                    let func_type = method_decl.func_decl.as_func_type();

                    if named_type.type_args.is_empty() {
                        return Some((
                            TypedFunctionAddress {
                                dispatch: TypedFunctionAddressDispatch::Method(method_decl_id),
                                ty: SemaType::FuncType(func_type),
                            },
                            SemaType::FuncType(method_decl.func_decl.as_func_type()),
                        ));
                    } else {
                        let monomorph_id = self.monomorph_registry.get_or_create(
                            MonomorphizableTemplateID::Method(method_decl_id),
                            named_type.type_args.clone(),
                            method_decl.func_decl.params.clone(),
                            method_decl.func_decl.ret_type.clone(),
                        );

                        return Some((
                            TypedFunctionAddress {
                                dispatch: TypedFunctionAddressDispatch::Monomorph(monomorph_id),
                                ty: SemaType::FuncType(func_type),
                            },
                            SemaType::FuncType(method_decl.func_decl.as_func_type()),
                        ));
                    }
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectMethodNotDefined {
                            object_name: format_sema_type(operand_type, self.formatter),
                            method_name: field_access.name.clone(),
                        }),
                        loc: Some(field_access.loc),
                        hint: None,
                    });
                    return None;
                }
            }
        }

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
