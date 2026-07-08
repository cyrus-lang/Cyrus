// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, env::generic_env::GenericEnv};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    GenericParamID,
    format::format_sema_type,
    stmts::{TypedGenericParams, TypedTypeArg, TypedTypeArgs},
    types::SemaType,
};

impl<'a> AnalysisContext<'a> {
    #[inline]
    pub(crate) fn push_generic_env(&mut self, env: GenericEnv) {
        self.generic_env_stack.push(env);
    }

    #[inline]
    pub(crate) fn pop_generic_env(&mut self) {
        self.generic_env_stack.pop();
    }

    pub(crate) fn lookup_generic_binding(&self, generic_param_id: GenericParamID) -> Option<&SemaType> {
        for env in self.generic_env_stack.iter().rev() {
            if let Some(ty) = env.lookup(generic_param_id) {
                return Some(ty);
            }
        }

        None
    }

    pub(crate) fn with_generic_env<F, R>(&mut self, generic_env: GenericEnv, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.push_generic_env(generic_env);
        let result = f(self);
        self.pop_generic_env();
        result
    }

    pub(crate) fn collect_instantiated_type_args(&mut self, generic_params: TypedGenericParams) -> TypedTypeArgs {
        let mut args = Vec::with_capacity(generic_params.len());

        for generic_param_id in generic_params.iter() {
            let generic_param = self.decl_tables.generic_param(*generic_param_id);

            let mut ty = {
                if let Some(ty) = self.lookup_generic_binding(*generic_param_id) {
                    ty.clone()
                } else {
                    unreachable!("missing generic binding")
                }
            };

            let infer = self.func_env.infer.as_mut().unwrap();

            ty = infer.resolve(&ty);

            args.push(TypedTypeArg::Type(ty, generic_param.name.loc));
        }

        TypedTypeArgs(args)
    }

    pub(crate) fn apply_generic_defaults(&mut self, generic_params: TypedGenericParams) {
        for generic_param_id in generic_params.iter() {
            let generic_param = self.decl_tables.generic_param(*generic_param_id);

            let binding = {
                let generic_env = self.current_generic_env_mut().unwrap();
                generic_env.lookup(*generic_param_id).cloned()
            };

            if let Some(bound_type) = binding {
                let resolved_type = self.func_env.infer.as_ref().unwrap().resolve(&bound_type);

                if let SemaType::InferVar(var) = resolved_type {
                    if let Some(mut default_type) = generic_param.default {
                        default_type = Box::new(self.substitute_type(&default_type));

                        self.func_env
                            .infer
                            .as_mut()
                            .unwrap()
                            .unify(&SemaType::InferVar(var), &default_type);
                    }
                }

                continue;
            }

            if let Some(mut default_type) = generic_param.default {
                default_type = Box::new(self.substitute_type(&default_type));

                let generic_env = self.current_generic_env_mut().unwrap();

                generic_env.bind(*generic_param_id, *default_type);
            }
        }
    }

    pub(crate) fn unify_with_expected_type(&mut self, operand_type: SemaType, expected_type: &Option<SemaType>) {
        let Some(expected_type) = expected_type else {
            return;
        };

        if let Some(infer) = &mut self.func_env.infer {
            infer.unify(&expected_type, &operand_type);
        }
    }

    pub(crate) fn create_inference_generic_env(
        &mut self,
        type_name: &String,
        params: TypedGenericParams,
        type_args: &TypedTypeArgs,
        loc: Loc,
    ) -> Option<GenericEnv> {
        if !type_args.is_empty() && params.len() != type_args.len() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::WrongNumberOfTypeArgs {
                    type_name: type_name.clone(),
                    expected: params.len(),
                    provided: type_args.len(),
                }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        }

        let mut generic_env = GenericEnv::new(params.clone());

        for (i, param_id) in params.iter().enumerate() {
            if let Some(infer) = &mut self.func_env.infer {
                let mut bound_type: Option<SemaType> = None;

                // 1. check if an explicit type argument was provided
                if let Some(type_arg) = type_args.get(i) {
                    match type_arg {
                        TypedTypeArg::Type(sema_type, _) => {
                            bound_type = Some(sema_type.clone());
                        }
                        TypedTypeArg::Infer => {
                            bound_type = Some(infer.new_var());
                        }
                    }
                }

                // 2. if nothing was provided (e.g. MyStruct { ... }), create an InferVar
                if bound_type.is_none() {
                    bound_type = Some(infer.new_var());
                }

                // bind it
                if let Some(ty) = bound_type {
                    generic_env.bind(*param_id, ty);
                }
            }
        }

        Some(generic_env)
    }

    pub(crate) fn analyze_generic_bounds(&mut self, generic_params: &TypedGenericParams) {
        for &generic_param_id in generic_params.iter() {
            let mut generic_param = self.decl_tables.generic_param(generic_param_id);

            if let Some(default_type) = &mut generic_param.default {
                if let Some(ty) = self.normalize_sema_type(*default_type.clone(), generic_param.name.loc, 0) {
                    *default_type = Box::new(ty);
                }
            }

            for bound in &mut generic_param.bounds {
                if let Some(ty) = self.normalize_sema_type(bound.ty.clone(), generic_param.name.loc, 0) {
                    if !ty.is_interface() {
                        let found = format_sema_type(ty.clone(), self.formatter);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ExpectedInterfaceInBound { found }),
                            loc: Some(bound.loc),
                            hint: None,
                        });
                    }

                    self.check_type_correctness(ty.clone(), bound.loc);

                    bound.ty = ty;
                }
            }

            self.decl_tables.with_generic_param(generic_param_id, |_generic_param| {
                *_generic_param = generic_param;
            });
        }
    }
}
