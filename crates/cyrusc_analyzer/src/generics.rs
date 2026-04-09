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

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, env::generic_env::GenericEnv};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    GenericParamID,
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

    #[inline]
    pub(crate) fn current_generic_env_mut(&mut self) -> Option<&mut GenericEnv> {
        self.generic_env_stack.last_mut()
    }

    pub(crate) fn lookup_generic_binding(&self, generic_param_id: GenericParamID) -> Option<&SemaType> {
        for env in self.generic_env_stack.iter().rev() {
            if let Some(ty) = env.lookup(generic_param_id) {
                return Some(ty);
            }
        }

        None
    }

    pub(crate) fn substitute_type(&self, ty: &SemaType) -> SemaType {
        let mut result = ty.clone();

        for env in self.generic_env_stack.iter().rev() {
            result = env.substitute_sema_type(&result);
        }

        result
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

            let mut sema_type = {
                if let Some(ty) = self.lookup_generic_binding(*generic_param_id) {
                    ty.clone()
                } else {
                    unreachable!("missing generic binding")
                }
            };

            let infer = self.func_env.infer.as_mut().unwrap();

            sema_type = infer.resolve(&sema_type);

            args.push(TypedTypeArg::Type(sema_type, generic_param.name.loc));
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

            if let Some(bound_ty) = binding {
                let resolved = self.func_env.infer.as_ref().unwrap().resolve(&bound_ty);

                if let SemaType::InferVar(var) = resolved {
                    if let Some(default_ty) = &generic_param.default {
                        let mut default_type = self
                            .normalize_sema_type(*default_ty.clone(), generic_param.name.loc)
                            .unwrap();

                        default_type = self.substitute_type(&default_type);

                        self.func_env
                            .infer
                            .as_mut()
                            .unwrap()
                            .unify(&SemaType::InferVar(var), &default_type);
                    }
                }

                continue;
            }

            if let Some(default_ty) = &generic_param.default {
                let mut default_type = self
                    .normalize_sema_type(*default_ty.clone(), generic_param.name.loc)
                    .unwrap();

                default_type = self.substitute_type(&default_type);

                let generic_env = self.current_generic_env_mut().unwrap();
                generic_env.bind(*generic_param_id, default_type);
            }
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

        let infer = self.func_env.infer.as_mut().unwrap();

        for (i, param_id) in params.iter().enumerate() {
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

        Some(generic_env)
    }
}
