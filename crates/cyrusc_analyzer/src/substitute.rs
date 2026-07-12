// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::context::AnalysisContext;
use cyrusc_typed_ast::{
    stmts::{TypedFuncParamKind, TypedFuncParams, TypedFuncTypeVariadicParam, TypedTypeArg},
    types::SemaType,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn substitute_type(&self, ty: &SemaType) -> SemaType {
        let mut result = ty.clone();

        for generic_env in self.generic_env_stack.iter().rev() {
            if let Some(object_type) = &self.func_env.current_object {
                result = self.substitute_self_type(result, object_type);
                continue;
            }

            result = generic_env.substitute_sema_type(&result);
        }

        if let Some(infer) = &self.func_env.infer {
            result = infer.resolve(&result);
        }

        result
    }

    pub(crate) fn substitute_func_params(&self, mut params: TypedFuncParams) -> TypedFuncParams {
        params.list.iter_mut().for_each(|param_kind| match param_kind {
            TypedFuncParamKind::FuncParam(func_param) => {
                func_param.ty = self.substitute_type(&func_param.ty);

                if let Some(infer) = &self.func_env.infer {
                    func_param.ty = infer.resolve(&func_param.ty);
                }
            }
            TypedFuncParamKind::SelfModifier(self_modifier) => {
                self_modifier.ty = self.substitute_type(&self_modifier.ty);

                if let Some(infer) = &self.func_env.infer {
                    self_modifier.ty = infer.resolve(&self_modifier.ty);
                }
            }
        });

        params
    }

    pub fn substitute_self_type(&self, ty: SemaType, self_type: &SemaType) -> SemaType {
        match ty {
            SemaType::InterfaceObject(_)
            | SemaType::Unresolved(_)
            | SemaType::GenericParam(_)
            | SemaType::InferVar(_)
            | SemaType::Plain(_)
            | SemaType::Placeholder
            | SemaType::Err(_) => ty.clone(),

            SemaType::SelfType(_) => self_type.clone(),

            SemaType::Named(mut named) => {
                named.type_args = named
                    .type_args
                    .iter()
                    .map(|arg| match arg {
                        TypedTypeArg::Type(ty, loc) => {
                            TypedTypeArg::Type(self.substitute_self_type(ty.clone(), self_type), *loc)
                        }
                        TypedTypeArg::Infer => TypedTypeArg::Infer,
                    })
                    .collect();

                SemaType::Named(named)
            }

            SemaType::Array(mut array) => {
                array.element_type = Box::new(self.substitute_self_type(*array.element_type, self_type));
                SemaType::Array(array)
            }

            SemaType::Const(mut inner) => {
                *inner = self.substitute_self_type(*inner, self_type);
                SemaType::Const(inner)
            }

            SemaType::Pointer(mut inner) => {
                *inner = self.substitute_self_type(*inner, self_type);
                SemaType::Pointer(inner)
            }

            SemaType::Tuple(mut tuple) => {
                tuple.elements = tuple
                    .elements
                    .into_iter()
                    .map(|(ty, loc)| (self.substitute_self_type(ty, self_type), loc))
                    .collect();

                SemaType::Tuple(tuple)
            }

            SemaType::FuncType(mut func) => {
                func.params.list = func
                    .params
                    .list
                    .into_iter()
                    .map(|ty| self.substitute_self_type(ty, self_type))
                    .collect();

                func.params.variadic = func.params.variadic.map(|vbox| {
                    Box::new(match vbox.as_ref() {
                        TypedFuncTypeVariadicParam::UntypedCStyle => vbox.as_ref().clone(),
                        TypedFuncTypeVariadicParam::Typed(ty) => {
                            TypedFuncTypeVariadicParam::Typed(self.substitute_self_type(ty.clone(), self_type))
                        }
                    })
                });

                func.ret_type = Box::new(self.substitute_self_type(*func.ret_type, self_type));

                SemaType::FuncType(func)
            }
        }
    }
}
