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

use crate::context::AnalysisContext;
use cyrusc_typed_ast::{
    decls::FuncDecl,
    stmts::{TypedFuncParamKind, TypedFuncParams, TypedFuncTypeParams, TypedFuncTypeVariadicParam, TypedTypeArg},
    types::{NamedType, SemaType, TypedArrayType, TypedFuncType, TypedTupleType},
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn substitute_type(&self, ty: &SemaType) -> SemaType {
        let mut result = ty.clone();

        for generic_env in self.generic_env_stack.iter().rev() {
            if result.is_self_type() {
                if let Some(object_type) = &self.func_env.current_object {
                    result = generic_env.substitute_sema_type(&object_type);
                    continue;
                }
            }

            result = generic_env.substitute_sema_type(&result);
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

    #[inline]
    pub(crate) fn substitute_ret_type(&mut self, ty: &SemaType) -> SemaType {
        let ret_type = self.substitute_type(&ty);

        if let Some(infer) = &mut self.func_env.infer {
            infer.resolve(&ret_type)
        } else {
            ret_type
        }
    }

    pub(crate) fn substitute_self_type_in_func_decl(
        &mut self,
        func_decl: &mut FuncDecl,
        concrete_self_type: &SemaType,
    ) {
        for param_kind in func_decl.params.list.iter_mut() {
            match param_kind {
                TypedFuncParamKind::FuncParam(func_param) => {
                    func_param.ty = self.substitute_self_type(func_param.ty.clone(), concrete_self_type);

                    if let Some(infer) = &self.func_env.infer {
                        func_param.ty = infer.resolve(&func_param.ty);
                    }
                }

                TypedFuncParamKind::SelfModifier(self_param) => {
                    self_param.ty = self.substitute_self_type(self_param.ty.clone(), concrete_self_type);

                    if let Some(infer) = &self.func_env.infer {
                        self_param.ty = infer.resolve(&self_param.ty);
                    }
                }
            }
        }

        let substituted_ret_type = self.substitute_self_type(func_decl.ret_type.clone(), concrete_self_type);

        func_decl.ret_type = if let Some(infer) = &self.func_env.infer {
            infer.resolve(&substituted_ret_type)
        } else {
            substituted_ret_type
        };
    }

    pub fn substitute_self_type(&mut self, mut sema_type: SemaType, self_type: &SemaType) -> SemaType {
        sema_type = self.substitute_type(&sema_type);

        if let Some(infer) = &mut self.func_env.infer {
            sema_type = infer.resolve(&sema_type);
        }

        match sema_type {
            SemaType::Unresolved(_)
            | SemaType::GenericParam(_)
            | SemaType::InferVar(_)
            | SemaType::Plain(_)
            | SemaType::Placeholder
            | SemaType::Err(_) => sema_type.clone(),

            SemaType::SelfType(_) => self_type.clone(),

            SemaType::Named(named) => {
                let type_args = named
                    .type_args
                    .iter()
                    .map(|arg| match arg {
                        TypedTypeArg::Type(ty, loc) => {
                            TypedTypeArg::Type(self.substitute_self_type(ty.clone(), self_type), *loc)
                        }

                        TypedTypeArg::Infer => TypedTypeArg::Infer,
                    })
                    .collect();

                SemaType::Named(NamedType {
                    type_decl_id: named.type_decl_id,
                    type_args,
                })
            }

            SemaType::Array(array) => SemaType::Array(TypedArrayType {
                element_type: Box::new(self.substitute_self_type(*array.element_type.clone(), self_type)),
                capacity: array.capacity.clone(),
                loc: array.loc,
            }),

            SemaType::Const(inner) => SemaType::Const(Box::new(self.substitute_self_type(*inner.clone(), self_type))),
            SemaType::Pointer(inner) => {
                SemaType::Pointer(Box::new(self.substitute_self_type(*inner.clone(), self_type)))
            }

            SemaType::Tuple(tuple) => SemaType::Tuple(TypedTupleType {
                elements: tuple
                    .elements
                    .iter()
                    .map(|ty| self.substitute_self_type(ty.clone(), self_type))
                    .collect(),
                loc: tuple.loc,
            }),

            SemaType::FuncType(func) => {
                let params = func
                    .params
                    .list
                    .iter()
                    .map(|ty| self.substitute_self_type(ty.clone(), self_type))
                    .collect();

                let variadic = func.params.variadic.as_ref().map(|vbox| match vbox.as_ref() {
                    TypedFuncTypeVariadicParam::UntypedCStyle => vbox.clone(),

                    TypedFuncTypeVariadicParam::Typed(ty) => Box::new(TypedFuncTypeVariadicParam::Typed(
                        self.substitute_self_type(ty.clone(), self_type),
                    )),
                });

                SemaType::FuncType(TypedFuncType {
                    params: TypedFuncTypeParams { list: params, variadic },
                    ret_type: Box::new(self.substitute_self_type(*func.ret_type.clone(), self_type)),
                    is_public: func.is_public,
                    loc: func.loc,
                })
            }

            SemaType::InterfaceType(interface) => SemaType::InterfaceType(interface.clone()),
        }
    }
}
