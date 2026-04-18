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

use cyrusc_typed_ast::{
    GenericParamID,
    stmts::{TypedFuncTypeParams, TypedFuncTypeVariadicParam, TypedGenericParams, TypedTypeArg, TypedTypeArgs},
    types::{NamedType, SemaType, TypedArrayType, TypedFuncType, TypedTupleType},
};

use crate::context::AnalysisContext;

#[derive(Debug, Clone)]
pub(crate) struct GenericEnv {
    pub params: TypedGenericParams,
    bindings: Vec<Option<SemaType>>,
}

impl GenericEnv {
    pub fn new(params: TypedGenericParams) -> Self {
        let bindings = vec![None; params.len()];

        Self { params, bindings }
    }

    /// Merge two generic environments.
    ///
    /// The resulting environment keeps:
    ///   - All params and bindings of `self` first
    ///   - Then all params and bindings of `other`
    ///
    /// Parameter IDs are unique, so no deduplication is required.
    pub fn merge(&self, other: GenericEnv) -> GenericEnv {
        let mut params = self.params.clone();
        params.0.extend(other.params.iter().cloned());

        let mut bindings = self.bindings.clone();
        bindings.extend(other.bindings.iter().cloned());

        GenericEnv { params, bindings }
    }

    pub fn from_type_args(params: TypedGenericParams, type_args: &TypedTypeArgs) -> Self {
        let mut generic_env = GenericEnv::new(params.clone());

        for (param, arg) in params.iter().zip(type_args.iter()) {
            if let TypedTypeArg::Type(ty, _) = arg {
                generic_env.bind(*param, ty.clone());
            }
        }

        generic_env
    }

    pub fn bind(&mut self, generic_param_id: GenericParamID, ty: SemaType) -> bool {
        let Some(idx) = self.slot(generic_param_id) else {
            return false;
        };

        match &self.bindings[idx] {
            Some(existing) => existing == &ty,
            None => {
                self.bindings[idx] = Some(ty);
                true
            }
        }
    }

    pub fn lookup(&self, generic_param_id: GenericParamID) -> Option<&SemaType> {
        let idx = self.slot(generic_param_id)?;
        self.bindings[idx].as_ref()
    }

    fn slot(&self, param: GenericParamID) -> Option<usize> {
        self.params.iter().position(|p| *p == param)
    }
}

impl GenericEnv {
    #[inline]
    pub fn substitute_sema_type(&self, sema_type: &SemaType) -> SemaType {
        match sema_type {
            SemaType::Unresolved(_) | SemaType::InferVar(_) | SemaType::Plain(_) | SemaType::Placeholder => {
                sema_type.clone()
            }
            SemaType::Named(named_type) => {
                let type_args = named_type
                    .type_args
                    .iter()
                    .map(|type_arg| match type_arg {
                        TypedTypeArg::Type(ty, loc) => TypedTypeArg::Type(self.substitute_sema_type(&ty), *loc),
                        TypedTypeArg::Infer => TypedTypeArg::Infer,
                    })
                    .collect();

                SemaType::Named(NamedType {
                    type_decl_id: named_type.type_decl_id,
                    type_args,
                })
            }
            SemaType::Array(array) => SemaType::Array(TypedArrayType {
                element_type: Box::new(self.substitute_sema_type(&array.element_type)),
                capacity: array.capacity.clone(),
                loc: array.loc,
            }),
            SemaType::Const(inner) => SemaType::Const(Box::new(self.substitute_sema_type(inner))),
            SemaType::Pointer(inner) => SemaType::Pointer(Box::new(self.substitute_sema_type(inner))),
            SemaType::FuncType(func) => {
                let params = func
                    .params
                    .list
                    .iter()
                    .map(|param_ty| self.substitute_sema_type(param_ty))
                    .collect();

                let variadic = func.params.variadic.as_ref().map(|variadic| {
                    Box::new(match variadic.as_ref() {
                        v @ TypedFuncTypeVariadicParam::UntypedCStyle => v.clone(),

                        TypedFuncTypeVariadicParam::Typed(ty) => {
                            TypedFuncTypeVariadicParam::Typed(self.substitute_sema_type(ty))
                        }
                    })
                });

                let ret_type = Box::new(self.substitute_sema_type(&func.ret_type));

                SemaType::FuncType(TypedFuncType {
                    params: TypedFuncTypeParams { list: params, variadic },
                    ret_type,
                    is_public: func.is_public,
                    loc: func.loc,
                })
            }
            SemaType::Tuple(tuple) => SemaType::Tuple(TypedTupleType {
                elements: tuple.elements.iter().map(|t| self.substitute_sema_type(t)).collect(),
                loc: tuple.loc,
            }),
            SemaType::GenericParam(id) => match self.lookup(*id) {
                Some(ty) => ty.clone(),
                None => sema_type.clone(),
            },
            SemaType::SelfType(self_type) => SemaType::SelfType(self_type.clone()),
            SemaType::InterfaceType(interface) => SemaType::InterfaceType(interface.clone()),

            SemaType::Err(_) => sema_type.clone(),
        }
    }
}

impl<'a> AnalysisContext<'a> {
    #[inline]
    pub(crate) fn current_generic_env_mut(&mut self) -> Option<&mut GenericEnv> {
        self.generic_env_stack.last_mut()
    }
}
