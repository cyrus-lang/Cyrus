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
    stmts::{TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedGenericParams, TypedTypeArg, TypedTypeArgs},
    types::{NamedType, SemanticType, TypedArrayType, TypedFuncType, TypedTupleType},
};

pub(crate) struct GenericEnv {
    pub params: TypedGenericParams,
    bindings: Vec<Option<SemanticType>>,
}

impl GenericEnv {
    pub fn new(params: TypedGenericParams) -> Self {
        let bindings = vec![None; params.len()];

        Self { params, bindings }
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

    pub fn bind(&mut self, generic_param_id: GenericParamID, ty: SemanticType) -> bool {
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

    pub fn lookup(&self, generic_param_id: GenericParamID) -> Option<&SemanticType> {
        let idx = self.slot(generic_param_id)?;
        self.bindings[idx].as_ref()
    }

    fn slot(&self, param: GenericParamID) -> Option<usize> {
        self.params.iter().position(|p| *p == param)
    }
}

impl GenericEnv {
    #[inline]
    pub fn substitute_sema_type(&self, sema_type: &SemanticType) -> SemanticType {
        match sema_type {
            SemanticType::Unresolved(_)
            | SemanticType::InferVar(_)
            | SemanticType::Plain(_)
            | SemanticType::Placeholder => sema_type.clone(),

            SemanticType::Named(named_type) => {
                let type_args = named_type
                    .type_args
                    .iter()
                    .map(|type_arg| match type_arg {
                        TypedTypeArg::Type(ty, loc) => TypedTypeArg::Type(self.substitute_sema_type(&ty), *loc),
                        TypedTypeArg::Infer => TypedTypeArg::Infer,
                    })
                    .collect();

                SemanticType::Named(NamedType {
                    decl_id: named_type.decl_id,
                    type_args,
                })
            }
            SemanticType::Array(array) => SemanticType::Array(TypedArrayType {
                element_type: Box::new(self.substitute_sema_type(&array.element_type)),
                capacity: array.capacity.clone(),
                loc: array.loc,
            }),
            SemanticType::Const(inner) => SemanticType::Const(Box::new(self.substitute_sema_type(inner))),
            SemanticType::Pointer(inner) => SemanticType::Pointer(Box::new(self.substitute_sema_type(inner))),
            SemanticType::FuncType(func) => {
                let params = func
                    .params
                    .list
                    .iter()
                    .map(|param_ty| self.substitute_sema_type(param_ty))
                    .collect();

                let variadic = func.params.variadic.as_ref().map(|variadic| {
                    Box::new(match variadic.as_ref() {
                        v @ TypedFuncTypeVariadicParams::UntypedCStyle => v.clone(),

                        TypedFuncTypeVariadicParams::Typed(ty) => {
                            TypedFuncTypeVariadicParams::Typed(self.substitute_sema_type(ty))
                        }
                    })
                });

                let ret_type = Box::new(self.substitute_sema_type(&func.ret_type));

                SemanticType::FuncType(TypedFuncType {
                    symbol_id: func.symbol_id,
                    params: TypedFuncTypeParams { list: params, variadic },
                    ret_type,
                    is_public: func.is_public,
                    loc: func.loc,
                })
            }
            SemanticType::Tuple(tuple) => SemanticType::Tuple(TypedTupleType {
                elements: tuple.elements.iter().map(|t| self.substitute_sema_type(t)).collect(),
                loc: tuple.loc,
            }),
            SemanticType::GenericParam(id) => match self.lookup(*id) {
                Some(ty) => ty.clone(),
                None => sema_type.clone(),
            },
            SemanticType::SelfType(self_ty) => SemanticType::SelfType(self_ty.clone()),
            SemanticType::InterfaceType(interface) => SemanticType::InterfaceType(interface.clone()),
        }
    }
}
