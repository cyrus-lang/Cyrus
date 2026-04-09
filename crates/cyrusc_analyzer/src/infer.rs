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
    stmts::{TypedFuncTypeVariadicParams, TypedTypeArg},
    types::{InferVarID, SemanticType},
};
use fx_hash::FxHashMap;

#[derive(Debug, Clone)]
pub(crate) struct InferCtx {
    next_var: u32,
    bindings: FxHashMap<InferVarID, SemanticType>,
}

impl InferCtx {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            bindings: FxHashMap::default(),
        }
    }

    pub fn new_var(&mut self) -> SemanticType {
        let id = self.next_var;
        self.next_var += 1;

        SemanticType::InferVar(InferVarID(id))
    }

    fn bind(&mut self, var: InferVarID, ty: SemanticType) {
        self.bindings.insert(var, ty);
    }

    pub fn resolve(&self, ty: &SemanticType) -> SemanticType {
        match ty {
            SemanticType::InferVar(id) => {
                if let Some(bound) = self.bindings.get(id) {
                    self.resolve(bound)
                } else {
                    ty.clone()
                }
            }
            _ => ty.clone(),
        }
    }

    fn occurs_check(&self, var: InferVarID, ty: &SemanticType) -> bool {
        match ty {
            SemanticType::InferVar(id) => *id == var,
            SemanticType::Named(named) => named.type_args.iter().any(|arg| match arg {
                TypedTypeArg::Type(t, _) => self.occurs_check(var, t),
                _ => false,
            }),
            SemanticType::Tuple(t) => t.elements.iter().any(|e| self.occurs_check(var, e)),
            SemanticType::Array(a) => self.occurs_check(var, &a.element_type),
            SemanticType::Pointer(inner) => self.occurs_check(var, inner),
            _ => false,
        }
    }

    pub(crate) fn unify(&mut self, a: &SemanticType, b: &SemanticType) -> bool {
        let a = self.resolve(a);
        let b = self.resolve(b);

        match (&a, &b) {
            (SemanticType::InferVar(id1), SemanticType::InferVar(id2)) if id1 == id2 => true,
            (SemanticType::InferVar(id1), ty) | (ty, SemanticType::InferVar(id1)) => {
                if self.occurs_check(*id1, &ty) {
                    return false;
                }

                self.bind(*id1, ty.clone());
                true
            }
            (SemanticType::Named(named_type1), SemanticType::Named(named_type2)) => {
                if named_type1.decl_id != named_type2.decl_id {
                    return false;
                }

                for (type_arg1, type_arg2) in named_type1.type_args.iter().zip(named_type2.type_args.iter()) {
                    if let (TypedTypeArg::Type(ty1, _), TypedTypeArg::Type(ty2, _)) = (type_arg1, type_arg2) {
                        if !self.unify(&ty1, &ty2) {
                            return false;
                        }
                    }
                }

                true
            }
            (SemanticType::Tuple(tuple1), SemanticType::Tuple(tuple2)) => {
                if tuple1.elements.len() != tuple2.elements.len() {
                    return false;
                }

                for (element1, element2) in tuple1.elements.iter().zip(tuple2.elements.iter()) {
                    if !self.unify(element1, element2) {
                        return false;
                    }
                }

                true
            }
            (SemanticType::Array(array1), SemanticType::Array(array2)) => {
                self.unify(&array1.element_type, &array2.element_type)
            }
            (SemanticType::Pointer(inner1), SemanticType::Pointer(inner2)) => self.unify(&inner1, &inner2),
            (SemanticType::FuncType(func1), SemanticType::FuncType(func2)) => {
                if func1.params.list.len() != func2.params.list.len() {
                    return false;
                }

                for (ty1, ty2) in func1.params.list.iter().zip(func2.params.list.iter()) {
                    if !self.unify(ty1, ty2) {
                        return false;
                    }
                }

                if let (Some(variadic1), Some(variadic2)) =
                    (func1.params.variadic.clone(), func2.params.variadic.clone())
                {
                    match (*variadic1, *variadic2) {
                        (TypedFuncTypeVariadicParams::Typed(ty1), TypedFuncTypeVariadicParams::Typed(ty2)) => {
                            if !self.unify(&ty1, &ty2) {
                                return false;
                            }
                        }
                        _ => {}
                    }
                }

                self.unify(&func1.ret_type, &func2.ret_type)
            }

            _ => a == b,
        }
    }
}
