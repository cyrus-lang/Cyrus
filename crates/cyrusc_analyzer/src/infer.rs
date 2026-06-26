// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use cyrusc_typed_ast::{
    stmts::{TypedFuncTypeParams, TypedFuncTypeVariadicParam, TypedTypeArg},
    types::{InferVarID, NamedType, SemaType, TypedArrayType, TypedFuncType, TypedTupleType},
};
use fx_hash::{FxHashMap, FxHashMapExt};

#[derive(Debug, Clone)]
pub(crate) struct InferCtx {
    next_var: u32,
    bindings: FxHashMap<InferVarID, SemaType>,
}

impl InferCtx {
    pub fn new() -> Self {
        Self {
            next_var: 0,
            bindings: FxHashMap::new(),
        }
    }

    pub fn new_var(&mut self) -> SemaType {
        let id = self.next_var;
        self.next_var += 1;

        SemaType::InferVar(InferVarID(id))
    }

    fn bind(&mut self, var: InferVarID, ty: SemaType) {
        self.bindings.insert(var, ty);
    }

    pub fn resolve(&self, ty: &SemaType) -> SemaType {
        match ty {
            SemaType::InferVar(id) => {
                if let Some(bound) = self.bindings.get(id) {
                    self.resolve(bound)
                } else {
                    ty.clone()
                }
            }
            SemaType::Named(named_type) => {
                let resolved_args = named_type
                    .type_args
                    .iter()
                    .map(|arg| match arg {
                        TypedTypeArg::Type(inner_ty, loc) => TypedTypeArg::Type(self.resolve(inner_ty), *loc),
                        TypedTypeArg::Infer => TypedTypeArg::Infer,
                    })
                    .collect();

                SemaType::Named(NamedType {
                    type_decl_id: named_type.type_decl_id,
                    type_args: resolved_args,
                })
            }

            SemaType::Array(array) => SemaType::Array(TypedArrayType {
                element_type: Box::new(self.resolve(&array.element_type)),
                capacity: array.capacity.clone(),
                loc: array.loc,
            }),
            SemaType::Const(inner) => SemaType::Const(Box::new(self.resolve(inner))),
            SemaType::Pointer(inner) => SemaType::Pointer(Box::new(self.resolve(inner))),
            SemaType::FuncType(func) => {
                let resolved_params = func.params.list.iter().map(|p| self.resolve(p)).collect();

                let resolved_variadic = func.params.variadic.as_ref().map(|vbox| match vbox.as_ref() {
                    TypedFuncTypeVariadicParam::UntypedCStyle => vbox.clone(),
                    TypedFuncTypeVariadicParam::Typed(inner) => {
                        Box::new(TypedFuncTypeVariadicParam::Typed(self.resolve(inner)))
                    }
                });

                let resolved_ret = Box::new(self.resolve(&func.ret_type));

                SemaType::FuncType(TypedFuncType {
                    params: TypedFuncTypeParams {
                        list: resolved_params,
                        variadic: resolved_variadic,
                    },
                    ret_type: resolved_ret,
                    is_public: func.is_public,
                    loc: func.loc,
                })
            }
            SemaType::Tuple(tuple) => {
                let elements = tuple
                    .elements
                    .iter()
                    .map(|(ty, loc)| (self.resolve(ty), *loc))
                    .collect();

                SemaType::Tuple(TypedTupleType {
                    elements,
                    loc: tuple.loc,
                })
            }

            SemaType::InterfaceObject(_) | SemaType::SelfType(_) | SemaType::Plain(_) => ty.clone(),

            SemaType::GenericParam(_) | SemaType::Err(_) | SemaType::Unresolved(_) | SemaType::Placeholder => {
                ty.clone()
            }
        }
    }

    fn occurs_check(&self, var: InferVarID, ty: &SemaType) -> bool {
        match ty {
            SemaType::InferVar(id) => *id == var,

            SemaType::Named(named) => named.type_args.iter().any(|arg| match arg {
                TypedTypeArg::Type(t, _) => self.occurs_check(var, t),
                _ => false,
            }),

            SemaType::Tuple(tuple_type) => tuple_type.elements.iter().any(|(ty, _)| self.occurs_check(var, ty)),

            SemaType::Array(array_type) => self.occurs_check(var, &array_type.element_type),

            SemaType::Pointer(inner) => self.occurs_check(var, inner),

            _ => false,
        }
    }

    pub(crate) fn unify(&mut self, a: &SemaType, b: &SemaType) -> bool {
        let a = self.resolve(a);
        let b = self.resolve(b);

        if a.is_err() || b.is_err() {
            return true;
        }

        match (&a, &b) {
            (SemaType::InferVar(id1), SemaType::InferVar(id2)) if id1 == id2 => true,

            (SemaType::InferVar(id), ty) | (ty, SemaType::InferVar(id)) => {
                if self.occurs_check(*id, ty) {
                    return false;
                }
                self.bind(*id, ty.clone());
                true
            }

            (SemaType::Named(n1), SemaType::Named(n2)) => {
                if n1.type_decl_id != n2.type_decl_id {
                    return false;
                }
                for (a, b) in n1.type_args.iter().zip(n2.type_args.iter()) {
                    if !self.unify_type_arg(a, b) {
                        return false;
                    }
                }
                true
            }

            (SemaType::Tuple(t1), SemaType::Tuple(t2)) => {
                t1.elements.len() == t2.elements.len()
                    && t1
                        .elements
                        .iter()
                        .zip(&t2.elements)
                        .all(|((a, _), (b, _))| self.unify(a, b))
            }

            (SemaType::Array(a1), SemaType::Array(a2)) => self.unify(&a1.element_type, &a2.element_type),

            (SemaType::Pointer(p1), SemaType::Pointer(p2)) => self.unify(p1, p2),

            (SemaType::Const(c1), SemaType::Const(c2)) => self.unify(c1, c2),
            
            (SemaType::FuncType(f1), SemaType::FuncType(f2)) => self.unify_func(f1, f2),

            // unify: array-to-pointer decay
            (SemaType::Array(arr), SemaType::Pointer(ptr)) | (SemaType::Pointer(ptr), SemaType::Array(arr)) => {
                self.unify(&arr.element_type, &ptr)
            }

            _ => false,
        }
    }

    fn unify_type_arg(&mut self, a: &TypedTypeArg, b: &TypedTypeArg) -> bool {
        match (a, b) {
            (TypedTypeArg::Type(t1, _), TypedTypeArg::Type(t2, _)) => self.unify(t1, t2),

            // `_` vs `_`
            (TypedTypeArg::Infer, TypedTypeArg::Infer) => true,

            // `_` vs concrete
            // we don't have a type yet, so we allow it
            (TypedTypeArg::Infer, TypedTypeArg::Type(_, _)) | (TypedTypeArg::Type(_, _), TypedTypeArg::Infer) => true,
        }
    }

    fn unify_func(&mut self, f1: &TypedFuncType, f2: &TypedFuncType) -> bool {
        if f1.params.list.len() != f2.params.list.len() {
            return false;
        }

        // unify parameters
        for (p1, p2) in f1.params.list.iter().zip(&f2.params.list) {
            if !self.unify(p1, p2) {
                return false;
            }
        }

        // unify variadic parameters
        match (&f1.params.variadic, &f2.params.variadic) {
            (None, None) => {}

            (Some(v1), Some(v2)) => match (&**v1, &**v2) {
                (TypedFuncTypeVariadicParam::Typed(t1), TypedFuncTypeVariadicParam::Typed(t2)) => {
                    if !self.unify(&t1, &t2) {
                        return false;
                    }
                }
                (TypedFuncTypeVariadicParam::UntypedCStyle, TypedFuncTypeVariadicParam::UntypedCStyle) => {}

                _ => return false,
            },

            _ => return false,
        }

        // unify return type
        self.unify(&f1.ret_type, &f2.ret_type)
    }
}
