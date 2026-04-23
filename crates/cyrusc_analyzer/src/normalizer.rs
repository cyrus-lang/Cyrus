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
use cyrusc_const_eval::value::is_comptime_valid;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    GenericParamID,
    decls::{DeclID, FuncDecl},
    exprs::TypedSelfType,
    stmts::{
        TypedFuncParamKind, TypedFuncParams, TypedFuncTypeParams, TypedFuncTypeVariadicParam, TypedFuncVariadicParam,
        TypedTypeArg, TypedTypeArgs,
    },
    types::{
        NamedType, SemaType, TypeDeclID, TypedArrayCapacity, TypedArrayType, TypedFuncType, TypedTupleType,
        UnresolvedType,
    },
};

impl<'a> AnalysisContext<'a> {
    /// Fully normalize AND validate a type
    /// This is what we call when an explicit type is used
    pub fn normalize_and_check_type_formation(&mut self, sema_type: SemaType, loc: Loc) -> Option<SemaType> {
        let sema_type = self.normalize_sema_type(sema_type, loc)?;

        self.check_type_formation(sema_type, loc)
    }

    // Fully normalize a type: remove UnresolvedSymbol, expand typedefs,
    // and recursively normalize children. Never returns UnresolvedSymbol.
    pub fn normalize_sema_type(&mut self, ty: SemaType, loc: Loc) -> Option<SemaType> {
        match ty {
            SemaType::Placeholder => Some(ty),
            SemaType::InferVar(_) => Some(ty),
            SemaType::Unresolved(unresolved_type) => self.normalize_unresolved_type(unresolved_type, loc),
            SemaType::Named(_) => Some(ty),
            SemaType::GenericParam(generic_param_id) => self.normalize_generic_param(generic_param_id),
            SemaType::Pointer(inner) => self.normalize_pointer(*inner, loc),
            SemaType::Const(inner) => self.normalize_const(*inner, loc),
            SemaType::Array(arr) => self.normalize_array(arr, loc),
            SemaType::FuncType(func_type) => self.normalize_func_type(func_type),
            SemaType::Tuple(tuple_type) => self.normalize_tuple(tuple_type),
            SemaType::SelfType(self_type) => self.normalize_self_type(self_type),
            SemaType::Plain(_) => Some(ty),

            SemaType::Err(_) => Some(ty),
        }
    }

    fn normalize_unresolved_type(&mut self, unresolved_type: UnresolvedType, loc: Loc) -> Option<SemaType> {
        let ty = match unresolved_type {
            UnresolvedType::Decl(symbol_id) => self.resolve_symbol_type(symbol_id, loc)?,
            UnresolvedType::GenericInst {
                base_decl_id,
                mut type_args,
            } => {
                let base_type = self.normalize_unresolved_type(UnresolvedType::Decl(base_decl_id), loc)?;

                if let Some(named_type) = base_type.as_named_type() {
                    self.normalize_type_args(&mut type_args);

                    SemaType::Named(NamedType {
                        type_decl_id: named_type.type_decl_id,
                        type_args,
                    })
                } else {
                    return None;
                }
            }
        };

        Some(self.expand_sema_type(ty, loc))
    }

    fn normalize_func_type(&mut self, mut func_type: TypedFuncType) -> Option<SemaType> {
        let params_len = func_type.params.list.len();
        let params: Vec<_> = func_type
            .params
            .list
            .into_iter()
            .filter_map(|param| self.normalize_sema_type(param, func_type.loc))
            .collect();

        if params.len() != params_len {
            return None;
        }

        func_type.params.list = params;

        // normalize variadic parameter if present
        if let Some(variadic) = func_type.params.variadic.take() {
            match *variadic {
                TypedFuncTypeVariadicParam::UntypedCStyle => {
                    func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParam::UntypedCStyle));
                }
                TypedFuncTypeVariadicParam::Typed(sema_type) => {
                    if let Some(normalized) = self.normalize_sema_type(sema_type, func_type.loc) {
                        func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParam::Typed(normalized)));
                    } else {
                        return None;
                    }
                }
            }
        }

        let normalized_ret = self.normalize_sema_type(*func_type.ret_type, func_type.loc)?;
        func_type.ret_type = Box::new(normalized_ret);

        Some(SemaType::FuncType(func_type))
    }

    fn normalize_func_decl_as_func_type(&mut self, func_decl: &FuncDecl) -> Option<SemaType> {
        let func_type = func_decl.as_func_type();
        self.normalize_func_type(func_type)
    }

    fn normalize_tuple(&mut self, tuple_type: TypedTupleType) -> Option<SemaType> {
        let elements_len = tuple_type.elements.len();

        let elements: Vec<_> = tuple_type
            .elements
            .into_iter()
            .filter_map(|ty| self.normalize_sema_type(ty, tuple_type.loc))
            .collect();

        // if we lost any types, return None
        if elements.len() != elements_len {
            return None;
        }

        Some(SemaType::Tuple(TypedTupleType {
            elements,
            loc: tuple_type.loc,
        }))
    }

    // TODO: Implement slices.
    fn normalize_array_capacity(&mut self, mut array: TypedArrayType, loc: Loc) -> Option<TypedArrayType> {
        match &mut array.capacity {
            TypedArrayCapacity::Fixed(expr) => {
                self.analyze_expr(expr, None);

                if !is_comptime_valid(&expr.kind) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                        loc: Some(loc),
                        hint: None,
                    });
                    return None;
                }
            }
            TypedArrayCapacity::Dynamic => todo!(),
        }

        array.element_type = Box::new(self.normalize_sema_type(*array.element_type, loc)?);
        Some(array)
    }

    fn normalize_array(&mut self, array: TypedArrayType, loc: Loc) -> Option<SemaType> {
        let array_type = self.normalize_array_capacity(array, loc)?;
        Some(SemaType::Array(array_type))
    }

    #[inline]
    fn normalize_const(&mut self, inner: SemaType, loc: Loc) -> Option<SemaType> {
        Some(SemaType::Const(Box::new(self.normalize_sema_type(inner, loc)?)))
    }

    fn normalize_pointer(&mut self, inner: SemaType, loc: Loc) -> Option<SemaType> {
        let ty = self.normalize_sema_type(inner, loc)?;
        Some(SemaType::Pointer(Box::new(ty)))
    }

    fn normalize_generic_param(&self, generic_param_id: GenericParamID) -> Option<SemaType> {
        if let Some(ty) = self.lookup_generic_binding(generic_param_id) {
            if let Some(infer) = &self.func_env.infer {
                return Some(infer.resolve(ty));
            }
        }

        // fallback
        Some(SemaType::GenericParam(generic_param_id))
    }

    fn normalize_self_type(&mut self, self_type: TypedSelfType) -> Option<SemaType> {
        self.func_env
            .current_object
            .clone()
            .or(Some(SemaType::SelfType(self_type)))
    }

    pub(crate) fn normalize_type_args(&mut self, type_args: &mut TypedTypeArgs) {
        for type_arg in type_args.iter_mut() {
            match type_arg {
                TypedTypeArg::Type(sema_type, loc) => {
                    *sema_type = match self.normalize_and_check_type_formation(sema_type.clone(), *loc) {
                        Some(sema_type) => sema_type,
                        None => continue,
                    };
                }
                TypedTypeArg::Infer => { /* skip */ }
            }
        }
    }

    pub(crate) fn normalize_func_params(&mut self, params: &mut TypedFuncParams) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    let normalized_type = self
                        .normalize_sema_type(typed_func_param.ty.clone(), typed_func_param.loc)
                        .unwrap();

                    typed_func_param.ty = normalized_type.clone();

                    self.validate_param_type(&typed_func_param.ty, typed_func_param.loc);
                }
                TypedFuncParamKind::SelfModifier(self_modifier) => {
                    let normalized_type = self
                        .normalize_sema_type(self_modifier.ty.clone(), self_modifier.loc)
                        .unwrap();

                    self.validate_param_type(&normalized_type, self_modifier.loc);
                }
            }
        }

        if let Some(variadic_params) = &mut params.variadic {
            if let TypedFuncVariadicParam::Typed { var_decl_id, ty, loc } = variadic_params {
                let ty = match self.normalize_sema_type(ty.clone(), *loc) {
                    Some(sema_type) => sema_type,
                    None => return,
                };

                self.validate_param_type(&ty, *loc);
                *variadic_params = TypedFuncVariadicParam::Typed {
                    var_decl_id: *var_decl_id,
                    ty,
                    loc: *loc,
                }
            }
        }
    }

    pub(crate) fn normalize_func_type_params(&mut self, params: &mut TypedFuncTypeParams, loc: Loc) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            let sema_type = self.normalize_sema_type(param.clone(), loc).unwrap();
            *param = sema_type.clone();

            self.validate_param_type(&sema_type, loc);
        }

        if let Some(variadic_params) = &mut params.variadic {
            match *variadic_params.clone() {
                TypedFuncTypeVariadicParam::UntypedCStyle => {}
                TypedFuncTypeVariadicParam::Typed(sema_type) => {
                    let sema_type = match self.normalize_sema_type(sema_type.clone(), loc) {
                        Some(sema_type) => sema_type,
                        None => return,
                    };

                    self.validate_param_type(&sema_type, loc);
                    *variadic_params = Box::new(TypedFuncTypeVariadicParam::Typed(sema_type));
                }
            }
        }
    }

    // FIXME: Remove later.
    #[inline]
    pub(crate) fn resolve_symbol_type_expanded(&mut self, decl_id: DeclID, loc: Loc) -> Option<SemaType> {
        let ty = self.resolve_symbol_type(decl_id, loc)?;

        Some(self.expand_sema_type(ty, loc))
    }

    pub(crate) fn resolve_symbol_type(&mut self, decl_id: DeclID, loc: Loc) -> Option<SemaType> {
        if let Some(cached_sema_ty) = self.type_cache.cache.get(&decl_id) {
            return Some(cached_sema_ty.clone());
        }

        if self.type_cache.push(decl_id).is_err() {
            let symbol_name = self.formatter.format_decl(decl_id);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CyclicTypeDefinition { symbol_name }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        };

        let mut sema_type_opt = self.resolve_symbol_type_internal(decl_id);

        if let Some(ty) = sema_type_opt.clone() {
            sema_type_opt = self.normalize_sema_type(ty, loc);
        }

        self.type_cache.pop(decl_id);

        if let Some(ref final_ty) = sema_type_opt {
            self.type_cache.cache.insert(decl_id, final_ty.clone());
        }

        sema_type_opt
    }

    fn resolve_symbol_type_internal(&mut self, decl_id: DeclID) -> Option<SemaType> {
        match decl_id {
            DeclID::GlobalVar(global_var_decl_id) => {
                let global_var_decl = self.decl_tables.global_var_decl(global_var_decl_id);
                global_var_decl.ty.clone()
            }
            DeclID::Var(var_decl_id) => {
                let var_decl = self.decl_tables.var_decl(var_decl_id);
                var_decl.ty.clone()
            }
            DeclID::Func(func_decl_id) => {
                let func_decl = self.decl_tables.func_decl(func_decl_id);
                self.normalize_func_decl_as_func_type(&func_decl)
            }
            DeclID::Struct(strut_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Struct(strut_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            DeclID::Enum(enum_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Enum(enum_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            DeclID::Union(union_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Union(union_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            DeclID::Typedef(typedef_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Typedef(typedef_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            DeclID::Interface(interface_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Interface(interface_decl_id),
                type_args: TypedTypeArgs::new(),
            })),

            DeclID::Method(_) => unreachable!(),
        }
    }
}
