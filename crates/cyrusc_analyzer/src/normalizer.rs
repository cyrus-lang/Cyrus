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
use cyrusc_internal::symbols::symbols::{SymbolEntry, SymbolEntryKind};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    GenericParamID, SymbolID,
    decls::{FuncDecl, InterfaceDeclID, TypedefDeclID},
    exprs::TypedSelfType,
    stmts::{
        TypedFuncParamKind, TypedFuncParams, TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedFuncVariadicParam,
        TypedTypeArg, TypedTypeArgs,
    },
    types::{
        NamedType, SemanticType, TypeDeclID, TypedArrayCapacity, TypedArrayType, TypedFuncType, TypedTupleType,
        UnresolvedType,
    },
};

impl<'a> AnalysisContext<'a> {
    /// Validate a semantic type.
    /// This does NOT normalize the type.
    pub fn check_sema_ty(&mut self, sema_type: SemanticType, loc: Loc) -> Option<SemanticType> {
        if sema_type.count_const_layers() > 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                loc: Some(loc),
                hint: None,
            });
        }

        if sema_type.is_self_type() && self.func_env.current_object.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::SelfTypeOutsideOfAnObject),
                loc: Some(loc),
                hint: None,
            });
        }

        // TODO: Check NamedTypes
        Some(sema_type)
    }

    /// Fully normalize AND validate a type
    /// This is what we call when an explicit type is used
    pub fn normalize_and_check_sema_ty(&mut self, sema_type: SemanticType, loc: Loc) -> Option<SemanticType> {
        let sema_type = self.normalize_sema_type(sema_type, loc)?;
        self.check_sema_ty(sema_type, loc)
    }

    // Fully normalize a type: remove UnresolvedSymbol, expand typedefs,
    // and recursively normalize children. Never returns UnresolvedSymbol.
    pub fn normalize_sema_type(&mut self, ty: SemanticType, loc: Loc) -> Option<SemanticType> {
        match ty {
            SemanticType::Unresolved(unresolved_type) => self.normalize_unresolved_type(unresolved_type, loc),
            SemanticType::Named(_) => Some(ty), // already normalized
            SemanticType::GenericParam(generic_param_id) => self.normalize_generic_param(generic_param_id),
            SemanticType::Pointer(inner) => self.normalize_pointer(*inner, loc),
            SemanticType::Const(inner) => self.normalize_const(*inner, loc),
            SemanticType::Array(arr) => self.normalize_array(arr, loc),
            SemanticType::FuncType(func_type) => self.normalize_func_type(func_type),
            SemanticType::Tuple(tuple_type) => self.normalize_tuple(tuple_type),
            SemanticType::SelfType(self_type) => self.normalize_self_type(self_type),
            SemanticType::Plain(_) | SemanticType::InterfaceType(_) => Some(ty),
        }
    }

    fn normalize_unresolved_type(&mut self, unresolved_type: UnresolvedType, loc: Loc) -> Option<SemanticType> {
        match unresolved_type {
            UnresolvedType::Infer => unreachable!(),
            UnresolvedType::Symbol(symbol_id) => self.resolve_symbol_type(symbol_id, loc),
            UnresolvedType::GenericInst { base, mut type_args } => {
                let base_type = self.normalize_unresolved_type(UnresolvedType::Symbol(base), loc)?;

                if let Some(named_type) = base_type.as_named_type() {
                    self.normalize_type_args(&mut type_args);

                    Some(SemanticType::Named(NamedType {
                        decl_id: named_type.decl_id,
                        type_args,
                    }))
                } else {
                    None
                }
            }
        }
    }

    fn normalize_func_type(&mut self, mut func_type: TypedFuncType) -> Option<SemanticType> {
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
                TypedFuncTypeVariadicParams::UntypedCStyle => {
                    func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle));
                }
                TypedFuncTypeVariadicParams::Typed(sema_type) => {
                    if let Some(normalized) = self.normalize_sema_type(sema_type, func_type.loc) {
                        func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParams::Typed(normalized)));
                    } else {
                        return None;
                    }
                }
            }
        }

        let normalized_ret = self.normalize_sema_type(*func_type.ret_type, func_type.loc)?;
        func_type.ret_type = Box::new(normalized_ret);

        Some(SemanticType::FuncType(func_type))
    }

    fn normalize_func_decl_as_func_type(&mut self, func_decl: &FuncDecl) -> Option<SemanticType> {
        let func_type = func_decl.as_func_type();
        self.normalize_func_type(func_type)
    }

    fn normalize_tuple(&mut self, tuple_type: TypedTupleType) -> Option<SemanticType> {
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

        Some(SemanticType::Tuple(TypedTupleType {
            elements,
            loc: tuple_type.loc,
        }))
    }

    // TODO: Implement slices.
    fn normalize_array_capacity(&mut self, mut arr: TypedArrayType, loc: Loc) -> Option<TypedArrayType> {
        match &mut arr.capacity {
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

        arr.element_type = Box::new(self.normalize_sema_type(*arr.element_type, loc)?);
        Some(arr)
    }

    fn normalize_array(&mut self, arr: TypedArrayType, loc: Loc) -> Option<SemanticType> {
        let array_type = self.normalize_array_capacity(arr, loc)?;
        Some(SemanticType::Array(array_type))
    }

    fn normalize_const(&mut self, inner: SemanticType, loc: Loc) -> Option<SemanticType> {
        let ty = self.normalize_sema_type(inner, loc)?;
        Some(ty.as_const())
    }

    fn normalize_pointer(&mut self, inner: SemanticType, loc: Loc) -> Option<SemanticType> {
        let ty = self.normalize_sema_type(inner, loc)?;
        Some(SemanticType::Pointer(Box::new(ty)))
    }

    fn normalize_typedef(&mut self, typedef_decl_id: TypedefDeclID) -> Option<SemanticType> {
        let typedef_decl = self.decl_tables.typedef_decl(typedef_decl_id);
        Some(*typedef_decl.ty.clone())
    }

    fn normalize_generic_param(&self, generic_param_id: GenericParamID) -> Option<SemanticType> {
        if let Some(ty) = self.lookup_generic_binding(generic_param_id) {
            return Some(ty.clone());
        }

        // fallback
        Some(SemanticType::GenericParam(generic_param_id))
    }

    fn normalize_self_type(&mut self, self_type: TypedSelfType) -> Option<SemanticType> {
        self.func_env
            .current_object
            .clone()
            .or(Some(SemanticType::SelfType(self_type)))
    }

    pub(crate) fn normalize_type_args(&mut self, type_args: &mut TypedTypeArgs) {
        for type_arg in type_args.iter_mut() {
            match type_arg {
                TypedTypeArg::Type(sema_type, loc) => {
                    *sema_type = match self.normalize_and_check_sema_ty(sema_type.clone(), *loc) {
                        Some(sema_type) => sema_type,
                        None => continue,
                    };
                }
            }
        }
    }

    // FIXME
    fn normalize_interface_type(&mut self, interface_decl_id: InterfaceDeclID) -> Option<SemanticType> {
        todo!();
        // let interface_decl = self.decl_tables.interface_decl(interface_decl_id);

        // let methods: Vec<FuncDecl> = interface_decl
        //     .methods
        //     .iter()
        //     .map(|func_decl| func_decl)
        //     .collect();

        // Some(SemanticType::Interface(InterfaceType {
        //     symbol_id: interface_decl_id.symbol_id,
        //     methods,
        //     loc,
        // }))
    }

    pub(crate) fn normalize_func_params(&mut self, params: &mut TypedFuncParams, loc: Loc) {
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
                        .normalize_sema_type(self_modifier.ty.clone().unwrap(), self_modifier.loc)
                        .unwrap();

                    self.validate_param_type(&normalized_type, self_modifier.loc);
                }
            }
        }

        if let Some(variadic_params) = &mut params.variadic {
            if let TypedFuncVariadicParam::Typed(ident, sema_type) = variadic_params {
                let sema_type = match self.normalize_sema_type(sema_type.clone(), loc) {
                    Some(sema_type) => sema_type,
                    None => return,
                };

                self.validate_param_type(&sema_type, ident.loc);
                *variadic_params = TypedFuncVariadicParam::Typed(ident.clone(), sema_type);
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
                TypedFuncTypeVariadicParams::UntypedCStyle => {}
                TypedFuncTypeVariadicParams::Typed(sema_type) => {
                    let sema_type = match self.normalize_sema_type(sema_type.clone(), loc) {
                        Some(sema_type) => sema_type,
                        None => return,
                    };

                    self.validate_param_type(&sema_type, loc);
                    *variadic_params = Box::new(TypedFuncTypeVariadicParams::Typed(sema_type));
                }
            }
        }
    }

    pub(crate) fn resolve_symbol_type(&mut self, symbol_id: SymbolID, loc: Loc) -> Option<SemanticType> {
        if let Some(cached_sema_ty) = self.type_cache.cache.get(&symbol_id) {
            return Some(cached_sema_ty.clone());
        }

        if self.type_cache.push(symbol_id).is_err() {
            let symbol_name = self.formatter.format_symbol_name(symbol_id);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CyclicTypeDefinition { symbol_name }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        };

        let symbol_entry = self.query.lookup_symbol_entry(symbol_id).unwrap();
        debug_assert!(!matches!(symbol_entry.kind, SymbolEntryKind::Unresolved));

        let mut sema_type_opt = self.resolve_symbol_type_internal(&symbol_entry);

        if let Some(ty) = sema_type_opt.clone() {
            sema_type_opt = self.normalize_sema_type(ty, loc);
        }

        self.type_cache.pop(symbol_id);

        if let Some(ref final_ty) = sema_type_opt {
            self.type_cache.cache.insert(symbol_id, final_ty.clone());
        }

        sema_type_opt
    }

    fn resolve_symbol_type_internal(&mut self, symbol_entry: &SymbolEntry) -> Option<SemanticType> {
        match &symbol_entry.kind {
            SymbolEntryKind::GlobalVar(global_var_decl_id) => {
                let global_var_decl = self.decl_tables.global_var_decl(*global_var_decl_id);
                global_var_decl.ty.clone()
            }
            SymbolEntryKind::Var(var_decl_id) => {
                let var_decl = self.decl_tables.var_decl(*var_decl_id);
                var_decl.ty.clone()
            }
            SymbolEntryKind::Func(func_decl_id) => {
                let func_decl = self.decl_tables.func_decl(*func_decl_id);
                self.normalize_func_decl_as_func_type(&func_decl)
            }
            SymbolEntryKind::Struct(strut_decl_id) => Some(SemanticType::Named(NamedType {
                decl_id: TypeDeclID::Struct(*strut_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            SymbolEntryKind::Enum(enum_decl_id) => Some(SemanticType::Named(NamedType {
                decl_id: TypeDeclID::Enum(*enum_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            SymbolEntryKind::Union(union_decl_id) => Some(SemanticType::Named(NamedType {
                decl_id: TypeDeclID::Union(*union_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            SymbolEntryKind::Interface(interface_decl_id) => self.normalize_interface_type(*interface_decl_id),
            SymbolEntryKind::Typedef(typedef_decl_id) => self.normalize_typedef(*typedef_decl_id),
            SymbolEntryKind::ProxiedSymbol { .. } => {
                unreachable!("proxied symbol entry kind should not appear here")
            }
            SymbolEntryKind::Method(..) => unreachable!("method symbols are not type expressions"),
            SymbolEntryKind::ProxiedModule { .. } => {
                unreachable!("proxied module symbol entry kind should not appear here")
            }
            SymbolEntryKind::Module { .. } => {
                unreachable!("module symbol entry kind should not appear here")
            }
            SymbolEntryKind::Namespace { .. } => {
                unreachable!("namespace symbol entry kind should not appear here")
            }
            SymbolEntryKind::Unresolved => unreachable!("unresolved symbol entry should not appear here"),
        }
    }
}
