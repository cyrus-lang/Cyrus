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
use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, source_loc::SourceLoc};
use cyrusc_resolver::symbols::{LocalOrGlobalSymbol, LocalScopeRef, LocalSymbolKind, ResolvedTypedef, SymbolEntryKind};
use cyrusc_tast::{
    ModuleID, ScopeID, SymbolID,
    generics::{generic_type::GenericType, mapping_ctx::GenericMappingCtx, substitute::substitute_type},
    mapping_ctx_arena,
    sigs::{FuncSig, typed_func_decl_as_func_sig},
    stmts::{
        TypedFuncParamKind, TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedFuncVariadicParams, TypedTypeArgs,
    },
    types::{
        DynamicType, ResolvedSymbol, SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType,
        TypedFuncType, TypedTupleType,
    },
};
use std::{cell::RefCell, rc::Rc};

impl<'a> AnalysisContext<'a> {
    // Fully normalize a type: remove UnresolvedSymbol, expand typedefs,
    // and recursively normalize children. Never returns UnresolvedSymbol.
    pub fn normalize_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        ty: SemanticType,
        loc: SourceLoc,
        no_generics_check: bool,
    ) -> Option<SemanticType> {
        let local_scope_opt = scope_id_opt.and_then(|sid| self.resolver.get_scope_ref(self.module_id, sid));

        let sema_ty_opt = match ty {
            SemanticType::GenericParam(generic_param) => {
                if let Some(sema_ty) = &self.current_obj_operand_ty {
                    if let Some(generic_type) = sema_ty.as_generic_type() {
                        mapping_ctx_arena!(self, mapping_ctx_arena, {
                            let mapping_ctx = generic_type.mapping_ctx.borrow();
                            if let Some(sema_ty) =
                                mapping_ctx.get_with_name(&*mapping_ctx_arena, &generic_param.param_name.name)
                            {
                                return Some(sema_ty);
                            }
                        });
                    }
                }
                Some(SemanticType::GenericParam(generic_param))
            }
            SemanticType::GenericType(mut generic_type) => {
                let sym = match self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), generic_type.base)
                {
                    Some(sym) => sym,
                    None => {
                        self.report_non_type_symbol(generic_type.base, loc.clone());
                        return None;
                    }
                };

                if let Some(resolved_typedef) = sym.as_typedef() {
                    self.resolve_generic_typedef(
                        scope_id_opt,
                        local_scope_opt,
                        resolved_typedef,
                        &generic_type.type_args,
                        resolved_typedef.typedef_sig.loc.clone(),
                    )
                } else {
                    let Some(generic_params) = sym.get_generic_params() else {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                            location: Some(DiagLoc::new(loc.clone())),
                            hint: None,
                        });
                        return None;
                    };

                    mapping_ctx_arena!(self, mapping_ctx_arena, {
                        if let Err(diag) = generic_type.init(
                            &*mapping_ctx_arena,
                            generic_params,
                            &(self.symbol_formatter)(scope_id_opt),
                        ) {
                            self.reporter.report(diag);
                            return None;
                        }
                    });

                    Some(SemanticType::GenericType(generic_type))
                }
            }
            SemanticType::UnresolvedSymbol(symbol_id) => {
                self.resolver.resolve_local_or_global_symbol(local_scope_opt, symbol_id);

                let resolved = self.resolve_symbol_type(scope_id_opt, symbol_id, loc.clone())?;
                self.normalize_type(scope_id_opt, resolved, loc.clone(), false)
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::Typedef(symbol_id)) => {
                let Some(sym) = self.resolver.resolve_local_or_global_symbol(local_scope_opt, symbol_id) else {
                    self.report_non_type_symbol(symbol_id, loc.clone());
                    return None;
                };

                let resolved_typedef_opt = match &sym {
                    LocalOrGlobalSymbol::LocalSymbol(local_symbol) => local_symbol.as_typedef(),
                    LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => symbol_entry.as_typedef(),
                };

                let Some(resolved_typedef) = resolved_typedef_opt else {
                    self.report_non_type_symbol(symbol_id, loc.clone());
                    return None;
                };

                self.resolve_typedef_inner_type(resolved_typedef)
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::Variable(symbol_id)) => {
                let local_symbol = self
                    .resolver
                    .resolve_symbol_from_local_scope(local_scope_opt.unwrap(), symbol_id)
                    .unwrap();
                let resolved_variable_opt = local_symbol.as_variable();

                if let Some(resolved_variable) = resolved_variable_opt {
                    if let Some(t) = &resolved_variable.typed_variable.ty {
                        return self.normalize_type(scope_id_opt, t.clone(), loc, false);
                    } else if let Some(rhs) = &resolved_variable.typed_variable.rhs {
                        let rhs_ty = self.analyze_expr_non_terminal(
                            scope_id_opt,
                            &mut rhs.clone(),
                            resolved_variable.typed_variable.ty.clone(),
                        )?;
                        return self.normalize_type(scope_id_opt, rhs_ty, loc, false);
                    }
                }

                self.report_non_type_symbol(symbol_id, loc.clone());
                None
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::GlobalVar(symbol_id)) => {
                let symbol_entry = self.resolver.resolve_global_symbol(symbol_id).unwrap();
                let resolved_global_var_opt = symbol_entry.as_global_var();

                if let Some(resolved_global_var) = resolved_global_var_opt {
                    if let Some(t) = &resolved_global_var.global_var_sig.ty {
                        return self.normalize_type(scope_id_opt, t.clone(), loc, false);
                    }
                }
                self.report_non_type_symbol(symbol_id, loc.clone());
                None
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::Func(..))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Method(..)) => {
                unreachable!()
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(_))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(_))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Union(_)) => Some(ty),
            SemanticType::ResolvedSymbol(ResolvedSymbol::Interface(symbol_id)) => {
                self.normalize_interface_as_dynamic_type(scope_id_opt, symbol_id, loc.clone())
            }
            SemanticType::Pointer(inner) => {
                let inner = self.normalize_type(scope_id_opt, *inner, loc.clone(), false)?;
                Some(SemanticType::Pointer(Box::new(inner)))
            }
            SemanticType::Const(inner) => {
                let inner = self.normalize_type(scope_id_opt, *inner, loc.clone(), false)?;
                Some(inner.as_const())
            }
            SemanticType::Array(arr) => match self.normalize_array_capacity(scope_id_opt, arr, loc.clone()) {
                Some(arr_type) => Some(SemanticType::Array(arr_type)),
                None => None,
            },
            SemanticType::FuncType(mut func_type) => {
                let mut new_params = Vec::with_capacity(func_type.params.list.len());
                for param in func_type.params.list {
                    match self.normalize_type(scope_id_opt, param, loc.clone(), false) {
                        Some(normalized) => new_params.push(normalized),
                        None => return None, // fail whole function type
                    }
                }
                func_type.params.list = new_params;

                if let Some(variadic) = func_type.params.variadic.clone() {
                    match *variadic {
                        TypedFuncTypeVariadicParams::UntypedCStyle => {}
                        TypedFuncTypeVariadicParams::Typed(sema_ty) => {
                            match self.normalize_type(scope_id_opt, sema_ty, loc.clone(), false) {
                                Some(normalized) => {
                                    func_type.params.variadic =
                                        Some(Box::new(TypedFuncTypeVariadicParams::Typed(normalized)))
                                }
                                None => return None, // fail whole function type
                            }
                        }
                    }
                }

                // Normalize return type
                match self.normalize_type(scope_id_opt, *func_type.return_type, loc.clone(), false) {
                    Some(new_ret) => func_type.return_type = Box::new(new_ret),
                    None => return None,
                }

                Some(SemanticType::FuncType(func_type))
            }
            SemanticType::PlainType(_) | SemanticType::UnnamedStruct(_) => Some(ty),
            SemanticType::Tuple(tuple_type) => {
                let mut type_list: Vec<SemanticType> = Vec::new();

                for sema_ty in &tuple_type.type_list {
                    match self.normalize_type(scope_id_opt, sema_ty.clone(), tuple_type.loc.clone(), false) {
                        Some(normalized) => type_list.push(normalized),
                        None => continue,
                    }
                }

                Some(SemanticType::Tuple(TypedTupleType {
                    type_list,
                    loc: tuple_type.loc,
                }))
            }
            SemanticType::SelfType(self_type) => {
                if let Some(sema_ty) = &self.current_self {
                    Some(sema_ty.clone())
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::SelfTypeOutsideOfAnObject),
                        location: Some(DiagLoc::new(self_type.loc.clone())),
                        hint: None,
                    });
                    None
                }
            }
            SemanticType::DynamicType(dynamic_type) => Some(SemanticType::DynamicType(dynamic_type)),
        };

        if let Some(sema_ty) = sema_ty_opt {
            if no_generics_check {
                Some(sema_ty)
            } else {
                self.check_sema_ty_for_missing_type_args(scope_id_opt, &sema_ty, loc)
            }
        } else {
            None
        }
    }

    fn normalize_interface_as_dynamic_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        interface_symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let local_scope_opt = scope_id_opt.and_then(|sid| self.resolver.get_scope_ref(self.module_id, sid));

        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, interface_symbol_id)
            .unwrap();
        let Some(resolved_interface) = sym.as_interface() else {
            let symbol_name = (self.symbol_formatter)(scope_id_opt)(interface_symbol_id);
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::SymbolIsNotInterface { symbol_name }),
                location: Some(DiagLoc::new(loc)),
                hint: None,
            });
            return None;
        };

        let mut method_sigs: Vec<FuncSig> = Vec::new();

        for func_decl in &resolved_interface.interface_sig.methods {
            let func_sig = typed_func_decl_as_func_sig(func_decl);
            method_sigs.push(func_sig);
        }

        Some(SemanticType::DynamicType(DynamicType {
            name: resolved_interface.interface_sig.name.clone(),
            method_sigs,
            loc,
        }))
    }

    fn normalize_array_capacity(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        mut arr: TypedArrayType,
        loc: SourceLoc,
    ) -> Option<TypedArrayType> {
        match &mut arr.capacity {
            TypedArrayCapacity::Fixed(capacity_value) => match capacity_value.clone() {
                TypedArrayFixedCapacityValue::Expr(typed_expr) => {
                    if let Some(value) = self.const_expr_as_raw_integer(scope_id_opt, &typed_expr) {
                        if let Ok(unsigned_integer) = value.try_into() {
                            arr.capacity =
                                TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(unsigned_integer));
                        } else {
                            todo!();
                        }
                    } else {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                            location: Some(DiagLoc::new(typed_expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                }
                TypedArrayFixedCapacityValue::Value(_) => {}
            },
            TypedArrayCapacity::Dynamic => {
                todo!();
            }
        }

        arr.element_type = Box::new(self.normalize_type(scope_id_opt, *arr.element_type, loc.clone(), false)?);
        Some(arr)
    }

    pub(crate) fn resolve_full_type_from_local_or_global_symbol(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sym: LocalOrGlobalSymbol,
    ) -> Option<SemanticType> {
        match sym {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol.kind {
                LocalSymbolKind::Variable(mut resolved_variable) => {
                    let var = &mut resolved_variable.typed_variable;

                    if let Some(sema_ty) = &var.ty {
                        let sema_ty = self.normalize_type(scope_id_opt, sema_ty.clone(), var.loc.clone(), false)?;

                        return Some(if var.is_const { sema_ty.as_const() } else { sema_ty });
                    }

                    let rhs = var.rhs.as_mut()?;

                    if var.analyzed {
                        if let Some(sema_ty) = &var.ty {
                            return Some(if var.is_const {
                                sema_ty.as_const()
                            } else {
                                sema_ty.clone()
                            });
                        }
                    }

                    let mut sema_ty = self.analyze_expr_non_terminal(scope_id_opt, rhs, None)?;

                    sema_ty = self.normalize_type(scope_id_opt, sema_ty, var.loc.clone(), false)?;

                    Some(if var.is_const { sema_ty.as_const() } else { sema_ty })
                }
                LocalSymbolKind::Struct(resolved_struct) => Some(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::NamedStruct(resolved_struct.symbol_id),
                )),
                LocalSymbolKind::Enum(resolved_enum) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                    resolved_enum.symbol_id,
                ))),
                LocalSymbolKind::Union(resolved_union) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Union(
                    resolved_union.symbol_id,
                ))),
                LocalSymbolKind::Interface(resolved_interface) => self.normalize_interface_as_dynamic_type(
                    scope_id_opt,
                    resolved_interface.symbol_id,
                    resolved_interface.interface_sig.loc.clone(),
                ),
                LocalSymbolKind::Typedef(resolved_typedef) => self.resolve_typedef_inner_type(&resolved_typedef),
            },
            LocalOrGlobalSymbol::GlobalSymbol(entry) => match entry.kind {
                SymbolEntryKind::Method(..) => unreachable!(),
                SymbolEntryKind::Func(resolved_func) => Some(SemanticType::FuncType(TypedFuncType {
                    symbol_id: Some(resolved_func.symbol_id),
                    def_module_id: Some(resolved_func.module_id),
                    params: TypedFuncTypeParams {
                        list: resolved_func
                            .func_sig
                            .params
                            .list
                            .iter()
                            .map(|param| match param {
                                TypedFuncParamKind::FuncParam(typed_func_param) => typed_func_param.ty.clone(),
                                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                                    typed_self_modifier.ty.clone().unwrap()
                                }
                            })
                            .collect(),
                        variadic: match resolved_func.func_sig.params.variadic {
                            Some(variadic) => match variadic {
                                TypedFuncVariadicParams::UntypedCStyle => {
                                    Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle))
                                }
                                TypedFuncVariadicParams::Typed(_, sema_ty) => {
                                    Some(Box::new(TypedFuncTypeVariadicParams::Typed(sema_ty)))
                                }
                            },
                            None => None,
                        },
                    },
                    is_public: resolved_func.func_sig.modifiers.vis.is_public(),
                    return_type: Box::new(resolved_func.func_sig.return_type.clone()),
                    loc: resolved_func.func_sig.loc.clone(),
                })),
                SymbolEntryKind::GlobalVar(mut resolved_global_var) => {
                    let var = &mut resolved_global_var.global_var_sig;

                    if let Some(sema_ty) = &var.ty {
                        return self.normalize_type(scope_id_opt, sema_ty.clone(), var.loc.clone(), false);
                    }

                    let rhs = var
                        .rhs
                        .as_mut()
                        .expect("Cannot resolve global variable type: no type and no rhs.");

                    if var.analyzed {
                        let sema_ty = var
                            .ty
                            .clone()
                            .expect("Global variable marked analyzed but has no type.");

                        return Some(sema_ty);
                    }

                    let mut sema_ty = self.analyze_expr(scope_id_opt, rhs, None)?;

                    sema_ty = self.normalize_type(scope_id_opt, sema_ty, var.loc.clone(), false)?;

                    Some(sema_ty)
                }
                SymbolEntryKind::Struct(s) => {
                    Some(SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(s.symbol_id)))
                }
                SymbolEntryKind::Enum(resolved_enum) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                    resolved_enum.symbol_id,
                ))),
                SymbolEntryKind::Union(resolved_union) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Union(
                    resolved_union.symbol_id,
                ))),
                SymbolEntryKind::Interface(resolved_interface) => self.normalize_interface_as_dynamic_type(
                    scope_id_opt,
                    resolved_interface.symbol_id,
                    resolved_interface.interface_sig.loc.clone(),
                ),
                SymbolEntryKind::Typedef(resolved_typedef) => self.resolve_typedef_inner_type(&resolved_typedef),
                SymbolEntryKind::ProxiedSymbol(_, symbol_id) => {
                    let local_scope_opt =
                        scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));
                    let sym = self
                        .resolver
                        .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
                        .unwrap();
                    self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, sym)
                }
            },
        }
    }

    fn resolve_generic_typedef(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        resolved_typedef: &ResolvedTypedef,
        type_args: &TypedTypeArgs,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let typedef_generic_params = resolved_typedef.typedef_sig.generic_params.as_ref().unwrap();

        // typedef is generating an another generic
        if let Some(mut generic_type) = resolved_typedef.typedef_sig.ty.as_generic_type().cloned() {
            let parent_mapping_ctx = Some(Rc::new(generic_type.mapping_ctx.borrow().clone()));

            match self
                .init_generic_type_with_symbol_id(
                    scope_id_opt,
                    local_scope_opt.clone(),
                    generic_type.base,
                    &Some(type_args.clone()),
                    parent_mapping_ctx,
                    Some(&typedef_generic_params),
                    false,
                    loc,
                )
                .transpose()
                .unwrap()
            {
                Ok((_, new_generic_type_opt)) => {
                    // new generic params gotta be used!
                    generic_type.altered_generic_params = resolved_typedef.typedef_sig.generic_params.clone();

                    new_generic_type_opt.map(|generic_type| SemanticType::GenericType(generic_type))
                }
                Err(diag) => {
                    self.reporter.report(diag);
                    None
                }
            }
        } else {
            // typedef itself is generic but it does not generate generic,
            // so simply initialize generic mapping ctx and substitute the type

            let mapping_ctx = Rc::new(RefCell::new(GenericMappingCtx::new_root()));
            let mut generic_type = GenericType::new_unresolved(
                0,
                type_args.clone(),
                mapping_ctx.clone(),
                self.mapping_ctx_arena.clone(),
                false,
                loc,
            );

            mapping_ctx_arena!(self, mapping_ctx_arena, {
                if let Err(diag) = generic_type.init(
                    &*mapping_ctx_arena,
                    typedef_generic_params.clone(),
                    &(self.symbol_formatter)(scope_id_opt),
                ) {
                    self.reporter.report(diag);
                    return None;
                }

                substitute_type(
                    &*mapping_ctx_arena,
                    resolved_typedef.typedef_sig.ty.clone(),
                    mapping_ctx,
                )
            })
        }
    }

    fn resolve_typedef_inner_type(&mut self, resolved_typedef: &ResolvedTypedef) -> Option<SemanticType> {
        let inner_ty = resolved_typedef.typedef_sig.ty.clone();

        if let Some(mut generic_type) = inner_ty.as_generic_type().cloned() {
            generic_type.altered_generic_params = resolved_typedef.typedef_sig.generic_params.clone();
            Some(SemanticType::GenericType(generic_type))
        } else {
            Some(inner_ty)
        }
    }

    pub(crate) fn resolve_symbol_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        if let Some(cached) = self.ty_caches.cache.get(&symbol_id) {
            return Some(cached.clone());
        }

        if !self.ty_caches.push(symbol_id) {
            let symbol = (self.symbol_formatter)(scope_id_opt)(symbol_id);
            self.report_cyclic_typedef(symbol, loc);
            return None;
        }

        let local_scope_opt = scope_id_opt.and_then(|sid| self.resolver.get_scope_ref(self.module_id, sid));
        let lg = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .or_else(|| {
                let symbol_entry = self.resolver.lookup_symbol_entry_with_id(symbol_id)?;
                Some(LocalOrGlobalSymbol::GlobalSymbol(symbol_entry))
            });

        let mut concrete_type_opt: Option<SemanticType> = None;

        if let Some(local_or_global) = lg {
            concrete_type_opt = self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, local_or_global);

            if let Some(ty) = concrete_type_opt.clone() {
                concrete_type_opt = self.normalize_type(scope_id_opt, ty, loc.clone(), true);
            }
        } else {
            self.report_non_type_symbol(symbol_id, loc);
        }

        self.ty_caches.pop(symbol_id);

        if let Some(ref final_ty) = concrete_type_opt {
            self.ty_caches.cache.insert(symbol_id, final_ty.clone());
        }

        concrete_type_opt
    }

    fn report_cyclic_typedef(&mut self, symbol: String, loc: SourceLoc) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(AnalyzerDiagKind::CyclicTypeDefinition { symbol }),
            location: Some(DiagLoc::new(loc)),
            hint: None,
        });
    }

    fn report_non_type_symbol(&mut self, symbol_id: SymbolID, loc: SourceLoc) {
        let symbol_name = (self.symbol_formatter)(None)(symbol_id);

        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(AnalyzerDiagKind::NonTypeSymbol { symbol_name }),
            location: Some(DiagLoc::new(loc)),
            hint: None,
        });
    }
}
