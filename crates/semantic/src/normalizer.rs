use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use ast::source_loc::SourceLoc;
use diagcentral::{Diag, DiagLevel, DiagLoc};
use resolver::scope::{LocalOrGlobalSymbol, LocalSymbolKind, ResolvedTypedef, SymbolEntryKind};
use typed_ast::{
    ScopeID, SymbolID, TypedFuncParamKind, TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedFuncVariadicParams,
    TypedTypeArg,
    types::{
        SemanticType, ResolvedSymbol, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType, TypedFuncType,
        TypedTupleType,
    },
};

impl<'a> AnalysisContext<'a> {
    // Fully normalize a type: remove UnresolvedSymbol, expand typedefs,
    // and recursively normalize children. Never returns UnresolvedSymbol.
    pub fn normalize_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,

        ty: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let local_scope_opt = scope_id_opt.and_then(|sid| self.resolver.get_scope_ref(self.module_id, sid));

        match &ty {
            SemanticType::UnresolvedSymbol(symbol_id)
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(symbol_id))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Typedef(symbol_id))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(symbol_id))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Interface(symbol_id))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::GlobalVar(symbol_id))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Variable(symbol_id))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Func(symbol_id))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Method(symbol_id)) => {
                let local_or_global_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), *symbol_id)?;

                // mark symbol used
                match local_or_global_symbol {
                    LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                        if let Some(local_scope) = self
                            .resolver
                            .get_scope_ref(self.module_id, local_symbol.get_symbol_id())
                        {
                            self.mark_local_symbol_used_once(local_scope, self.module_id, *symbol_id);
                        }
                    }
                    LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => {
                        self.mark_symbol_used_once(symbol_entry.get_module_id(), symbol_entry.get_symbol_id());
                    }
                    _ => {}
                };
            }
            _ => {}
        };

        match ty {
            ty @ SemanticType::GenericParam(..) => Some(ty),
            SemanticType::GenericType(mut generic_type) => {
                for type_arg in &mut generic_type.type_args {
                    let normalized_type_arg = match type_arg {
                        TypedTypeArg::Positional(concrete_type) => {
                            let normalized_ty =
                                self.normalize_type(scope_id_opt, concrete_type.clone(), loc.clone())?;
                            TypedTypeArg::Positional(normalized_ty)
                        }
                        TypedTypeArg::Named { key, value } => {
                            let normalized_ty = self.normalize_type(scope_id_opt, value.clone(), loc.clone())?;
                            TypedTypeArg::Named {
                                key: key.clone(),
                                value: normalized_ty,
                            }
                        }
                    };
                    *type_arg = normalized_type_arg;
                }
                Some(SemanticType::GenericType(generic_type))
            }
            SemanticType::UnresolvedSymbol(symbol_id) => {
                self.resolver.resolve_local_or_global_symbol(local_scope_opt, symbol_id);

                let resolved = self.resolve_symbol_type(scope_id_opt, symbol_id, loc.clone())?;
                self.normalize_type(scope_id_opt, resolved, loc)
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::Typedef(symbol_id)) => {
                let local_or_global_symbol =
                    match self.resolver.resolve_local_or_global_symbol(local_scope_opt, symbol_id) {
                        Some(local_or_global_symbol) => local_or_global_symbol,
                        None => {
                            self.report_non_type_symbol(symbol_id, loc.clone());
                            return None;
                        }
                    };

                let resolved_typedef_opt = match &local_or_global_symbol {
                    LocalOrGlobalSymbol::LocalSymbol(local_symbol) => local_symbol.as_typedef(),
                    LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => symbol_entry.as_typedef(),
                };
                let resolved_typedef = match resolved_typedef_opt {
                    Some(resolved_typedef) => resolved_typedef,
                    None => {
                        self.report_non_type_symbol(symbol_id, loc.clone());
                        return None;
                    }
                };

                let inner = resolved_typedef.typedef_sig.ty.clone();
                self.normalize_type(scope_id_opt, inner, loc)
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::Variable(symbol_id)) => {
                let local_symbol = self
                    .resolver
                    .resolve_symbol_from_local_scope(local_scope_opt.unwrap(), symbol_id)
                    .unwrap();
                let resolved_variable_opt = local_symbol.as_variable();

                if let Some(resolved_variable) = resolved_variable_opt {
                    if let Some(t) = &resolved_variable.typed_variable.ty {
                        return self.normalize_type(scope_id_opt, t.clone(), loc);
                    } else if let Some(rhs) = &resolved_variable.typed_variable.rhs {
                        let rhs_ty = self.analyze_typed_expr_type_non_terminal(
                            scope_id_opt,
                            &mut rhs.clone(),
                            resolved_variable.typed_variable.ty.clone(),
                        )?;
                        return self.normalize_type(scope_id_opt, rhs_ty, loc);
                    }
                }

                self.report_non_type_symbol(symbol_id, loc);
                None
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::GlobalVar(symbol_id)) => {
                let symbol_entry = self.resolver.resolve_global_symbol(symbol_id).unwrap();
                let resolved_global_var_opt = symbol_entry.as_global_var();

                if let Some(resolved_global_var) = resolved_global_var_opt {
                    if let Some(t) = &resolved_global_var.global_var_sig.ty {
                        return self.normalize_type(scope_id_opt, t.clone(), loc);
                    }
                }
                self.report_non_type_symbol(symbol_id, loc);
                None
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::Func(..))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Method(..)) => {
                unreachable!()
            }
            SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(_))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(_))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Union(_))
            | SemanticType::ResolvedSymbol(ResolvedSymbol::Interface(_)) => Some(ty),
            SemanticType::Pointer(inner) => {
                let inner = self.normalize_type(scope_id_opt, *inner, loc)?;
                Some(SemanticType::Pointer(Box::new(inner)))
            }
            SemanticType::Const(inner) => {
                let inner = self.normalize_type(scope_id_opt, *inner, loc)?;
                Some(SemanticType::Const(Box::new(inner)))
            }
            SemanticType::Array(arr) => match self.normalize_array_capacity(scope_id_opt, arr, loc) {
                Some(arr_type) => Some(SemanticType::Array(arr_type)),
                None => None,
            },
            SemanticType::FuncType(mut func_type) => {
                let mut new_params = Vec::with_capacity(func_type.params.list.len());
                for param in func_type.params.list {
                    match self.normalize_type(scope_id_opt, param, loc.clone()) {
                        Some(normalized) => new_params.push(normalized),
                        None => return None, // fail whole function type
                    }
                }
                func_type.params.list = new_params;

                if let Some(variadic) = func_type.params.variadic.clone() {
                    match *variadic {
                        TypedFuncTypeVariadicParams::UntypedCStyle => {}
                        TypedFuncTypeVariadicParams::Typed(concrete_type) => {
                            match self.normalize_type(scope_id_opt, concrete_type, loc.clone()) {
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
                match self.normalize_type(scope_id_opt, *func_type.return_type, loc) {
                    Some(new_ret) => func_type.return_type = Box::new(new_ret),
                    None => return None,
                }

                Some(SemanticType::FuncType(func_type))
            }
            SemanticType::BasicType(_) | SemanticType::UnnamedStruct(_) => Some(ty),
            SemanticType::Tuple(tuple_type) => {
                let mut type_list: Vec<SemanticType> = Vec::new();

                for concrete_type in &tuple_type.type_list {
                    match self.normalize_type(scope_id_opt, concrete_type.clone(), tuple_type.loc.clone()) {
                        Some(normalized) => type_list.push(normalized),
                        None => continue,
                    }
                }

                Some(SemanticType::Tuple(TypedTupleType {
                    type_list,
                    loc: tuple_type.loc,
                }))
            }
        }
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
                            kind: AnalyzerDiagKind::ExprNotComptimeValid,
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

        arr.element_type = Box::new(self.normalize_type(scope_id_opt, *arr.element_type, loc.clone())?);
        Some(arr)
    }

    pub(crate) fn resolve_full_type_from_local_or_global_symbol(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_or_global_symbol: LocalOrGlobalSymbol,
    ) -> Option<SemanticType> {
        match local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol.kind {
                LocalSymbolKind::Variable(resolved_variable) => {
                    if let Some(concrete_type) = &resolved_variable.typed_variable.ty {
                        self.normalize_type(
                            scope_id_opt,
                            concrete_type.clone(),
                            resolved_variable.typed_variable.loc.clone(),
                        )
                    } else if let Some(rhs) = &resolved_variable.typed_variable.rhs {
                        let rhs_ty = self.analyze_typed_expr_type_non_terminal(
                            scope_id_opt,
                            &mut rhs.clone(),
                            resolved_variable.typed_variable.ty.clone(),
                        )?;
                        self.normalize_type(scope_id_opt, rhs_ty, resolved_variable.typed_variable.loc.clone())
                    } else {
                        panic!("Cannot resolve variable type.")
                    }
                }
                LocalSymbolKind::Struct(s) => {
                    Some(SemanticType::ResolvedSymbol(ResolvedSymbol::NamedStruct(s.symbol_id)))
                }
                LocalSymbolKind::Enum(e) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(e.symbol_id))),
                LocalSymbolKind::Union(e) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Union(e.symbol_id))),
                LocalSymbolKind::Interface(i) => {
                    Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Interface(i.symbol_id)))
                }
                LocalSymbolKind::Typedef(resolved_typedef) => self
                    .resolve_typedef_inner_type(&resolved_typedef)
                    .and_then(|t| self.normalize_type(scope_id_opt, t, resolved_typedef.typedef_sig.loc.clone())),
            },

            LocalOrGlobalSymbol::GlobalSymbol(entry) => match entry.kind {
                SymbolEntryKind::Method(..) => unreachable!(),
                SymbolEntryKind::Func(resolved_func) => Some(SemanticType::FuncType(TypedFuncType {
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
                                TypedFuncVariadicParams::Typed(_, concrete_type) => {
                                    Some(Box::new(TypedFuncTypeVariadicParams::Typed(concrete_type)))
                                }
                            },
                            None => None,
                        },
                    },
                    return_type: Box::new(resolved_func.func_sig.return_type.clone()),
                    loc: resolved_func.func_sig.loc.clone(),
                    vis_opt: Some(resolved_func.func_sig.vis.clone()),
                })),
                SymbolEntryKind::GlobalVar(resolved_global_var) => {
                    if let Some(ty) = &resolved_global_var.global_var_sig.ty {
                        self.normalize_type(scope_id_opt, ty.clone(), resolved_global_var.global_var_sig.loc.clone())
                    } else {
                        self.analyze_typed_expr_type(
                            scope_id_opt,
                            &mut resolved_global_var.global_var_sig.rhs.unwrap(),
                            None,
                        )
                    }
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
                SymbolEntryKind::Interface(resolved_interface) => Some(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::Interface(resolved_interface.symbol_id),
                )),
                SymbolEntryKind::Typedef(resolved_typedef) => self
                    .resolve_typedef_inner_type(&resolved_typedef)
                    .and_then(|concrete_type| {
                        self.normalize_type(scope_id_opt, concrete_type, resolved_typedef.typedef_sig.loc.clone())
                    }),
            },
        }
    }

    fn resolve_typedef_inner_type(&mut self, resolved_typedef: &ResolvedTypedef) -> Option<SemanticType> {
        Some(resolved_typedef.typedef_sig.ty.clone())
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
            self.report_cyclic_type_def(symbol, loc);
            return None;
        }

        let local_scope_opt = scope_id_opt.and_then(|sid| self.resolver.get_scope_ref(self.module_id, sid));
        let lg = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .or_else(|| {
                let module_id = self.resolver.lookup_symbol_id_in_modules(symbol_id)?;
                let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, symbol_id)?;
                Some(LocalOrGlobalSymbol::GlobalSymbol(symbol_entry))
            });

        let mut concrete_type_opt: Option<SemanticType> = None;

        if let Some(local_or_global) = lg {
            concrete_type_opt = self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, local_or_global);

            if let Some(ty) = concrete_type_opt.clone() {
                concrete_type_opt = self.normalize_type(scope_id_opt, ty, loc.clone());
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

    fn report_cyclic_type_def(&mut self, symbol: String, loc: SourceLoc) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::CyclicTypeDefinition { symbol },
            location: Some(DiagLoc::new(loc)),
            hint: None,
        });
    }

    fn report_non_type_symbol(&mut self, symbol_id: SymbolID, loc: SourceLoc) {
        let symbol_name = (self.symbol_formatter)(None)(symbol_id);

        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::NonTypeSymbol { symbol_name },
            location: Some(DiagLoc::new(loc)),
            hint: None,
        });
    }
}
