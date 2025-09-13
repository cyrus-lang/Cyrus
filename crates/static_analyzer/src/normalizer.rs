use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use ast::token::Location;
use diagcentral::{Diag, DiagLevel, DiagLoc};
use resolver::scope::{LocalOrGlobalSymbol, LocalSymbolKind, ResolvedStruct, ResolvedTypedef, SymbolEntryKind};
use typed_ast::{
    ScopeID, SymbolID,
    types::{BasicConcreteType, ConcreteType, ResolvedSymbol, TypedArrayCapacity, TypedArrayFixedCapacityValue},
};

impl<'a> AnalysisContext<'a> {
    // Fully normalize a type: remove UnresolvedSymbol, expand typedefs,
    // and recursively normalize children. Never returns UnresolvedSymbol.
    pub fn normalize_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        ty: ConcreteType,
        loc: Location,
    ) -> Option<ConcreteType> {
        let local_scope_opt = scope_id_opt.and_then(|sid| self.resolver.get_scope_ref(self.module_id, sid));

        match &ty {
            ConcreteType::UnresolvedSymbol(symbol_id)
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(symbol_id))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Typedef(symbol_id))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(symbol_id))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Interface(symbol_id))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::GlobalVar(symbol_id))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Variable(symbol_id))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Func(symbol_id))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Method(symbol_id)) => {
                let local_or_global_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt.clone(), *symbol_id)?;

                // mark symbol used
                match &local_or_global_symbol {
                    LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                        let local_scope_opt = self
                            .resolver
                            .get_scope_ref(self.module_id, local_symbol.get_symbol_id());

                        if local_scope_opt.is_some() {
                            self.mark_local_symbol_used_once(local_scope_opt.unwrap(), self.module_id, *symbol_id);
                        }
                    }
                    LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => {
                        self.mark_symbol_used_once(symbol_entry.get_module_id(), symbol_entry.get_symbol_id());
                    }
                }
            }
            _ => {}
        }

        match ty {
            ConcreteType::UnresolvedSymbol(symbol_id) => {
                self.resolver.resolve_local_or_global_symbol(local_scope_opt, symbol_id);

                let resolved = self.resolve_symbol_type(scope_id_opt, symbol_id, loc.clone())?;
                self.normalize_type(scope_id_opt, resolved, loc)
            }
            ConcreteType::ResolvedSymbol(ResolvedSymbol::Typedef(symbol_id)) => {
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
            ConcreteType::ResolvedSymbol(ResolvedSymbol::Variable(symbol_id)) => {
                let local_symbol = self
                    .resolver
                    .resolve_symbol_from_local_scope(local_scope_opt.unwrap(), symbol_id)
                    .unwrap();
                let resolved_variable_opt = local_symbol.as_variable();

                if let Some(resolved_variable) = resolved_variable_opt {
                    if let Some(t) = &resolved_variable.typed_variable.ty {
                        return self.normalize_type(scope_id_opt, t.clone(), loc);
                    } else if let Some(rhs) = &resolved_variable.typed_variable.rhs {
                        let rhs_ty = self.analyze_typed_expr_type(
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
            ConcreteType::ResolvedSymbol(ResolvedSymbol::GlobalVar(symbol_id)) => {
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
            ConcreteType::ResolvedSymbol(ResolvedSymbol::Func(..))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Method(..)) => {
                // Convert function symbol to its function TYPE (signature) and normalize it.
                // let f = self.resolver.get_function(self.module_id, symbol_id);
                // if let Some(f) = f {
                //     let fty = ConcreteType::from_function_sig(&f.sig); // define this
                //     return self.normalize_type(scope_id_opt, fty, loc);
                // }
                // self.report_non_type_symbol(symbol_id, loc);
                // None
                // FIXME
                todo!();
            }
            ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(_))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(_))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Union(_))
            | ConcreteType::ResolvedSymbol(ResolvedSymbol::Interface(_)) => Some(ty),
            ConcreteType::Pointer(inner) => {
                let inner = self.normalize_type(scope_id_opt, *inner, loc)?;
                Some(ConcreteType::Pointer(Box::new(inner)))
            }
            ConcreteType::Const(inner) => {
                let inner = self.normalize_type(scope_id_opt, *inner, loc)?;
                Some(ConcreteType::Const(Box::new(inner)))
            }
            ConcreteType::Array(arr) => {
                let mut arr = arr;

                match &mut arr.capacity {
                    TypedArrayCapacity::Fixed(capacity_value) => match capacity_value.clone() {
                        TypedArrayFixedCapacityValue::Expr(mut typed_expr) => {
                            typed_expr.concrete_type = self.analyze_typed_expr_type(
                                scope_id_opt,
                                &mut typed_expr,
                                Some(ConcreteType::BasicType(BasicConcreteType::SizeT)),
                            );

                            arr.capacity = TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Expr(typed_expr));
                        }
                        TypedArrayFixedCapacityValue::Value(_) => {}
                    },
                    TypedArrayCapacity::Dynamic => {}
                }

                arr.element_type = Box::new(self.normalize_type(scope_id_opt, *arr.element_type, loc.clone())?);
                Some(ConcreteType::Array(arr))
            }
            ConcreteType::BasicType(_) | ConcreteType::UnnamedStruct(_) => Some(ty),
        }
    }

    pub(crate) fn resolve_full_type_from_local_or_global_symbol(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_or_global_symbol: LocalOrGlobalSymbol,
    ) -> Option<ConcreteType> {
        match local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol.kind {
                LocalSymbolKind::Variable(resolved_variable) => {
                    if let Some(t) = &resolved_variable.typed_variable.ty {
                        self.normalize_type(scope_id_opt, t.clone(), resolved_variable.typed_variable.loc.clone())
                    } else if let Some(rhs) = &resolved_variable.typed_variable.rhs {
                        let rhs_ty = self.analyze_typed_expr_type(
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
                    Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(s.symbol_id)))
                }
                LocalSymbolKind::Enum(e) => Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(e.symbol_id))),
                LocalSymbolKind::Union(e) => Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Union(e.symbol_id))),
                LocalSymbolKind::Interface(i) => {
                    Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Interface(i.symbol_id)))
                }
                LocalSymbolKind::Typedef(resolved_typedef) => self
                    .resolve_typedef_inner_type(&resolved_typedef)
                    .and_then(|t| self.normalize_type(scope_id_opt, t, resolved_typedef.typedef_sig.loc.clone())),
            },

            LocalOrGlobalSymbol::GlobalSymbol(entry) => match entry.kind {
                SymbolEntryKind::Method(..) => {
                    // let fty = ConcreteType::from_function_sig(&m.sig);
                    // self.normalize_type(scope_id_opt, fty, m.loc.clone())
                    todo!();
                }
                SymbolEntryKind::Func(..) => {
                    // let fty = ConcreteType::from_function_sig(&f.sig);
                    // self.normalize_type(scope_id_opt, fty, f.loc.clone())
                    todo!();
                }
                SymbolEntryKind::GlobalVar(resolved_global_var) => {
                    if let Some(ty) = &resolved_global_var.global_var_sig.ty {
                        self.normalize_type(scope_id_opt, ty.clone(), resolved_global_var.global_var_sig.loc.clone())
                    } else {
                        panic!("Cannot resolve variable type.")
                    }
                }
                SymbolEntryKind::Struct(s) => {
                    Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(s.symbol_id)))
                }
                SymbolEntryKind::Enum(e) => Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(e.symbol_id))),
                SymbolEntryKind::Union(e) => Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Union(e.symbol_id))),
                SymbolEntryKind::Interface(i) => {
                    Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Interface(i.symbol_id)))
                }
                SymbolEntryKind::Typedef(resolved_typedef) => self
                    .resolve_typedef_inner_type(&resolved_typedef)
                    .and_then(|t| self.normalize_type(scope_id_opt, t, resolved_typedef.typedef_sig.loc.clone())),
            },
        }
    }

    pub(crate) fn resolve_symbol_as_struct(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        loc: Location,
    ) -> Option<ResolvedStruct> {
        let local_scope_opt = {
            if let Some(scope_id) = scope_id_opt {
                Some(self.resolver.get_scope_ref(self.module_id, scope_id).unwrap())
            } else {
                None
            }
        };

        match match self.normalize_type(scope_id_opt, ConcreteType::UnresolvedSymbol(symbol_id), loc.clone())? {
            ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(symbol_id)) => {
                match self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
                    .unwrap()
                {
                    LocalOrGlobalSymbol::LocalSymbol(local_symbol) => local_symbol.as_struct().cloned(),
                    LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => symbol_entry.as_struct().cloned(),
                }
            }
            _ => None,
        } {
            Some(resolved_struct) => Some(resolved_struct),
            None => {
                let symbol_name = (self.symbol_formatter)(scope_id_opt)(symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NonStructSymbol { symbol_name },
                    location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
                    hint: None,
                });

                None
            }
        }
    }

    fn resolve_typedef_inner_type(&mut self, resolved_typedef: &ResolvedTypedef) -> Option<ConcreteType> {
        Some(resolved_typedef.typedef_sig.ty.clone())
    }

    pub(crate) fn resolve_symbol_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        loc: Location,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        if let Some(cached) = self.ty_caches.cache.get(&symbol_id) {
            return Some(cached.clone());
        }

        if !self.ty_caches.push(symbol_id) {
            let symbol = formatter_closure(symbol_id);
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

        let mut concrete_type_opt: Option<ConcreteType> = None;

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

    fn report_cyclic_type_def(&mut self, symbol: String, loc: Location) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::CyclicTypeDefinition { symbol },
            location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
            hint: None,
        });
    }

    fn report_non_type_symbol(&mut self, symbol_id: SymbolID, loc: Location) {
        let symbol_name = (self.symbol_formatter)(None)(symbol_id);

        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::NonTypeSymbol { symbol_name },
            location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
            hint: None,
        });
    }
}
