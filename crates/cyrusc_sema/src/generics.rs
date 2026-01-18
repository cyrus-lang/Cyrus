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
use cyrusc_resolver::symbols::{LocalScopeRef, generate_scope_id};
use cyrusc_tast::{
    ScopeID, SymbolID,
    format::format_sema_ty,
    generics::{
        generic_type::GenericType,
        mapping_ctx::{GenericMappingCtx, GenericMappingEntry},
        monomorph::{MonomorphKey, SpecializedFuncEntry},
    },
    mapping_ctx_arena,
    sigs::{FuncSig, typed_func_type_from_func_sig},
    stmts::*,
    types::SemanticType,
};
use std::{cell::RefCell, ops::RangeInclusive, rc::Rc};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn initial_generic_params_and_mapping_ctx(
        &mut self,
        sema_ty: &SemanticType,
        generic_params: Option<&TypedGenericParamsList>,
        expected_type: Option<SemanticType>,
    ) -> (Option<TypedGenericParamsList>, Option<Rc<GenericMappingCtx>>) {
        // extract use-site generics (may or may not exist)
        let (generic_params, mut mapping_ctx) = sema_ty
            .extract_generic_for_use(generic_params)
            .map(|(params, ctx)| (Some(params), Some(ctx)))
            .unwrap_or((None, None));

        // merge expected-type generics if present
        if let Some(expected_ty) = expected_type {
            if let Some(expected_generic_ty) = expected_ty.as_generic_type() {
                let parent_generic_mapping_id = mapping_ctx_arena!(self, mapping_ctx_arena, {
                    mapping_ctx_arena.insert(expected_generic_ty.mapping_ctx.borrow().clone())
                });

                mapping_ctx = Some(match mapping_ctx {
                    Some(old_ctx) => Rc::new(GenericMappingCtx::new_manual(
                        old_ctx.get_named_mapping().clone(),
                        old_ctx.get_links().clone(),
                        Some(parent_generic_mapping_id),
                    )),
                    None => Rc::new({
                        let mut default_generic_mapping_ctx = GenericMappingCtx::default();
                        default_generic_mapping_ctx.set_parent_id(parent_generic_mapping_id);
                        default_generic_mapping_ctx
                    }),
                });
            }
        }

        (generic_params, mapping_ctx)
    }

    pub(crate) fn try_infer_generic_param_as_expected_type(
        &self,
        sema_ty: SemanticType,
        generic_type_opt: &Option<GenericType>,
    ) -> Option<SemanticType> {
        let Some(generic_type) = generic_type_opt.clone() else {
            return None;
        };

        let Some(generic_param) = sema_ty.as_generic_param() else {
            return None;
        };

        let ctx = generic_type.mapping_ctx.borrow();

        let inferred_sema_ty = mapping_ctx_arena!(self, mapping_ctx_arena, {
            ctx.get_with_name(&*mapping_ctx_arena, &generic_param.param_name.name)
                .or(generic_param.default.clone().map(|sema_ty| *sema_ty))
        });

        // NOTE: here we check that if generic mapping ctx is empty,
        // we consider that there is no expected type for the generic param.
        // this is special because to prevent nested parent mapping ctx creation in arena.
        if let Some(generic_type) = inferred_sema_ty
            .as_ref()
            .and_then(|sema_ty| sema_ty.as_generic_type().cloned())
        {
            let is_generic_mapping_ctx_empty = { generic_type.mapping_ctx.borrow().is_empty() };

            if is_generic_mapping_ctx_empty {
                None
            } else {
                inferred_sema_ty
            }
        } else {
            inferred_sema_ty
        }
    }

    pub(crate) fn register_specialized_generic_func(
        &mut self,
        func_sig: &FuncSig,
        generic_type: &GenericType,
        // only used for methods (optional)
        self_modifier_ty: Option<SemanticType>,
        func_call_loc: &SourceLoc,
    ) -> Option<MonomorphKey> {
        let current_diag_len = self.reporter.diags.len();

        let (mut template_body, mapping_ctx, base_symbol) = {
            let ctx = self.monomorph_registry.lock().unwrap();

            if let Some(monomorph_key) = ctx.get_func_entry_by_mapping_ctx(
                self.mapping_ctx_arena.clone(),
                func_sig.symbol_id.unwrap(),
                generic_type.mapping_ctx.clone(),
            ) {
                // already registered
                return Some(monomorph_key.clone());
            }

            let generic_template_entry = ctx.get_template(func_sig.symbol_id.unwrap()).unwrap().clone();

            let mapping = generic_type.mapping_ctx.borrow().clone();
            let base = func_sig.symbol_id.unwrap();

            (generic_template_entry.body, mapping, base)
        };

        let template_body_scope = self
            .resolver
            .get_scope_ref(func_sig.module_id, template_body.scope_id)
            .unwrap();

        // make new scope for specialized func body
        let new_body_scope_id = generate_scope_id();
        {
            template_body.scope_id = new_body_scope_id;
            let template_body_scope = template_body_scope.borrow();
            let new_body_scope = template_body_scope.deep_clone();
            self.resolver
                .insert_scope_ref(self.module_id, new_body_scope_id, new_body_scope.clone());
        };

        self.current_func = Some(typed_func_type_from_func_sig(func_sig));
        self.substitute_func_params_in_body_scope(new_body_scope_id, &func_sig.params);

        if let Some(sema_ty) = self_modifier_ty {
            self.analyze_generic_self_modifier(new_body_scope_id, &func_sig.params, sema_ty);
        }

        let mut analyzed_body = template_body.clone();

        self.analyze_func_body(&mut analyzed_body, &func_sig.return_type);

        {
            let diag_len = self.reporter.diags.len();
            if diag_len > current_diag_len {
                self.apply_error_originated_from_on_diag_range(current_diag_len..=diag_len, |diag| {
                    diag.hint = Some(format!(
                        "Error originates from this function call at {}:{}:{}.",
                        func_call_loc.file_path.clone(),
                        func_call_loc.line,
                        func_call_loc.column
                    ));
                });
            }
        }

        let monomorph_key = {
            let mut ctx = self.monomorph_registry.lock().unwrap();
            let (monomorph_key, _) = ctx.register_func(base_symbol, mapping_ctx);
            ctx.register_specialized_func_instance(monomorph_key.clone(), SpecializedFuncEntry { body: analyzed_body });
            monomorph_key
        };

        Some(monomorph_key)
    }

    fn apply_error_originated_from_on_diag_range<F>(&mut self, range: RangeInclusive<usize>, mut f: F)
    where
        F: FnMut(&mut Diag),
    {
        let len = self.reporter.diags.len();
        let start = *range.start();
        let end_inclusive = *range.end();

        if start > end_inclusive {
            return;
        }
        if start >= len {
            return;
        }

        let end = (end_inclusive + 1).min(len);

        for diag in &mut self.reporter.diags[start..end] {
            f(diag);
        }
    }

    fn substitute_func_params_in_body_scope(&mut self, body_scope_id: ScopeID, params: &TypedFuncParams) {
        let local_scope_rc = self.resolver.get_scope_ref(self.module_id, body_scope_id).unwrap();
        {
            let mut local_scope = local_scope_rc.borrow_mut();

            for param_kind in &params.list {
                match param_kind {
                    TypedFuncParamKind::FuncParam(typed_func_param) => {
                        local_scope.with_symbol_id_mut(typed_func_param.symbol_id, |local_symbol| {
                            let resolved_var = local_symbol.as_variable_mut().unwrap();
                            resolved_var.typed_variable.ty = Some(typed_func_param.ty.clone());
                        });
                    }
                    TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                        local_scope.with_symbol_id_mut(typed_self_modifier.symbol_id.unwrap(), |local_symbol| {
                            let resolved_var = local_symbol.as_variable_mut().unwrap();
                            resolved_var.typed_variable.ty = Some(typed_self_modifier.ty.clone().unwrap());
                        });
                    }
                }
            }

            if let Some(variadic) = &params.variadic {
                match variadic {
                    TypedFuncVariadicParams::Typed(ident, sema_ty) => {
                        local_scope.with_symbol_id_mut(ident.symbol_id, |local_symbol| {
                            let resolved_var = local_symbol.as_variable_mut().unwrap();
                            resolved_var.typed_variable.ty = Some(sema_ty.clone());
                        });
                    }
                    TypedFuncVariadicParams::UntypedCStyle => {}
                }
            }
        }
    }

    pub(crate) fn init_generic_type_with_symbol_id(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
        type_args: &Option<TypedTypeArgs>,
        parent_mapping_ctx: Option<Rc<GenericMappingCtx>>,
        // gets it from symbol entry if not specified
        generic_params: Option<&TypedGenericParamsList>,
        is_const: bool,
        loc: SourceLoc,
    ) -> Result<Option<(SymbolID, Option<GenericType>)>, Diag> {
        let type_args = type_args.clone().unwrap_or(Vec::new());

        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        let symbol_generic_params = sym.get_generic_params();
        let generic_params = match generic_params.or(symbol_generic_params.as_ref()) {
            Some(generic_params) => generic_params,
            None => {
                if type_args.is_empty() {
                    return Ok(Some((sym.get_symbol_id(), None)));
                } else {
                    return Err(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });
                }
            }
        };

        let generic_mapping_ctx = Rc::new(RefCell::new(
            parent_mapping_ctx
                .as_ref()
                .map(|parent_mapping_ctx| {
                    let parent_id = mapping_ctx_arena!(self, mapping_ctx_arena, {
                        mapping_ctx_arena.insert(Rc::unwrap_or_clone(parent_mapping_ctx.clone()))
                    });

                    GenericMappingCtx::new_child(parent_id)
                })
                .unwrap_or(GenericMappingCtx::new_root()),
        ));

        let mut generic_type = GenericType::new_unresolved(
            sym.get_symbol_id(),
            type_args,
            generic_mapping_ctx,
            self.mapping_ctx_arena.clone(),
            is_const,
            loc,
        );

        mapping_ctx_arena!(self, mapping_ctx_arena, {
            generic_type.init(
                &*mapping_ctx_arena,
                generic_params.clone(),
                &(self.symbol_formatter)(scope_id_opt),
            )?;
        });
        Ok(Some((sym.get_symbol_id(), Some(generic_type))))
    }

    pub(crate) fn infer_generic_param(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        generic_type_opt: &Option<GenericType>,
        target_ty: SemanticType,
        expr_ty: Option<SemanticType>,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        macro_rules! check_type_mismatch {
            ($lhs:expr, $rhs:expr) => {
                if !self.check_type_mismatch(scope_id_opt, $lhs.clone(), $rhs.clone(), loc.clone()) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_ty($lhs, &(self.symbol_formatter)(scope_id_opt)),
                            rhs_type: format_sema_ty($rhs, &(self.symbol_formatter)(scope_id_opt)),
                        }),
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });
                    return None;
                }
            };
        }

        let Some(generic_type) = generic_type_opt.clone() else {
            return None;
        };

        // skip function if situation is not good for type inferring
        let generic_param = target_ty.as_generic_param().cloned()?;
        let generic_mapping_entry = GenericMappingEntry::from(generic_param.param_name);
        let expr_ty = expr_ty?;

        {
            let mut mapping_ctx = generic_type.mapping_ctx.borrow_mut();

            // resolve linked parent by name
            let linked_parent_opt = mapping_ctx_arena!(self, mapping_ctx_arena, {
                mapping_ctx.get_linked_by_name(&*mapping_ctx_arena, &generic_mapping_entry.name)
            });

            // if a local value exists, use it immediately
            {
                let sema_ty_opt = mapping_ctx_arena!(self, mapping_ctx_arena, {
                    mapping_ctx.get_with_name(&*mapping_ctx_arena, &generic_mapping_entry.name)
                });

                if let Some(sema_ty) = sema_ty_opt {
                    check_type_mismatch!(expr_ty, sema_ty);

                    return Some(sema_ty);
                }
            }

            // if any parent directly has a concrete value for this symbol id, use it
            if let Some(parent_sema_ty) = mapping_ctx.get_parent_id().and_then(|parent_id| {
                mapping_ctx_arena!(self, mapping_ctx_arena, {
                    let generic_mapping_ctx = mapping_ctx_arena.get(parent_id).unwrap();
                    generic_mapping_ctx.get_with_name(&*mapping_ctx_arena, &generic_mapping_entry.name)
                })
            }) {
                check_type_mismatch!(expr_ty, parent_sema_ty);

                mapping_ctx.insert_named(generic_mapping_entry.clone(), parent_sema_ty.clone());
                return Some(parent_sema_ty);
            }

            // if a linked parent by name exists, resolve its type
            {
                if let Some(parent_entry) = linked_parent_opt {
                    let parent_sema_ty = mapping_ctx_arena!(self, mapping_ctx_arena, {
                        mapping_ctx.get_with_name(&*mapping_ctx_arena, &parent_entry.name)
                    });

                    if let Some(sema_ty) = parent_sema_ty {
                        check_type_mismatch!(expr_ty, sema_ty);

                        mapping_ctx.insert_named(generic_mapping_entry.clone(), sema_ty.clone());
                        mapping_ctx.insert_linked(generic_mapping_entry.clone(), parent_entry);

                        return Some(sema_ty);
                    }
                }
            }

            // otherwise, just insert the expression type as the generic param value
            mapping_ctx.insert_named(generic_mapping_entry.clone(), expr_ty.clone());
            Some(expr_ty)
        }
    }

    pub(crate) fn merge_generic_operand_as_expected_type(
        &self,
        operand_ty: SemanticType,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let generic_type = operand_ty.as_generic_type()?;

        let parent_id_opt = expected_type
            .as_ref()
            .and_then(|t| t.as_generic_type())
            .map(|expected_generic| {
                mapping_ctx_arena!(self, mapping_ctx_arena, {
                    mapping_ctx_arena.insert(expected_generic.mapping_ctx.borrow().clone())
                })
            });

        if let Some(parent_id) = parent_id_opt {
            let mut ctx = generic_type.mapping_ctx.borrow_mut();
            ctx.set_parent_id(parent_id);
        }

        Some(SemanticType::GenericType(generic_type.clone()))
    }

    pub(crate) fn export_expected_generic_mapping_ctx(
        &self,
        expected_type: Option<SemanticType>,
    ) -> Option<Rc<GenericMappingCtx>> {
        expected_type.and_then(|sema_ty| {
            sema_ty
                .as_generic_type()
                .map(|g| Rc::new(g.mapping_ctx.borrow().clone()))
        })
    }
}
