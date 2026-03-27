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
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::symbols::table::SymbolEntryMut;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    SymbolID,
    format::{SymbolFormatterFn, format_sema_type},
    generics::{
        generic_type::GenericType,
        mapping_ctx::{GenericMappingCtx, GenericMappingEntry},
        monomorph::MonomorphID,
        substitute::substitute_type,
    },
    sigs::FuncSig,
    stmts::*,
    types::SemanticType,
};
use std::{
    cell::{RefCell, RefMut},
    ops::RangeInclusive,
    rc::Rc,
};

impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    pub(crate) fn merge_generic_type(&self, generic_type1: &GenericType, generic_type2: &GenericType) -> GenericType {
        let mut merged_generic_type = generic_type1.clone();

        let params_in_type2_not_in_type1: Vec<_> = generic_type2
            .generic_params
            .list
            .iter()
            .filter(|param| !generic_type1.generic_params.list.contains(param))
            .cloned()
            .collect();

        merged_generic_type
            .generic_params
            .list
            .extend(params_in_type2_not_in_type1);

        let merged_mapping_ctx = merged_generic_type
            .mapping_ctx
            .borrow_mut()
            .merge(&generic_type2.mapping_ctx.borrow());

        merged_generic_type.type_args = None;

        *merged_generic_type.mapping_ctx.borrow_mut() = merged_mapping_ctx;
        merged_generic_type
    }

    /// Extracts and merges generic parameters and mapping contexts from type information.
    ///
    /// This function serves as a starting point for generic type operations by:
    /// 1. Extracting generic parameters and mapping context from a semantic type (if it's a generic type)
    /// 2. Merging generic context from an expected type (if provided)
    /// 3. Preserving the parent relationship in the mapping context hierarchy
    pub(crate) fn extract_and_merge_generic_context(
        &mut self,
        sema_type: &SemanticType,
        generic_params: Option<&TypedGenericParamsList>,
        expected_type: Option<SemanticType>,
    ) -> (Option<TypedGenericParamsList>, Option<Rc<GenericMappingCtx>>) {
        // try to extract generics from the semantic type
        let mut mapping_ctx = None;
        let mut params = None;

        if let Some(generic_type) = sema_type.as_generic_type() {
            if let Some(generic_params) = generic_params.cloned().or(Some(generic_type.generic_params.clone())) {
                params = Some(generic_params);
                mapping_ctx = Some(Rc::new(generic_type.mapping_ctx.borrow().clone()));
            }
        }

        // merge expected-type generics if present
        if let Some(expected_type) = expected_type {
            if let Some(expected_generic_ty) = expected_type.as_generic_type() {
                let parent_generic_mapping_id = {
                    let mut mapping_ctx_arena = self.mapping_ctx_arena.lock().unwrap();
                    mapping_ctx_arena.insert(expected_generic_ty.mapping_ctx.borrow().clone())
                };

                mapping_ctx = Some(match mapping_ctx {
                    Some(old_ctx) => Rc::new(GenericMappingCtx::new_manual(
                        old_ctx.named_mapping().clone(),
                        old_ctx.links().clone(),
                        parent_generic_mapping_id,
                    )),
                    None => Rc::new({
                        let mut default_generic_mapping_ctx = GenericMappingCtx::default();
                        if let Some(parent_id) = parent_generic_mapping_id {
                            default_generic_mapping_ctx.set_parent_id(parent_id);
                        }
                        default_generic_mapping_ctx
                    }),
                });
            }
        }

        (params, mapping_ctx)
    }

    pub(crate) fn try_infer_generic_param_as_expected_type(
        &self,
        sema_type: SemanticType,
        generic_type_opt: &Option<GenericType>,
    ) -> Option<SemanticType> {
        let Some(generic_type) = generic_type_opt.clone() else {
            return None;
        };

        let Some(generic_param) = sema_type.as_generic_param() else {
            return None;
        };

        let inferred_sema_ty = {
            let mapping_ctx = generic_type.mapping_ctx.borrow();

            mapping_ctx
                .resolve_with_name(self.mapping_ctx_arena.clone(), &generic_param.name.value)
                .or(generic_param.default.clone().map(|sema_type| *sema_type))
        };

        // NOTE: here we check that if generic mapping ctx is empty,
        // we consider that there is no expected type for the generic param.
        // intended to prevent redundant nested parent mapping ctx insertion into arena.
        if let Some(generic_type) = inferred_sema_ty
            .as_ref()
            .and_then(|sema_type| sema_type.as_generic_type().cloned())
        {
            // is generic mapping ctx empty?
            let is_empty = { generic_type.mapping_ctx.borrow().is_empty() };

            if is_empty { None } else { inferred_sema_ty }
        } else {
            inferred_sema_ty
        }
    }

    // FIXME
    pub(crate) fn register_specialized_generic_func(
        &mut self,
        func_sig: &mut FuncSig,
        generic_type: &GenericType,
        // only used for methods (optional)
        self_modifier_ty: Option<SemanticType>,
        func_call_loc: &Loc,
    ) -> Option<MonomorphID> {
        todo!();

        // let current_diag_len = self.reporter.len();

        // let (mut template_body, mapping_ctx, base_symbol) = {
        //     let monomorph_registry = self.monomorph_registry.lock().unwrap();

        //     if let Some(monomorph_id) = monomorph_registry.resolve_func_entry_by_mapping_ctx(
        //         self.mapping_ctx_arena.clone(),
        //         func_sig.symbol_id.unwrap(),
        //         generic_type.generic_params.clone(),
        //         generic_type.mapping_ctx.clone(),
        //     ) {
        //         // already registered
        //         return Some(monomorph_id.clone());
        //     }

        //     let generic_template_entry = monomorph_registry
        //         .resolve_template(func_sig.symbol_id.unwrap())
        //         .unwrap()
        //         .clone();

        //     let mapping = generic_type.mapping_ctx.borrow().clone();
        //     let base = func_sig.symbol_id.unwrap();

        //     (generic_template_entry.body, mapping, base)
        // };

        // let template_body_scope = self
        //     .resolver
        //     .resolve_local_scope(func_sig.module_id, template_body.scope_id)
        //     .unwrap();

        // // make new scope for specialized func body
        // let new_body_scope_id = generate_scope_id();
        // {
        //     template_body.scope_id = new_body_scope_id;
        //     let template_body_scope = template_body_scope.borrow();
        //     let new_body_scope = template_body_scope.deep_clone();
        //     self.resolver
        //         .insert_scope_ref(self.module_id, new_body_scope_id, new_body_scope.clone());
        // };

        // self.fn_env.current_func = Some(typed_func_type_from_func_sig(func_sig));
        // self.substitute_func_params_in_body_scope(new_body_scope_id, &func_sig.params);
        // func_sig.ret_type = substitute_type(
        //     self.mapping_ctx_arena.clone(),
        //     func_sig.ret_type.clone(),
        //     Rc::new(RefCell::new(mapping_ctx.clone())),
        // )
        // .unwrap();

        // if let Some(sema_type) = self_modifier_ty {
        //     self.analyze_generic_self_modifier(new_body_scope_id, &func_sig.params, sema_type);
        // }

        // let monomorph_id = {
        //     let mut monomorph_registry = self.monomorph_registry.lock().unwrap();
        //     let (monomorph_id, _) =
        //         monomorph_registry.register_func(base_symbol, generic_type.generic_params.clone(), mapping_ctx);
        //     monomorph_id
        // };

        // let mut analyzed_body = template_body.clone();

        // self.analyze_func_body(&mut analyzed_body, &func_sig.ret_type);

        // {
        //     let diag_len = self.reporter.len();
        //     if diag_len > current_diag_len {
        //         self.apply_error_originated_from_on_diag_range(current_diag_len..=diag_len, |diag| {
        //             diag.hint = Some(format!(
        //                 "Error originates from this function call at {}.",
        //                 format_loc(self.source_map, *func_call_loc),
        //             ));
        //         });
        //     }
        // }

        // {
        //     let mut monomorph_registry = self.monomorph_registry.lock().unwrap();
        //     monomorph_registry.register_specialized_func_instance(
        //         monomorph_id.clone(),
        //         SpecializedFuncEntry { body: analyzed_body },
        //     );
        // }

        // Some(monomorph_id)
    }

    fn apply_error_originated_from_on_diag_range<F>(&mut self, range: RangeInclusive<usize>, mut f: F)
    where
        F: FnMut(&mut Diag),
    {
        let len = self.reporter.len();
        let start = *range.start();
        let end_inclusive = *range.end();

        if start > end_inclusive {
            return;
        }
        if start >= len {
            return;
        }

        let end = (end_inclusive + 1).min(len);

        let mut diags = self.reporter.diags_mut();
        for diag in &mut diags[start..end] {
            f(diag);
        }
        drop(diags);
    }

    // fn substitute_func_params_in_body_scope(&mut self, body_scope_id: ScopeID, params: &TypedFuncParams) {
    //     let scope_rc = self
    //         .resolver
    //         .resolve_local_scope(self.module_id, body_scope_id)
    //         .unwrap();
    //     {
    //         let mut scope = scope_rc.borrow_mut();

    //         for param_kind in &params.list {
    //             match param_kind {
    //                 TypedFuncParamKind::FuncParam(typed_func_param) => {
    //                     scope.with_symbol_id_mut(typed_func_param.symbol_id, |local_symbol| {
    //                         let resolved_var = local_symbol.as_variable_mut().unwrap();
    //                         resolved_var.variable.ty = Some(typed_func_param.ty.clone());
    //                     });
    //                 }
    //                 TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
    //                     scope.with_symbol_id_mut(typed_self_modifier.symbol_id.unwrap(), |local_symbol| {
    //                         let resolved_var = local_symbol.as_variable_mut().unwrap();
    //                         resolved_var.variable.ty = Some(typed_self_modifier.ty.clone().unwrap());
    //                     });
    //                 }
    //             }
    //         }

    //         if let Some(variadic) = &params.variadic {
    //             match variadic {
    //                 TypedFuncVariadicParams::Typed(ident, sema_type) => {
    //                     scope.with_symbol_id_mut(ident.symbol_id, |local_symbol| {
    //                         let resolved_var = local_symbol.as_variable_mut().unwrap();
    //                         resolved_var.variable.ty = Some(sema_type.clone());
    //                     });
    //                 }
    //                 TypedFuncVariadicParams::UntypedCStyle => {}
    //             }
    //         }
    //     }
    // }

    // REVIEW: Refactor required.
    // Consider to rename this constructor method,
    // and also make base(symbol_id) optional,
    // because it's possible to form a mapping_ctx without having
    // symbol entry; which is currently used for GenericInterface/GenericTypedef substitution.
    pub(crate) fn init_generic_type_with_symbol_id(
        &mut self,
        symbol_id: SymbolID,
        type_args: &mut Option<TypedTypeArgs>,
        parent_mapping_ctx: Option<Rc<GenericMappingCtx>>,
        generic_params: Option<&TypedGenericParamsList>,
        loc: Loc,
    ) -> Result<Option<(SymbolID, Option<GenericType>)>, Diag> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        self.normalize_type_args(type_args.as_mut());

        let symbol_entry = self.query.get_symbol(symbol_id).unwrap();

        // get genera params from symbol entry if not specified explicitly
        let symbol_generic_params = symbol_entry.symbol_generic_params();
        let generic_params = match generic_params.or(symbol_generic_params.as_ref()) {
            Some(generic_params) => generic_params,
            None => {
                if type_args.is_none() {
                    return Ok(Some((symbol_id, None)));
                } else {
                    return Err(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                        loc: Some(loc),
                        hint: None,
                    });
                }
            }
        };

        let mapping_ctx = Rc::new(RefCell::new(
            parent_mapping_ctx
                .as_ref()
                .and_then(|parent_mapping_ctx| {
                    let parent_id_opt = {
                        let mut mapping_ctx_arena = self.mapping_ctx_arena.lock().unwrap();
                        mapping_ctx_arena.insert(Rc::unwrap_or_clone(parent_mapping_ctx.clone()))
                    };

                    if let Some(parent_id) = parent_id_opt {
                        Some(GenericMappingCtx::new_child(parent_id))
                    } else {
                        None
                    }
                })
                .unwrap_or(GenericMappingCtx::new_root()),
        ));

        let mut generic_type = GenericType::new_unresolved(
            symbol_id,
            type_args.clone(),
            mapping_ctx,
            self.mapping_ctx_arena.clone(),
            generic_params.clone(),
            loc,
        );

        generic_type.init(self.mapping_ctx_arena.clone(), fmt_symbol)?;
        Ok(Some((symbol_id, Some(generic_type))))
    }

    pub(crate) fn unify_generic_types_from_expected_type(
        &mut self,
        generic_type: &GenericType,
        expected_type_opt: Option<SemanticType>,
    ) -> Option<()> {
        let expected_type = expected_type_opt?;
        let expected_generic_type = expected_type.as_generic_type()?;

        // Store the mutated mapping_ctx
        let mut mapping_ctx = generic_type.mapping_ctx.borrow_mut().clone();
        let expected_mapping_ctx = expected_generic_type.mapping_ctx.borrow().clone();

        self.unify_generic_types(
            &mut mapping_ctx,
            &Some(expected_mapping_ctx),
            &SemanticType::GenericType(generic_type.clone()),
            &expected_type,
        );

        *generic_type.mapping_ctx.borrow_mut() = mapping_ctx;
        None
    }

    fn unify_generic_types(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        expr_mapping_ctx_opt: &Option<GenericMappingCtx>,
        expr_ty: &SemanticType,
        target_ty: &SemanticType,
    ) {
        match (target_ty.const_inner(), expr_ty.const_inner()) {
            (SemanticType::GenericParam(generic_param), _) => {
                mapping_ctx.insert_named(
                    GenericMappingEntry::from(generic_param.name.clone()),
                    expr_ty.clone(),
                );
            }
            (SemanticType::Pointer(target_pointer_inner), SemanticType::Pointer(expr_pointer_inner)) => {
                self.unify_generic_types(
                    mapping_ctx,
                    expr_mapping_ctx_opt,
                    &expr_pointer_inner,
                    &target_pointer_inner,
                );
            }
            (SemanticType::Tuple(target_tuple), SemanticType::Tuple(expr_tuple)) => {
                if target_tuple.elements.len() != expr_tuple.elements.len() {
                    return;
                }

                for (target_ty, expr_ty) in target_tuple.elements.iter().zip(&expr_tuple.elements) {
                    self.unify_generic_types(mapping_ctx, expr_mapping_ctx_opt, &expr_ty, target_ty);
                }
            }
            (SemanticType::FuncType(target_func_type), SemanticType::FuncType(expr_func_type)) => {
                for (target_ty, expr_ty) in target_func_type.params.list.iter().zip(&expr_func_type.params.list) {
                    self.unify_generic_types(mapping_ctx, expr_mapping_ctx_opt, &expr_ty, target_ty);
                }

                if let (Some(target_variadic_type), Some(expr_variadic_type)) = (
                    target_func_type.params.as_typed_variadic(),
                    expr_func_type.params.as_typed_variadic(),
                ) {
                    self.unify_generic_types(
                        mapping_ctx,
                        expr_mapping_ctx_opt,
                        &expr_variadic_type,
                        &target_variadic_type,
                    );
                }

                self.unify_generic_types(
                    mapping_ctx,
                    expr_mapping_ctx_opt,
                    &expr_func_type.ret_type,
                    &target_func_type.ret_type,
                );
            }
            (SemanticType::GenericType(target_generic_type), SemanticType::GenericType(expr_generic_type)) => {
                // if both generic types have type arguments, unify them structurally
                if let (Some(target_args), Some(expr_args)) =
                    (&target_generic_type.type_args, &expr_generic_type.type_args)
                {
                    let len = std::cmp::min(target_args.len(), expr_args.len());

                    for i in 0..len {
                        self.unify_generic_types(
                            mapping_ctx,
                            expr_mapping_ctx_opt,
                            expr_args[i].ty(),
                            target_args[i].ty(),
                        );
                    }
                }

                {
                    // resolve from expression's mapping context

                    let Some(expr_mapping_ctx) = expr_mapping_ctx_opt else {
                        return;
                    };

                    for generic_param in &target_generic_type.generic_params.list {
                        let Some(mut sema_type) = expr_mapping_ctx
                            .resolve_with_name(self.mapping_ctx_arena.clone(), &generic_param.name.value)
                        else {
                            continue;
                        };

                        sema_type = match self.normalize_sema_type(sema_type, target_generic_type.loc) {
                            Some(sema_type) => sema_type,
                            None => continue,
                        };

                        mapping_ctx
                            .insert_named(GenericMappingEntry::from(generic_param.name.clone()), sema_type);
                    }
                }
            }
            (SemanticType::Array(target_array_type), SemanticType::Array(expr_array_type)) => {
                self.unify_generic_types(
                    mapping_ctx,
                    expr_mapping_ctx_opt,
                    &expr_array_type.element_type,
                    &target_array_type.element_type,
                );
            }
            _ => return,
        }
    }

    pub(crate) fn infer_generic_param(
        &mut self,
        generic_type_opt: Option<&GenericType>,
        target_ty: SemanticType,
        expr_ty: Option<SemanticType>,
        loc: Loc,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        macro_rules! check_type_mismatch {
            ($lhs:expr, $rhs:expr) => {
                if !self.check_type_mismatch($lhs.clone(), $rhs.clone(), loc) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_type($lhs, fmt_symbol),
                            rhs_type: format_sema_type($rhs, fmt_symbol),
                        }),
                        loc: Some(loc),
                        hint: None,
                    });
                    return None;
                }
            };
        }

        let expr_ty = expr_ty?;

        let Some(generic_type) = generic_type_opt.clone() else {
            return None;
        };

        let mut mapping_ctx_ref: RefMut<GenericMappingCtx>;

        {
            mapping_ctx_ref = generic_type.mapping_ctx.borrow_mut();
            let expr_mapping_ctx_opt = expr_ty
                .as_generic_type()
                .and_then(|generic_type| Some(generic_type.mapping_ctx.borrow().clone()));

            // recursively unify expr_ty with target_ty
            self.unify_generic_types(&mut mapping_ctx_ref, &expr_mapping_ctx_opt, &expr_ty, &target_ty);

            // reconcile with parent/linked contexts
            let generic_entries: Vec<GenericMappingEntry> = mapping_ctx_ref.named_mapping().keys().cloned().collect();

            for entry in generic_entries {
                // if a parent mapping has a value, use it
                if let Some(parent_sema_ty) = mapping_ctx_ref.parent_id().and_then(|parent_id| {
                    let parent_mapping_ctx = {
                        let mapping_ctx_arena = self.mapping_ctx_arena.lock().unwrap();
                        mapping_ctx_arena.get(parent_id).unwrap().clone()
                    };

                    parent_mapping_ctx.resolve_with_name(self.mapping_ctx_arena.clone(), &entry.name)
                }) {
                    // substitute target_ty using the parent value for this generic
                    let substituted_target = substitute_type(
                        self.mapping_ctx_arena.clone(),
                        target_ty.clone(),
                        Rc::new(RefCell::new(mapping_ctx_ref.clone())),
                    )
                    .unwrap();

                    check_type_mismatch!(expr_ty, substituted_target);

                    mapping_ctx_ref.insert_named(entry.clone(), parent_sema_ty);
                    continue;
                }

                // if a linked parent exists, resolve its type
                let linked_parent_opt =
                    mapping_ctx_ref.resolve_linked_by_name(self.mapping_ctx_arena.clone(), &entry.name);

                if let Some(parent_entry) = linked_parent_opt {
                    if let Some(linked_val) =
                        mapping_ctx_ref.resolve_with_name(self.mapping_ctx_arena.clone(), &parent_entry.name)
                    {
                        let substituted_target = substitute_type(
                            self.mapping_ctx_arena.clone(),
                            target_ty.clone(),
                            Rc::new(RefCell::new(mapping_ctx_ref.clone())),
                        )
                        .unwrap();
                        check_type_mismatch!(expr_ty, substituted_target);

                        mapping_ctx_ref.insert_named(entry.clone(), linked_val.clone());
                        mapping_ctx_ref.insert_linked(entry.clone(), parent_entry);
                        continue;
                    }
                }
            }
        }

        {
            let mapping_ctx_rc = Rc::new(RefCell::new(mapping_ctx_ref.clone()));

            if let Some(type_args) = &generic_type.type_args {
                self.substitute_generic_type_args_after_inference(mapping_ctx_rc.clone(), type_args.clone());
            }

            // substitute generics in target_ty for final type check
            let substituted_target_ty = substitute_type(
                self.mapping_ctx_arena.clone(),
                target_ty.clone(),
                mapping_ctx_rc.clone(),
            )
            .unwrap();

            self.normalize_sema_type(substituted_target_ty, loc)
        }
    }

    fn substitute_generic_type_args_after_inference(
        &self,
        mapping_ctx: Rc<RefCell<GenericMappingCtx>>,
        mut type_args: TypedTypeArgs,
    ) -> TypedTypeArgs {
        for type_arg in &mut type_args {
            match type_arg {
                TypedTypeArg::Positional { ty, .. } => {
                    *ty = substitute_type(self.mapping_ctx_arena.clone(), ty.clone(), mapping_ctx.clone()).unwrap();
                }
                TypedTypeArg::Named { ty, .. } => {
                    *ty = substitute_type(self.mapping_ctx_arena.clone(), ty.clone(), mapping_ctx.clone()).unwrap();
                }
            }
        }

        type_args
    }

    pub(crate) fn merge_generic_operand_with_expected_type(
        &self,
        operand_type: SemanticType,
        expected_type: Option<SemanticType>,
    ) -> Option<GenericType> {
        let Some(generic_type) = operand_type.as_generic_type() else {
            return expected_type.and_then(|sema_type| sema_type.as_generic_type().cloned());
        };

        let parent_id_opt = expected_type
            .as_ref()
            .and_then(|t| t.as_generic_type())
            .and_then(|expected_generic| {
                let mut mapping_ctx_arena = self.mapping_ctx_arena.lock().unwrap();
                mapping_ctx_arena.insert(expected_generic.mapping_ctx.borrow().clone())
            });

        if let Some(parent_id) = parent_id_opt {
            let mut ctx = generic_type.mapping_ctx.borrow_mut();
            ctx.set_parent_id(parent_id);
        }

        Some(generic_type.clone())
    }

    pub(crate) fn export_expected_generic_mapping_ctx(
        &self,
        expected_type: Option<SemanticType>,
    ) -> Option<Rc<GenericMappingCtx>> {
        expected_type.and_then(|sema_type| {
            sema_type
                .as_generic_type()
                .map(|g| Rc::new(g.mapping_ctx.borrow().clone()))
        })
    }
}
