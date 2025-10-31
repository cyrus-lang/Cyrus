use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind, with_monomorph_registry};
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc};
use cyrusc_resolver::sigs::{EnumSig, StructSig, UnionSig};
use cyrusc_tast::{
    ScopeID, SymbolID,
    exprs::{TypedExprStmt, TypedIdentifier},
    format::format_concrete_type,
    stmts::*,
    types::{
        GenericType, SemanticType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUStructType,
        TypedUnnamedStructTypeField,
    },
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[macro_export]
macro_rules! generic_mapping_ctx_scope {
    // parent context provided explicitly
    ($self:ident, $generic_params:expr, $type_args:expr, $loc:expr, $parent_ctx:expr, $exposed_ctx:ident, $body:block) => {{
        let mut __ctx = $parent_ctx
            .and_then(|ctx| Some(ctx.new_child()))
            .unwrap_or(GenericMappingCtx::new_root());

        #[allow(unused_mut)]
        let mut __child_ctx = $self.get_generic_mapping_ctx(&mut __ctx, &$generic_params, &$type_args, $loc);

        $self.generic_ctx_stack.push(__child_ctx.clone());
        let __result = {
            let mut $exposed_ctx = __child_ctx;
            $body
        };
        $self.generic_ctx_stack.pop();

        __result
    }};
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn resolve_generic_typedef(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
    ) -> Option<(
        SemanticType,
        Option<Box<GenericMappingCtx>>,
        Option<TypedGenericParamsList>,
    )> {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)?;

        if let Some(resolved_typedef) = sym.as_typedef() {
            if let Some(generic_type) = resolved_typedef.typedef_sig.ty.as_generic_type() {
                let inner_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, generic_type.base)?;

                // Traverse in the target generic type, and link them to the external type_args
                // for example:
                //   struct Record<K, V> {
                //     pub key: K;
                //     pub value: V;
                //  }
                //
                //  type Foo<P> = Record<P, int>
                //
                // It'd just link the `K -> P`, Also `V` is automatically is set to `int` at this point.

                let loc = resolved_typedef.typedef_sig.loc.clone();
                let generic_params = &inner_symbol.get_generic_type();

                let typedef_mapping_ctx = self.get_generic_mapping_ctx(
                    mapping_ctx,
                    generic_params,
                    &Some(generic_type.type_args.clone()),
                    loc,
                );

                return Some((
                    resolved_typedef.typedef_sig.ty.clone(),
                    Some(Box::new(typedef_mapping_ctx)),
                    resolved_typedef.typedef_sig.generic_params.clone(),
                ));
            }
        }

        match self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, sym)? {
            SemanticType::GenericType(generic_type) => {
                self.resolve_generic_typedef(mapping_ctx, scope_id_opt, generic_type.base)
            }
            ty @ _ => Some((ty, None, None)),
        }
    }

    pub(crate) fn substitute_field_access_type(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        field_access_operand: &mut TypedExprStmt,
        generic_params: &Option<TypedGenericParamsList>,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        match (generic_type_opt, field_access_operand.sema_ty.clone()) {
            (Some(generic_type), Some(operand_ty)) => {
                let mapping_ctx = self.get_generic_mapping_ctx(
                    mapping_ctx,
                    &generic_params,
                    &Some(generic_type.type_args.clone()),
                    loc,
                );
                field_access_operand.sema_ty = self.substitute_type(operand_ty, &mapping_ctx, None);
            }
            _ => {}
        }
    }

    pub(crate) fn substitute_struct_type_args(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        struct_sig: &mut StructSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        match generic_type_opt {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(
                    mapping_ctx,
                    &struct_sig.generic_params,
                    &Some(generic_type.type_args.clone()),
                    loc,
                );
                struct_sig.fields.iter_mut().enumerate().for_each(|(idx, field)| {
                    if let Some(sema_ty) = self.substitute_type(field.ty.clone(), &mapping_ctx, Some(idx)) {
                        field.ty = sema_ty;
                    }
                });
            }
            _ => {}
        }
    }

    pub(crate) fn substitute_union_type_args(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        union_sig: &mut UnionSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        match generic_type_opt {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(
                    mapping_ctx,
                    &union_sig.generic_params,
                    &Some(generic_type.type_args.clone()),
                    loc,
                );
                union_sig.fields.iter_mut().enumerate().for_each(|(idx, field)| {
                    if let Some(sema_ty) = self.substitute_type(field.ty.clone(), &mapping_ctx, Some(idx)) {
                        field.ty = sema_ty;
                    }
                });
            }
            _ => {}
        }
    }

    pub(crate) fn substitute_enum_type_args(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        scope_id_opt: Option<ScopeID>,
        enum_sig: &mut EnumSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        match generic_type_opt {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(
                    mapping_ctx,
                    &enum_sig.generic_params,
                    &Some(generic_type.type_args.clone()),
                    loc,
                );
                enum_sig
                    .variants
                    .iter_mut()
                    .enumerate()
                    .for_each(|(idx, variant)| match variant {
                        TypedEnumVariant::Identifier(..) => {}
                        TypedEnumVariant::Valued(_, typed_expr) => {
                            self.analyze_typed_expr_type(scope_id_opt, typed_expr, None)
                                .inspect(|sema_ty| {
                                    self.substitute_type(sema_ty.clone(), &mapping_ctx, Some(idx));
                                });
                        }
                        TypedEnumVariant::Variant(_, typed_enum_valued_fields) => {
                            for valued_field in typed_enum_valued_fields {
                                if let Some(substituted_type) =
                                    self.substitute_type(valued_field.field_type.clone(), &mapping_ctx, Some(idx))
                                {
                                    valued_field.field_type = substituted_type;
                                }
                            }
                        }
                    });
            }
            _ => {}
        }
    }

    pub(crate) fn inferred_types_as_positional_type_args(&self, types: Vec<SemanticType>) -> TypedTypeArgs {
        types
            .iter()
            .map(|sema_ty| TypedTypeArg::Positional(sema_ty.clone()))
            .collect()
    }

    pub(crate) fn substitute_type_or_infer_with(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        generic_params_opt: &Option<TypedGenericParamsList>,
        sema_ty: SemanticType,
        expr: &mut TypedExprStmt,
        generic_mapping_ctx: &mut GenericMappingCtx,
        positional_index: Option<usize>,
    ) -> Option<SemanticType> {
        // evaluate expr type before substitution
        let expr_concrete_typ = expr
            .sema_ty
            .clone()
            .or(self.analyze_typed_expr_type(scope_id_opt, expr, None));

        // substitute or infer
        let final_concrete_type = match self.substitute_type(sema_ty.clone(), generic_mapping_ctx, positional_index) {
            Some(substituted) => {
                self.analyze_typed_expr_type(scope_id_opt, expr, Some(substituted.clone()));
                Some(substituted)
            }
            None => {
                // insert freshly inferred generic param to mapping ctx
                if let Some(expr_type) = self.analyze_typed_expr_type(scope_id_opt, expr, None) {
                    self.substitute_type(expr_type, generic_mapping_ctx, positional_index)
                        .and_then(|substituted| {
                            if let Some(generic_params) = generic_params_opt {
                                if let Some(idx) = positional_index {
                                    if let Some(generic_param) = generic_params.get(idx) {
                                        generic_mapping_ctx
                                            .insert_named(generic_param.param_name.clone(), substituted.clone());
                                    }
                                }
                            }
                            Some(substituted)
                        })
                } else {
                    None
                }
            }
        };

        // check mismatch with original expr type
        if let Some(expr_ty) = &expr_concrete_typ {
            if !self.check_type_mismatch(
                scope_id_opt,
                expr_ty.clone(),
                final_concrete_type.clone()?,
                expr.loc.clone(),
            ) {
                let expected_type =
                    format_concrete_type(final_concrete_type.clone()?, &(self.symbol_formatter)(scope_id_opt));
                let found_type = format_concrete_type(expr_ty.clone(), &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                        lhs_type: expected_type,
                        rhs_type: found_type,
                    }),
                    location: Some(DiagLoc::new(expr.loc.clone())),
                    hint: None,
                });
                return None;
            }
        }

        final_concrete_type
    }

    pub(crate) fn get_generic_mapping_ctx(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        generic_params_opt: &Option<TypedGenericParamsList>,
        input_type_args_opt: &Option<TypedTypeArgs>,
        loc: SourceLoc,
    ) -> GenericMappingCtx {
        // when generic parameters exist
        if let Some(generic_params) = generic_params_opt {
            if let Some(type_args) = input_type_args_opt {
                // link positional and named type arguments
                for (idx, arg) in type_args.iter().enumerate() {
                    match arg {
                        TypedTypeArg::Positional(sema_ty) => {
                            let current_gp = match generic_params.get(idx) {
                                Some(gp) => gp,
                                None => continue, // skip if there's no corresponding param
                            };

                            if let Some(generic_param) = sema_ty.as_generic_param() {
                                // Link generic → generic (e.g. T = U)
                                mapping_ctx.insert_linked(current_gp.param_name.symbol_id, generic_param.symbol_id);
                            } else {
                                // Bind concrete type to generic param
                                mapping_ctx.insert_named(current_gp.param_name.clone(), sema_ty.clone());
                            }
                        }

                        TypedTypeArg::Named { key, value } => {
                            if let Some(generic_param) = generic_params.iter().find(|gp| gp.param_name.name == *key) {
                                mapping_ctx.insert_named(generic_param.param_name.clone(), value.clone());
                            }
                        }
                    }
                }

                // check for excess type arguments
                if type_args.len() > generic_params.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: Some(format!(
                            "Too many positional type arguments (expected <= {}, found {}).",
                            generic_params.len(),
                            type_args.len(),
                        )),
                    });
                }
            }
        }

        mapping_ctx.clone()
    }

    pub(crate) fn normalize_type_args_and_register(
        &mut self,
        mapping_ctx: &GenericMappingCtx,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        generic_params: &Option<TypedGenericParamsList>,
        expected_type: Option<SemanticType>,
        loc: SourceLoc,
        allow_inference_without_error: bool,
        register_monomorph: bool,
    ) -> Option<Vec<SemanticType>> {
        let Some(generic_params) = generic_params else {
            return None;
        };

        let mut normalized_type_args: Vec<Option<SemanticType>> = vec![None; generic_params.len()];
        let mut missing: Vec<String> = Vec::new();

        for (name, ty) in &mapping_ctx.named {
            if let Some(idx) = generic_params
                .iter()
                .position(|param| param.param_name.symbol_id == name.symbol_id)
            {
                normalized_type_args[idx] = Some(ty.clone());
            }
        }

        for (i, param) in generic_params.iter().enumerate() {
            if normalized_type_args[i].is_none() {
                if let Some(default_ty) = &param.default {
                    normalized_type_args[i] = Some(default_ty.clone());
                }
            }
        }

        for (i, ty_slot) in normalized_type_args.iter_mut().enumerate() {
            if ty_slot.is_none() {
                if let Some(inferred) = self.infer_generic_type_from_expected_type(
                    symbol_id,
                    &generic_params[i..=i].to_vec(),
                    mapping_ctx,
                    expected_type.clone(),
                ) {
                    *ty_slot = Some(inferred[0].clone());
                } else {
                    missing.push(generic_params[i].param_name.name.clone());
                }
            }
        }

        if !missing.is_empty() && !allow_inference_without_error {
            let type_name = (self.symbol_formatter)(scope_id_opt)(symbol_id);
            let hint = format!("Provide explicit type arguments for {}", missing.join(", "));
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ExplicitTypeArgsRequired { type_name }),
                location: Some(DiagLoc::new(loc)),
                hint: Some(hint),
            });
            return None;
        }

        let final_args: Vec<SemanticType> = normalized_type_args.into_iter().filter_map(|x| x).collect();

        if register_monomorph {
            with_monomorph_registry!(self, registry, {
                registry.register(symbol_id, final_args.clone());
            });
        }

        Some(final_args)
    }

    pub(crate) fn substitute_type(
        &self,
        sema_ty: SemanticType,
        generic_mapping_ctx: &GenericMappingCtx,
        positional_index: Option<usize>,
    ) -> Option<SemanticType> {
        match sema_ty {
            SemanticType::GenericParam(param) => generic_mapping_ctx.get_with_symbol_id(param.symbol_id),
            SemanticType::Pointer(inner) => Some(SemanticType::Pointer(Box::new(self.substitute_type(
                *inner,
                generic_mapping_ctx,
                positional_index,
            )?))),
            SemanticType::Array(inner) => Some(SemanticType::Array(TypedArrayType {
                element_type: Box::new(self.substitute_type(
                    *inner.element_type,
                    generic_mapping_ctx,
                    positional_index,
                )?),
                capacity: inner.capacity,
                loc: inner.loc.clone(),
            })),
            SemanticType::Const(inner) => Some(SemanticType::Const(Box::new(self.substitute_type(
                *inner,
                generic_mapping_ctx,
                positional_index,
            )?))),
            SemanticType::Tuple(tuple) => {
                let new_list = tuple
                    .type_list
                    .into_iter()
                    .map(|t| self.substitute_type(t, generic_mapping_ctx, positional_index))
                    .collect::<Option<Vec<_>>>()?;
                Some(SemanticType::Tuple(TypedTupleType {
                    type_list: new_list,
                    loc: tuple.loc.clone(),
                }))
            }
            SemanticType::FuncType(func) => {
                let new_params = func
                    .params
                    .list
                    .into_iter()
                    .map(|p| self.substitute_type(p, generic_mapping_ctx, positional_index))
                    .collect::<Option<Vec<_>>>()?;
                let new_return =
                    Box::new(self.substitute_type(*func.return_type, generic_mapping_ctx, positional_index)?);
                Some(SemanticType::FuncType(TypedFuncType {
                    def_module_id: func.def_module_id,
                    params: TypedFuncTypeParams {
                        list: new_params,
                        variadic: func.params.variadic,
                    },
                    return_type: new_return,
                    vis_opt: func.vis_opt,
                    loc: func.loc,
                }))
            }
            SemanticType::UnnamedStruct(s) => {
                let new_fields = s
                    .fields
                    .iter()
                    .map(|f| {
                        Some(TypedUnnamedStructTypeField {
                            field_name: f.field_name.clone(),
                            field_type: Box::new(self.substitute_type(
                                *f.field_type.clone(),
                                generic_mapping_ctx,
                                positional_index,
                            )?),
                            loc: f.loc.clone(),
                        })
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(SemanticType::UnnamedStruct(TypedUStructType {
                    fields: new_fields,
                    is_packed: s.is_packed,
                    loc: s.loc.clone(),
                }))
            }
            other => Some(other),
        }
    }

    pub(crate) fn infer_generic_type_from_expected_type(
        &mut self,
        symbol_id: SymbolID,
        generic_params: &TypedGenericParamsList,
        generic_mapping_ctx: &GenericMappingCtx,
        expected_type: Option<SemanticType>,
    ) -> Option<Vec<SemanticType>> {
        let expected = expected_type.as_ref()?;

        let SemanticType::GenericType(generic_type) = expected else {
            return None;
        };
        if generic_type.base != symbol_id {
            return None;
        }

        let mut merged_args: Vec<SemanticType> = Vec::with_capacity(generic_params.len());

        dbg!(expected_type.clone());
        todo!();
        // FIXME
        // for (i, param) in generic_params.iter().enumerate() {
        //     let concrete = if i < generic_mapping_ctx.positional.len() {
        //         Some(generic_mapping_ctx.positional[i].clone())
        //     } else {
        //         None
        //     }
        //     .or_else(|| generic_mapping_ctx.named.get(&param.param_name.clone()).cloned())
        //     .or_else(|| param.default.clone())
        //     .or_else(|| {
        //         if i >= generic_type.type_args.len() {
        //             None
        //         } else {
        //             match &generic_type.type_args[i] {
        //                 TypedTypeArg::Positional(ty) => Some(ty.clone()),
        //                 TypedTypeArg::Named { value, .. } => Some(value.clone()),
        //             }
        //         }
        //     });

        //     if let Some(sema_ty) = concrete {
        //         merged_args.push(sema_ty);
        //     } else {
        //         // could not infer this generic param
        //         return None;
        //     }
        // }

        with_monomorph_registry!(self, registry, {
            registry.register(symbol_id, merged_args.clone());
        });

        Some(merged_args)
    }
}

pub type ChildGenericParamSymbolID = SymbolID;

#[derive(Debug, Clone, Default)]
pub struct GenericMappingCtx {
    pub named: HashMap<TypedIdentifier, SemanticType>,
    pub parent: Option<Box<GenericMappingCtx>>,
    pub linked_gps: Rc<RefCell<HashMap<ChildGenericParamSymbolID, SymbolID>>>,
}

impl GenericMappingCtx {
    pub fn get_with_symbol_id(&self, symbol_id: SymbolID) -> Option<SemanticType> {
        fn lookup_named(ctx: &GenericMappingCtx, sid: SymbolID) -> Option<SemanticType> {
            ctx.named
                .iter()
                .find_map(|(key, val)| (key.symbol_id == sid).then(|| val.clone()))
        }

        fn lookup_in_chain(mut ctx_opt: Option<&Box<GenericMappingCtx>>, sid: SymbolID) -> Option<SemanticType> {
            while let Some(ctx) = ctx_opt {
                if let Some(t) = lookup_named(ctx, sid) {
                    return Some(t);
                }
                ctx_opt = ctx.parent.as_ref();
            }
            None
        }

        fn lookup_via_links(ctx: &GenericMappingCtx, sid: SymbolID) -> Option<SemanticType> {
            let linked_gps = ctx.linked_gps.borrow();

            for (child, mapped) in &*linked_gps {
                if *child == sid {
                    if let Some(t) =
                        lookup_named(ctx, *mapped).or_else(|| lookup_in_chain(ctx.parent.as_ref(), *mapped))
                    {
                        return Some(t);
                    }
                } else if *mapped == sid {
                    if let Some(t) = lookup_named(ctx, *child).or_else(|| lookup_in_chain(ctx.parent.as_ref(), *child))
                    {
                        return Some(t);
                    }
                }
            }
            None
        }

        lookup_named(self, symbol_id)
            .or_else(|| lookup_via_links(self, symbol_id))
            .or_else(|| self.parent.as_ref()?.get_with_symbol_id(symbol_id))
    }

    pub fn insert_named(&mut self, id: TypedIdentifier, ty: SemanticType) {
        // only insert if this id is not yet inferred
        if !self.named.contains_key(&id) {
            self.named.insert(id.clone(), ty.clone());
        }
    }

    pub fn insert_linked(&self, child_id: SymbolID, parent_id: SymbolID) {
        self.linked_gps.borrow_mut().insert(child_id, parent_id);
    }

    pub fn new_root() -> Self {
        Self {
            named: HashMap::new(),
            linked_gps: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    pub fn new_child(&self) -> Self {
        Self {
            named: HashMap::new(),
            linked_gps: self.linked_gps.clone(),
            parent: Some(Box::new(self.clone())),
        }
    }
}
