use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, with_monomorph_registry};
use ast::source_loc::SourceLoc;
use diagcentral::{Diag, DiagLevel, DiagLoc};
use partialmatch::partial_match;
use resolver::{
    scope::LocalScopeRef,
    signatures::{EnumSig, StructSig, UnionSig},
};
use std::collections::HashMap;
use typed_ast::{
    ScopeID, SymbolID, TypedEnumVariant, TypedExpression, TypedFuncTypeParams, TypedGenericParamsList, TypedTypeArg,
    TypedTypeArgs,
    format::format_concrete_type,
    types::{
        ConcreteType, GenericType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUnnamedStructType,
        TypedUnnamedStructTypeField,
    },
};

#[derive(Debug, Clone, Default)]
pub(crate) struct GenericMappingCtx {
    pub(crate) parent: Option<Box<GenericMappingCtx>>,
    pub(crate) positional: Vec<ConcreteType>,
    pub(crate) named: HashMap<String, ConcreteType>,
}

#[macro_export]
macro_rules! generic_mapping_ctx_scope {
    ($self:ident, $generic_params:expr, $type_args:expr, $loc:expr, $ctx:ident, $body:block) => {{
        #[allow(unused_mut)]
        let mut $ctx =
            $self.get_generic_mapping_ctx(&$generic_params, &$type_args, None, $loc);

        $self.generic_ctx_stack.push($ctx.clone());
        $body
        $self.generic_ctx_stack.pop();
    }};
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn get_linked_typedef_mapping_ctx(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        typedef_symbol_id: SymbolID,
        type_args_opt: &Option<TypedTypeArgs>,
        loc: SourceLoc,
    ) -> Option<(ConcreteType, Option<Box<GenericMappingCtx>>)> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, typedef_symbol_id)
            .unwrap();

        local_or_global_symbol
            .as_typedef()
            .and_then(|resolved_typedef| {
                let typedef_generic_params = &resolved_typedef.typedef_sig.generic_params;

                // build typedef-level mapping ctx
                let mut typedef_ctx =
                    self.get_generic_mapping_ctx(typedef_generic_params, type_args_opt, None, loc.clone());

                self.generic_ctx_stack.push(typedef_ctx.clone());

                // substitute inner type and traverse it
                let concrete_type =
                    self.traverse_and_link_inner_generics(resolved_typedef.typedef_sig.ty.clone(), &mut typedef_ctx)?;

                self.generic_ctx_stack.pop();

                Some((concrete_type, Some(Box::new(typedef_ctx))))
            })
            .or(self
                .resolve_full_type_from_local_or_global_symbol(scope_id_opt, local_or_global_symbol)
                .map(|concrete_type| (concrete_type, None)))
    }

    // traverse a concrete type and replace inner generic params with the typedef's generic params
    fn traverse_and_link_inner_generics(
        &mut self,
        ty: ConcreteType,
        typedef_ctx: &mut GenericMappingCtx,
    ) -> Option<ConcreteType> {
        match ty {
            ConcreteType::GenericParam(param) => Some(
                typedef_ctx
                    .named
                    .get(&param.name)
                    .cloned()
                    .unwrap_or(ConcreteType::GenericParam(param)),
            ),
            ConcreteType::Pointer(inner) => Some(ConcreteType::Pointer(Box::new(
                self.traverse_and_link_inner_generics(*inner, typedef_ctx)?,
            ))),
            ConcreteType::Array(inner) => Some(ConcreteType::Array(TypedArrayType {
                element_type: Box::new(self.traverse_and_link_inner_generics(*inner.element_type, typedef_ctx)?),
                capacity: inner.capacity,
                loc: inner.loc,
            })),
            ConcreteType::Const(inner) => Some(ConcreteType::Const(Box::new(
                self.traverse_and_link_inner_generics(*inner, typedef_ctx)?,
            ))),
            ConcreteType::Tuple(t) => {
                let new_list = t
                    .type_list
                    .into_iter()
                    .map(|t| self.traverse_and_link_inner_generics(t, typedef_ctx))
                    .collect::<Option<Vec<_>>>()?;
                Some(ConcreteType::Tuple(TypedTupleType {
                    type_list: new_list,
                    loc: t.loc,
                }))
            }
            ConcreteType::FuncType(f) => {
                let new_params = f
                    .params
                    .list
                    .into_iter()
                    .map(|p| self.traverse_and_link_inner_generics(p, typedef_ctx))
                    .collect::<Option<Vec<_>>>()?;
                let new_return = Box::new(self.traverse_and_link_inner_generics(*f.return_type, typedef_ctx)?);
                Some(ConcreteType::FuncType(TypedFuncType {
                    def_module_id: f.def_module_id,
                    params: TypedFuncTypeParams {
                        list: new_params,
                        variadic: f.params.variadic,
                    },
                    return_type: new_return,
                    vis_opt: f.vis_opt,
                    loc: f.loc,
                }))
            }
            ConcreteType::UnnamedStruct(s) => {
                let new_fields = s
                    .fields
                    .into_iter()
                    .map(|f| {
                        Some(TypedUnnamedStructTypeField {
                            field_name: f.field_name,
                            field_type: Box::new(self.traverse_and_link_inner_generics(*f.field_type, typedef_ctx)?),
                            loc: f.loc,
                        })
                    })
                    .collect::<Option<Vec<_>>>()?;
                Some(ConcreteType::UnnamedStruct(TypedUnnamedStructType {
                    fields: new_fields,
                    packed: s.packed,
                    loc: s.loc,
                }))
            }
            other => Some(other),
        }
    }

    // pub(crate) fn substitute_typedef_to_concrete_type(
    //     &mut self,
    //     scope_id_opt: Option<ScopeID>,
    //     local_scope_opt: Option<LocalScopeRef>,
    //     typedef_symbol_id: SymbolID,
    //     type_args_opt: &Option<TypedTypeArgs>,
    //     loc: SourceLoc,
    // ) -> Option<(ConcreteType, Option<GenericMappingCtx>)> {
    //     let local_or_global_symbol = self
    //         .resolver
    //         .resolve_local_or_global_symbol(local_scope_opt, typedef_symbol_id)
    //         .unwrap();

    //     local_or_global_symbol
    //         .as_typedef()
    //         .and_then(|resolved_typedef| {
    //             let generic_params = &resolved_typedef.typedef_sig.generic_params;

    //             let generic_mapping_ctx =
    //                 self.get_generic_mapping_ctx(generic_params, type_args_opt, None, loc.clone());

    //             self.generic_ctx_stack.push(generic_mapping_ctx.clone());

    //             let concrete_type =
    //                 self.substitute_type(resolved_typedef.typedef_sig.ty.clone(), &generic_mapping_ctx, None)?;

    //             self.generic_ctx_stack.pop();

    //             Some((concrete_type, Some(generic_mapping_ctx.clone())))
    //         })
    //         .or(self
    //             .resolve_full_type_from_local_or_global_symbol(scope_id_opt, local_or_global_symbol)
    //             .map(|concrete_type| (concrete_type, None)))
    // }

    pub(crate) fn substitute_field_access_type(
        &mut self,
        field_access_operand: &mut TypedExpression,
        generic_params: &Option<TypedGenericParamsList>,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!((generic_type_opt, field_access_operand.concrete_type.clone()), {
            (Some(generic_type), Some(operand_ty)) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&generic_params, &Some(generic_type.type_args.clone()), None, loc);
                field_access_operand.concrete_type = self.substitute_type(operand_ty, &mapping_ctx, None);
            }
        })
    }

    pub(crate) fn substitute_struct_type_args(
        &mut self,
        struct_sig: &mut StructSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&struct_sig.generic_params, &Some(generic_type.type_args.clone()), None, loc);
                struct_sig.fields.iter_mut().enumerate().for_each(|(idx, field)| {
                    if let Some(concrete_type) = self.substitute_type(field.ty.clone(), &mapping_ctx, Some(idx)){
                        field.ty = concrete_type;
                    }
                });
            }
        })
    }

    pub(crate) fn substitute_union_type_args(
        &mut self,
        union_sig: &mut UnionSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&union_sig.generic_params, &Some(generic_type.type_args.clone()), None, loc);
                union_sig.fields.iter_mut().enumerate().for_each(|(idx, field)| {
                    if let Some(concrete_type) = self.substitute_type(field.ty.clone(), &mapping_ctx, Some(idx)){
                        field.ty = concrete_type;
                    }
                });
            }
        })
    }

    pub(crate) fn substitute_enum_type_args(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        enum_sig: &mut EnumSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(&enum_sig.generic_params, &Some(generic_type.type_args.clone()), None, loc);
                enum_sig.variants.iter_mut().enumerate().for_each(|(idx, variant)| {
                    match variant {
                        TypedEnumVariant::Identifier(..) => {},
                        TypedEnumVariant::Valued(_, typed_expr) => {
                            self.analyze_typed_expr_type(scope_id_opt, typed_expr, None).inspect(|concrete_type| {
                                self.substitute_type(concrete_type.clone(), &mapping_ctx, Some(idx));
                            });
                        },
                        TypedEnumVariant::Variant(_, typed_enum_valued_fields) => {
                            for valued_field in typed_enum_valued_fields {
                                if let Some(substituted_type) = self.substitute_type(valued_field.field_type.clone(), &mapping_ctx, Some(idx)) {
                                    valued_field.field_type = substituted_type;
                                }
                            }
                        },
                    }
                });
            }
        })
    }

    pub(crate) fn normalize_type_args_and_register(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        generic_params: &Option<TypedGenericParamsList>,
        generic_mapping_ctx: &GenericMappingCtx,
        expected_type: Option<ConcreteType>,
        loc: SourceLoc,
        allow_inference_without_error: bool,
        register_monomorph: bool,
    ) -> Option<Vec<ConcreteType>> {
        let Some(generic_params) = generic_params else {
            return None;
        };

        let mut normalized_type_args: Vec<Option<ConcreteType>> = vec![None; generic_params.len()];
        let mut missing: Vec<String> = Vec::new();

        for (i, _) in generic_params.iter().enumerate() {
            if let Some(concrete_type) = generic_mapping_ctx.positional.get(i) {
                normalized_type_args[i] = Some(concrete_type.clone());
            }
        }

        for (name, ty) in &generic_mapping_ctx.named {
            if let Some(idx) = generic_params.iter().position(|param| param.param_name.name == *name) {
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
                    generic_mapping_ctx,
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
                kind: AnalyzerDiagKind::ExplicitTypeArgsRequired { type_name },
                location: Some(DiagLoc::new(loc)),
                hint: Some(hint),
            });
            return None;
        }

        let final_args: Vec<ConcreteType> = normalized_type_args.into_iter().filter_map(|x| x).collect();

        if register_monomorph {
            with_monomorph_registry!(self, registry, {
                registry.register(symbol_id, final_args.clone());
            });
        }

        Some(final_args)
    }

    pub(crate) fn inferred_types_as_positional_type_args(&self, types: Vec<ConcreteType>) -> TypedTypeArgs {
        types
            .iter()
            .map(|concrete_type| TypedTypeArg::Positional(concrete_type.clone()))
            .collect()
    }

    pub(crate) fn substitute_type_or_infer_with(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        concrete_type: ConcreteType,
        expr: &mut TypedExpression,
        generic_mapping_ctx: &mut GenericMappingCtx,
        positional_index: Option<usize>,
    ) -> Option<ConcreteType> {
        match self.substitute_type(concrete_type.clone(), generic_mapping_ctx, positional_index) {
            Some(substituted) => {
                if let Some(expr_type) = self.analyze_typed_expr_type(scope_id_opt, expr, Some(substituted.clone())) {
                    if !self.check_type_mismatch(scope_id_opt, expr_type.clone(), substituted.clone(), expr.loc.clone())
                    {
                        let expected_type =
                            format_concrete_type(substituted.clone(), &(self.symbol_formatter)(scope_id_opt));
                        let found_type =
                            format_concrete_type(expr_type.clone(), &(self.symbol_formatter)(scope_id_opt));
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::AssignmentTypeMismatch {
                                lhs_type: expected_type,
                                rhs_type: found_type,
                            },
                            location: Some(DiagLoc::new(expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                    Some(substituted)
                } else {
                    None
                }
            }
            None => {
                if let Some(expr_type) = self.analyze_typed_expr_type(scope_id_opt, expr, None) {
                    if let ConcreteType::GenericParam(param) = &concrete_type {
                        // insert inferred type into named map using symbol_id
                        generic_mapping_ctx.named.insert(param.name.clone(), expr_type.clone());

                        // also update positional if this param exists there
                        if let Some(pos_idx) = positional_index {
                            if pos_idx < generic_mapping_ctx.positional.len() {
                                generic_mapping_ctx.positional[pos_idx] = expr_type.clone();
                            }
                        }
                    }

                    self.substitute_type(concrete_type, generic_mapping_ctx, positional_index)
                } else {
                    None
                }
            }
        }
    }

    pub(crate) fn substitute_type(
        &self,
        concrete_type: ConcreteType,
        generic_mapping_ctx: &GenericMappingCtx,
        positional_index: Option<usize>,
    ) -> Option<ConcreteType> {
        match concrete_type {
            ConcreteType::GenericParam(param) => generic_mapping_ctx.get(param.name, positional_index),
            ConcreteType::Pointer(inner) => Some(ConcreteType::Pointer(Box::new(self.substitute_type(
                *inner,
                generic_mapping_ctx,
                positional_index,
            )?))),
            ConcreteType::Array(inner) => Some(ConcreteType::Array(TypedArrayType {
                element_type: Box::new(self.substitute_type(
                    *inner.element_type,
                    generic_mapping_ctx,
                    positional_index,
                )?),
                capacity: inner.capacity,
                loc: inner.loc.clone(),
            })),
            ConcreteType::Const(inner) => Some(ConcreteType::Const(Box::new(self.substitute_type(
                *inner,
                generic_mapping_ctx,
                positional_index,
            )?))),
            ConcreteType::Tuple(tuple) => {
                let new_list = tuple
                    .type_list
                    .into_iter()
                    .map(|t| self.substitute_type(t, generic_mapping_ctx, positional_index))
                    .collect::<Option<Vec<_>>>()?;

                Some(ConcreteType::Tuple(TypedTupleType {
                    type_list: new_list,
                    loc: tuple.loc.clone(),
                }))
            }
            ConcreteType::FuncType(func) => {
                let new_params = func
                    .params
                    .list
                    .into_iter()
                    .map(|p| self.substitute_type(p, generic_mapping_ctx, positional_index))
                    .collect::<Option<Vec<_>>>()?;

                let new_return =
                    Box::new(self.substitute_type(*func.return_type, generic_mapping_ctx, positional_index)?);
                Some(ConcreteType::FuncType(TypedFuncType {
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
            ConcreteType::UnnamedStruct(s) => {
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

                Some(ConcreteType::UnnamedStruct(TypedUnnamedStructType {
                    fields: new_fields,
                    packed: s.packed,
                    loc: s.loc.clone(),
                }))
            }
            other => Some(other),
        }
    }

    pub(crate) fn get_generic_mapping_ctx(
        &mut self,
        generic_params_opt: &Option<TypedGenericParamsList>,
        type_args_opt: &Option<TypedTypeArgs>,
        parent: Option<Box<GenericMappingCtx>>,
        loc: SourceLoc,
    ) -> GenericMappingCtx {
        let mut positional: Vec<ConcreteType> = Vec::new();
        let mut named: HashMap<String, ConcreteType> = HashMap::new();

        if let Some(generic_params) = generic_params_opt {
            if let Some(type_args) = type_args_opt.as_ref() {
                // zip generic_params with type_args (positional + named)
                for arg in type_args.iter() {
                    match arg {
                        TypedTypeArg::Positional(ct) => {
                            positional.push(ct.clone());
                        }
                        TypedTypeArg::Named { key, value } => {
                            named.insert(key.clone(), value.clone());
                        }
                    }
                }

                if positional.len() > generic_params.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::UnexpectedTypeArgs,
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: Some(format!(
                            "Too many positional type arguments (expected <= {}, found {})",
                            generic_params.len(),
                            positional.len()
                        )),
                    });
                }
            }
        } else if type_args_opt.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UnexpectedTypeArgs,
                location: Some(DiagLoc::new(loc.clone())),
                hint: Some("Remove the type arguments or add generic parameters to the type.".to_string()),
            });
        }

        GenericMappingCtx {
            positional,
            named,
            parent,
        }
    }

    pub(crate) fn infer_generic_type_from_expected_type(
        &mut self,
        symbol_id: SymbolID,
        generic_params: &TypedGenericParamsList,
        generic_mapping_ctx: &GenericMappingCtx,
        expected_type: Option<ConcreteType>,
    ) -> Option<Vec<ConcreteType>> {
        let expected = expected_type.as_ref()?;

        let ConcreteType::GenericType(generic_type) = expected else {
            return None;
        };
        if generic_type.base != symbol_id {
            return None;
        }

        let mut merged_args: Vec<ConcreteType> = Vec::with_capacity(generic_params.len());

        for (i, param) in generic_params.iter().enumerate() {
            let key = param.param_name.name.clone();

            let concrete = if i < generic_mapping_ctx.positional.len() {
                Some(generic_mapping_ctx.positional[i].clone())
            } else {
                None
            }
            .or_else(|| generic_mapping_ctx.named.get(&key).cloned())
            .or_else(|| param.default.clone())
            .or_else(|| {
                if i >= generic_type.type_args.len() {
                    None
                } else {
                    match &generic_type.type_args[i] {
                        TypedTypeArg::Positional(ty) => Some(ty.clone()),
                        TypedTypeArg::Named { value, .. } => Some(value.clone()),
                    }
                }
            });

            if let Some(concrete_type) = concrete {
                merged_args.push(concrete_type);
            } else {
                // could not infer this generic param
                return None;
            }
        }

        with_monomorph_registry!(self, registry, {
            registry.register(symbol_id, merged_args.clone());
        });

        Some(merged_args)
    }
}

impl GenericMappingCtx {
    pub(crate) fn get(&self, name: String, positional_index: Option<usize>) -> Option<ConcreteType> {
        if let Some(idx) = positional_index {
            if idx < self.positional.len() {
                return Some(self.positional[idx].clone());
            }
        }
        if let Some(concrete) = self.named.get(&name) {
            return Some(concrete.clone());
        }

        self.parent.as_ref()?.get(name, positional_index)
    }
}
