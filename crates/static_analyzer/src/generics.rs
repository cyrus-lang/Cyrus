use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, with_monomorph_registry};
use ast::source_loc::SourceLoc;
use diagcentral::{Diag, DiagLevel, DiagLoc};
use generics::GenericMappingCtx;
use partialmatch::partial_match;
use resolver::{
    scope::LocalScopeRef,
    signatures::{EnumSig, StructSig, UnionSig},
};
use typed_ast::{
    ScopeID, SymbolID, TypedEnumVariant, TypedExpression, TypedFuncTypeParams, TypedGenericParam,
    TypedGenericParamsList, TypedIdentifier, TypedStructInit, TypedTypeArg, TypedTypeArgs,
    format::format_concrete_type,
    types::{
        ConcreteType, GenericType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUnnamedStructType,
        TypedUnnamedStructTypeField,
    },
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn resolve_generic_typedef(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
    ) -> Option<(
        ConcreteType,
        Option<Box<GenericMappingCtx>>,
        Option<TypedGenericParamsList>,
    )> {
        let local_scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.get_scope_ref(self.module_id, scope_id));

        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), symbol_id)?;

        if let Some(resolved_typedef) = local_or_global_symbol.as_typedef() {
            if let Some(generic_type) = resolved_typedef.typedef_sig.ty.as_generic_type() {
                let inner_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, generic_type.base)?;

                // traverse in the target generic type, and link them to the external type_args
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

        match self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, local_or_global_symbol)? {
            ConcreteType::GenericType(generic_type) => {
                self.resolve_generic_typedef(mapping_ctx, scope_id_opt, generic_type.base)
            }
            ty @ _ => Some((ty, None, None)),
        }
    }

    pub(crate) fn substitute_field_access_type(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        field_access_operand: &mut TypedExpression,
        generic_params: &Option<TypedGenericParamsList>,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!((generic_type_opt, field_access_operand.concrete_type.clone()), {
            (Some(generic_type), Some(operand_ty)) => {
                let mapping_ctx = self.get_generic_mapping_ctx(mapping_ctx, &generic_params, &Some(generic_type.type_args.clone()), loc);
                field_access_operand.concrete_type = self.substitute_type(operand_ty, &mapping_ctx, None);
            }
        })
    }

    pub(crate) fn substitute_struct_type_args(
        &mut self,
        mapping_ctx: &mut GenericMappingCtx,
        struct_sig: &mut StructSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(mapping_ctx, &struct_sig.generic_params, &Some(generic_type.type_args.clone()), loc);
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
        mapping_ctx: &mut GenericMappingCtx,
        union_sig: &mut UnionSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(mapping_ctx, &union_sig.generic_params, &Some(generic_type.type_args.clone()), loc);
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
        mapping_ctx: &mut GenericMappingCtx,
        scope_id_opt: Option<ScopeID>,
        enum_sig: &mut EnumSig,
        generic_type_opt: Option<&GenericType>,
        loc: SourceLoc,
    ) {
        partial_match!(generic_type_opt, {
            Some(generic_type) => {
                let mapping_ctx = self.get_generic_mapping_ctx(mapping_ctx, &enum_sig.generic_params, &Some(generic_type.type_args.clone()), loc);
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
        // evaluate expr type before substitution
        let expr_concrete_typ = expr
            .concrete_type
            .clone()
            .or(self.analyze_typed_expr_type(scope_id_opt, expr, None));

        // substitute or infer
        let final_concrete_type =
            match self.substitute_type(concrete_type.clone(), generic_mapping_ctx, positional_index) {
                Some(substituted) => {
                    self.analyze_typed_expr_type(scope_id_opt, expr, Some(substituted.clone()));
                    Some(substituted)
                }
                None => {
                    if let Some(expr_type) = self.analyze_typed_expr_type(scope_id_opt, expr, None) {
                        self.substitute_type(expr_type, generic_mapping_ctx, positional_index)
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
                    kind: AnalyzerDiagKind::AssignmentTypeMismatch {
                        lhs_type: expected_type,
                        rhs_type: found_type,
                    },
                    location: Some(DiagLoc::new(expr.loc.clone())),
                    hint: None,
                });
                return None;
            }
        }

        final_concrete_type
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

        //     if let Some(concrete_type) = concrete {
        //         merged_args.push(concrete_type);
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
