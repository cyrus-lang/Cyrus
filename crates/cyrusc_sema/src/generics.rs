use crate::analyze::AnalysisContext;
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::Diag;
use cyrusc_resolver::symbols::LocalScopeRef;
use cyrusc_tast::{
    SymbolID,
    generics::{generic_type::GenericType, mapping_ctx::GenericMappingCtx},
    stmts::*,
    types::{
        SemanticType, TypedArrayType, TypedFuncType, TypedTupleType, TypedUStructType, TypedUnnamedStructTypeField,
    },
};
use std::{cell::RefCell, rc::Rc};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn init_generic_type_with_symbol_id(
        &self,
        local_scope_opt: Option<LocalScopeRef>,
        symbol_id: SymbolID,
        type_args: &Option<TypedTypeArgs>,
        is_const: bool,
        loc: SourceLoc,
    ) -> Result<Option<(SymbolID, Option<GenericType>)>, Diag> {
        let type_args = type_args.clone().unwrap_or(Vec::new());

        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        let mut parent_mapping_ctx: Option<GenericMappingCtx> = None;
        if let Some(_resolved_typedef) = sym.as_typedef() {
            // REVIEW Maybe we need to merge current mapping ctx with type args with mapping ctx of the typedef and return unified one.
            parent_mapping_ctx = None;
        }

        let generic_params = match sym.get_generic_params() {
            Some(generic_params) => generic_params,
            None => {
                panic!("This symbol does not accept type args!");
            }
        };

        let mapping_ctx = Rc::new(RefCell::new(
            parent_mapping_ctx
                .and_then(|parent| Some(GenericMappingCtx::new_child(Rc::new(parent))))
                .unwrap_or(GenericMappingCtx::new_root()),
        ));

        let mut generic_type = GenericType::new_unresolved(sym.get_symbol_id(), type_args, mapping_ctx, is_const, loc);

        generic_type.init(generic_params)?;
        Ok(Some((sym.get_symbol_id(), Some(generic_type))))
    }

    pub(crate) fn infer_or_substitute(
        &self,
        generic_type_opt: &Option<GenericType>,
        target_ty: SemanticType,
        expr_ty: Option<SemanticType>,
    ) -> Option<SemanticType> {
        generic_type_opt.clone().and_then(|generic_type| {
            let generic_param = target_ty.as_generic_param()?;
            let expr_ty = expr_ty?;
            
            let mut mapping_ctx = generic_type.mapping_ctx.borrow_mut();
            if let Some(previously_inferred_ty) = mapping_ctx.get_with_symbol_id(generic_param.symbol_id) {
                drop(mapping_ctx);
                Some(previously_inferred_ty)
            } else {
                mapping_ctx.insert_named(generic_param.clone(), expr_ty.clone());
                drop(mapping_ctx);
                Some(expr_ty)
            }
        })
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
}
