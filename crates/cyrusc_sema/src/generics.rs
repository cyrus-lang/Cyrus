use crate::analyze::AnalysisContext;
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::Diag;
use cyrusc_resolver::symbols::LocalScopeRef;
use cyrusc_tast::{
    SymbolID,
    generics::{generic_type::GenericType, mapping_ctx::GenericMappingCtx},
    stmts::*,
    types::SemanticType,
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
                if type_args.is_empty() {
                    return Ok(Some((sym.get_symbol_id(), None)));
                } else {
                    panic!("Does not accept type args.");
                }
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

    pub(crate) fn infer_generic_param(
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
}
