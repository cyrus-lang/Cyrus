use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc};
use cyrusc_resolver::symbols::LocalScopeRef;
use cyrusc_tast::{
    ScopeID, SymbolID,
    format::format_sema_ty,
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
        parent_mapping_ctx: Option<Rc<GenericMappingCtx>>,
        is_const: bool,
        loc: SourceLoc,
    ) -> Result<Option<(SymbolID, Option<GenericType>)>, Diag> {
        let type_args = type_args.clone().unwrap_or(Vec::new());

        let sym = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

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
                .and_then(|parent| Some(GenericMappingCtx::new_child(parent)))
                .unwrap_or(GenericMappingCtx::new_root()),
        ));

        let mut generic_type = GenericType::new_unresolved(sym.get_symbol_id(), type_args, mapping_ctx, is_const, loc);

        generic_type.init(generic_params)?;
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
        generic_type_opt.clone().and_then(|generic_type| {
            let generic_param = target_ty.as_generic_param()?;
            let expr_ty = expr_ty?;

            let mapping_ctx_rc = &generic_type.mapping_ctx;
            let mut mapping_ctx = mapping_ctx_rc.borrow_mut();

            // if the parent already inferred this generic param, return its value and do not overwrite locally.
            if mapping_ctx.parent.is_some() {
                if let Some(parent_sema_ty) = mapping_ctx
                    .parent
                    .as_ref()
                    .unwrap()
                    .get_with_symbol_id(generic_param.symbol_id)
                {
                    // check type mismatch with sema_ty and expr_ty
                    if !self.check_type_mismatch(scope_id_opt, expr_ty.clone(), parent_sema_ty.clone(), loc.clone()) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                                lhs_type: format_sema_ty(
                                    parent_sema_ty.clone(),
                                    &(self.symbol_formatter)(scope_id_opt),
                                ),
                                rhs_type: format_sema_ty(expr_ty, &(self.symbol_formatter)(scope_id_opt)),
                            }),
                            location: Some(DiagLoc::new(loc)),
                            hint: None,
                        });
                        drop(mapping_ctx);
                        return None;
                    }

                    drop(mapping_ctx);
                    return Some(parent_sema_ty);
                }
            }

            if let Some(existing) = mapping_ctx.get_local_with_symbol_id(generic_param.symbol_id) {
                drop(mapping_ctx);
                return Some(existing);
            }

            mapping_ctx.insert_named(generic_param.clone(), expr_ty.clone());
            drop(mapping_ctx);

            Some(expr_ty)
        })
    }
}
