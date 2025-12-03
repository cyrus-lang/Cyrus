use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc};
use cyrusc_resolver::symbols::LocalScopeRef;
use cyrusc_tast::{
    ScopeID, SymbolID,
    format::format_sema_ty,
    generics::{
        generic_type::GenericType,
        mapping_ctx::{GenericMappingCtx, GenericMappingEntry},
    },
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
                    panic!("Does not accept type args.");
                }
            }
        };

        let mapping_ctx = Rc::new(RefCell::new(
            parent_mapping_ctx
                .as_ref()
                .map(|parent| GenericMappingCtx::new_child(parent.clone()))
                .unwrap_or(GenericMappingCtx::new_root()),
        ));

        let mut generic_type = GenericType::new_unresolved(sym.get_symbol_id(), type_args, mapping_ctx, is_const, loc);

        generic_type.init(generic_params.clone())?;
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
        let Some(generic_type) = generic_type_opt.clone() else {
            return None;
        };

        let generic_param = GenericMappingEntry::from(target_ty.as_generic_param().cloned().unwrap());
        let expr_ty = expr_ty.unwrap();

        let mapping_ctx_rc = &generic_type.mapping_ctx;
        let cloned_ctx = mapping_ctx_rc.borrow().clone();

        // resolve linked parent by name
        let linked_parent_opt = cloned_ctx.get_linked_by_name(&generic_param.name);

        let mut ctx = mapping_ctx_rc.borrow_mut();

        // if a local value exists, use it immediately
        if let Some(local_sema_ty) = ctx.get_with_name(&generic_param.name) {
            if !self.check_type_mismatch(scope_id_opt, expr_ty.clone(), local_sema_ty.clone(), loc.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                        lhs_type: format_sema_ty(local_sema_ty, &(self.symbol_formatter)(scope_id_opt)),
                        rhs_type: format_sema_ty(expr_ty, &(self.symbol_formatter)(scope_id_opt)),
                    }),
                    location: Some(DiagLoc::new(loc)),
                    hint: None,
                });
                return None;
            }

            return Some(local_sema_ty);
        }

        // if any ancestor directly has a concrete value for this symbol id, obey it
        if let Some(parent_val) = ctx.parent.as_ref().and_then(|p| p.get_with_name(&generic_param.name)) {
            if !self.check_type_mismatch(scope_id_opt, expr_ty.clone(), parent_val.clone(), loc.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                        lhs_type: format_sema_ty(parent_val.clone(), &(self.symbol_formatter)(scope_id_opt)),
                        rhs_type: format_sema_ty(expr_ty.clone(), &(self.symbol_formatter)(scope_id_opt)),
                    }),
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
                return None;
            }

            ctx.insert_named(generic_param.clone(), parent_val.clone());
            return Some(parent_val);
        }

        // if a linked parent by name exists, resolve its type
        if let Some(parent_entry) = linked_parent_opt {
            if let Some(resolved_ty) = ctx.get_with_name(&parent_entry.name) {
                if !self.check_type_mismatch(scope_id_opt, expr_ty.clone(), resolved_ty.clone(), loc.clone()) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_ty(resolved_ty.clone(), &(self.symbol_formatter)(scope_id_opt)),
                            rhs_type: format_sema_ty(expr_ty.clone(), &(self.symbol_formatter)(scope_id_opt)),
                        }),
                        location: Some(DiagLoc::new(loc)),
                        hint: None,
                    });
                    return None;
                }

                ctx.insert_named(generic_param.clone(), resolved_ty.clone());
                ctx.insert_linked(generic_param.clone(), parent_entry);

                return Some(resolved_ty);
            }
        }

        // otherwise, just insert the expression type as the generic param value
        ctx.insert_named(generic_param.clone(), expr_ty.clone());
        Some(expr_ty)
    }

    pub(crate) fn merge_generic_operand_as_expected_type(
        &self,
        operand_ty: SemanticType,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        if let Some(generic_type) = operand_ty.as_generic_type() {
            if let Some(expected_sema_ty) = &expected_type {
                if let Some(expected_generic_type) = expected_sema_ty.as_generic_type() {
                    let mut mapping_ctx = generic_type.mapping_ctx.borrow_mut();
                    mapping_ctx.parent = Some(Rc::new(expected_generic_type.mapping_ctx.borrow().clone()));
                }
            }

            Some(SemanticType::GenericType(generic_type.clone()))
        } else {
            None
        }
    }
}
