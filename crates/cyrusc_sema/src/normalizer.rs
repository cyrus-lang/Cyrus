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
use cyrusc_ast::SelfModifierKind;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, source_loc::SourceLoc};
use cyrusc_resolver::{
    symbols::{LocalOrGlobalSymbol, LocalScopeRef, LocalSymbolKind, ResolvedTypedef, SymbolEntryKind},
    update_global_symbol, update_local_symbol,
};
use cyrusc_tast::{
    ScopeID, SymbolID,
    exprs::TypedSelfType,
    format::{format_unnamed_enum_ty, format_unnamed_struct_ty, format_unnamed_union_ty},
    generics::{
        generic_type::GenericType,
        mapping_ctx::GenericMappingCtx,
        substitute::{substitute_func_sig, substitute_type},
    },
    sigs::{FuncSig, InterfaceSig, typed_func_decl_as_func_sig},
    stmts::{
        TypedFuncParamKind, TypedFuncParams, TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedFuncVariadicParams,
        TypedGenericParam, TypedTypeArg, TypedTypeArgs,
    },
    types::{
        InterfaceType, ResolvedSymbol, SemanticType, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType,
        TypedFuncType, TypedTupleType, TypedUnnamedEnumType, TypedUnnamedEnumVariant, TypedUnnamedStructType,
        TypedUnnamedUnionType,
    },
};
use fx_hash::FxHashMap;
use smallvec::SmallVec;
use std::{cell::RefCell, rc::Rc};

#[derive(Default)]
pub struct TypeResolutionCache {
    // Canonical, fully normalized result for a symbol (no UnresolvedSymbol, no Typedef)
    pub cache: FxHashMap<SymbolID, SemanticType>,
    // Guard against cycles
    pub in_progress: SmallVec<[SymbolID; 16]>,
}

impl TypeResolutionCache {
    pub fn push(&mut self, sym: SymbolID) -> Result<(), ()> {
        if self.in_progress.contains(&sym) {
            return Err(());
        }
        self.in_progress.push(sym);
        Ok(())
    }

    pub fn pop(&mut self, sym: SymbolID) {
        debug_assert!(self.in_progress.last() == Some(&sym));
        self.in_progress.pop();
    }
}

impl<'a> AnalysisContext<'a> {
    /// Validate a semantic type.
    /// This does NOT normalize the type.
    pub fn check_sema_ty(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        mut sema_ty: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        if sema_ty.count_const_layers() > 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
        }

        if sema_ty.is_self_type() && self.ty_ctx.current_self.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::SelfTypeOutsideOfAnObject),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
        }

        if let Some(generic_type) = sema_ty.as_generic_type_mut() {
            generic_type
                .init(self.mapping_ctx_arena.clone(), &(self.symbol_formatter)(scope_id_opt))
                .ok();
        }

        if let Some(unnamed_enum_type) = sema_ty.as_unnamed_enum_mut() {
            self.check_unnamed_enum_type(scope_id_opt, unnamed_enum_type);
        }

        if let Some(unnamed_struct_type) = sema_ty.as_unnamed_struct_mut() {
            self.check_unnamed_struct_type(scope_id_opt, unnamed_struct_type);
        }

        if let Some(unnamed_union_type) = sema_ty.as_unnamed_union_mut() {
            self.check_unnamed_union_type(scope_id_opt, unnamed_union_type);
        }

        self.check_sema_ty_for_missing_type_args(scope_id_opt, &sema_ty, loc.clone());
        Some(sema_ty)
    }

    fn check_unnamed_union_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_union_type: &mut TypedUnnamedUnionType,
    ) {
        self.validate_union_repr_attr(
            &unnamed_union_type.repr_attr,
            unnamed_union_type.fields.len(),
            &unnamed_union_type.loc,
        );
        self.validate_align(&unnamed_union_type.align, &unnamed_union_type.loc);

        let union_name = format_unnamed_union_ty(unnamed_union_type, &(self.symbol_formatter)(scope_id_opt));

        let mut field_names: Vec<String> = Vec::new();

        for field in &mut unnamed_union_type.fields {
            field.ty = match self.normalize_sema_type(scope_id_opt, *field.ty.clone(), field.loc.clone()) {
                Some(sema_ty) => Box::new(sema_ty),
                None => continue,
            };

            self.validate_field_type(scope_id_opt, 0, &field.ty, field.loc.clone());

            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: union_name.clone(),
                        field_name: field.name.clone(),
                    }),
                    location: Some(DiagLoc::new(field.loc.clone())),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field_names.push(field.name.clone());
        }
    }

    fn check_unnamed_struct_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_type: &mut TypedUnnamedStructType,
    ) {
        self.validate_struct_repr_attr(
            &unnamed_struct_type.repr_attr,
            unnamed_struct_type.fields.len(),
            &unnamed_struct_type.loc,
        );
        self.validate_align(&unnamed_struct_type.align, &unnamed_struct_type.loc);

        let struct_name = format_unnamed_struct_ty(unnamed_struct_type, &(self.symbol_formatter)(scope_id_opt));

        let mut field_names: Vec<String> = Vec::new();

        for field in &mut unnamed_struct_type.fields {
            field.ty = match self.normalize_sema_type(scope_id_opt, *field.ty.clone(), field.loc.clone()) {
                Some(sema_ty) => Box::new(sema_ty),
                None => continue,
            };

            self.validate_field_type(scope_id_opt, 0, &field.ty, field.loc.clone());

            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: struct_name.clone(),
                        field_name: field.name.clone(),
                    }),
                    location: Some(DiagLoc::new(field.loc.clone())),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field_names.push(field.name.clone());
        }
    }

    fn check_unnamed_enum_type(&mut self, scope_id_opt: Option<ScopeID>, unnamed_enum_type: &mut TypedUnnamedEnumType) {
        self.validate_enum_repr_attr(
            &unnamed_enum_type.repr_attr,
            unnamed_enum_type.align.is_some(),
            &unnamed_enum_type.loc,
        );
        self.validate_align(&unnamed_enum_type.align, &unnamed_enum_type.loc);
        self.validate_enum_tag_type(
            scope_id_opt,
            &unnamed_enum_type.tag_type.clone().map(|sema_ty| *sema_ty),
            &unnamed_enum_type.loc,
        );

        let is_repr_c = unnamed_enum_type.is_repr_c();
        let mut variant_names: Vec<String> = Vec::new();

        let enum_name = format_unnamed_enum_ty(unnamed_enum_type, &(self.symbol_formatter)(scope_id_opt));

        for variant in &mut unnamed_enum_type.variants {
            let variant_ident = match variant {
                TypedUnnamedEnumVariant::Ident(ident) => ident,
                TypedUnnamedEnumVariant::Valued(ident, typed_expr) => {
                    typed_expr.sema_ty = match self.analyze_expr(scope_id_opt, typed_expr, None) {
                        Some(sema_ty) => Some(sema_ty),
                        None => continue,
                    };

                    if is_repr_c && !typed_expr.sema_ty.as_ref().unwrap().is_integer() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                ident.loc.clone(),
                                unnamed_enum_type.loc.file_path.clone(),
                            ))),
                            hint: None,
                        });
                        continue;
                    }

                    ident
                }
                TypedUnnamedEnumVariant::Variant(ident, typed_enum_valued_fields) => {
                    if is_repr_c {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                ident.loc.clone(),
                                unnamed_enum_type.loc.file_path.clone(),
                            ))),
                            hint: None,
                        });
                        continue;
                    }

                    for field in typed_enum_valued_fields {
                        field.ty = match self.normalize_sema_type(scope_id_opt, field.ty.clone(), field.loc.clone()) {
                            Some(sema_ty) => sema_ty,
                            None => continue,
                        };

                        self.validate_field_type(scope_id_opt, 0, &field.ty, field.loc.clone());
                    }
                    ident
                }
            };

            if variant_names.contains(&variant_ident.value) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateEnumVariantName {
                        enum_name: enum_name.clone(),
                        variant_name: variant_ident.value.clone(),
                    }),
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        variant_ident.loc.clone(),
                        unnamed_enum_type.loc.file_path.clone(),
                    ))),
                    hint: Some("Consider to rename the variant to a different name.".to_string()),
                });
                continue;
            }

            variant_names.push(variant_ident.value.clone());
        }
    }

    /// Fully normalize AND validate a type
    /// This is what we call when an explicit type is used
    pub fn normalize_and_check_sema_ty(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sema_ty: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let sema_ty = self.normalize_sema_type(scope_id_opt, sema_ty, loc.clone())?;
        self.check_sema_ty(scope_id_opt, sema_ty, loc.clone())
    }

    // Fully normalize a type: remove UnresolvedSymbol, expand typedefs,
    // and recursively normalize children. Never returns UnresolvedSymbol.
    pub fn normalize_sema_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        ty: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        match ty {
            SemanticType::GenericParam(generic_param) => self.normalize_generic_param(generic_param),
            SemanticType::GenericType(generic_type) => self.normalize_generic_type(scope_id_opt, generic_type),
            SemanticType::UnresolvedSymbol(symbol_id) => self.normalize_unresolved_symbol(scope_id_opt, symbol_id, loc),
            SemanticType::ResolvedSymbol(resolved) => self.normalize_resolved_symbol(scope_id_opt, resolved, loc),
            SemanticType::Pointer(inner) => self.normalize_pointer(scope_id_opt, *inner, loc),
            SemanticType::Const(inner) => self.normalize_const(scope_id_opt, *inner, loc),
            SemanticType::Array(arr) => self.normalize_array(scope_id_opt, arr, loc),
            SemanticType::FuncType(func_type) => self.normalize_func_type(scope_id_opt, func_type),
            SemanticType::Tuple(tuple_type) => self.normalize_tuple(scope_id_opt, tuple_type),
            SemanticType::SelfType(self_type) => self.normalize_self_type(self_type),
            SemanticType::UnnamedEnum(unnamed_enum_type) => Some(SemanticType::UnnamedEnum(
                self.normalize_unnamed_enum_ty(scope_id_opt, unnamed_enum_type),
            )),
            SemanticType::UnnamedUnion(unnamed_union_type) => {
                self.normalize_unnamed_union_ty(scope_id_opt, unnamed_union_type)
            }
            SemanticType::UnnamedStruct(unnamed_struct_type) => {
                self.normalize_unnamed_struct_ty(scope_id_opt, unnamed_struct_type)
            }
            SemanticType::PlainType(_) | SemanticType::DynamicType(_) | SemanticType::Interface(_) => Some(ty),
        }
    }

    fn normalize_unnamed_union_ty(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        mut unnamed_union_type: TypedUnnamedUnionType,
    ) -> Option<SemanticType> {
        for field in &mut unnamed_union_type.fields {
            field.ty = match self.normalize_sema_type(scope_id_opt, *field.ty.clone(), field.loc.clone()) {
                Some(sema_ty) => Box::new(sema_ty),
                None => continue,
            };
        }
        Some(SemanticType::UnnamedUnion(unnamed_union_type))
    }

    fn normalize_unnamed_struct_ty(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        mut unnamed_struct_type: TypedUnnamedStructType,
    ) -> Option<SemanticType> {
        for field in &mut unnamed_struct_type.fields {
            field.ty = match self.normalize_sema_type(scope_id_opt, *field.ty.clone(), field.loc.clone()) {
                Some(sema_ty) => Box::new(sema_ty),
                None => continue,
            };
        }
        Some(SemanticType::UnnamedStruct(unnamed_struct_type))
    }

    fn normalize_unnamed_enum_ty(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        mut unnamed_enum_type: TypedUnnamedEnumType,
    ) -> TypedUnnamedEnumType {
        for variant in &mut unnamed_enum_type.variants {
            match variant {
                TypedUnnamedEnumVariant::Ident(_) => continue,
                TypedUnnamedEnumVariant::Variant(_, valued_fields) => {
                    for valued_field in valued_fields {
                        match self.normalize_sema_type(scope_id_opt, valued_field.ty.clone(), valued_field.loc.clone())
                        {
                            Some(sema_ty) => valued_field.ty = sema_ty,
                            None => continue,
                        }
                    }
                }
                TypedUnnamedEnumVariant::Valued(_, expr) => {
                    self.analyze_expr(scope_id_opt, expr, None);
                }
            }
        }
        unnamed_enum_type
    }

    fn normalize_func_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        mut func_type: TypedFuncType,
    ) -> Option<SemanticType> {
        let params_len = func_type.params.list.len();
        let params: Vec<_> = func_type
            .params
            .list
            .into_iter()
            .filter_map(|param| self.normalize_sema_type(scope_id_opt, param, func_type.loc.clone()))
            .collect();

        if params.len() != params_len {
            return None;
        }

        func_type.params.list = params;

        // normalize variadic parameter if present
        if let Some(variadic) = func_type.params.variadic.take() {
            match *variadic {
                TypedFuncTypeVariadicParams::UntypedCStyle => {
                    func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle));
                }
                TypedFuncTypeVariadicParams::Typed(sema_ty) => {
                    if let Some(normalized) = self.normalize_sema_type(scope_id_opt, sema_ty, func_type.loc.clone()) {
                        func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParams::Typed(normalized)));
                    } else {
                        return None;
                    }
                }
            }
        }

        let normalized_ret = self.normalize_sema_type(scope_id_opt, *func_type.return_type, func_type.loc.clone())?;
        func_type.return_type = Box::new(normalized_ret);

        Some(SemanticType::FuncType(func_type))
    }

    fn normalize_tuple(&mut self, scope_id_opt: Option<ScopeID>, tuple_type: TypedTupleType) -> Option<SemanticType> {
        let type_list_len = tuple_type.type_list.len();
        let type_list: Vec<_> = tuple_type
            .type_list
            .into_iter()
            .filter_map(|sema_ty| self.normalize_sema_type(scope_id_opt, sema_ty, tuple_type.loc.clone()))
            .collect();

        // if we lost any types, return None
        if type_list.len() != type_list_len {
            return None;
        }

        Some(SemanticType::Tuple(TypedTupleType {
            type_list,
            loc: tuple_type.loc,
        }))
    }

    // NOTE: This logic here is temporary and it's not completed yet!
    // TODO: Implement comptime array capacity evaluation later.
    // TODO: Implement slices.
    fn normalize_array_capacity(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        mut arr: TypedArrayType,
        loc: SourceLoc,
    ) -> Option<TypedArrayType> {
        match &mut arr.capacity {
            TypedArrayCapacity::Fixed(capacity_value) => match capacity_value.clone() {
                TypedArrayFixedCapacityValue::Expr(typed_expr) => {
                    if let Some(value) = self.const_expr_as_raw_integer(scope_id_opt, &typed_expr) {
                        if let Ok(unsigned_integer) = value.try_into() {
                            arr.capacity =
                                TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(unsigned_integer));
                        } else {
                            todo!();
                        }
                    } else {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                            location: Some(DiagLoc::new(typed_expr.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }
                }
                TypedArrayFixedCapacityValue::Value(_) => {}
            },
            TypedArrayCapacity::Dynamic => {
                todo!();
            }
        }

        arr.element_type = Box::new(self.normalize_sema_type(scope_id_opt, *arr.element_type, loc.clone())?);
        Some(arr)
    }

    fn normalize_array(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        arr: TypedArrayType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let normalized_arr = self.normalize_array_capacity(scope_id_opt, arr, loc.clone())?;
        Some(SemanticType::Array(normalized_arr))
    }

    fn normalize_const(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        inner: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let normalized = self.normalize_sema_type(scope_id_opt, inner, loc.clone())?;
        Some(normalized.as_const())
    }

    fn normalize_pointer(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        inner: SemanticType,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let normalized = self.normalize_sema_type(scope_id_opt, inner, loc.clone())?;
        Some(SemanticType::Pointer(Box::new(normalized)))
    }

    fn normalize_global_var(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let symbol_entry = self.resolver.resolve_global_symbol(symbol_id)?;
        let global_var = symbol_entry.as_global_var()?;

        if let Some(ty) = &global_var.global_var_sig.ty {
            self.normalize_sema_type(scope_id_opt, ty.clone(), loc)
        } else {
            None
        }
    }

    fn normalize_variable(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let scope = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id))?;
        let local_symbol = self.resolver.resolve_symbol_from_local_scope(scope, symbol_id)?;
        let variable = local_symbol.as_variable()?;

        // try to get type from annotation
        if let Some(ty) = &variable.typed_variable.ty {
            return self.normalize_sema_type(scope_id_opt, ty.clone(), loc);
        }

        // try to infer from RHS
        if let Some(rhs) = &variable.typed_variable.rhs {
            let rhs_ty =
                self.analyze_expr_non_terminal(scope_id_opt, &mut rhs.clone(), variable.typed_variable.ty.clone())?;
            return self.normalize_sema_type(scope_id_opt, rhs_ty, loc);
        }

        None
    }

    fn normalize_typedef(&mut self, scope_id_opt: Option<ScopeID>, symbol_id: SymbolID) -> Option<SemanticType> {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
        let sym = self.resolver.resolve_local_or_global_symbol(scope_opt, symbol_id)?;

        let resolved_typedef = sym.as_typedef().or_else(|| None)?;

        self.resolve_typedef_inner_type(resolved_typedef)
    }

    fn normalize_resolved_symbol(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        resolved_symbol: ResolvedSymbol,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        match resolved_symbol {
            ResolvedSymbol::Typedef(symbol_id) => self.normalize_typedef(scope_id_opt, symbol_id),
            ResolvedSymbol::Variable(symbol_id) => self.normalize_variable(scope_id_opt, symbol_id, loc),
            ResolvedSymbol::GlobalVar(symbol_id) => self.normalize_global_var(scope_id_opt, symbol_id, loc),
            ResolvedSymbol::Interface(symbol_id) => self.normalize_interface_type(scope_id_opt, symbol_id, loc),
            ResolvedSymbol::Struct(_) | ResolvedSymbol::Enum(_) | ResolvedSymbol::Union(_) => {
                Some(SemanticType::ResolvedSymbol(resolved_symbol))
            }
            ResolvedSymbol::Func(..) | ResolvedSymbol::Method(..) => {
                unreachable!("Function symbols should not appear as types")
            }
        }
    }

    fn normalize_unresolved_symbol(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
        self.resolver.resolve_local_or_global_symbol(scope_opt, symbol_id);

        if !self.check_generic_typedef_missing_args(scope_id_opt, symbol_id, loc.clone()) {
            return None;
        }

        let resolved = self.resolve_symbol_type(scope_id_opt, symbol_id, loc.clone())?;
        self.normalize_sema_type(scope_id_opt, resolved, loc)
    }

    fn normalize_generic_param(&self, generic_param: TypedGenericParam) -> Option<SemanticType> {
        // try to resolve from current object operand context
        if let Some(sema_ty) = &self.ty_ctx.current_obj_operand_ty {
            if let Some(generic_type) = sema_ty.as_generic_type() {
                let mapping_ctx = generic_type.mapping_ctx.borrow();
                if let Some(sema_ty) =
                    mapping_ctx.resolve_with_name(self.mapping_ctx_arena.clone(), &generic_param.param_name.name)
                {
                    return Some(sema_ty);
                }
            }
        }
        Some(SemanticType::GenericParam(generic_param))
    }

    fn normalize_self_type(&mut self, self_type: TypedSelfType) -> Option<SemanticType> {
        self.ty_ctx
            .current_self
            .clone()
            .or(Some(SemanticType::SelfType(self_type)))
    }

    fn normalize_generic_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        mut generic_type: GenericType,
    ) -> Option<SemanticType> {
        let scope_opt = scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt.clone(), generic_type.base)?;

        if generic_type.generic_params.list.is_empty() {
            if let Some(symbol_generic_params) = sym.symbol_generic_params() {
                generic_type.generic_params = symbol_generic_params;
            }
        }

        self.normalize_type_args(scope_id_opt, generic_type.type_args.as_mut());

        debug_assert!(generic_type.generic_params.list.is_empty() == false);

        if let Some(typedef) = sym.as_typedef() {
            self.resolve_generic_typedef(
                scope_id_opt,
                scope_opt,
                typedef,
                generic_type.type_args,
                typedef.typedef_sig.loc.clone(),
            )
        } else if let Some(resolved_interface) = sym.as_interface() {
            if let Err(diag) = generic_type.init(self.mapping_ctx_arena.clone(), &(self.symbol_formatter)(scope_id_opt))
            {
                self.reporter.report(diag);
                return None;
            }

            self.normalize_generic_interface_type(&generic_type, &resolved_interface.interface_sig)
        } else {
            if let Err(diag) = generic_type.init(self.mapping_ctx_arena.clone(), &(self.symbol_formatter)(scope_id_opt))
            {
                self.reporter.report(diag);
                return None;
            }

            if generic_type.is_const {
                Some(SemanticType::GenericType(generic_type).as_const())
            } else {
                Some(SemanticType::GenericType(generic_type))
            }
        }
    }

    pub(crate) fn normalize_type_args(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        type_args_opt: Option<&mut TypedTypeArgs>,
    ) {
        let Some(type_args) = type_args_opt else {
            return;
        };

        for type_arg in type_args {
            match type_arg {
                TypedTypeArg::Positional { ty, loc, .. } => {
                    *ty = match self.normalize_and_check_sema_ty(scope_id_opt, ty.clone(), loc.clone()) {
                        Some(sema_ty) => sema_ty,
                        None => continue,
                    };
                }
                TypedTypeArg::Named { ty, loc, .. } => {
                    *ty = match self.normalize_and_check_sema_ty(scope_id_opt, ty.clone(), loc.clone()) {
                        Some(sema_ty) => sema_ty,
                        None => continue,
                    };
                }
            }
        }
    }

    fn normalize_interface_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        interface_symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let scope_opt = scope_id_opt.and_then(|sid| self.resolver.resolve_local_scope(self.module_id, sid));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt, interface_symbol_id)
            .unwrap();

        let resolved_interface = sym.as_interface()?;

        let methods: Vec<FuncSig> = resolved_interface
            .interface_sig
            .methods
            .iter()
            .map(|func_decl| typed_func_decl_as_func_sig(func_decl))
            .collect();

        Some(SemanticType::Interface(InterfaceType {
            symbol_id: resolved_interface.symbol_id,
            methods,
            loc,
        }))
    }

    fn normalize_generic_interface_type(
        &mut self,
        generic_type: &GenericType,
        interface_sig: &InterfaceSig,
    ) -> Option<SemanticType> {
        let mut methods: Vec<FuncSig> = Vec::new();

        for func_decl in &interface_sig.methods {
            let mut func_sig = typed_func_decl_as_func_sig(func_decl);
            func_sig = substitute_func_sig(
                self.mapping_ctx_arena.clone(),
                &func_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            methods.push(func_sig);
        }

        Some(SemanticType::Interface(InterfaceType {
            symbol_id: generic_type.base,
            methods,
            loc: generic_type.loc.clone(),
        }))
    }

    fn resolve_generic_typedef(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        scope_opt: Option<LocalScopeRef>,
        resolved_typedef: &ResolvedTypedef,
        mut type_args: Option<TypedTypeArgs>,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        let typedef_generic_params = resolved_typedef.typedef_sig.generic_params.as_ref().unwrap();

        // typedef is generating an another generic
        if let Some(mut generic_type) = resolved_typedef.typedef_sig.ty.as_generic_type().cloned() {
            let parent_mapping_ctx = Some(Rc::new(generic_type.mapping_ctx.borrow().clone()));

            match self
                .init_generic_type_with_symbol_id(
                    scope_id_opt,
                    scope_opt.clone(),
                    generic_type.base,
                    &mut type_args,
                    parent_mapping_ctx,
                    Some(&typedef_generic_params),
                    false,
                    loc,
                )
                .transpose()
                .unwrap()
            {
                Ok((_, new_generic_type_opt)) => {
                    // update generic params
                    generic_type.generic_params = typedef_generic_params.clone();

                    new_generic_type_opt.map(|generic_type| SemanticType::GenericType(generic_type))
                }
                Err(diag) => {
                    self.reporter.report(diag);
                    None
                }
            }
        } else {
            // typedef itself is generic but it does not generate generic,
            // so simply initialize generic mapping ctx and substitute the type

            let mapping_ctx = Rc::new(RefCell::new(GenericMappingCtx::new_root()));
            let mut generic_type = GenericType::new_unresolved(
                0,
                type_args.clone(),
                mapping_ctx.clone(),
                self.mapping_ctx_arena.clone(),
                typedef_generic_params.clone(),
                false,
                loc,
            );

            if let Err(diag) = generic_type.init(self.mapping_ctx_arena.clone(), &(self.symbol_formatter)(scope_id_opt))
            {
                self.reporter.report(diag);
                return None;
            }

            substitute_type(
                self.mapping_ctx_arena.clone(),
                resolved_typedef.typedef_sig.ty.clone(),
                mapping_ctx,
            )
        }
    }

    fn resolve_typedef_inner_type(&mut self, resolved_typedef: &ResolvedTypedef) -> Option<SemanticType> {
        let inner_ty = resolved_typedef.typedef_sig.ty.clone();

        if let Some(mut generic_type) = inner_ty.as_generic_type().cloned() {
            if let Some(generic_params) = &resolved_typedef.typedef_sig.generic_params {
                generic_type.generic_params = generic_params.clone();
            }

            Some(SemanticType::GenericType(generic_type))
        } else {
            Some(inner_ty)
        }
    }

    pub(crate) fn normalize_func_params(&mut self, params: &mut TypedFuncParams, loc: SourceLoc) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    let normalized_type = self
                        .normalize_sema_type(None, typed_func_param.ty.clone(), typed_func_param.loc.clone())
                        .unwrap();

                    typed_func_param.ty = normalized_type.clone();

                    self.validate_param_type(None, &typed_func_param.ty, typed_func_param.loc.clone());
                }
                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                    let normalized_type = self
                        .normalize_sema_type(
                            None,
                            SemanticType::UnresolvedSymbol(typed_self_modifier.symbol_id.unwrap()),
                            typed_self_modifier.loc.clone(),
                        )
                        .unwrap();

                    match typed_self_modifier.kind {
                        SelfModifierKind::Copied => {
                            typed_self_modifier.ty = Some(normalized_type);
                        }
                        SelfModifierKind::Referenced => {
                            typed_self_modifier.ty = Some(SemanticType::Pointer(Box::new(normalized_type)));
                        }
                    }

                    self.validate_param_type(
                        None,
                        &typed_self_modifier.ty.as_ref().unwrap(),
                        typed_self_modifier.loc.clone(),
                    );
                }
            }
        }

        if let Some(variadic_params) = &mut params.variadic {
            if let TypedFuncVariadicParams::Typed(ident, sema_ty) = variadic_params {
                let sema_ty = match self.normalize_sema_type(None, sema_ty.clone(), loc.clone()) {
                    Some(sema_ty) => sema_ty,
                    None => return,
                };

                self.validate_param_type(None, &sema_ty, ident.loc.clone());
                *variadic_params = TypedFuncVariadicParams::Typed(ident.clone(), sema_ty);
            }
        }
    }

    pub(crate) fn normalize_func_type_params(&mut self, params: &mut TypedFuncTypeParams, loc: SourceLoc) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            let sema_ty = self.normalize_sema_type(None, param.clone(), loc.clone()).unwrap();
            *param = sema_ty.clone();

            self.validate_param_type(None, &sema_ty, loc.clone());
        }

        if let Some(variadic_params) = &mut params.variadic {
            match *variadic_params.clone() {
                TypedFuncTypeVariadicParams::UntypedCStyle => {}
                TypedFuncTypeVariadicParams::Typed(sema_ty) => {
                    let sema_ty = match self.normalize_sema_type(None, sema_ty.clone(), loc.clone()) {
                        Some(sema_ty) => sema_ty,
                        None => return,
                    };

                    self.validate_param_type(None, &sema_ty, loc.clone());
                    *variadic_params = Box::new(TypedFuncTypeVariadicParams::Typed(sema_ty));
                }
            }
        }
    }

    pub(crate) fn resolve_symbol_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        symbol_id: SymbolID,
        loc: SourceLoc,
    ) -> Option<SemanticType> {
        if let Some(cached_sema_ty) = self.ty_caches.cache.get(&symbol_id) {
            return Some(cached_sema_ty.clone());
        }

        if self.ty_caches.push(symbol_id).is_err() {
            let symbol_name = (self.symbol_formatter)(scope_id_opt)(symbol_id);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CyclicTypeDefinition { symbol_name }),
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            return None;
        };

        let scope_opt = scope_id_opt.and_then(|sid| self.resolver.resolve_local_scope(self.module_id, sid));
        let sym = self
            .resolver
            .resolve_local_or_global_symbol(scope_opt, symbol_id)
            .or_else(|| {
                let symbol_entry = self.resolver.lookup_symbol_entry_with_id(symbol_id)?;
                Some(LocalOrGlobalSymbol::GlobalSymbol(symbol_entry))
            });

        let mut sema_ty_opt: Option<SemanticType>;

        if let Some(local_or_global) = sym {
            sema_ty_opt = self.resolve_symbol_type_internal(scope_id_opt, local_or_global);

            if let Some(ty) = sema_ty_opt.clone() {
                sema_ty_opt = self.normalize_sema_type(scope_id_opt, ty, loc.clone());
            }
        } else {
            return None;
        }

        self.ty_caches.pop(symbol_id);

        if let Some(ref final_ty) = sema_ty_opt {
            self.ty_caches.cache.insert(symbol_id, final_ty.clone());
        }

        sema_ty_opt
    }

    fn resolve_symbol_type_internal(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sym: LocalOrGlobalSymbol,
    ) -> Option<SemanticType> {
        match sym {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol.kind {
                LocalSymbolKind::Variable(mut resolved_variable) => {
                    let resolved_var = &mut resolved_variable.typed_variable;

                    if let Some(sema_ty) = &resolved_var.ty {
                        let sema_ty =
                            self.normalize_sema_type(scope_id_opt, sema_ty.clone(), resolved_var.loc.clone())?;

                        return Some(if resolved_var.is_const {
                            sema_ty.as_const()
                        } else {
                            sema_ty
                        });
                    }

                    if resolved_var.analyzed {
                        let sema_ty_opt = resolved_var
                            .ty
                            .clone()
                            .or(resolved_var.rhs.clone().and_then(|expr| expr.sema_ty));

                        return if resolved_var.is_const {
                            sema_ty_opt.and_then(|sema_ty| Some(sema_ty.as_const()))
                        } else {
                            sema_ty_opt
                        };
                    }

                    let variable_expr = resolved_var.rhs.as_mut()?;
                    let sema_ty_opt = self.analyze_expr_non_terminal(scope_id_opt, variable_expr, None);

                    update_local_symbol!(self, scope_id_opt.unwrap(), resolved_var.symbol_id,
                        LocalSymbolKind::Variable(resolved_var_mut) => resolved_var_mut, {
                            resolved_var_mut.typed_variable.analyzed = true;
                            resolved_var_mut.typed_variable.rhs = Some(variable_expr.clone());
                        }
                    );

                    if resolved_var.is_const {
                        sema_ty_opt.and_then(|sema_ty| Some(sema_ty.as_const()))
                    } else {
                        sema_ty_opt
                    }
                }
                LocalSymbolKind::Struct(resolved_struct) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Struct(
                    resolved_struct.symbol_id,
                ))),
                LocalSymbolKind::Enum(resolved_enum) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                    resolved_enum.symbol_id,
                ))),
                LocalSymbolKind::Union(resolved_union) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Union(
                    resolved_union.symbol_id,
                ))),
                LocalSymbolKind::Interface(resolved_interface) => self.normalize_interface_type(
                    scope_id_opt,
                    resolved_interface.symbol_id,
                    resolved_interface.interface_sig.loc.clone(),
                ),
                LocalSymbolKind::Typedef(resolved_typedef) => self.resolve_typedef_inner_type(&resolved_typedef),
            },
            LocalOrGlobalSymbol::GlobalSymbol(entry) => match entry.kind {
                SymbolEntryKind::Method(..) => unreachable!(),
                SymbolEntryKind::Func(resolved_func) => Some(SemanticType::FuncType(TypedFuncType {
                    symbol_id: Some(resolved_func.symbol_id),
                    def_module_id: Some(resolved_func.module_id),
                    params: TypedFuncTypeParams {
                        list: resolved_func
                            .func_sig
                            .params
                            .list
                            .iter()
                            .map(|param| match param {
                                TypedFuncParamKind::FuncParam(typed_func_param) => typed_func_param.ty.clone(),
                                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                                    typed_self_modifier.ty.clone().unwrap()
                                }
                            })
                            .collect(),
                        variadic: match resolved_func.func_sig.params.variadic {
                            Some(variadic) => match variadic {
                                TypedFuncVariadicParams::UntypedCStyle => {
                                    Some(Box::new(TypedFuncTypeVariadicParams::UntypedCStyle))
                                }
                                TypedFuncVariadicParams::Typed(_, sema_ty) => {
                                    Some(Box::new(TypedFuncTypeVariadicParams::Typed(sema_ty)))
                                }
                            },
                            None => None,
                        },
                    },
                    is_public: resolved_func.func_sig.modifiers.vis.is_public(),
                    return_type: Box::new(resolved_func.func_sig.return_type.clone()),
                    loc: resolved_func.func_sig.loc.clone(),
                })),
                SymbolEntryKind::GlobalVar(mut resolved_global_var) => {
                    let global_var_sig = &mut resolved_global_var.global_var_sig;

                    if let Some(sema_ty) = &global_var_sig.ty {
                        let sema_ty =
                            self.normalize_sema_type(scope_id_opt, sema_ty.clone(), global_var_sig.loc.clone())?;

                        return Some(if global_var_sig.is_const {
                            sema_ty.as_const()
                        } else {
                            sema_ty
                        });
                    }

                    if global_var_sig.analyzed {
                        let sema_ty_opt = global_var_sig
                            .ty
                            .clone()
                            .or(global_var_sig.rhs.clone().and_then(|expr| expr.sema_ty));

                        return if global_var_sig.is_const {
                            sema_ty_opt.and_then(|sema_ty| Some(sema_ty.as_const()))
                        } else {
                            sema_ty_opt
                        };
                    }

                    let variable_expr = global_var_sig.rhs.as_mut()?;
                    let sema_ty_opt = self.analyze_expr_non_terminal(scope_id_opt, variable_expr, None);

                    update_global_symbol!(self, global_var_sig.module_id, global_var_sig.symbol_id,
                        SymbolEntryKind::GlobalVar(resolved_global_var_mut) => resolved_global_var_mut, {
                            resolved_global_var_mut.global_var_sig.analyzed = true;
                            resolved_global_var_mut.global_var_sig.rhs = Some(variable_expr.clone());
                        }
                    );

                    if global_var_sig.is_const {
                        sema_ty_opt.and_then(|sema_ty| Some(sema_ty.as_const()))
                    } else {
                        sema_ty_opt
                    }
                }
                SymbolEntryKind::Struct(s) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Struct(s.symbol_id))),
                SymbolEntryKind::Enum(resolved_enum) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                    resolved_enum.symbol_id,
                ))),
                SymbolEntryKind::Union(resolved_union) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Union(
                    resolved_union.symbol_id,
                ))),
                SymbolEntryKind::Interface(resolved_interface) => self.normalize_interface_type(
                    scope_id_opt,
                    resolved_interface.symbol_id,
                    resolved_interface.interface_sig.loc.clone(),
                ),
                SymbolEntryKind::Typedef(resolved_typedef) => self.resolve_typedef_inner_type(&resolved_typedef),
                SymbolEntryKind::ProxiedSymbol(_, symbol_id) => {
                    let scope_opt =
                        scope_id_opt.and_then(|scope_id| self.resolver.resolve_local_scope(self.module_id, scope_id));
                    let sym = self
                        .resolver
                        .resolve_local_or_global_symbol(scope_opt, symbol_id)
                        .unwrap();
                    self.resolve_symbol_type_internal(scope_id_opt, sym)
                }
            },
        }
    }
}
