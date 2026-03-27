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
use cyrusc_const_eval::value::is_comptime_valid;
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::symbols::{
    symbols::{ResolvedTypedef, SymbolEntry, SymbolEntryKind},
    table::SymbolEntryMut,
};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    SymbolID,
    exprs::TypedSelfType,
    format::{SymbolFormatterFn, format_unnamed_enum_type, format_unnamed_struct_type, format_unnamed_union_type},
    generics::{generic_type::GenericType, substitute::substitute_func_sig},
    sigs::{FuncSig, InterfaceSig, typed_func_decl_as_func_sig},
    stmts::{
        TypedFuncParamKind, TypedFuncParams, TypedFuncTypeParams, TypedFuncTypeVariadicParams, TypedFuncVariadicParams,
        TypedGenericParam, TypedTypeArg, TypedTypeArgs,
    },
    types::{
        InterfaceType, ResolvedSymbol, SemanticType, TypedArrayCapacity, TypedArrayType, TypedFuncType, TypedTupleType,
        TypedUnnamedEnumType, TypedUnnamedEnumVariant, TypedUnnamedStructType, TypedUnnamedUnionType,
    },
};
use fx_hash::FxHashMap;
use smallvec::SmallVec;
use std::rc::Rc;

#[derive(Default)]
pub struct TypeCache {
    // Canonical, fully normalized result for a symbol (no UnresolvedSymbol, no Typedef)
    pub cache: FxHashMap<SymbolID, SemanticType>,
    // Guard against cycles
    pub in_progress: SmallVec<[SymbolID; 16]>,
}

impl TypeCache {
    pub fn push(&mut self, symbol_id: SymbolID) -> Result<(), ()> {
        if self.in_progress.contains(&symbol_id) {
            return Err(());
        }
        self.in_progress.push(symbol_id);
        Ok(())
    }

    pub fn pop(&mut self, symbol_id: SymbolID) {
        debug_assert!(self.in_progress.last() == Some(&symbol_id));
        self.in_progress.pop();
    }
}

impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    /// Validate a semantic type.
    /// This does NOT normalize the type.
    pub fn check_sema_ty(&mut self, mut sema_type: SemanticType, loc: Loc) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        if sema_type.count_const_layers() > 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::RedundantConstQualifier),
                loc: Some(loc),
                hint: None,
            });
        }

        if sema_type.is_self_type() && self.fn_env.current_self_type.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::SelfTypeOutsideOfAnObject),
                loc: Some(loc),
                hint: None,
            });
        }

        if let Some(generic_type) = sema_type.as_generic_type_mut() {
            generic_type.init(self.mapping_ctx_arena.clone(), fmt_symbol).ok();
        }

        if let Some(unnamed_enum_type) = sema_type.as_unnamed_enum_mut() {
            self.check_unnamed_enum_type(unnamed_enum_type);
        }

        if let Some(unnamed_struct_type) = sema_type.as_unnamed_struct_mut() {
            self.check_unnamed_struct_type(unnamed_struct_type);
        }

        if let Some(unnamed_union_type) = sema_type.as_unnamed_union_mut() {
            self.check_unnamed_union_type(unnamed_union_type);
        }

        self.is_sema_type_missing_type_args(&sema_type, loc);
        Some(sema_type)
    }

    fn check_unnamed_union_type(&mut self, unnamed_union_type: &mut TypedUnnamedUnionType) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        self.validate_union_repr_attr(
            &unnamed_union_type.repr_attr,
            unnamed_union_type.fields.len(),
            unnamed_union_type.loc,
        );

        self.validate_align(&unnamed_union_type.align, unnamed_union_type.loc);

        let union_name = format_unnamed_union_type(unnamed_union_type, fmt_symbol);

        let mut field_names: Vec<String> = Vec::new();

        for field in &mut unnamed_union_type.fields {
            field.ty = match self.normalize_sema_type(*field.ty.clone(), field.loc) {
                Some(sema_type) => Box::new(sema_type),
                None => continue,
            };

            self.validate_field_type(None, &field.ty, field.loc);

            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: union_name.clone(),
                        field_name: field.name.clone(),
                    }),
                    loc: Some(field.loc),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field_names.push(field.name.clone());
        }
    }

    fn check_unnamed_struct_type(&mut self, unnamed_struct_type: &mut TypedUnnamedStructType) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        self.validate_struct_repr_attr(
            &unnamed_struct_type.repr_attr,
            unnamed_struct_type.fields.len(),
            unnamed_struct_type.loc,
        );

        self.validate_align(&unnamed_struct_type.align, unnamed_struct_type.loc);

        let struct_name = format_unnamed_struct_type(unnamed_struct_type, fmt_symbol);

        let mut field_names: Vec<String> = Vec::new();

        for field in &mut unnamed_struct_type.fields {
            field.ty = match self.normalize_sema_type(*field.ty.clone(), field.loc) {
                Some(sema_type) => Box::new(sema_type),
                None => continue,
            };

            self.validate_field_type(None, &field.ty, field.loc);

            if field_names.contains(&field.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: struct_name.clone(),
                        field_name: field.name.clone(),
                    }),
                    loc: Some(field.loc),
                    hint: Some("Consider to rename the field to a different name.".to_string()),
                });
                continue;
            }

            field_names.push(field.name.clone());
        }
    }

    fn check_unnamed_enum_type(&mut self, unnamed_enum_type: &mut TypedUnnamedEnumType) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        self.validate_enum_repr_attr(
            &unnamed_enum_type.repr_attr,
            unnamed_enum_type.align.is_some(),
            unnamed_enum_type.loc,
        );

        self.validate_align(&unnamed_enum_type.align, unnamed_enum_type.loc);

        self.validate_enum_tag_type(
            &unnamed_enum_type.tag_type.clone().map(|sema_type| *sema_type),
            unnamed_enum_type.loc,
        );

        let is_repr_c = unnamed_enum_type.is_repr_c();
        let mut variant_names: Vec<String> = Vec::new();

        let enum_name = format_unnamed_enum_type(unnamed_enum_type, fmt_symbol);

        for variant in &mut unnamed_enum_type.variants {
            let variant_ident = match variant {
                TypedUnnamedEnumVariant::Ident(ident) => ident,
                TypedUnnamedEnumVariant::Valued(ident, typed_expr) => {
                    typed_expr.sema_type = match self.analyze_expr(typed_expr, None) {
                        Some(sema_type) => Some(sema_type),
                        None => continue,
                    };

                    if is_repr_c && !typed_expr.sema_type.as_ref().unwrap().is_integer() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ReprCEnumWithNonIntegerVariant),
                            loc: Some(ident.loc),
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
                            loc: Some(ident.loc),
                            hint: None,
                        });
                        continue;
                    }

                    for field in typed_enum_valued_fields {
                        field.ty = match self.normalize_sema_type(field.ty.clone(), field.loc) {
                            Some(sema_type) => sema_type,
                            None => continue,
                        };

                        self.validate_field_type(None, &field.ty, field.loc);
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
                    loc: Some(variant_ident.loc),
                    hint: Some("Consider to rename the variant to a different name.".to_string()),
                });
                continue;
            }

            variant_names.push(variant_ident.value.clone());
        }
    }

    /// Fully normalize AND validate a type
    /// This is what we call when an explicit type is used
    pub fn normalize_and_check_sema_ty(&mut self, sema_type: SemanticType, loc: Loc) -> Option<SemanticType> {
        let sema_type = self.normalize_sema_type(sema_type, loc)?;
        self.check_sema_ty(sema_type, loc)
    }

    // Fully normalize a type: remove UnresolvedSymbol, expand typedefs,
    // and recursively normalize children. Never returns UnresolvedSymbol.
    pub fn normalize_sema_type(&mut self, ty: SemanticType, loc: Loc) -> Option<SemanticType> {
        match ty {
            SemanticType::GenericParam(generic_param) => self.normalize_generic_param(generic_param),
            SemanticType::GenericType(generic_type) => self.normalize_generic_type(generic_type),
            SemanticType::UnresolvedSymbol(symbol_id) => self.normalize_unresolved_symbol(symbol_id, loc),
            SemanticType::ResolvedSymbol(resolved) => self.normalize_resolved_symbol(resolved, loc),
            SemanticType::Pointer(inner) => self.normalize_pointer(*inner, loc),
            SemanticType::Const(inner) => self.normalize_const(*inner, loc),
            SemanticType::Array(arr) => self.normalize_array(arr, loc),
            SemanticType::FuncType(func_type) => self.normalize_func_type(func_type),
            SemanticType::Tuple(tuple_type) => self.normalize_tuple(tuple_type),
            SemanticType::SelfType(self_type) => self.normalize_self_type(self_type),
            SemanticType::UnnamedEnum(unnamed_enum_type) => Some(SemanticType::UnnamedEnum(
                self.normalize_unnamed_enum_ty(unnamed_enum_type),
            )),
            SemanticType::UnnamedUnion(unnamed_union_type) => self.normalize_unnamed_union_type(unnamed_union_type),
            SemanticType::UnnamedStruct(unnamed_struct_type) => self.normalize_unnamed_struct_ty(unnamed_struct_type),
            SemanticType::PlainType(_) | SemanticType::DynamicType(_) | SemanticType::Interface(_) => Some(ty),
        }
    }

    fn normalize_unnamed_union_type(&mut self, mut unnamed_union_type: TypedUnnamedUnionType) -> Option<SemanticType> {
        for field in &mut unnamed_union_type.fields {
            field.ty = match self.normalize_sema_type(*field.ty.clone(), field.loc) {
                Some(sema_type) => Box::new(sema_type),
                None => continue,
            };
        }
        Some(SemanticType::UnnamedUnion(unnamed_union_type))
    }

    fn normalize_unnamed_struct_ty(&mut self, mut unnamed_struct_type: TypedUnnamedStructType) -> Option<SemanticType> {
        for field in &mut unnamed_struct_type.fields {
            field.ty = match self.normalize_sema_type(*field.ty.clone(), field.loc) {
                Some(sema_type) => Box::new(sema_type),
                None => continue,
            };
        }
        Some(SemanticType::UnnamedStruct(unnamed_struct_type))
    }

    fn normalize_unnamed_enum_ty(&mut self, mut unnamed_enum_type: TypedUnnamedEnumType) -> TypedUnnamedEnumType {
        for variant in &mut unnamed_enum_type.variants {
            match variant {
                TypedUnnamedEnumVariant::Ident(_) => continue,
                TypedUnnamedEnumVariant::Variant(_, valued_fields) => {
                    for valued_field in valued_fields {
                        match self.normalize_sema_type(valued_field.ty.clone(), valued_field.loc) {
                            Some(sema_type) => valued_field.ty = sema_type,
                            None => continue,
                        }
                    }
                }
                TypedUnnamedEnumVariant::Valued(_, expr) => {
                    self.analyze_expr(expr, None);
                }
            }
        }
        unnamed_enum_type
    }

    fn normalize_func_type(&mut self, mut func_type: TypedFuncType) -> Option<SemanticType> {
        let params_len = func_type.params.list.len();
        let params: Vec<_> = func_type
            .params
            .list
            .into_iter()
            .filter_map(|param| self.normalize_sema_type(param, func_type.loc))
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
                TypedFuncTypeVariadicParams::Typed(sema_type) => {
                    if let Some(normalized) = self.normalize_sema_type(sema_type, func_type.loc) {
                        func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParams::Typed(normalized)));
                    } else {
                        return None;
                    }
                }
            }
        }

        let normalized_ret = self.normalize_sema_type(*func_type.ret_type, func_type.loc)?;
        func_type.ret_type = Box::new(normalized_ret);

        Some(SemanticType::FuncType(func_type))
    }

    fn normalize_tuple(&mut self, tuple_type: TypedTupleType) -> Option<SemanticType> {
        let type_list_len = tuple_type.elements.len();
        let type_list: Vec<_> = tuple_type
            .elements
            .into_iter()
            .filter_map(|sema_type| self.normalize_sema_type(sema_type, tuple_type.loc))
            .collect();

        // if we lost any types, return None
        if type_list.len() != type_list_len {
            return None;
        }

        Some(SemanticType::Tuple(TypedTupleType {
            elements: type_list,
            loc: tuple_type.loc,
        }))
    }

    // TODO: Implement slices.
    fn normalize_array_capacity(&mut self, mut arr: TypedArrayType, loc: Loc) -> Option<TypedArrayType> {
        match &mut arr.capacity {
            TypedArrayCapacity::Fixed(expr) => {
                self.analyze_expr(expr, None);

                if !is_comptime_valid(&expr.kind) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                        loc: Some(loc),
                        hint: None,
                    });
                    return None;
                }
            }
            TypedArrayCapacity::Dynamic => todo!(),
        }

        arr.element_type = Box::new(self.normalize_sema_type(*arr.element_type, loc)?);
        Some(arr)
    }

    fn normalize_array(&mut self, arr: TypedArrayType, loc: Loc) -> Option<SemanticType> {
        let array_type = self.normalize_array_capacity(arr, loc)?;
        Some(SemanticType::Array(array_type))
    }

    fn normalize_const(&mut self, inner: SemanticType, loc: Loc) -> Option<SemanticType> {
        let ty = self.normalize_sema_type(inner, loc)?;
        Some(ty.as_const())
    }

    fn normalize_pointer(&mut self, inner: SemanticType, loc: Loc) -> Option<SemanticType> {
        let ty = self.normalize_sema_type(inner, loc)?;
        Some(SemanticType::Pointer(Box::new(ty)))
    }

    fn normalize_global_var(&mut self, symbol_id: SymbolID, loc: Loc) -> Option<SemanticType> {
        let resolved_global_var = self.query.get_global_var(symbol_id)?;

        if let Some(ty) = &resolved_global_var.global_var_sig.ty {
            self.normalize_sema_type(ty.clone(), loc)
        } else {
            None
        }
    }

    fn normalize_var(&mut self, symbol_id: SymbolID, loc: Loc) -> Option<SemanticType> {
        let resolved_var = self.query.get_var(symbol_id)?;
        let var_type = &resolved_var.variable.ty;
        let var_rhs = &resolved_var.variable.rhs;

        // try to get type from annotation
        if let Some(ty) = var_type {
            return self.normalize_sema_type(ty.clone(), loc);
        }

        // try to infer from RHS
        if let Some(rhs) = var_rhs {
            let rhs_ty = self.analyze_expr_non_terminal(&mut rhs.clone(), var_type.clone())?;
            return self.normalize_sema_type(rhs_ty, loc);
        }

        None
    }

    fn normalize_typedef(&mut self, symbol_id: SymbolID) -> Option<SemanticType> {
        let resolved_typedef = self.query.get_typedef(symbol_id)?;
        self.resolve_typedef_inner_type(&resolved_typedef)
    }

    fn normalize_resolved_symbol(&mut self, resolved_symbol: ResolvedSymbol, loc: Loc) -> Option<SemanticType> {
        match resolved_symbol {
            ResolvedSymbol::Typedef(symbol_id) => self.normalize_typedef(symbol_id),
            ResolvedSymbol::Variable(symbol_id) => self.normalize_var(symbol_id, loc),
            ResolvedSymbol::GlobalVar(symbol_id) => self.normalize_global_var(symbol_id, loc),
            ResolvedSymbol::Interface(symbol_id) => self.normalize_interface_type(symbol_id, loc),
            ResolvedSymbol::Struct(_) | ResolvedSymbol::Enum(_) | ResolvedSymbol::Union(_) => {
                Some(SemanticType::ResolvedSymbol(resolved_symbol))
            }
            ResolvedSymbol::Func(..) | ResolvedSymbol::Method(..) => {
                unreachable!("Function symbols should not appear as types")
            }
        }
    }

    fn normalize_unresolved_symbol(&mut self, symbol_id: SymbolID, loc: Loc) -> Option<SemanticType> {
        // check generic typedef argument completeness
        if !self.check_generic_typedef_missing_args(symbol_id, loc) {
            return None;
        }

        // resolve the symbol to a semantic_type
        let unresolved_type = self.resolve_symbol_type(symbol_id, loc)?;

        // Normalize the resulting type
        self.normalize_sema_type(unresolved_type, loc)
    }

    fn normalize_generic_param(&self, generic_param: TypedGenericParam) -> Option<SemanticType> {
        // try to resolve from current object operand context
        if let Some(sema_type) = &self.fn_env.current_object_type {
            if let Some(generic_type) = sema_type.as_generic_type() {
                let mapping_ctx = generic_type.mapping_ctx.borrow();

                if let Some(sema_type) =
                    mapping_ctx.resolve_with_name(self.mapping_ctx_arena.clone(), &generic_param.name.value)
                {
                    return Some(sema_type);
                }
            }
        }

        Some(SemanticType::GenericParam(generic_param))
    }

    fn normalize_self_type(&mut self, self_type: TypedSelfType) -> Option<SemanticType> {
        self.fn_env
            .current_self_type
            .clone()
            .or(Some(SemanticType::SelfType(self_type)))
    }

    fn normalize_generic_type(&mut self, mut generic_type: GenericType) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let symbol_entry = self.query.get_symbol_entry(generic_type.base).unwrap();

        if generic_type.generic_params.list.is_empty() {
            if let Some(generic_params) = symbol_entry.symbol_generic_params() {
                generic_type.generic_params = generic_params;
            }
        }

        self.normalize_type_args(generic_type.type_args.as_mut());

        debug_assert!(generic_type.generic_params.list.is_empty() == false);

        if let Some(typedef) = symbol_entry.as_typedef() {
            self.resolve_generic_typedef(typedef, generic_type.type_args, typedef.typedef_sig.loc)
        } else if let Some(resolved_interface) = symbol_entry.as_interface() {
            if let Err(diag) = generic_type.init(self.mapping_ctx_arena.clone(), fmt_symbol) {
                self.reporter.report(diag);
                return None;
            }

            self.normalize_generic_interface_type(&generic_type, &resolved_interface.interface_sig)
        } else {
            if let Err(diag) = generic_type.init(self.mapping_ctx_arena.clone(), fmt_symbol) {
                self.reporter.report(diag);
                return None;
            }

            Some(SemanticType::GenericType(generic_type))
        }
    }

    pub(crate) fn normalize_type_args(&mut self, type_args_opt: Option<&mut TypedTypeArgs>) {
        let Some(type_args) = type_args_opt else {
            return;
        };

        for type_arg in type_args {
            match type_arg {
                TypedTypeArg::Positional { ty, loc, .. } => {
                    *ty = match self.normalize_and_check_sema_ty(ty.clone(), *loc) {
                        Some(sema_type) => sema_type,
                        None => continue,
                    };
                }
                TypedTypeArg::Named { ty, loc, .. } => {
                    *ty = match self.normalize_and_check_sema_ty(ty.clone(), *loc) {
                        Some(sema_type) => sema_type,
                        None => continue,
                    };
                }
            }
        }
    }

    fn normalize_interface_type(&mut self, symbol_id: SymbolID, loc: Loc) -> Option<SemanticType> {
        let resolved_interface = self.query.get_interface(symbol_id)?;

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
            loc: generic_type.loc,
        }))
    }

    fn resolve_generic_typedef(
        &mut self,
        resolved_typedef: &ResolvedTypedef,
        mut type_args: Option<TypedTypeArgs>,
        loc: Loc,
    ) -> Option<SemanticType> {
        // let fmt_symbol = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let generic_params = resolved_typedef.typedef_sig.generic_params.as_ref().unwrap();

        // typedef is generating an another generic
        if let Some(mut generic_type) = resolved_typedef.typedef_sig.ty.as_generic_type().cloned() {
            let parent_mapping_ctx = Some(Rc::new(generic_type.mapping_ctx.borrow().clone()));

            match self
                .init_generic_type_with_symbol_id(
                    generic_type.base,
                    &mut type_args,
                    parent_mapping_ctx,
                    Some(&generic_params),
                    loc,
                )
                .transpose()
                .unwrap()
            {
                Ok((_, new_generic_type_opt)) => {
                    // update generic params
                    generic_type.generic_params = generic_params.clone();

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

            // FIXME
            todo!();
            // let mapping_ctx = Rc::new(RefCell::new(GenericMappingCtx::new_root()));
            // let mut generic_type = GenericType::new_unresolved(
            //     SymbolID::new(), // FIXME: Change to None
            //     type_args.clone(),
            //     mapping_ctx.clone(),
            //     self.mapping_ctx_arena.clone(),
            //     generic_params.clone(),
            //     loc,
            // );

            // if let Err(diag) = generic_type.init(self.mapping_ctx_arena.clone(), fmt_symbol) {
            //     self.reporter.report(diag);
            //     return None;
            // }

            // substitute_type(
            //     self.mapping_ctx_arena.clone(),
            //     resolved_typedef.typedef_sig.ty.clone(),
            //     mapping_ctx,
            // )
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

    pub(crate) fn normalize_func_params(&mut self, params: &mut TypedFuncParams, loc: Loc) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    let normalized_type = self
                        .normalize_sema_type(typed_func_param.ty.clone(), typed_func_param.loc)
                        .unwrap();

                    typed_func_param.ty = normalized_type.clone();

                    self.validate_param_type(&typed_func_param.ty, typed_func_param.loc);
                }
                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                    let normalized_type = self
                        .normalize_sema_type(
                            SemanticType::UnresolvedSymbol(typed_self_modifier.symbol_id.unwrap()),
                            typed_self_modifier.loc,
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

                    self.validate_param_type(&typed_self_modifier.ty.as_ref().unwrap(), typed_self_modifier.loc);
                }
            }
        }

        if let Some(variadic_params) = &mut params.variadic {
            if let TypedFuncVariadicParams::Typed(ident, sema_type) = variadic_params {
                let sema_type = match self.normalize_sema_type(sema_type.clone(), loc) {
                    Some(sema_type) => sema_type,
                    None => return,
                };

                self.validate_param_type(&sema_type, ident.loc);
                *variadic_params = TypedFuncVariadicParams::Typed(ident.clone(), sema_type);
            }
        }
    }

    pub(crate) fn normalize_func_type_params(&mut self, params: &mut TypedFuncTypeParams, loc: Loc) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            let sema_type = self.normalize_sema_type(param.clone(), loc).unwrap();
            *param = sema_type.clone();

            self.validate_param_type(&sema_type, loc);
        }

        if let Some(variadic_params) = &mut params.variadic {
            match *variadic_params.clone() {
                TypedFuncTypeVariadicParams::UntypedCStyle => {}
                TypedFuncTypeVariadicParams::Typed(sema_type) => {
                    let sema_type = match self.normalize_sema_type(sema_type.clone(), loc) {
                        Some(sema_type) => sema_type,
                        None => return,
                    };

                    self.validate_param_type(&sema_type, loc);
                    *variadic_params = Box::new(TypedFuncTypeVariadicParams::Typed(sema_type));
                }
            }
        }
    }

    pub(crate) fn resolve_symbol_type(&mut self, symbol_id: SymbolID, loc: Loc) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        if let Some(cached_sema_ty) = self.type_cache.cache.get(&symbol_id) {
            return Some(cached_sema_ty.clone());
        }

        if self.type_cache.push(symbol_id).is_err() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CyclicTypeDefinition {
                    symbol_name: fmt_symbol(symbol_id),
                }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        };

        let symbol_entry = self.query.get_symbol_entry(symbol_id).unwrap();
        debug_assert!(!matches!(symbol_entry.kind, SymbolEntryKind::Unresolved));

        let mut sema_type_opt = self.resolve_symbol_type_internal(&symbol_entry);

        if let Some(ty) = sema_type_opt.clone() {
            sema_type_opt = self.normalize_sema_type(ty, loc);
        }

        self.type_cache.pop(symbol_id);

        if let Some(ref final_ty) = sema_type_opt {
            self.type_cache.cache.insert(symbol_id, final_ty.clone());
        }

        sema_type_opt
    }

    fn resolve_symbol_type_internal(&mut self, symbol_entry: &SymbolEntry) -> Option<SemanticType> {
        match &symbol_entry.kind {
            SymbolEntryKind::GlobalVar(resolved_global_var) => resolved_global_var.global_var_sig.ty.clone(),
            SymbolEntryKind::Var(resolved_var) => resolved_var.variable.ty.clone(),
            SymbolEntryKind::Func(resolved_func) => {
                let params_list = resolved_func
                    .func_sig
                    .params
                    .list
                    .iter()
                    .map(|param| match param {
                        TypedFuncParamKind::FuncParam(p) => p.ty.clone(),
                        TypedFuncParamKind::SelfModifier(self_mod) => {
                            self_mod.ty.clone().expect("self modifier must have type")
                        }
                    })
                    .collect();

                let params_variadic = resolved_func.func_sig.params.variadic.as_ref().map(|v| match v {
                    TypedFuncVariadicParams::UntypedCStyle => Box::new(TypedFuncTypeVariadicParams::UntypedCStyle),
                    TypedFuncVariadicParams::Typed(_, sema_type) => {
                        Box::new(TypedFuncTypeVariadicParams::Typed(sema_type.clone()))
                    }
                });

                Some(SemanticType::FuncType(TypedFuncType {
                    symbol_id: Some(resolved_func.symbol_id),
                    params: TypedFuncTypeParams {
                        list: params_list,
                        variadic: params_variadic,
                    },
                    is_public: resolved_func.func_sig.modifiers.vis.is_public(),
                    ret_type: Box::new(resolved_func.func_sig.ret_type.clone()),
                    loc: resolved_func.func_sig.loc,
                }))
            }
            SymbolEntryKind::Struct(resolved_struct) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Struct(
                resolved_struct.symbol_id,
            ))),
            SymbolEntryKind::Enum(resolved_enum) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                resolved_enum.symbol_id,
            ))),
            SymbolEntryKind::Union(resolved_union) => Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Union(
                resolved_union.symbol_id,
            ))),
            SymbolEntryKind::Interface(interface) => {
                self.normalize_interface_type(interface.symbol_id, interface.interface_sig.loc)
            }
            SymbolEntryKind::Typedef(typedef) => self.resolve_typedef_inner_type(typedef),
            SymbolEntryKind::ProxiedSymbol {
                symbol_id: target_symbol_id,
                ..
            } => {
                let target_entry = self.query.get_symbol_entry(*target_symbol_id)?;
                self.resolve_symbol_type_internal(&target_entry)
            }
            SymbolEntryKind::Method(..) => unreachable!("method symbols are not type expressions"),
            SymbolEntryKind::ProxiedModule { .. } => {
                unreachable!("proxied module symbol entry kind should not appear here")
            }
            SymbolEntryKind::Module { .. } => {
                unreachable!("module symbol entry kind should not appear here")
            }
            SymbolEntryKind::Namespace { .. } => {
                unreachable!("namespace symbol entry kind should not appear here")
            }
            SymbolEntryKind::Unresolved => unreachable!("unresolved symbol entry should not appear here"),
        }
    }
}
