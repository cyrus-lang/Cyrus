// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, typecheck::type_cache::TypeCacheEnterResult};
use cyrusc_const_eval::{fold::ConstFolder, resolver::ConstResolver};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::{
    GenericParamID,
    builtins::TypedBuiltin,
    decls::{DeclID, EnumDeclID, FuncDecl, StructDeclID, UnionDeclID},
    exprs::TypedSelfType,
    stmts::{
        TypedEnumVariant, TypedFuncParamKind, TypedFuncParams, TypedFuncTypeParams, TypedFuncTypeVariadicParam,
        TypedFuncVariadicParam, TypedTypeArg, TypedTypeArgs,
    },
    types::{
        NamedType, SemaType, TypeDeclID, TypedArrayCapacity, TypedArrayType, TypedFuncType, TypedTupleType,
        UnresolvedType,
    },
};

impl<'a> AnalysisContext<'a> {
    /// Fully normalize AND validate a type
    /// This is what we call when an explicit type is used
    pub fn normalize_and_check_type_formation(&mut self, ty: SemaType, loc: Loc, indirection: u8) -> Option<SemaType> {
        let ty = self.normalize_sema_type(ty, loc, indirection)?;

        self.check_type_formation(ty, loc)
    }

    // Fully normalize a type: remove UnresolvedSymbol, expand typedefs,
    // and recursively normalize children. Never returns UnresolvedSymbol.
    pub fn normalize_sema_type(&mut self, ty: SemaType, loc: Loc, indirection: u8) -> Option<SemaType> {
        match ty {
            SemaType::Placeholder => Some(ty),
            SemaType::InferVar(_) => Some(ty),
            SemaType::Unresolved(unresolved_type) => self.normalize_unresolved_type(unresolved_type, loc, indirection),
            SemaType::Named(ref named_type) => self.normalize_named_type(named_type, indirection),
            SemaType::GenericParam(generic_param_id) => self.normalize_generic_param(generic_param_id),
            SemaType::Pointer(inner) => self.normalize_pointer(*inner, loc, indirection),
            SemaType::Const(inner) => self.normalize_const(*inner, loc, indirection),
            SemaType::Array(arr) => self.normalize_array(arr, loc, indirection),
            SemaType::FuncType(func_type) => self.normalize_func_type(func_type, indirection),
            SemaType::Tuple(tuple_type) => self.normalize_tuple(tuple_type, indirection),
            SemaType::SelfType(self_type) => self.normalize_self_type(self_type),
            SemaType::InterfaceObject(_) => Some(ty),
            SemaType::Plain(_) => Some(ty),

            SemaType::Err(_) => Some(ty),
        }
    }

    fn normalize_named_type(&mut self, named_type: &NamedType, indirection: u8) -> Option<SemaType> {
        match named_type.type_decl_id {
            TypeDeclID::Enum(enum_decl_id) => {
                self.normalize_enum_decl(enum_decl_id, &named_type.type_args, indirection)
            }
            TypeDeclID::Struct(struct_decl_id) => {
                self.normalize_struct_decl(struct_decl_id, &named_type.type_args, indirection)
            }
            TypeDeclID::Union(union_decl_id) => {
                self.normalize_union_decl(union_decl_id, &named_type.type_args, indirection)
            }

            TypeDeclID::Interface(_) | TypeDeclID::Typedef(_) => Some(SemaType::Named(named_type.clone())),
        }
    }

    fn normalize_struct_decl(
        &mut self,
        struct_decl_id: StructDeclID,
        type_args: &TypedTypeArgs,
        indirection: u8,
    ) -> Option<SemaType> {
        let mut struct_decl = self.decl_tables.struct_decl(struct_decl_id);

        let struct_type = SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: type_args.clone(),
        });

        if struct_decl.is_normalized {
            return Some(struct_type);
        }

        self.decl_tables.with_struct_decl_mut(struct_decl_id, |_struct_decl| {
            _struct_decl.is_normalized = true;
        });

        for field in &mut struct_decl.fields {
            field.ty = match self.normalize_sema_type(field.ty.clone(), field.loc, indirection) {
                Some(ty) => ty,
                None => continue,
            };
        }

        self.decl_tables.with_struct_decl_mut(struct_decl_id, |_struct_decl| {
            _struct_decl.fields = struct_decl.fields;
        });

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Struct(struct_decl_id),
            type_args: type_args.clone(),
        }))
    }

    fn normalize_union_decl(
        &mut self,
        union_decl_id: UnionDeclID,
        type_args: &TypedTypeArgs,
        indirection: u8,
    ) -> Option<SemaType> {
        let mut union_decl = self.decl_tables.union_decl(union_decl_id);

        if union_decl.is_normalized {
            return Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Union(union_decl_id),
                type_args: type_args.clone(),
            }));
        }

        self.decl_tables.with_union_decl_mut(union_decl_id, |_union_decl| {
            _union_decl.is_normalized = true;
        });

        for field in &mut union_decl.fields {
            field.ty = match self.normalize_sema_type(field.ty.clone(), field.loc, indirection) {
                Some(ty) => ty,
                None => continue,
            };
        }

        self.decl_tables.with_union_decl_mut(union_decl_id, |_union_decl| {
            _union_decl.fields = union_decl.fields;
        });

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Union(union_decl_id),
            type_args: type_args.clone(),
        }))
    }

    fn normalize_enum_decl(
        &mut self,
        enum_decl_id: EnumDeclID,
        type_args: &TypedTypeArgs,
        _indirection: u8,
    ) -> Option<SemaType> {
        let mut enum_decl = self.decl_tables.enum_decl(enum_decl_id);

        if enum_decl.is_normalized {
            return Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Enum(enum_decl_id),
                type_args: type_args.clone(),
            }));
        }

        self.decl_tables.with_enum_decl_mut(enum_decl_id, |_enum_decl| {
            _enum_decl.is_normalized = true;
        });

        for variant in &mut enum_decl.variants {
            if let TypedEnumVariant::Valued { value, .. } = variant {
                self.analyze_expr(value, enum_decl.tag_type.clone());
            }
        }

        self.decl_tables.with_enum_decl_mut(enum_decl_id, |_enum_decl| {
            _enum_decl.variants = enum_decl.variants;
        });

        Some(SemaType::Named(NamedType {
            type_decl_id: TypeDeclID::Enum(enum_decl_id),
            type_args: type_args.clone(),
        }))
    }

    fn normalize_unresolved_type(
        &mut self,
        unresolved_type: UnresolvedType,
        loc: Loc,
        indirection: u8,
    ) -> Option<SemaType> {
        let ty = match unresolved_type {
            UnresolvedType::Decl(symbol_id) => self
                .lookup_symbol_as_decl_id(symbol_id)
                .and_then(|decl_id| self.resolve_symbol_type(decl_id, loc, indirection))?,

            UnresolvedType::GenericInst {
                base_symbol_id,
                mut type_args,
            } => {
                let base_type =
                    self.normalize_unresolved_type(UnresolvedType::Decl(base_symbol_id), loc, indirection)?;

                if let Some(named_type) = base_type.as_named_type() {
                    self.normalize_type_args(&mut type_args, indirection);

                    SemaType::Named(NamedType {
                        type_decl_id: named_type.type_decl_id,
                        type_args,
                    })
                } else {
                    return None;
                }
            }
            UnresolvedType::BuiltinFunc(mut builtin_func) => {
                self.analyze_builtin_expr(&mut builtin_func);

                let folder = ConstFolder::new(self, &self.decl_tables, self.target, self);

                if let Ok(const_value) =
                    folder.fold_builtin_func(TypedBuiltin::BuiltinFunc(*builtin_func.clone()), self)
                {
                    if let Some(ty) = const_value.as_type() {
                        return Some(self.expand_sema_type(ty.clone(), builtin_func.loc));
                    }
                }

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::BuiltinDoesNotProduceType {
                        name: builtin_func.name.to_string(),
                    }),
                    loc: Some(builtin_func.loc),
                    hint: None,
                });
                return Some(SemaType::Err(builtin_func.loc));
            }
        };

        Some(self.expand_sema_type(ty, loc))
    }

    fn normalize_func_type(&mut self, mut func_type: TypedFuncType, indirection: u8) -> Option<SemaType> {
        let params_len = func_type.params.list.len();
        let params: Vec<_> = func_type
            .params
            .list
            .into_iter()
            .filter_map(|param| self.normalize_sema_type(param, func_type.loc, indirection))
            .collect();

        if params.len() != params_len {
            return None;
        }

        func_type.params.list = params;

        // normalize variadic parameter if present
        if let Some(variadic) = func_type.params.variadic.take() {
            match *variadic {
                TypedFuncTypeVariadicParam::UntypedCStyle => {
                    func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParam::UntypedCStyle));
                }
                TypedFuncTypeVariadicParam::Typed(sema_type) => {
                    if let Some(normalized) = self.normalize_sema_type(sema_type, func_type.loc, indirection) {
                        func_type.params.variadic = Some(Box::new(TypedFuncTypeVariadicParam::Typed(normalized)));
                    } else {
                        return None;
                    }
                }
            }
        }

        let normalized_ret = self.normalize_sema_type(*func_type.ret_type, func_type.loc, indirection)?;
        func_type.ret_type = Box::new(normalized_ret);

        Some(SemaType::FuncType(func_type))
    }

    fn normalize_func_decl_as_func_type(&mut self, func_decl: &FuncDecl, indirection: u8) -> Option<SemaType> {
        let func_type = func_decl.as_func_type();

        self.normalize_func_type(func_type, indirection)
    }

    fn normalize_tuple(&mut self, tuple_type: TypedTupleType, indirection: u8) -> Option<SemaType> {
        let elements_len = tuple_type.elements.len();

        let elements: Vec<_> = tuple_type
            .elements
            .into_iter()
            .filter_map(|ty| self.normalize_sema_type(ty, tuple_type.loc, indirection))
            .collect();

        // if we lost any types, return None
        if elements.len() != elements_len {
            return None;
        }

        Some(SemaType::Tuple(TypedTupleType {
            elements,
            loc: tuple_type.loc,
        }))
    }

    // TODO: Implement slices.
    fn normalize_array_capacity(
        &mut self,
        mut array: TypedArrayType,
        loc: Loc,
        indirection: u8,
    ) -> Option<TypedArrayType> {
        match &mut array.capacity {
            TypedArrayCapacity::Fixed(expr) => {
                self.analyze_expr(expr, None);

                if expr.literal_const_int_value().is_none() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ArrayCapacityNotConst),
                        loc: Some(loc),
                        hint: None,
                    });
                    return None;
                }
            }
            TypedArrayCapacity::Dynamic => {
                self.reporter.report(Diag {
                    level: DiagLevel::Unimplemented,
                    kind: Box::new(AnalyzerDiagKind::UnimplementedFeatureSlice),
                    loc: Some(loc),
                    hint: None,
                });
                return None;
            }
        }

        array.element_type = Box::new(self.normalize_sema_type(*array.element_type, loc, indirection)?);

        Some(array)
    }

    fn normalize_array(&mut self, array: TypedArrayType, loc: Loc, indirection: u8) -> Option<SemaType> {
        let array_type = self.normalize_array_capacity(array, loc, indirection)?;
        Some(SemaType::Array(array_type))
    }

    #[inline]
    fn normalize_const(&mut self, inner: SemaType, loc: Loc, indirection: u8) -> Option<SemaType> {
        Some(SemaType::Const(Box::new(self.normalize_sema_type(
            inner,
            loc,
            indirection,
        )?)))
    }

    fn normalize_pointer(&mut self, inner: SemaType, loc: Loc, indirection: u8) -> Option<SemaType> {
        let ty = self.normalize_sema_type(inner, loc, indirection + 1)?;

        Some(SemaType::Pointer(Box::new(ty)))
    }

    fn normalize_generic_param(&self, generic_param_id: GenericParamID) -> Option<SemaType> {
        if let Some(ty) = self.lookup_generic_binding(generic_param_id) {
            if let Some(infer) = &self.func_env.infer {
                return Some(infer.resolve(ty));
            }
        }

        // fallback
        Some(SemaType::GenericParam(generic_param_id))
    }

    fn normalize_self_type(&mut self, self_type: TypedSelfType) -> Option<SemaType> {
        self.func_env
            .current_object
            .clone()
            .or(Some(SemaType::SelfType(self_type)))
    }

    pub(crate) fn normalize_type_args(&mut self, type_args: &mut TypedTypeArgs, indirection: u8) {
        for type_arg in type_args.iter_mut() {
            match type_arg {
                TypedTypeArg::Type(ty, loc) => {
                    *ty = match self.normalize_and_check_type_formation(ty.clone(), *loc, indirection) {
                        Some(sema_type) => sema_type,
                        None => continue,
                    };
                }
                TypedTypeArg::Infer => { /* skip */ }
            }
        }
    }

    pub(crate) fn normalize_func_params(&mut self, params: &mut TypedFuncParams, validate: bool, indirection: u8) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    let normalized_type = self
                        .normalize_sema_type(typed_func_param.ty.clone(), typed_func_param.loc, indirection)
                        .unwrap();

                    typed_func_param.ty = normalized_type.clone();

                    if validate {
                        self.validate_param_type(&typed_func_param.ty, typed_func_param.loc);
                    }
                }
                TypedFuncParamKind::SelfModifier(self_modifier) => {
                    let normalized_type = self
                        .normalize_sema_type(self_modifier.ty.clone(), self_modifier.loc, indirection)
                        .unwrap();

                    if validate {
                        self.validate_param_type(&normalized_type, self_modifier.loc);
                    }
                }
            }
        }

        if let Some(variadic_params) = &mut params.variadic {
            if let TypedFuncVariadicParam::Typed { var_decl_id, ty, loc } = variadic_params {
                let ty = match self.normalize_sema_type(ty.clone(), *loc, indirection) {
                    Some(sema_type) => sema_type,
                    None => return,
                };

                if validate {
                    self.validate_param_type(&ty, *loc);
                }

                *variadic_params = TypedFuncVariadicParam::Typed {
                    var_decl_id: *var_decl_id,
                    ty,
                    loc: *loc,
                }
            }
        }
    }

    pub(crate) fn normalize_func_type_params(&mut self, params: &mut TypedFuncTypeParams, loc: Loc, indirection: u8) {
        // analyze static arguments
        for param in params.list.iter_mut() {
            let sema_type = self.normalize_sema_type(param.clone(), loc, indirection).unwrap();
            *param = sema_type.clone();

            self.validate_param_type(&sema_type, loc);
        }

        if let Some(variadic_params) = &mut params.variadic {
            match *variadic_params.clone() {
                TypedFuncTypeVariadicParam::UntypedCStyle => {}
                TypedFuncTypeVariadicParam::Typed(sema_type) => {
                    let sema_type = match self.normalize_sema_type(sema_type.clone(), loc, indirection) {
                        Some(sema_type) => sema_type,
                        None => return,
                    };

                    self.validate_param_type(&sema_type, loc);
                    *variadic_params = Box::new(TypedFuncTypeVariadicParam::Typed(sema_type));
                }
            }
        }
    }

    pub(crate) fn resolve_symbol_type(&mut self, decl_id: DeclID, loc: Loc, indirection: u8) -> Option<SemaType> {
        if let Some(ty) = self.type_cache.get(decl_id) {
            return Some(ty.clone());
        }

        match self.type_cache.enter(decl_id, indirection) {
            TypeCacheEnterResult::Entered => {}

            TypeCacheEnterResult::AlreadyResolving(frame) => {
                // any indirection along the recursive edge makes it legal
                if frame.indirection > 0 || indirection > 0 {
                    return Some(SemaType::Named(NamedType {
                        type_decl_id: decl_id.as_type_decl_id().unwrap(),
                        type_args: TypedTypeArgs::new(),
                    }));
                }

                let symbol_name = self.formatter.format_decl(decl_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::CyclicTypeDefinition { symbol_name }),
                    loc: Some(loc),
                    hint: None,
                });
                return None;
            }
        }

        let mut ty = self.resolve_symbol_type_internal(decl_id, indirection)?;

        ty = self.normalize_sema_type(ty, loc, indirection)?;

        self.type_cache.leave(decl_id, ty.clone());

        Some(ty)
    }

    fn resolve_symbol_type_internal(&mut self, decl_id: DeclID, indirection: u8) -> Option<SemaType> {
        match decl_id {
            DeclID::GlobalVar(global_var_decl_id) => {
                let global_var_decl = self.decl_tables.global_var_decl(global_var_decl_id);

                global_var_decl.ty.clone()
            }
            DeclID::Var(var_decl_id) => {
                let var_decl = self.decl_tables.var_decl(var_decl_id);

                var_decl.ty.clone()
            }
            DeclID::Func(func_decl_id) => {
                let func_decl = self.decl_tables.func_decl(func_decl_id);

                self.normalize_func_decl_as_func_type(&func_decl, indirection)
            }
            DeclID::Struct(strut_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Struct(strut_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            DeclID::Enum(enum_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Enum(enum_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            DeclID::Union(union_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Union(union_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            DeclID::Typedef(typedef_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Typedef(typedef_decl_id),
                type_args: TypedTypeArgs::new(),
            })),
            DeclID::Interface(interface_decl_id) => Some(SemaType::Named(NamedType {
                type_decl_id: TypeDeclID::Interface(interface_decl_id),
                type_args: TypedTypeArgs::new(),
            })),

            DeclID::Method(_) => unreachable!(),
        }
    }
}
