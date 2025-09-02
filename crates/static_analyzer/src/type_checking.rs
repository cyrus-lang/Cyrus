use std::collections::HashMap;

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, update_global_symbol_type};
use ast::{
    LiteralKind, SelfModifierKind,
    operators::{InfixOperator, PrefixOperator},
    token::{Location, TokenKind},
};
use diagcentral::{Diag, DiagLevel, DiagLoc};
use resolver::{
    declsign::FuncSig,
    scope::{LocalOrGlobalSymbol, LocalScopeRef, ResolvedMethod, SymbolEntryKind},
};
use typed_ast::{
    format::format_concrete_type,
    types::{
        BasicConcreteType::{self, *},
        ConcreteType, ResolvedSymbol, TypedArrayCapacity, TypedUnnamedStructType, TypedUnnamedStructTypeField,
    },
    *,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_type_mismatch(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        value_type: ConcreteType,
        target_type: ConcreteType,
        loc: Location,
    ) -> bool {
        match (
            value_type.get_const_inner().clone(),
            target_type.get_const_inner().clone(),
        ) {
            (ConcreteType::ResolvedSymbol(resolved_symbol1), ConcreteType::ResolvedSymbol(resolved_symbol2)) => {
                resolved_symbol1 == resolved_symbol2
            }
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                self.check_basic_type_mismatch(basic_concrete_type1, basic_concrete_type2)
            }
            (concrete_type1, ConcreteType::Const(inner_concrete_type2)) => {
                self.check_type_mismatch(scope_id_opt, concrete_type1, *inner_concrete_type2, loc)
            }
            (ConcreteType::Array(array_type1), ConcreteType::Array(array_type2)) => {
                let capacity = {
                    match (array_type1.capacity, array_type2.capacity) {
                        (TypedArrayCapacity::Fixed(size1), TypedArrayCapacity::Fixed(size2)) => size1 == size2,
                        (TypedArrayCapacity::Dynamic, TypedArrayCapacity::Dynamic) => true,
                        _ => false,
                    }
                };

                capacity
                    && self.check_type_mismatch(scope_id_opt, *array_type1.element_type, *array_type2.element_type, loc)
            }
            (ConcreteType::Pointer(inner_concrete_type1), ConcreteType::Pointer(inner_concrete_type2)) => {
                (inner_concrete_type1.is_void() || inner_concrete_type2.is_void())
                    || self.check_type_mismatch(scope_id_opt, *inner_concrete_type1, *inner_concrete_type2, loc)
            }
            (ConcreteType::UnnamedStruct(unnamed_struct1), ConcreteType::UnnamedStruct(unnamed_struct2)) => {
                let packed = unnamed_struct1.packed == unnamed_struct2.packed;
                let mut fields = true;
                for (field1, field2) in unnamed_struct1.fields.iter().zip(unnamed_struct2.fields) {
                    if *field1 != field2 {
                        fields = false;
                        break;
                    }
                }
                packed && fields
            }
            (ConcreteType::BasicType(BasicConcreteType::Null), ConcreteType::Pointer(..)) => true,
            _ => false,
        }
    }

    pub(crate) fn check_basic_type_mismatch(&self, value: BasicConcreteType, target: BasicConcreteType) -> bool {
        match (value, target) {
            // Same type is always compatible
            (a, b) if a == b => true,

            // Integer compatibility (widening is allowed)
            (Int8, Int16 | Int32 | Int64 | Int128 | Int) => true,
            (Int16, Int32 | Int64 | Int128 | Int) => true,
            (Int32, Int64 | Int128 | Int) => true,
            (Int64, Int128) => true,
            (Int, Int64 | Int128) => true,

            (UInt8, UInt16 | UInt32 | UInt64 | UInt128 | UInt) => true,
            (UInt16, UInt32 | UInt64 | UInt128 | UInt) => true,
            (UInt32, UInt64 | UInt128 | UInt) => true,
            (UInt64, UInt128) => true,
            (UInt, UInt64 | UInt128) => true,

            // Cross unsigned-to-signed conversions (only if target is wider)
            (UInt8, Int16 | Int32 | Int64 | Int128 | Int) => true,
            (UInt16, Int32 | Int64 | Int128 | Int) => true,
            (UInt32, Int64 | Int128 | Int) => true,
            (UInt64, Int128 | Int) => true,

            // Floating-point widening
            (Float16, Float32 | Float64 | Float128) => true,
            (Float32, Float64 | Float128) => true,
            (Float64, Float128) => true,

            // Pointer int compatibility (if same bit width)
            (UIntPtr, IntPtr) | (IntPtr, UIntPtr) => true,

            // Integer to intptr (safe if value fits)
            (
                BasicConcreteType::Int | BasicConcreteType::Int8 | BasicConcreteType::Int16 | BasicConcreteType::Int32,
                BasicConcreteType::IntPtr,
            ) => true,

            // Unsigned to intptr (less safe, maybe allow some)
            (
                BasicConcreteType::UInt
                | BasicConcreteType::UInt8
                | BasicConcreteType::UInt16
                | BasicConcreteType::UInt32,
                BasicConcreteType::UIntPtr,
            ) => true,

            (Null, Null) => true,

            // Char to Int
            (Char, Int8 | UInt8) => true,

            // Bool to Int
            (Bool, Int8 | UInt8) => true,

            (Bool, Bool) => true,

            _ => false,
        }
    }

    pub(crate) fn is_integer_type(&self, concrete_type: ConcreteType) -> bool {
        fn is_integer_basic(basic: &BasicConcreteType) -> bool {
            matches!(
                basic,
                BasicConcreteType::UIntPtr
                    | BasicConcreteType::IntPtr
                    | BasicConcreteType::SizeT
                    | BasicConcreteType::Int
                    | BasicConcreteType::Int8
                    | BasicConcreteType::Int16
                    | BasicConcreteType::Int32
                    | BasicConcreteType::Int64
                    | BasicConcreteType::Int128
                    | BasicConcreteType::UInt
                    | BasicConcreteType::UInt8
                    | BasicConcreteType::UInt16
                    | BasicConcreteType::UInt32
                    | BasicConcreteType::UInt64
                    | BasicConcreteType::UInt128
            )
        }

        match concrete_type {
            ConcreteType::BasicType(basic) => is_integer_basic(&basic),
            ConcreteType::Const(inner_boxed) => {
                if let ConcreteType::BasicType(basic) = *inner_boxed {
                    is_integer_basic(&basic)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub(crate) fn is_float_type(&self, concrete_type: ConcreteType) -> bool {
        fn is_float_basic(basic: &BasicConcreteType) -> bool {
            matches!(
                basic,
                BasicConcreteType::Float16
                    | BasicConcreteType::Float32
                    | BasicConcreteType::Float64
                    | BasicConcreteType::Float128
            )
        }

        match concrete_type {
            ConcreteType::BasicType(basic) => is_float_basic(&basic),
            ConcreteType::Const(inner_boxed) => {
                if let ConcreteType::BasicType(basic) = *inner_boxed {
                    is_float_basic(&basic)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub(crate) fn analyze_literal_type(
        &mut self,
        typed_literal: &mut TypedLiteral,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        match &typed_literal.kind {
            LiteralKind::Integer(_, suffix_opt) => {
                let inferred = self.infer_integer_type(typed_literal, suffix_opt, expected_type);
                typed_literal.ty = Some(inferred.clone());
                Some(inferred)
            }
            LiteralKind::Float(_, suffix_opt) => {
                let inferred = self.infer_float_type(typed_literal, suffix_opt, expected_type);
                typed_literal.ty = Some(inferred.clone());
                Some(inferred)
            }
            LiteralKind::Bool(_) => {
                let ty = ConcreteType::BasicType(BasicConcreteType::Bool);
                typed_literal.ty = Some(ty.clone());
                Some(ty)
            }
            LiteralKind::String(_, _) => {
                let ty = ConcreteType::Pointer(Box::new(ConcreteType::BasicType(BasicConcreteType::Char)));
                typed_literal.ty = Some(ty.clone());
                Some(ty)
            }
            LiteralKind::Char(_) => {
                let ty = ConcreteType::BasicType(BasicConcreteType::Char);
                typed_literal.ty = Some(ty.clone());
                Some(ty)
            }
            LiteralKind::Null => {
                let ty = ConcreteType::BasicType(BasicConcreteType::Null);
                typed_literal.ty = Some(ty.clone());
                Some(ty)
            }
        }
    }

    fn infer_integer_type(
        &mut self,
        literal: &TypedLiteral,
        suffix_opt: &Option<Box<TokenKind>>,
        expected: Option<ConcreteType>,
    ) -> ConcreteType {
        if let Some(suffix) = suffix_opt {
            match self.map_integer_suffix_to_type(&suffix) {
                Some(ty) => ty,
                None => {
                    self.report_invalid_integer_suffix(&suffix, literal.loc.clone());
                    ConcreteType::BasicType(BasicConcreteType::Int)
                }
            }
        } else if let Some(ctx_ty) = expected {
            ctx_ty
        } else {
            ConcreteType::BasicType(BasicConcreteType::Int)
        }
    }

    fn infer_float_type(
        &mut self,
        literal: &TypedLiteral,
        suffix_opt: &Option<Box<TokenKind>>,
        expected: Option<ConcreteType>,
    ) -> ConcreteType {
        if let Some(suffix) = suffix_opt {
            match self.map_float_suffix_to_type(&suffix) {
                Some(ty) => ty,
                None => {
                    self.report_invalid_float_suffix(&suffix, literal.loc.clone());
                    ConcreteType::BasicType(BasicConcreteType::Float64)
                }
            }
        } else if let Some(ctx_ty) = expected {
            ctx_ty
        } else {
            ConcreteType::BasicType(BasicConcreteType::Float64)
        }
    }

    fn map_integer_suffix_to_type(&self, suffix: &TokenKind) -> Option<ConcreteType> {
        let ty = match suffix {
            TokenKind::UIntPtr => BasicConcreteType::UIntPtr,
            TokenKind::IntPtr => BasicConcreteType::IntPtr,
            TokenKind::SizeT => BasicConcreteType::SizeT,

            TokenKind::Int => BasicConcreteType::Int,
            TokenKind::Int8 => BasicConcreteType::Int8,
            TokenKind::Int16 => BasicConcreteType::Int16,
            TokenKind::Int32 => BasicConcreteType::Int32,
            TokenKind::Int64 => BasicConcreteType::Int64,
            TokenKind::Int128 => BasicConcreteType::Int128,

            TokenKind::UInt => BasicConcreteType::UInt,
            TokenKind::UInt8 => BasicConcreteType::UInt8,
            TokenKind::UInt16 => BasicConcreteType::UInt16,
            TokenKind::UInt32 => BasicConcreteType::UInt32,
            TokenKind::UInt64 => BasicConcreteType::UInt64,
            TokenKind::UInt128 => BasicConcreteType::UInt128,

            _ => return None,
        };

        Some(ConcreteType::BasicType(ty))
    }

    fn map_float_suffix_to_type(&self, suffix: &TokenKind) -> Option<ConcreteType> {
        let ty = match suffix {
            TokenKind::Float16 => BasicConcreteType::Float16,
            TokenKind::Float32 => BasicConcreteType::Float32,
            TokenKind::Float64 => BasicConcreteType::Float64,
            TokenKind::Float128 => BasicConcreteType::Float128,
            _ => return None,
        };

        Some(ConcreteType::BasicType(ty))
    }

    fn report_invalid_integer_suffix(&mut self, suffix: &TokenKind, loc: Location) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::InvalidIntegerLiteralSuffix,
            location: Some(DiagLoc::new(
                self.resolver.get_current_module_file_path(),
                loc.clone(),
                0,
            )),
            hint: Some(format!("Invalid suffix {:?} for integer literal.", suffix)),
        });
    }

    fn report_invalid_float_suffix(&mut self, suffix: &TokenKind, loc: Location) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::InvalidFloatLiteralSuffix,
            location: Some(DiagLoc::new(
                self.resolver.get_current_module_file_path(),
                loc.clone(),
                0,
            )),
            hint: Some(format!("Invalid suffix {:?} for float literal.", suffix)),
        });
    }

    pub(crate) fn analyze_typed_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExpression,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let concrete_type = match &mut typed_expr.kind {
            TypedExpressionKind::Symbol(symbol_id, ..) => {
                let local_scope_ref_opt = {
                    if let Some(scope_id) = scope_id_opt {
                        self.resolver.get_scope_ref(self.module_id, scope_id)
                    } else {
                        None
                    }
                };

                let local_or_global_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_ref_opt, *symbol_id)?;

                // mark symbol used
                match &local_or_global_symbol {
                    LocalOrGlobalSymbol::LocalSymbol(..) => {
                        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt?);
                        self.mark_local_symbol_used_once(local_scope_opt.unwrap(), self.module_id, *symbol_id);
                    }
                    LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => {
                        self.mark_symbol_used_once(symbol_entry.get_module_id(), symbol_entry.get_symbol_id());
                    }
                }

                self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, local_or_global_symbol)
            }
            TypedExpressionKind::Literal(typed_literal) => self.analyze_literal_type(typed_literal, expected_type),
            TypedExpressionKind::Prefix(typed_prefix_expr) => {
                self.analyze_prefix_expr_type(scope_id_opt, typed_prefix_expr, expected_type)
            }
            TypedExpressionKind::Infix(typed_infix_expr) => {
                self.analyze_infix_expr_type(scope_id_opt, typed_infix_expr, expected_type)
            }
            TypedExpressionKind::Unary(typed_unary_expr) => {
                self.analyze_unary_expr_type(scope_id_opt, typed_unary_expr)
            }
            TypedExpressionKind::Assignment(typed_assignment) => {
                self.analyze_assignment(scope_id_opt, typed_assignment);
                None
            }
            TypedExpressionKind::Cast(typed_cast) => self.analyze_cast_expr_type(scope_id_opt, typed_cast),
            TypedExpressionKind::Array(typed_array) => self.analyze_array_expr_type(scope_id_opt, typed_array),
            TypedExpressionKind::ArrayIndex(typed_array_index) => {
                self.analyze_array_index_expr_type(scope_id_opt, typed_array_index)
            }
            TypedExpressionKind::AddressOf(typed_address_of) => {
                self.analyze_address_of_expr_type(scope_id_opt, typed_address_of)
            }
            TypedExpressionKind::Dereference(typed_dereference) => {
                self.analyze_dereference_expr_type(scope_id_opt, typed_dereference)
            }
            TypedExpressionKind::StructInit(typed_struct_init) => {
                self.analyze_struct_init_expr_type(scope_id_opt, typed_struct_init)
            }
            TypedExpressionKind::FuncCall(typed_func_call) => {
                self.analyze_func_call_expr_type(scope_id_opt, typed_func_call)
            }
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => {
                self.analyze_unnamed_struct_value_expr_type(scope_id_opt, typed_unnamed_struct_value)
            }
            TypedExpressionKind::FieldAccess(field_access) => {
                self.analyze_field_access_type(scope_id_opt, field_access)
            }
            TypedExpressionKind::MethodCall(method_call) => {
                self.analyze_method_call_expr_type(scope_id_opt, method_call, expected_type)
            }
            TypedExpressionKind::SizeOfExpression(typed_size_of_expression) => {
                self.analyze_sizeof_expr_type(scope_id_opt, typed_size_of_expression, expected_type)
            }
            TypedExpressionKind::ConcreteType(..) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidUsageOfTheConcreteType,
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        typed_expr.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        let normalized_type = self.normalize_type(scope_id_opt, concrete_type.clone()?, typed_expr.loc.clone());
        typed_expr.concrete_type = normalized_type.clone();

        if cfg!(debug_assertions) {
            if let Some(concrete_type_clone) = typed_expr.concrete_type.clone() {
                let is_unresolved_symbol = matches!(concrete_type_clone, ConcreteType::UnresolvedSymbol(..));
                assert!(is_unresolved_symbol == false);
            }
            assert!(typed_expr.concrete_type != None);
        }

        normalized_type
    }

    pub(crate) fn check_expr_type_must_be_condition(&mut self, concrete_type: ConcreteType, loc: Location) {
        if !concrete_type.is_bool() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ConditionExprMustBeOfTypeBool,
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    loc.clone(),
                    0,
                )),
                hint: None,
            });
        }
    }

    fn analyze_unnamed_struct_value_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_value: &mut TypedUnnamedStructValue,
    ) -> Option<ConcreteType> {
        let mut fields: Vec<TypedUnnamedStructTypeField> = Vec::new();

        for field in &mut unnamed_struct_value.fields {
            let field_value_type =
                match self.analyze_typed_expr_type(scope_id_opt, &mut field.field_value, field.field_type.clone()) {
                    Some(concrete_type) => concrete_type,
                    None => continue,
                };

            fields.push(TypedUnnamedStructTypeField {
                field_name: field.field_name.clone(),
                field_type: Box::new(field.field_type.clone().unwrap_or(field_value_type)),
                loc: field.loc.clone(),
            });
        }

        let unnamed_struct_type = TypedUnnamedStructType {
            fields,
            packed: unnamed_struct_value.packed,
            loc: unnamed_struct_value.loc.clone(),
        };

        unnamed_struct_value.unnamed_struct_type = Some(unnamed_struct_type.clone());
        Some(ConcreteType::UnnamedStruct(unnamed_struct_type))
    }

    fn analyze_array_index_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        array_index: &mut TypedArrayIndex,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut array_index.operand, None) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        if !operand_type.is_array()
            || array_index
                .operand
                .concrete_type
                .clone()
                .unwrap()
                .as_array_type()
                .is_none()
        {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ArrayIndexOnNonArrayOperand,
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    array_index.loc.clone(),
                    0,
                )),
                hint: None,
            });
            return None;
        }

        let index_inner_type = array_index.index.concrete_type.clone();
        let index_concrete_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut array_index.index, index_inner_type) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        if !index_concrete_type
            .as_basic_type()
            .and_then(|b| Some(b.is_integer()))
            .is_some()
        {
            let found_type = format_concrete_type(index_concrete_type, &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ArrayNonIntegerIndex { found_type },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    array_index.loc.clone(),
                    0,
                )),
                hint: None,
            });
        }

        let concrete_type = array_index.operand.concrete_type.clone().unwrap();
        let array_type = concrete_type.as_array_type().unwrap();
        Some(*array_type.element_type.clone())
    }

    fn extract_struct_or_enum_symbol_id(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        var_type: ConcreteType,
        loc: Location,
    ) -> Option<SymbolID> {
        let normalized = self.normalize_type(scope_id_opt, var_type, loc.clone())?;

        match normalized {
            ConcreteType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.get_symbol_id()),
            ConcreteType::Pointer(concrete_type) => {
                self.extract_struct_or_enum_symbol_id(scope_id_opt, *concrete_type, loc)
            }
            _ => None,
        }
    }

    fn resolve_var_or_global_var_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        instance_symbol_id: SymbolID,
        loc: Location,
    ) -> Option<ConcreteType> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), instance_symbol_id)
            .unwrap();

        let concrete_type = match match &local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                let typed_variable = &local_symbol.as_variable().unwrap().typed_variable;

                match &typed_variable.ty {
                    Some(concrete_type) => self.normalize_type(scope_id_opt, concrete_type.clone(), loc.clone()),
                    None => {
                        let rhs = typed_variable.rhs.clone().unwrap();
                        self.analyze_typed_expr_type(scope_id_opt, &mut rhs.clone(), None)
                    }
                }
            }
            LocalOrGlobalSymbol::GlobalSymbol(global_symbol) => match global_symbol.as_global_var() {
                Some(resolved_global_var) => Some(resolved_global_var.global_var_sig.ty.clone().unwrap()),
                None => None,
            },
        } {
            Some(concrete_type) => Some(concrete_type),
            None => None,
        };

        if concrete_type.is_some() {
            let normalized_type = self
                .normalize_type(scope_id_opt, concrete_type.unwrap(), loc.clone())
                .unwrap();

            Some(normalized_type)
        } else {
            None
        }
    }

    fn analyze_unnamed_struct_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_type: TypedUnnamedStructType,
        field_access: &mut TypedFieldAccess,
        resolved_var_type: ConcreteType,
    ) -> Option<ConcreteType> {
        let struct_name = format_concrete_type(resolved_var_type, &(self.symbol_formatter)(scope_id_opt));

        let field_index = match unnamed_struct_type
            .fields
            .iter()
            .position(|typed_struct_field| typed_struct_field.field_name == field_access.field_name)
        {
            Some(typed_struct_field) => typed_struct_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::StructHasNoFieldNamed {
                        struct_name,
                        field_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        field_access.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        let typed_struct_field = unnamed_struct_type.fields.get(field_index).unwrap();

        field_access.field_index = Some(field_index);
        field_access.field_ty = Some(*typed_struct_field.field_type.clone());

        Some(*typed_struct_field.field_type.clone())
    }

    fn validate_field_access(
        &mut self,
        module_id: ModuleID,
        operand_concrete_type: ConcreteType,
        field_access: &TypedFieldAccess,
        field: &TypedStructField,
        struct_methods: HashMap<String, SymbolID>,
        struct_name: String,
    ) -> bool {
        let mut result = true;
        let method_symbol_ids = struct_methods.values().into_iter().cloned().collect::<Vec<SymbolID>>();
        let field_access_from_struct_methods = method_symbol_ids.contains(&self.cur_func_symbol_id.unwrap());

        if !field_access_from_struct_methods && field.vis.is_private() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InternalFieldAccess {
                    field_name: field_access.field_name.clone(),
                    struct_name,
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_module_file_path(module_id).unwrap(),
                    field_access.loc.clone(),
                    0,
                )),
                hint: None,
            });
            result = false;
        }

        if operand_concrete_type.is_pointer() && !field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UseFatArrow,
                location: Some(DiagLoc::new(
                    self.resolver.get_module_file_path(module_id).unwrap(),
                    field_access.loc.clone(),
                    0,
                )),
                hint: None,
            });
            result = false;
        } else if !operand_concrete_type.is_pointer() && field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidFatArrow,
                location: Some(DiagLoc::new(
                    self.resolver.get_module_file_path(module_id).unwrap(),
                    field_access.loc.clone(),
                    0,
                )),
                hint: Some("Use '.' instead of '->'.".to_string()),
            });
            result = false;
        }

        result
    }

    fn analyze_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        field_access: &mut TypedFieldAccess,
    ) -> Option<ConcreteType> {
        let instance_symbol_id = match &field_access.operand.kind {
            TypedExpressionKind::Symbol(symbol_id, ..) => symbol_id,
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        field_access.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());

        self.mark_local_symbol_used_once(local_scope_opt.clone().unwrap(), self.module_id, *instance_symbol_id);

        let resolved_var_type = self
            .resolve_var_or_global_var_type(
                scope_id_opt,
                local_scope_opt.clone(),
                *instance_symbol_id,
                field_access.loc.clone(),
            )
            .unwrap();

        let (struct_module_id, struct_name, struct_fields, struct_methods, struct_symbol_id) = match self
            .extract_struct_or_enum_symbol_id(scope_id_opt, resolved_var_type.clone(), field_access.loc.clone())
        {
            Some(struct_or_enum_symbol_id) => {
                let local_or_global_symbol = match self
                    .resolver
                    .resolve_local_or_global_symbol(local_scope_opt, struct_or_enum_symbol_id)
                {
                    Some(local_or_global_symbol) => local_or_global_symbol,
                    None => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                            location: Some(DiagLoc::new(
                                self.resolver.get_current_module_file_path(),
                                field_access.loc.clone(),
                                0,
                            )),
                            hint: None,
                        });
                        return None;
                    }
                };

                if let Some(resolved_struct) = local_or_global_symbol.as_struct() {
                    (
                        resolved_struct.module_id,
                        resolved_struct.struct_sig.name.clone(),
                        resolved_struct.struct_sig.fields.clone(),
                        resolved_struct.struct_sig.methods.clone(),
                        resolved_struct.symbol_id,
                    )
                } else if let Some(_resolved_enum) = local_or_global_symbol.as_enum() {
                    todo!();
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                        location: Some(DiagLoc::new(
                            self.resolver.get_current_module_file_path(),
                            field_access.loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                    return None;
                }
            }
            None => {
                // handle unnamed struct

                let is_resolved_var_type_const = resolved_var_type.is_const();

                if let Some(unnamed_struct_type) = resolved_var_type.as_const_or_unnamed_struct() {
                    let mut concrete_type = self
                        .analyze_unnamed_struct_type(scope_id_opt, unnamed_struct_type, field_access, resolved_var_type)
                        .unwrap();

                    if is_resolved_var_type_const {
                        concrete_type = ConcreteType::Const(Box::new(concrete_type.clone()));
                    }

                    return Some(concrete_type);
                } else {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                        location: Some(DiagLoc::new(
                            self.resolver.get_current_module_file_path(),
                            field_access.loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        let field_index = match struct_fields
            .iter()
            .position(|typed_struct_field| typed_struct_field.name == field_access.field_name)
        {
            Some(typed_struct_field) => typed_struct_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::StructHasNoFieldNamed {
                        struct_name,
                        field_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        field_access.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        let typed_struct_field = struct_fields.get(field_index).unwrap();

        if !self.validate_field_access(
            struct_module_id,
            resolved_var_type,
            &field_access,
            typed_struct_field,
            struct_methods,
            struct_name.clone(),
        ) {
            return None;
        }

        if field_access.is_fat_arrow {
            field_access.operand = Box::new(TypedExpression {
                kind: TypedExpressionKind::Dereference(TypedDereference {
                    operand: field_access.operand.clone(),
                    loc: field_access.loc.clone(),
                }),
                concrete_type: None,
                loc: field_access.loc.clone(),
            });
        }

        field_access.field_index = Some(field_index);
        field_access.field_ty = Some(typed_struct_field.ty.clone());
        field_access.object_symbol_id = Some(struct_symbol_id);

        Some(typed_struct_field.ty.clone())
    }

    fn analyze_struct_init_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_struct_init: &mut TypedStructInit,
    ) -> Option<ConcreteType> {
        let normalized = self
            .normalize_type(
                scope_id_opt,
                ConcreteType::UnresolvedSymbol(typed_struct_init.symbol_id),
                typed_struct_init.loc.clone(),
            )
            .unwrap();

        let resolved_struct = self
            .resolve_symbol_as_struct(
                scope_id_opt,
                normalized.as_struct_symbol_id().unwrap(),
                typed_struct_init.loc.clone(),
            )
            .unwrap();

        // check duplicate field inits
        let mut field_names: Vec<String> = Vec::new();
        for field_init in &typed_struct_init.fields {
            let struct_name = (self.symbol_formatter)(scope_id_opt)(typed_struct_init.symbol_id);

            if field_names.contains(&field_init.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateFieldName {
                        struct_name,
                        field_name: field_init.name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        field_init.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                continue;
            }

            field_names.push(field_init.name.clone());
        }

        let mut missing_fields: Vec<String> = resolved_struct
            .struct_sig
            .fields
            .iter()
            .map(|field| field.name.clone())
            .collect();

        for field_init in &mut typed_struct_init.fields {
            let field = resolved_struct
                .struct_sig
                .fields
                .iter()
                .find(|field| field.name == field_init.name);

            if field.is_none() {
                let struct_name = (self.symbol_formatter)(scope_id_opt)(typed_struct_init.symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::StructHasNoFieldNamed {
                        struct_name,
                        field_name: field_init.name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        field_init.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                continue;
            }

            let field_target_type = field.unwrap().ty.clone();
            let field_value_type = match self.analyze_typed_expr_type(
                scope_id_opt,
                &mut field_init.value,
                Some(field_target_type.clone()),
            ) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            if !self.check_type_mismatch(
                scope_id_opt,
                field_value_type.clone(),
                field_target_type.clone(),
                field_init.loc.clone(),
            ) {
                let struct_name = (self.symbol_formatter)(scope_id_opt)(typed_struct_init.symbol_id);
                let expected_type = format_concrete_type(field_target_type, &(self.symbol_formatter)(scope_id_opt));
                let found_type = format_concrete_type(field_value_type, &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::StructFieldTypeMismatch {
                        struct_name,
                        field_name: field_init.name.clone(),
                        expected_type,
                        found_type,
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        typed_struct_init.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
            }

            let missing_fields_idx = match missing_fields
                .iter()
                .position(|field_name| *field_name == field_init.name.clone())
            {
                Some(idx) => idx,
                None => continue,
            };
            missing_fields.remove(missing_fields_idx);
        }

        if !missing_fields.is_empty() {
            let struct_name = (self.symbol_formatter)(scope_id_opt)(typed_struct_init.symbol_id);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::StructMissingFields {
                    struct_name,
                    missing_field_names: missing_fields,
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    typed_struct_init.loc.clone(),
                    0,
                )),
                hint: None,
            });
        }

        typed_struct_init.symbol_id = normalized.as_struct_symbol_id().unwrap();

        Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(
            typed_struct_init.symbol_id,
        )))
    }

    fn analyze_address_of_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        address_of: &mut TypedAddressOf,
    ) -> Option<ConcreteType> {
        if !address_of.operand.kind.is_lvalue() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::AddressOfRvalue,
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    address_of.loc.clone(),
                    0,
                )),
                hint: None,
            });
            return None;
        }

        let operand_inner_type = address_of.operand.concrete_type.clone();
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut address_of.operand, operand_inner_type)
        {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        Some(ConcreteType::Pointer(Box::new(operand_type)))
    }

    fn analyze_dereference_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        dereference: &mut TypedDereference,
    ) -> Option<ConcreteType> {
        let operand_inner_type = dereference.operand.concrete_type.clone();
        let operand_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut dereference.operand, operand_inner_type) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        dereference.operand.concrete_type = Some(operand_type.clone());

        let pointer_inner_type = match operand_type {
            ConcreteType::Pointer(concrete_type) => *concrete_type,
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DerefNonPointerValue,
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        dereference.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        if pointer_inner_type.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::DerefVoidPointerValue,
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    dereference.loc.clone(),
                    0,
                )),
                hint: Some("Typecast 'void*' to a concrete pointer type before dereferencing it.".to_string()),
            });
            return None;
        }

        Some(pointer_inner_type)
    }

    fn check_func_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_sig: &mut FuncSig,
        args: &mut Vec<TypedExpression>,
        loc: Location,
    ) -> Option<ConcreteType> {
        let is_variadic = func_sig.params.variadic.is_some();

        if !is_variadic && args.len() != func_sig.params.list.len() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: func_sig.params.list.len() as u32,
                    func_name: func_sig.name.clone(),
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    loc.clone(),
                    0,
                )),
                hint: None,
            });
            return None;
        } else if is_variadic && args.len() < func_sig.params.list.len() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: func_sig.params.list.len() as u32,
                    func_name: func_sig.name.clone(),
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    loc.clone(),
                    0,
                )),
                hint: None,
            });
            return None;
        }

        if is_variadic {
            let static_params_len = func_sig.params.list.len();
            let variadic_args = &mut args[static_params_len..];

            match func_sig.params.variadic.clone().unwrap() {
                TypedFuncVariadicParams::Typed(_, variadic_param_type) => {
                    for (argument_idx, argument) in variadic_args.iter_mut().enumerate() {
                        let argument_type = match self.analyze_typed_expr_type(
                            scope_id_opt,
                            argument,
                            argument.concrete_type.clone(),
                        ) {
                            Some(concrete_type) => concrete_type,
                            None => continue,
                        };

                        if !self.check_type_mismatch(
                            scope_id_opt,
                            argument_type.clone(),
                            variadic_param_type.clone(),
                            argument.loc.clone(),
                        ) {
                            let param_type = format_concrete_type(
                                variadic_param_type.clone(),
                                &(self.symbol_formatter)(scope_id_opt),
                            );
                            let argument_type =
                                format_concrete_type(argument_type, &(self.symbol_formatter)(scope_id_opt));

                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                    param_type,
                                    argument_type,
                                    argument_idx: (argument_idx + static_params_len) as u32,
                                },
                                location: Some(DiagLoc::new(
                                    self.resolver.get_current_module_file_path(),
                                    loc.clone(),
                                    0,
                                )),
                                hint: None,
                            });
                        }
                    }
                }
                TypedFuncVariadicParams::UntypedCStyle => {
                    for argument in variadic_args.iter_mut() {
                        match self.analyze_typed_expr_type(scope_id_opt, argument, argument.concrete_type.clone()) {
                            Some(concrete_type) => concrete_type,
                            None => continue,
                        };
                    }
                }
            }
        }

        // analyze static arguments
        for (param, arg) in func_sig.params.list.iter_mut().zip(args.iter_mut()) {
            let param_type = match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    let normalized_type = self
                        .normalize_type(scope_id_opt, typed_func_param.ty.clone(), typed_func_param.loc.clone())
                        .unwrap();
                    typed_func_param.ty = normalized_type.clone();
                    normalized_type
                }
                TypedFuncParamKind::SelfModifier(typed_self_modifier) => {
                    let normalized_type = self
                        .normalize_type(
                            scope_id_opt,
                            typed_self_modifier.ty.clone().unwrap(),
                            typed_self_modifier.loc.clone(),
                        )
                        .unwrap();

                    match typed_self_modifier.kind {
                        SelfModifierKind::Copied => {
                            typed_self_modifier.ty = Some(normalized_type.clone());
                        }
                        SelfModifierKind::Referenced => {
                            typed_self_modifier.ty = Some(ConcreteType::Pointer(Box::new(normalized_type.clone())));
                        }
                    }

                    typed_self_modifier.ty.clone().unwrap()
                }
            };

            self.analyze_typed_expr_type(scope_id_opt, arg, Some(param_type));
        }

        self.check_duplicate_param_names(
            &func_sig.params.list,
            func_sig.params.variadic.as_ref(),
            DiagLoc::new(self.resolver.get_current_module_file_path(), loc.clone(), 0),
        );

        Some(func_sig.return_type.clone())
    }

    fn analyze_func_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_call: &mut TypedFuncCall,
    ) -> Option<ConcreteType> {
        let module_id = self.resolver.lookup_symbol_id_in_modules(func_call.symbol_id).unwrap();

        let local_scope_opt = self.resolver.get_scope_ref(module_id, scope_id_opt.unwrap());
        let local_or_global_symbol = {
            match self
                .resolver
                .lookup_symbol_entry_with_id(module_id, func_call.symbol_id)
            {
                Some(symbol_entry) => Some(LocalOrGlobalSymbol::GlobalSymbol(symbol_entry)),
                None => {
                    match self
                        .resolver
                        .resolve_symbol_from_local_scope(local_scope_opt.clone().unwrap(), func_call.symbol_id)
                    {
                        Some(local_symbol) => Some(LocalOrGlobalSymbol::LocalSymbol(local_symbol)),
                        None => None,
                    }
                }
            }
        }
        .unwrap();

        self.resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), func_call.symbol_id)
            .unwrap();

        let func_sig_opt = match local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(_) => None,
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match symbol_entry.kind {
                SymbolEntryKind::Func(resolved_function) => Some(resolved_function.func_sig),
                _ => None,
            },
        };

        let mut func_sig = match func_sig_opt {
            Some(func_sig) => func_sig,
            None => {
                let func_name = (self.symbol_formatter)(scope_id_opt)(func_call.symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NonFunctionSymbol { symbol_name: func_name },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        func_call.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        self.mark_func_used(local_scope_opt, module_id, func_call.symbol_id);
        let return_type = self.check_func_call(scope_id_opt, &mut func_sig, &mut func_call.args, func_call.loc.clone());

        update_global_symbol_type!(self, func_sig.module_id, func_call.symbol_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func, {
                resolved_func.func_sig = func_sig.clone();
            }
        );

        return_type
    }

    fn analyze_method_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &mut TypedMethodCall,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let method_name = method_call.method_name.clone();
        let loc = method_call.loc.clone();

        let operand_concrete_type =
            self.analyze_typed_expr_type(scope_id_opt, &mut method_call.operand, expected_type)?;

        let instance_symbol_id = match method_call.operand.kind {
            TypedExpressionKind::Symbol(symbol_id, ..) => symbol_id,
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectNotSupportsMethods,
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        method_call.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt?);
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), instance_symbol_id)
            .unwrap();

        let struct_id_opt = {
            if let Some(resolved_struct) = local_or_global_symbol.as_struct() {
                // static method call
                Some((resolved_struct.module_id, resolved_struct.symbol_id))
            } else if let Some(_resolved_enum) = local_or_global_symbol.as_enum() {
                // TODO enum variant
                todo!();
            } else {
                // instance method call
                if let Some(resolved_var) = local_or_global_symbol.as_variable() {
                    let var_type = resolved_var
                        .typed_variable
                        .ty
                        .clone()
                        .unwrap_or(resolved_var.typed_variable.rhs.unwrap().concrete_type.unwrap())
                        .get_const_inner()
                        .clone();

                    match self.extract_struct_or_enum_symbol_id(scope_id_opt, var_type, loc) {
                        Some(struct_id) => Some((resolved_var.module_id, struct_id)),
                        None => None,
                    }
                } else if let Some(resolved_global_var) = local_or_global_symbol.as_global_var() {
                    let var_type = resolved_global_var.global_var_sig.ty.unwrap().get_const_inner().clone();

                    match self.extract_struct_or_enum_symbol_id(scope_id_opt, var_type, loc) {
                        Some(struct_id) => Some((resolved_global_var.module_id, struct_id)),
                        None => None,
                    }
                } else {
                    None
                }
            }
        };

        let (module_id, struct_id) = match struct_id_opt {
            Some((module_id, struct_id)) => (module_id, struct_id),
            None => {
                let symbol_name = (self.symbol_formatter)(scope_id_opt)(instance_symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NonStructSymbol { symbol_name },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        method_call.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        method_call.symbol_id = struct_id;
        let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, struct_id).unwrap();

        let (object_name, object_methods, object_module_id) = {
            match symbol_entry.kind {
                SymbolEntryKind::Struct(resolved_struct) => (
                    resolved_struct.struct_sig.name,
                    resolved_struct.struct_sig.methods,
                    resolved_struct.module_id,
                ),
                SymbolEntryKind::Enum(_resolved_enum) => todo!(),
                _ => unreachable!(),
            }
        };

        let method_symbol_id = match object_methods.get(&method_name) {
            Some(symbol_id) => *symbol_id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::StructMethodNotDefined {
                        struct_name: object_name.clone(),
                        method_name: method_name.clone(),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        method_call.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        };

        let mut method_symbol_entry = self
            .resolver
            .lookup_symbol_entry_with_id(object_module_id, method_symbol_id)
            .unwrap();

        let resolved_method = match &mut method_symbol_entry.kind {
            SymbolEntryKind::Method(resolved_method) => resolved_method,
            _ => unreachable!(),
        };

        let first_param_opt = resolved_method.func_sig.params.list.first();

        if !self.validate_method_call(
            object_module_id,
            scope_id_opt,
            instance_symbol_id,
            operand_concrete_type.clone(),
            method_call,
            first_param_opt,
            object_methods,
            object_name.clone(),
            &resolved_method,
        ) {
            return None;
        }

        {
            // FIXME I still don't know why this isn't working?
            // But i know i can fix it in the future and does not matter for me at moment =/
            // self.mark_func_used(local_scope_opt, module_id, method_symbol_id);
        }

        Some(resolved_method.func_sig.return_type.clone())
    }

    fn validate_method_call(
        &mut self,
        module_id: ModuleID,
        scope_id_opt: Option<ScopeID>,
        instance_symbol_id: SymbolID,
        operand_concrete_type: ConcreteType,
        method_call: &TypedMethodCall,
        first_param_opt: Option<&TypedFuncParamKind>,
        object_methods: HashMap<String, SymbolID>,
        object_name: String,
        resolved_method: &ResolvedMethod,
    ) -> bool {
        let mut result = true;
        let method_symbol_ids = object_methods.values().into_iter().cloned().collect::<Vec<SymbolID>>();
        let field_access_from_struct_methods = method_symbol_ids.contains(&self.cur_func_symbol_id.unwrap());

        if !field_access_from_struct_methods && resolved_method.func_sig.vis.is_private() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InternalMethodCall {
                    method_name: resolved_method.func_sig.name.clone(),
                    object_name,
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_module_file_path(module_id).unwrap(),
                    method_call.loc.clone(),
                    0,
                )),
                hint: None,
            });
            result = false;
        }

        if operand_concrete_type.is_pointer() && !method_call.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UseFatArrow,
                location: Some(DiagLoc::new(
                    self.resolver.get_module_file_path(module_id).unwrap(),
                    method_call.loc.clone(),
                    0,
                )),
                hint: None,
            });
            result = false;
        } else if !operand_concrete_type.is_pointer() && method_call.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidFatArrow,
                location: Some(DiagLoc::new(
                    self.resolver.get_module_file_path(module_id).unwrap(),
                    method_call.loc.clone(),
                    0,
                )),
                hint: Some("Use '.' instead of '->'.".to_string()),
            });
            result = false;
        } else {
            if let Some(first_param) = first_param_opt {
                if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = first_param {
                    if typed_self_modifier.kind == SelfModifierKind::Referenced && operand_concrete_type.is_const() {
                        let instance_name = (self.symbol_formatter)(scope_id_opt)(instance_symbol_id);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::MutationPossibleMethodCallOnConstInstance {
                                method_name: method_call.method_name.clone(),
                                instance_name: instance_name.clone(),
                            },
                            location: Some(DiagLoc::new(
                                self.resolver.get_module_file_path(module_id).unwrap(),
                                method_call.loc.clone(),
                                0,
                            )),
                            hint: Some(format!(
                                "Instance '{}' is declared as 'const' and cannot be modified.",
                                instance_name
                            )),
                        });
                        result = false;
                    }
                }
            }
        }

        result
    }

    fn analyze_array_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_array: &mut TypedArray,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let typed_array_type = match &typed_array.array_type {
            ConcreteType::Array(typed_array_type) => typed_array_type,
            _ => unreachable!(),
        };

        if let TypedArrayCapacity::Fixed(fixed_capacity) = typed_array_type.capacity {
            if typed_array.elements.len() != fixed_capacity.try_into().unwrap() {}
        }

        for (element_index, element) in typed_array.elements.iter_mut().enumerate() {
            let element_type =
                match self.analyze_typed_expr_type(scope_id_opt, element, Some(*typed_array_type.element_type.clone()))
                {
                    Some(concrete_type) => concrete_type,
                    None => continue,
                };

            if !self.check_type_mismatch(
                scope_id_opt,
                element_type.clone(),
                *typed_array_type.element_type.clone(),
                element.loc.clone(),
            ) {
                let element_type = format_concrete_type(element_type, &formatter_closure);
                let expected_type = format_concrete_type(*typed_array_type.element_type.clone(), &formatter_closure);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ArrayElementTypeMismatch {
                        element_type,
                        element_index: element_index.try_into().unwrap(),
                        expected_type,
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        typed_array.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
            }
        }

        Some(ConcreteType::Array(typed_array_type.clone()))
    }

    fn analyze_cast_expr_type(&mut self, scope_id_opt: Option<ScopeID>, cast: &mut TypedCast) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let operand =
            match self.analyze_typed_expr_type(scope_id_opt, &mut cast.operand, Some(cast.target_type.clone())) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        if !self.check_type_mismatch(
            scope_id_opt,
            operand.clone(),
            cast.target_type.clone(),
            cast.loc.clone(),
        ) {
            let lhs_type = format_concrete_type(cast.target_type.clone(), &formatter_closure);
            let rhs_type = format_concrete_type(operand, &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CastTypeMismatch { lhs_type, rhs_type },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    cast.loc.clone(),
                    0,
                )),
                hint: None,
            });
            return None;
        }

        Some(cast.target_type.clone())
    }

    fn analyze_arithmetic_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        loc: Location,
    ) -> Option<ConcreteType> {
        self.analyze_binary_expr(
            scope_id_opt,
            lhs_type.clone(),
            rhs_type.clone(),
            loc,
            |this, lhs, rhs| {
                let valid = (this.is_integer_type(lhs.clone()) && this.is_integer_type(rhs.clone()))
                    || (this.is_float_type(lhs.clone()) && this.is_float_type(rhs.clone()));

                if valid {
                    if let (ConcreteType::BasicType(lhs_basic), ConcreteType::BasicType(rhs_basic)) = (lhs, rhs) {
                        BasicConcreteType::bigger_type(lhs_basic, rhs_basic)
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
        )
    }

    fn analyze_compare_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        cmp_eq: bool,
        loc: Location,
    ) -> Option<ConcreteType> {
        let lhs_type = lhs_type.get_const_inner();
        let rhs_type = rhs_type.get_const_inner();

        if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), loc.clone()) {
            return None;
        }

        self.analyze_binary_expr(
            scope_id_opt,
            lhs_type.clone(),
            rhs_type.clone(),
            loc,
            |this, lhs, rhs| {
                if (this.is_integer_type(lhs.clone()) && this.is_integer_type(rhs.clone()))
                    || (this.is_float_type(lhs.clone()) && this.is_float_type(rhs.clone()))
                {
                    Some(BasicConcreteType::Bool)
                } else if cmp_eq {
                    // allow pointer comparisons
                    if let (ConcreteType::Pointer(_), ConcreteType::Pointer(_)) = (&lhs, &rhs) {
                        Some(BasicConcreteType::Bool)
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
        )
    }

    fn analyze_binary_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        loc: Location,
        type_checker: impl Fn(&mut Self, ConcreteType, ConcreteType) -> Option<BasicConcreteType>,
    ) -> Option<ConcreteType> {
        let lhs_type = lhs_type.get_const_inner();
        let rhs_type = rhs_type.get_const_inner();

        if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), loc.clone()) {
            let lhs_type_str = format_concrete_type(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_concrete_type(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidInfix {
                    lhs_type: lhs_type_str,
                    rhs_type: rhs_type_str,
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    loc.clone(),
                    0,
                )),
                hint: Some("Consider adding an explicit cast to either the left-hand or right-hand operand to make their types compatible.".to_string()),
            });
            return None;
        }

        match type_checker(self, lhs_type.clone(), rhs_type.clone()) {
            Some(result_basic) => Some(ConcreteType::BasicType(result_basic)),
            None => {
                let lhs_type_str = format_concrete_type(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
                let rhs_type_str = format_concrete_type(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidInfix {
                        lhs_type: lhs_type_str,
                        rhs_type: rhs_type_str,
                    },
                    location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
                    hint: None,
                });
                None
            }
        }
    }

    fn analyze_or_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        loc: Location,
    ) -> Option<ConcreteType> {
        match (lhs_type.clone(), rhs_type.clone()) {
            (ConcreteType::BasicType(BasicConcreteType::Null), ConcreteType::Pointer(inner_pointer_type)) => {
                Some(ConcreteType::Pointer(inner_pointer_type))
            }
            (ConcreteType::Pointer(inner_pointer_type), ConcreteType::BasicType(BasicConcreteType::Null)) => {
                Some(ConcreteType::Pointer(inner_pointer_type))
            }
            (ConcreteType::Pointer(inner_pointer_type1), ConcreteType::Pointer(inner_pointer_type2)) => {
                if *inner_pointer_type1 == *inner_pointer_type2 {
                    Some(ConcreteType::Pointer(inner_pointer_type1))
                } else {
                    None
                }
            }
            (
                null_concrete_type @ ConcreteType::BasicType(BasicConcreteType::Null),
                ConcreteType::BasicType(BasicConcreteType::Null),
            ) => Some(null_concrete_type),
            _ => self.analyze_binary_expr(scope_id_opt, lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
                (ConcreteType::BasicType(lhs_basic), ConcreteType::BasicType(rhs_basic))
                    if lhs_basic.is_bool() && rhs_basic.is_bool() =>
                {
                    Some(BasicConcreteType::Bool)
                }
                _ => None,
            }),
        }
    }

    fn analyze_and_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        loc: Location,
    ) -> Option<ConcreteType> {
        self.analyze_binary_expr(scope_id_opt, lhs_type, rhs_type, loc, |_, lhs, rhs| match (lhs, rhs) {
            (ConcreteType::BasicType(lhs_basic), ConcreteType::BasicType(rhs_basic))
                if lhs_basic.is_bool() && rhs_basic.is_bool() =>
            {
                Some(BasicConcreteType::Bool)
            }
            _ => None,
        })
    }

    fn analyze_left_shift_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        loc: Location,
    ) -> Option<ConcreteType> {
        self.analyze_binary_expr(scope_id_opt, lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            if let (ConcreteType::BasicType(lhs_basic), ConcreteType::BasicType(rhs_basic)) = (&lhs, &rhs) {
                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    Some(BasicConcreteType::bigger_type(lhs_basic.clone(), rhs_basic.clone())?)
                } else {
                    None
                }
            } else {
                None
            }
        })
    }

    fn analyze_right_shift_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        loc: Location,
    ) -> Option<ConcreteType> {
        self.analyze_binary_expr(
            scope_id_opt,
            lhs_type.clone(),
            rhs_type.clone(),
            loc.clone(),
            |this, lhs, rhs| {
                if let (ConcreteType::BasicType(lhs_basic), ConcreteType::BasicType(rhs_basic)) = (&lhs, &rhs) {
                    // rhs must be unsigned
                    if rhs_basic.is_signed() {
                        this.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::RhsOfShiftMustBeUnsignedInteger,
                            location: Some(DiagLoc::new(
                                this.resolver.get_current_module_file_path(),
                                loc.clone(),
                                0,
                            )),
                            hint: None,
                        });
                        return None;
                    }

                    if lhs_basic.is_integer() && rhs_basic.is_integer() {
                        Some(BasicConcreteType::bigger_type(lhs_basic.clone(), rhs_basic.clone())?)
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
        )
    }

    fn analyze_bitwise_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        loc: Location,
    ) -> Option<ConcreteType> {
        self.analyze_binary_expr(scope_id_opt, lhs_type.clone(), rhs_type.clone(), loc, |_, lhs, rhs| {
            // only allow integer types
            if let (Some(lhs_basic), Some(rhs_basic)) = (lhs.as_basic_type(), rhs.as_basic_type()) {
                if lhs_basic.is_integer() && rhs_basic.is_integer() {
                    return Some(BasicConcreteType::bigger_type(lhs_basic.clone(), rhs_basic.clone()).unwrap());
                }
            }

            None
        })
    }

    fn analyze_infix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        infix_expr: &mut TypedInfixExpression,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let lhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut infix_expr.lhs, expected_type.clone()) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let rhs_type = match self.analyze_typed_expr_type(
            scope_id_opt,
            &mut infix_expr.rhs,
            expected_type.or(Some(lhs_type.clone())),
        ) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        match infix_expr.op {
            InfixOperator::Add | InfixOperator::Sub | InfixOperator::Mul | InfixOperator::Div | InfixOperator::Rem => {
                self.analyze_arithmetic_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone())
            }
            InfixOperator::LessThan
            | InfixOperator::LessEqual
            | InfixOperator::GreaterThan
            | InfixOperator::GreaterEqual => {
                self.analyze_compare_expr(scope_id_opt, lhs_type, rhs_type, false, infix_expr.loc.clone())
            }
            InfixOperator::Equal | InfixOperator::NotEqual => {
                self.analyze_compare_expr(scope_id_opt, lhs_type, rhs_type, true, infix_expr.loc.clone())
            }
            InfixOperator::Or => self.analyze_or_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone()),
            InfixOperator::And => self.analyze_and_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone()),
            InfixOperator::BitwiseAnd
            | InfixOperator::BitwiseOr
            | InfixOperator::BitwiseXor
            | InfixOperator::BitwiseAndNot => {
                self.analyze_bitwise_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone())
            }
            InfixOperator::ShiftLeft => {
                self.analyze_left_shift_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone())
            }
            InfixOperator::ShiftRight => {
                self.analyze_right_shift_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone())
            }
        }
    }

    fn analyze_sizeof_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        sizeof_expr: &mut TypedSizeOfExpression,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        self.analyze_typed_expr_type(scope_id_opt, &mut sizeof_expr.expr, expected_type);

        Some(ConcreteType::BasicType(BasicConcreteType::SizeT))
    }

    fn analyze_prefix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        prefix_expr: &mut TypedPrefixExpression,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut prefix_expr.operand, expected_type) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        match prefix_expr.op {
            PrefixOperator::BitwiseNot => {
                let valid_concrete_type = match &operand_type {
                    ConcreteType::BasicType(basic_concrete_type) => {
                        if basic_concrete_type.is_integer() {
                            Some(basic_concrete_type.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type.clone())),
                    None => {
                        let operand_type = format_concrete_type(operand_type, &formatter_closure);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixMinusOnNonInteger { operand_type },
                            location: Some(DiagLoc::new(
                                self.resolver.get_current_module_file_path(),
                                prefix_expr.loc.clone(),
                                0,
                            )),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Bang => {
                let valid_concrete_type = match &operand_type {
                    ConcreteType::BasicType(basic_concrete_type) => {
                        if basic_concrete_type.is_bool() {
                            Some(basic_concrete_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type.clone())),
                    None => {
                        let operand_type = format_concrete_type(operand_type, &formatter_closure);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixBangOnNonBool { operand_type },
                            location: Some(DiagLoc::new(
                                self.resolver.get_current_module_file_path(),
                                prefix_expr.loc.clone(),
                                0,
                            )),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
            PrefixOperator::Minus => {
                let valid_concrete_type = match &operand_type {
                    ConcreteType::BasicType(basic_concrete_type) => {
                        if basic_concrete_type.is_integer() || basic_concrete_type.is_float() {
                            Some(basic_concrete_type)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type.clone())),
                    None => {
                        let operand_type = format_concrete_type(operand_type, &formatter_closure);

                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixMinusOnNonInteger { operand_type },
                            location: Some(DiagLoc::new(
                                self.resolver.get_current_module_file_path(),
                                prefix_expr.loc.clone(),
                                0,
                            )),
                            hint: None,
                        });
                        return None;
                    }
                }
            }
        }
    }

    fn analyze_unary_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unary_expr: &mut TypedUnaryExpression,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let operand_inner_type = unary_expr.operand.concrete_type.clone();
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut unary_expr.operand, operand_inner_type)
        {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        if self.is_integer_type(operand_type.get_const_inner().clone()) {
            let operand_type = format_concrete_type(operand_type, &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidUnary { operand_type },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    unary_expr.loc.clone(),
                    0,
                )),
                hint: None,
            });
            return None;
        }

        Some(operand_type)
    }
}
