use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind, update_global_symbol, update_local_symbol};
use ast::{
    AccessSpecifier, AssignmentKind, LiteralKind, SelfModifierKind, StringPrefix,
    operators::{InfixOperator, PrefixOperator},
    source_loc::SourceLoc,
    token::TokenKind,
};
use diagcentral::{Diag, DiagLevel, DiagLoc};
use resolver::{
    scope::{LocalOrGlobalSymbol, LocalScopeRef, LocalSymbolKind, ResolvedMethod, ResolvedUnion, SymbolEntryKind},
    signatures::FuncSig,
    typed_func_params_as_func_type_params,
};
use std::collections::HashMap;
use typed_ast::{
    format::{format_concrete_type, format_func_type},
    types::{
        BasicConcreteType::{self, *},
        ConcreteType, ResolvedSymbol, TypedArrayCapacity, TypedArrayFixedCapacityValue, TypedArrayType, TypedFuncType,
        TypedUnnamedStructType, TypedUnnamedStructTypeField,
    },
    *,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_type_mismatch(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        value_type: ConcreteType,
        target_type: ConcreteType,
        loc: SourceLoc,
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
            (ConcreteType::Array(array_type1), ConcreteType::Array(array_type2)) => {
                let valid_capacity = self.check_const_str_to_array_assignment(array_type1.clone(), array_type2.clone());

                valid_capacity
                    && self.check_type_mismatch(scope_id_opt, *array_type1.element_type, *array_type2.element_type, loc)
            }
            (ConcreteType::Pointer(inner_concrete_type1), ConcreteType::Pointer(inner_concrete_type2)) => {
                if let Some(arr_type) = inner_concrete_type1.as_array_type() {
                    *arr_type.element_type == *inner_concrete_type2
                } else {
                    (inner_concrete_type1.is_void() || inner_concrete_type2.is_void())
                        || self.check_type_mismatch(scope_id_opt, *inner_concrete_type1, *inner_concrete_type2, loc)
                }
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
            (ConcreteType::FuncType(func_type1), ConcreteType::FuncType(func_type2)) => func_type1 == func_type2,
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

            // char to int
            (Char, Int8 | Int16 | Int32 | Int64 | Int128 | Int) => true,

            // int8 to char
            (Int8, Char) => true,

            // Bool to Int
            (Bool, Int8 | UInt8) => true,

            (Bool, Bool) => true,

            _ => false,
        }
    }

    fn check_const_str_to_array_assignment(&self, value_type: TypedArrayType, target_type: TypedArrayType) -> bool {
        match (value_type.capacity, target_type.capacity) {
            (
                TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(value_capacity)),
                TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(target_capacity)),
            ) => value_capacity == target_capacity,
            _ => false, // not valid
        }
    }

    fn check_explicit_typecast(&mut self, value_type: ConcreteType, target_type: ConcreteType) -> bool {
        match (value_type, target_type) {
            // Same type, always fine
            (a, b) if a == b => true,

            // Any integer to any integer
            (ConcreteType::BasicType(value), ConcreteType::BasicType(target))
                if value.is_integer() && target.is_integer() =>
            {
                true
            }

            // Any float to any float
            (ConcreteType::BasicType(value), ConcreteType::BasicType(target))
                if value.is_float() && target.is_float() =>
            {
                true
            }

            // Bool to anything integer-ish (common in C-style languages)
            (ConcreteType::BasicType(BasicConcreteType::Bool), ConcreteType::BasicType(target))
                if target.is_integer() =>
            {
                true
            }

            // Char to integer and back
            (ConcreteType::BasicType(BasicConcreteType::Char), ConcreteType::BasicType(target))
                if target.is_integer() =>
            {
                true
            }
            (ConcreteType::BasicType(value), ConcreteType::BasicType(BasicConcreteType::Char))
                if value.is_integer() =>
            {
                true
            }

            // void* <-> intptr/uintptr
            (ConcreteType::Pointer(..), ConcreteType::BasicType(BasicConcreteType::IntPtr))
            | (ConcreteType::Pointer(..), ConcreteType::BasicType(BasicConcreteType::UIntPtr))
            | (ConcreteType::BasicType(BasicConcreteType::IntPtr), ConcreteType::Pointer(..))
            | (ConcreteType::BasicType(BasicConcreteType::UIntPtr), ConcreteType::Pointer(..)) => true,

            (ConcreteType::FuncType(..), ConcreteType::Pointer(pointer_type)) => pointer_type.is_void(),

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
                let inferred = self.infer_integer_type(typed_literal, suffix_opt, expected_type.clone());
                typed_literal.ty = Some(inferred.clone());
                Some(inferred)
            }
            LiteralKind::Float(_, suffix_opt) => {
                let inferred = self.infer_float_type(typed_literal, suffix_opt, expected_type.clone());
                typed_literal.ty = Some(inferred.clone());
                Some(inferred)
            }
            LiteralKind::Bool(_) => {
                let ty = ConcreteType::BasicType(BasicConcreteType::Bool);
                typed_literal.ty = Some(ty.clone());
                Some(ty)
            }
            LiteralKind::String(value, prefix_opt) => {
                if let Some(prefix) = &prefix_opt {
                    match prefix {
                        StringPrefix::C => {}
                        StringPrefix::B => {
                            let ty = ConcreteType::Array(TypedArrayType {
                                element_type: Box::new(ConcreteType::Const(Box::new(ConcreteType::BasicType(
                                    BasicConcreteType::Char,
                                )))),
                                capacity: TypedArrayCapacity::Fixed(TypedArrayFixedCapacityValue::Value(value.len())),
                                loc: typed_literal.loc.clone(),
                            });
                            typed_literal.ty = Some(ty.clone());
                            return Some(ty);
                        }
                    }
                }

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
            if self.is_integer_type(ctx_ty.clone()) {
                ctx_ty
            } else {
                ConcreteType::BasicType(BasicConcreteType::Int) // keep a safe default
            }
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
            if self.is_float_type(ctx_ty.clone()) {
                ctx_ty
            } else {
                ConcreteType::BasicType(BasicConcreteType::Float64) // keep a safe default
            }
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

    fn report_invalid_integer_suffix(&mut self, suffix: &TokenKind, loc: SourceLoc) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::InvalidIntegerLiteralSuffix,
            location: Some(DiagLoc::new(loc)),
            hint: Some(format!("Invalid suffix {:?} for integer literal.", suffix)),
        });
    }

    fn report_invalid_float_suffix(&mut self, suffix: &TokenKind, loc: SourceLoc) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::InvalidFloatLiteralSuffix,
            location: Some(DiagLoc::new(loc)),
            hint: Some(format!("Invalid suffix {:?} for float literal.", suffix)),
        });
    }

    fn lower_prefix_bang_with_pointer_operand(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        expected_type: Option<ConcreteType>,
        prefix_expr: &mut TypedPrefixExpression,
    ) -> Option<TypedExpression> {
        let operand_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut prefix_expr.operand, expected_type.clone()) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        let null_literal_expr = TypedExpression {
            kind: TypedExpressionKind::Literal(TypedLiteral {
                ty: Some(ConcreteType::Pointer(Box::new(ConcreteType::BasicType(
                    BasicConcreteType::Void,
                )))),
                kind: LiteralKind::Null,
                loc: prefix_expr.loc.clone(),
            }),
            value_category: ValueCategory::Rvalue,
            concrete_type: None,
            loc: prefix_expr.loc.clone(),
        };

        if operand_type.is_pointer() {
            let lhs = prefix_expr.operand.clone();

            let new_infix_expr = TypedExpressionKind::Infix(TypedInfixExpression {
                op: InfixOperator::Equal,
                lhs,
                rhs: Box::new(null_literal_expr),
                loc: prefix_expr.loc.clone(),
            });

            Some(TypedExpression {
                kind: new_infix_expr,
                value_category: ValueCategory::Rvalue,
                concrete_type: None,
                loc: prefix_expr.loc.clone(),
            })
        } else {
            None
        }
    }

    pub(crate) fn analyze_typed_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExpression,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        // lowering
        match &mut typed_expr.kind {
            TypedExpressionKind::Assignment(typed_assignment) => {
                if typed_assignment.kind != AssignmentKind::Default {
                    typed_expr.kind = self.lower_assign_to_infix_expr(typed_assignment);
                }
            }
            TypedExpressionKind::Prefix(prefix_expr) => match &prefix_expr.op {
                PrefixOperator::Bang => {
                    if let Some(lowered_typed_expr) =
                        self.lower_prefix_bang_with_pointer_operand(scope_id_opt, expected_type.clone(), prefix_expr)
                    {
                        *typed_expr = lowered_typed_expr;
                    };
                }
                _ => {}
            },
            _ => {}
        }

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
                        if let Some(mut resolved_global_var) = symbol_entry.as_global_var().cloned() {
                            if scope_id_opt.is_none()
                                && !resolved_global_var.global_var_sig.ty.clone().unwrap().is_const()
                            {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: AnalyzerDiagKind::ValueIsNotACompTimeConst,
                                    location: Some(DiagLoc::new(typed_expr.loc.clone())),
                                    hint: None,
                                });
                                return None;
                            }

                            if let Some(concrete_type) = &resolved_global_var.global_var_sig.ty {
                                resolved_global_var.global_var_sig.ty = Some(
                                    self.normalize_type(scope_id_opt, concrete_type.clone(), typed_expr.loc.clone())
                                        .unwrap(),
                                );
                            }

                            if let Some(mut typed_expr) = resolved_global_var.global_var_sig.rhs.clone() {
                                self.analyze_typed_expr_type(scope_id_opt, &mut typed_expr, expected_type);

                                update_global_symbol!(self, resolved_global_var.module_id, resolved_global_var.symbol_id,
                                    SymbolEntryKind::GlobalVar(global_var) => global_var, {
                                        global_var.global_var_sig.rhs = Some(typed_expr);
                                    }
                                );
                            }
                        }

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
                self.analyze_struct_init_expr_type(scope_id_opt, typed_struct_init, expected_type)
            }
            TypedExpressionKind::FuncCall(typed_func_call) => {
                self.analyze_func_call_expr_type(scope_id_opt, typed_func_call)
            }
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => {
                self.analyze_unnamed_struct_value_expr_type(scope_id_opt, typed_unnamed_struct_value)
            }
            TypedExpressionKind::FieldAccess(field_access) => {
                self.analyze_field_access_type(scope_id_opt, field_access, expected_type)
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
                    location: Some(DiagLoc::new(typed_expr.loc.clone())),
                    hint: None,
                });
                return None;
            }
            TypedExpressionKind::Lambda(typed_lambda) => self.analyze_lambda_expr(scope_id_opt, typed_lambda),
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

    fn analyze_lambda_expr(&mut self, scope_id_opt: Option<ScopeID>, lambda: &mut TypedLambda) -> Option<ConcreteType> {
        let current_func_clone = self.current_func.clone();

        self.normalize_func_params(&mut lambda.params, lambda.loc.clone());
        lambda.return_type = self.normalize_type(scope_id_opt, lambda.return_type.clone(), lambda.loc.clone())?;
        let params = typed_func_params_as_func_type_params(&lambda.params);
        let func_type = TypedFuncType {
            params,
            return_type: Box::new(lambda.return_type.clone()),
        };

        self.current_func = Some(func_type.clone());
        self.analyze_block_statement(&mut lambda.body);

        self.current_func = current_func_clone;
        Some(ConcreteType::FuncType(func_type))
    }

    pub(crate) fn check_expr_type_must_be_condition(&mut self, concrete_type: ConcreteType, loc: SourceLoc) {
        if !concrete_type.is_bool() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ConditionExprMustBeOfTypeBool,
                location: Some(DiagLoc::new(loc)),
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

        if unnamed_struct_value.is_const {
            Some(ConcreteType::Const(Box::new(ConcreteType::UnnamedStruct(
                unnamed_struct_type,
            ))))
        } else {
            Some(ConcreteType::UnnamedStruct(unnamed_struct_type))
        }
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

        let is_operand_array = operand_type.is_array();

        if !(operand_type.is_pointer() || is_operand_array) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ArrayIndexOnNonArrayOperand,
                location: Some(DiagLoc::new(array_index.loc.clone())),
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
            .get_const_inner()
            .as_basic_type()
            .and_then(|b| Some(b.is_integer()))
            .is_some()
        {
            let found_type = format_concrete_type(index_concrete_type, &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ArrayNonIntegerIndex { found_type },
                location: Some(DiagLoc::new(array_index.loc.clone())),
                hint: None,
            });
            return None;
        }

        let concrete_type = array_index.operand.concrete_type.clone().unwrap();

        if is_operand_array {
            let array_type = concrete_type.as_array_type().unwrap();
            Some(*array_type.element_type.clone())
        } else {
            // array index on pointer operand
            let element_type = concrete_type.get_pointer_inner().unwrap();

            if element_type.is_void() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DerefVoidPointerValue,
                    location: Some(DiagLoc::new(array_index.loc.clone())),
                    hint: None,
                });
                return None;
            }

            Some(element_type.clone())
        }
    }

    fn resolve_var_or_global_var_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_scope_opt: Option<LocalScopeRef>,
        instance_symbol_id: SymbolID,
        loc: SourceLoc,
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

    fn validate_field_access(
        &mut self,
        operand: &TypedExpression,
        field_access: &TypedFieldAccess,
        field_vis: AccessSpecifier,
        struct_methods: &HashMap<String, SymbolID>,
        struct_name: &str,
    ) -> bool {
        let mut result = true;

        let method_symbol_ids = struct_methods.values().into_iter().cloned().collect::<Vec<SymbolID>>();
        let field_access_from_struct_methods = method_symbol_ids.contains(&self.current_method_symbol_id.unwrap());
        if !(field_access_from_struct_methods || field_vis.is_private()) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InternalFieldAccess {
                    field_name: field_access.field_name.clone(),
                    struct_name: struct_name.to_string(),
                },
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            result = false;
        }

        let is_pointer = operand.concrete_type.clone().unwrap().get_const_inner().is_pointer();
        let is_struct = operand
            .concrete_type
            .clone()
            .unwrap()
            .get_const_inner()
            .is_resolved_symbol();

        if field_access.is_fat_arrow {
            if !is_pointer {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidFatArrow,
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
                result = false;
            }
        } else {
            if !is_struct {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::UseFatArrow,
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
                result = false;
            }
        }

        result
    }

    fn validate_union_field_access(
        &mut self,
        operand_concrete_type: ConcreteType,
        field_access: &TypedFieldAccess,
    ) -> bool {
        let mut result = true;

        if operand_concrete_type.is_pointer() && !field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UseFatArrow,
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            result = false;
        } else if !operand_concrete_type.is_pointer() && field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidFatArrow,
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: Some("Use '.' instead of '->'.".to_string()),
            });
            result = false;
        }

        result
    }

    fn analyze_union_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        resolved_union: &ResolvedUnion,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut field_access.operand, expected_type) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        field_access.operand.concrete_type = Some(operand_type.clone());

        let union_field_idx = match resolved_union
            .union_sig
            .fields
            .iter()
            .position(|field| *field.name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: resolved_union.union_sig.name.clone(),
                        field_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let union_field = &resolved_union.union_sig.fields[union_field_idx];
        let union_field_type = match self.normalize_type(scope_id_opt, union_field.ty.clone(), field_access.loc.clone())
        {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        if !self.validate_union_field_access(operand_type.get_const_inner().clone(), &field_access) {
            return None;
        }

        if field_access.is_fat_arrow {
            field_access.operand = Box::new(TypedExpression {
                kind: TypedExpressionKind::Dereference(TypedDereference {
                    operand: field_access.operand.clone(),
                    loc: field_access.loc.clone(),
                }),
                concrete_type: None,
                value_category: ValueCategory::Lvalue,
                loc: field_access.loc.clone(),
            });
        }

        field_access.field_index = Some(union_field_idx);
        field_access.field_ty = Some(union_field_type);
        field_access.object_symbol_id = Some(resolved_union.symbol_id);

        Some(union_field.ty.clone())
    }

    fn analyze_unnamed_struct_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unnamed_struct_type: &TypedUnnamedStructType,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut field_access.operand, expected_type) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        field_access.operand.concrete_type = Some(operand_type.clone());

        let field_idx = match unnamed_struct_type
            .fields
            .iter()
            .position(|field| *field.field_name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: format_concrete_type(
                            ConcreteType::UnnamedStruct(unnamed_struct_type.clone()),
                            &(self.symbol_formatter)(scope_id_opt),
                        ),
                        field_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let field = &unnamed_struct_type.fields[field_idx];
        let field_type = match self.normalize_type(scope_id_opt, *field.field_type.clone(), field_access.loc.clone()) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        field_access.field_index = Some(field_idx);
        field_access.field_ty = Some(field_type.clone());

        Some(field_type.clone())
    }

    fn analyze_struct_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        field_access: &mut TypedFieldAccess,
        struct_name: String,
        struct_fields: Vec<TypedStructField>,
        struct_methods: HashMap<String, SymbolID>,
        struct_symbol_id: SymbolID,
    ) -> Option<ConcreteType> {
        let field_index = match struct_fields
            .iter()
            .position(|typed_struct_field| typed_struct_field.name == field_access.field_name)
        {
            Some(typed_struct_field) => typed_struct_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name,
                        field_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let mut typed_struct_field = struct_fields.get(field_index).unwrap().clone();
        typed_struct_field.ty = self
            .normalize_type(scope_id_opt, typed_struct_field.ty.clone(), field_access.loc.clone())
            .unwrap();

        if !self.validate_field_access(
            &field_access.operand,
            &field_access,
            typed_struct_field.vis.clone(),
            &struct_methods,
            &struct_name,
        ) {
            return None;
        }

        field_access.field_index = Some(field_index);
        field_access.field_ty = Some(typed_struct_field.ty.clone());
        field_access.object_symbol_id = Some(struct_symbol_id);

        Some(typed_struct_field.ty.clone())
    }

    fn analyze_enum_variant(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        enum_symbol_id: SymbolID,
        enum_variant: &TypedEnumVariant,
        method_call: &mut TypedMethodCall,
    ) -> Option<ConcreteType> {
        let valued_fields = match enum_variant {
            TypedEnumVariant::Variant(_, valued_fields) => {
                if valued_fields.len() != method_call.args.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::EnumVariantArgCountMismatch {
                            variant_name: method_call.method_name.clone(),
                            expected: valued_fields.len() as u32,
                            provided: method_call.args.len() as u32,
                        },
                        location: Some(DiagLoc::new(method_call.loc.clone())),
                        hint: None,
                    });
                    return None;
                }

                valued_fields
            }
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                        variant_name: method_call.method_name.clone(),
                    },
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        for (typed_expr, enum_valued_field) in method_call.args.iter_mut().zip(valued_fields) {
            typed_expr.concrete_type =
                self.analyze_typed_expr_type(scope_id_opt, typed_expr, Some(enum_valued_field.field_type.clone()));
        }

        Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(enum_symbol_id)))
    }

    fn analyze_enum_variant_no_field(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        enum_symbol_id: SymbolID,
        field_access: &TypedFieldAccess,
    ) -> Option<ConcreteType> {
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, enum_symbol_id)
            .unwrap();
        let resolved_enum = local_or_global_symbol.as_enum().unwrap();

        let enum_variant = match resolved_enum
            .enum_sig
            .variants
            .iter()
            .find(|variant| variant.get_identifier().as_string() == field_access.field_name)
        {
            Some(enum_variant) => enum_variant,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::VariantNotDefinedForEnum {
                        enum_name: resolved_enum.enum_sig.name.clone(),
                        variant_name: field_access.field_name.clone(),
                    },
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        if matches!(enum_variant, TypedEnumVariant::Variant(..)) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::VariantMissingFields {
                    enum_name: resolved_enum.enum_sig.name.clone(),
                    variant_name: field_access.field_name.clone(),
                },
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            return None;
        }

        Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(
            resolved_enum.symbol_id,
        )))
    }

    fn analyze_field_access_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        field_access.operand.concrete_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut field_access.operand, expected_type.clone()) {
                Some(concrete_type) => Some(concrete_type),
                None => return None,
            };

        if let Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(enum_symbol_id))) =
            field_access.operand.concrete_type
        {
            let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());
            return self.analyze_enum_variant_no_field(local_scope_opt.clone(), enum_symbol_id, &field_access);
        }

        let object_symbol_id = {
            let operand_type = match &field_access.operand.kind {
                TypedExpressionKind::Symbol(instance_symbol_id, ..) => {
                    let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());

                    self.mark_local_symbol_used_once(
                        local_scope_opt.clone().unwrap(),
                        self.module_id,
                        *instance_symbol_id,
                    );

                    let resolved_var_type = match self.resolve_var_or_global_var_type(
                        scope_id_opt,
                        local_scope_opt.clone(),
                        *instance_symbol_id,
                        field_access.loc.clone(),
                    ) {
                        Some(concrete_type) => concrete_type,
                        None => {
                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                                location: Some(DiagLoc::new(field_access.loc.clone())),
                                hint: None,
                            });
                            return None;
                        }
                    };

                    resolved_var_type.clone()
                }
                _ => match self.analyze_typed_expr_type(scope_id_opt, &mut field_access.operand, expected_type.clone())
                {
                    Some(concrete_type) => concrete_type,
                    None => return None,
                },
            };

            match match operand_type.get_const_inner() {
                ConcreteType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.get_symbol_id()),
                ConcreteType::Pointer(concrete_type) => {
                    if concrete_type.is_void() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                            location: Some(DiagLoc::new(field_access.loc.clone())),
                            hint: None,
                        });
                        return None;
                    } else if let Some(unnamed_struct_type) = concrete_type.as_unnamed_struct() {
                        return self.analyze_unnamed_struct_field_access_type(
                            scope_id_opt,
                            &unnamed_struct_type,
                            field_access,
                            expected_type.clone(),
                        );
                    }

                    self.extract_object_symbol_id(scope_id_opt, *concrete_type.clone(), field_access.loc.clone())
                }
                ConcreteType::UnnamedStruct(unnamed_struct_type) => {
                    return self.analyze_unnamed_struct_field_access_type(
                        scope_id_opt,
                        &unnamed_struct_type,
                        field_access,
                        expected_type.clone(),
                    );
                }
                _ => None,
            } {
                Some(symbol_id) => symbol_id,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                        location: Some(DiagLoc::new(field_access.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        // FIXME self.module_id must be changed
        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());

        let local_or_global_symbol = match self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, object_symbol_id)
        {
            Some(local_or_global_symbol) => local_or_global_symbol,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                    location: Some(DiagLoc::new(field_access.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        if let Some(resolved_struct) = local_or_global_symbol.as_struct() {
            return self.analyze_struct_field_access_type(
                scope_id_opt,
                field_access,
                resolved_struct.struct_sig.name.clone(),
                resolved_struct.struct_sig.fields.clone(),
                resolved_struct.struct_sig.methods.clone(),
                resolved_struct.symbol_id,
            );
        } else if let Some(resolved_union) = &local_or_global_symbol.as_union() {
            return self.analyze_union_field_access_type(scope_id_opt, resolved_union, field_access, expected_type);
        } else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                location: Some(DiagLoc::new(field_access.loc.clone())),
                hint: None,
            });
            return None;
        }
    }

    fn analyze_union_init_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        union_symbol_id: SymbolID,
        typed_struct_init: &mut TypedStructInit,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        if typed_struct_init.fields.len() > 1 || typed_struct_init.fields.len() == 0 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::UnionInitWithInvalidFields,
                location: Some(DiagLoc::new(typed_struct_init.loc.clone())),
                hint: None,
            });
            return None;
        }

        // FIXME self.module_id must be changed
        let local_scope_opt = self
            .resolver
            .get_scope_ref(self.module_id, scope_id_opt.unwrap())
            .unwrap();

        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(Some(local_scope_opt), union_symbol_id)
            .unwrap();

        let resolved_union = match local_or_global_symbol.as_union() {
            Some(resolved_union) => resolved_union,
            None => {
                let symbol_name = format_concrete_type(
                    ConcreteType::UnresolvedSymbol(union_symbol_id),
                    &(self.symbol_formatter)(scope_id_opt),
                );

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NonUnionSymbol { symbol_name },
                    location: Some(DiagLoc::new(typed_struct_init.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let field = &mut typed_struct_init.fields[0];
        self.analyze_typed_expr_type(scope_id_opt, &mut field.value, expected_type);

        Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Union(
            resolved_union.symbol_id,
        )))
    }

    fn analyze_struct_init_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_struct_init: &mut TypedStructInit,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let normalized = self
            .normalize_type(
                scope_id_opt,
                ConcreteType::UnresolvedSymbol(typed_struct_init.symbol_id),
                typed_struct_init.loc.clone(),
            )
            .unwrap();

        let struct_symbol_id = match normalized.get_const_inner().as_struct_symbol_id() {
            Some(symbol_id) => symbol_id,
            None => {
                if let Some(union_symbol_id) = normalized.as_union_symbol_id() {
                    return self.analyze_union_init_expr_type(
                        scope_id_opt,
                        union_symbol_id,
                        typed_struct_init,
                        expected_type,
                    );
                } else {
                    let symbol_name = format_concrete_type(normalized, &(self.symbol_formatter)(scope_id_opt));

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::NonStructSymbol { symbol_name },
                        location: Some(DiagLoc::new(typed_struct_init.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        let resolved_struct = self
            .resolve_symbol_as_struct(scope_id_opt, struct_symbol_id, typed_struct_init.loc.clone())
            .unwrap();

        // check duplicate field inits
        let mut field_names: Vec<String> = Vec::new();
        for field_init in &typed_struct_init.fields {
            let struct_name = (self.symbol_formatter)(scope_id_opt)(typed_struct_init.symbol_id);

            if field_names.contains(&field_init.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::DuplicateFieldName {
                        object_name: struct_name,
                        field_name: field_init.name.clone(),
                    },
                    location: Some(DiagLoc::new(field_init.loc.clone())),
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
                    kind: AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name,
                        field_name: field_init.name.clone(),
                    },
                    location: Some(DiagLoc::new(field_init.loc.clone())),
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
                    location: Some(DiagLoc::new(typed_struct_init.loc.clone())),
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
                location: Some(DiagLoc::new(typed_struct_init.loc.clone())),
                hint: None,
            });
        }

        typed_struct_init.symbol_id = normalized.as_struct_symbol_id().unwrap();

        if typed_struct_init.is_const {
            Some(ConcreteType::Const(Box::new(ConcreteType::ResolvedSymbol(
                ResolvedSymbol::NamedStruct(typed_struct_init.symbol_id),
            ))))
        } else {
            Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(
                typed_struct_init.symbol_id,
            )))
        }
    }

    fn analyze_address_of_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        address_of: &mut TypedAddressOf,
    ) -> Option<ConcreteType> {
        if !address_of.operand.is_lvalue() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::AddressOfRvalue,
                location: Some(DiagLoc::new(address_of.loc.clone())),
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

        if !dereference.operand.is_lvalue() || operand_type.as_func_type().is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::DerefNonPointerValue,
                location: Some(DiagLoc::new(dereference.loc.clone())),
                hint: None,
            });
            return None;
        }

        let pointer_inner_type = match operand_type {
            ConcreteType::Pointer(concrete_type) => *concrete_type,
            _ => unreachable!(),
        };

        if pointer_inner_type.is_void() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::DerefVoidPointerValue,
                location: Some(DiagLoc::new(dereference.loc.clone())),
                hint: Some("Cast 'void*' to a concrete pointer type before dereferencing it.".to_string()),
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
        loc: SourceLoc,
        instance_method_call: bool,
    ) -> Option<ConcreteType> {
        let is_variadic = func_sig.params.variadic.is_some();
        let mut expected_args_len = func_sig.params.list.len();

        // If this is an instance method call, `&self` will be pushed later
        if instance_method_call && !func_sig.params.list.is_empty() {
            expected_args_len = expected_args_len.saturating_sub(1);
        }

        // Check argument count
        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name: func_sig.name.clone(),
                },
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            return None;
        }

        // Handle variadic arguments
        if is_variadic {
            let static_params_len = func_sig.params.list.len();
            let variadic_args = &mut args[static_params_len..];

            if let Some(var_param) = &func_sig.params.variadic {
                match var_param.clone() {
                    TypedFuncVariadicParams::Typed(_, variadic_param_type) => {
                        for (idx, arg) in variadic_args.iter_mut().enumerate() {
                            if let Some(arg_type) =
                                self.analyze_typed_expr_type(scope_id_opt, arg, arg.concrete_type.clone())
                            {
                                if !self.check_type_mismatch(
                                    scope_id_opt,
                                    arg_type.clone(),
                                    variadic_param_type.clone(),
                                    arg.loc.clone(),
                                ) {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                            param_type: format_concrete_type(
                                                variadic_param_type.clone(),
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_type: format_concrete_type(
                                                arg_type,
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_idx: (idx + static_params_len) as u32,
                                        },
                                        location: Some(DiagLoc::new(loc.clone())),
                                        hint: None,
                                    });
                                }
                            }
                        }
                    }
                    TypedFuncVariadicParams::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            let _ = self.analyze_typed_expr_type(scope_id_opt, arg, arg.concrete_type.clone());
                        }
                    }
                }
            }
        }

        // Analyze static arguments
        let start_idx = if instance_method_call { 1 } else { 0 };
        for (param_idx, (param, arg)) in func_sig
            .params
            .list
            .iter_mut()
            .skip(start_idx)
            .zip(args.iter_mut())
            .enumerate()
        {
            let param_type = match param {
                TypedFuncParamKind::FuncParam(p) => {
                    let normalized = self.normalize_type(scope_id_opt, p.ty.clone(), p.loc.clone()).unwrap();
                    p.ty = normalized.clone();
                    normalized
                }
                TypedFuncParamKind::SelfModifier(s) => {
                    let normalized = self
                        .normalize_type(scope_id_opt, s.ty.clone().unwrap(), s.loc.clone())
                        .unwrap();
                    s.ty = Some(match s.kind {
                        SelfModifierKind::Copied => normalized.clone(),
                        SelfModifierKind::Referenced => ConcreteType::Pointer(Box::new(normalized.clone())),
                    });
                    s.ty.clone().unwrap()
                }
            };

            let arg_type = match self.analyze_typed_expr_type(scope_id_opt, arg, Some(param_type.clone())) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            if !self.check_type_mismatch(scope_id_opt, arg_type.clone(), param_type.clone(), arg.loc.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_concrete_type(param_type.clone(), &(self.symbol_formatter)(scope_id_opt)),
                        argument_type: format_concrete_type(arg_type, &(self.symbol_formatter)(scope_id_opt)),
                        argument_idx: param_idx as u32,
                    },
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
            }
        }

        // Check for duplicate parameter names
        self.check_duplicate_param_names(
            &func_sig.params.list,
            func_sig.params.variadic.as_ref(),
            DiagLoc::new(loc),
        );

        Some(func_sig.return_type.clone())
    }

    fn check_func_type_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_type: &mut TypedFuncType,
        args: &mut Vec<TypedExpression>,
        loc: SourceLoc,
    ) -> Option<ConcreteType> {
        let is_variadic = func_type.params.variadic.is_some();
        let expected_args_len = func_type.params.list.len();
        let func_name = format_func_type(func_type, &(self.symbol_formatter)(scope_id_opt));

        // Check argument count
        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name,
                },
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
            return None;
        }

        // Handle variadic arguments
        if is_variadic {
            let static_params_len = func_type.params.list.len();
            let variadic_args = &mut args[static_params_len..];

            if let Some(var_param) = &func_type.params.variadic {
                match *var_param.clone() {
                    TypedFuncTypeVariadicParams::Typed(variadic_param_type) => {
                        for (idx, arg) in variadic_args.iter_mut().enumerate() {
                            if let Some(arg_type) =
                                self.analyze_typed_expr_type(scope_id_opt, arg, arg.concrete_type.clone())
                            {
                                if !self.check_type_mismatch(
                                    scope_id_opt,
                                    arg_type.clone(),
                                    variadic_param_type.clone(),
                                    arg.loc.clone(),
                                ) {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                            param_type: format_concrete_type(
                                                variadic_param_type.clone(),
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_type: format_concrete_type(
                                                arg_type,
                                                &(self.symbol_formatter)(scope_id_opt),
                                            ),
                                            argument_idx: (idx + static_params_len) as u32,
                                        },
                                        location: Some(DiagLoc::new(loc.clone())),
                                        hint: None,
                                    });
                                }
                            }
                        }
                    }
                    TypedFuncTypeVariadicParams::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            let _ = self.analyze_typed_expr_type(scope_id_opt, arg, arg.concrete_type.clone());
                        }
                    }
                }
            }
        }

        // Analyze static arguments
        let start_idx = 0;
        for (param_idx, (param, arg)) in func_type
            .params
            .list
            .iter_mut()
            .skip(start_idx)
            .zip(args.iter_mut())
            .enumerate()
        {
            let param_type = self.normalize_type(scope_id_opt, param.clone(), loc.clone()).unwrap();

            let arg_type = match self.analyze_typed_expr_type(scope_id_opt, arg, Some(param_type.clone())) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            if !self.check_type_mismatch(scope_id_opt, arg_type.clone(), param_type.clone(), arg.loc.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_concrete_type(param_type.clone(), &(self.symbol_formatter)(scope_id_opt)),
                        argument_type: format_concrete_type(arg_type, &(self.symbol_formatter)(scope_id_opt)),
                        argument_idx: param_idx as u32,
                    },
                    location: Some(DiagLoc::new(loc.clone())),
                    hint: None,
                });
            }
        }

        Some(*func_type.return_type.clone())
    }

    fn analyze_func_call_on_func_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_type: &mut TypedFuncType,
        args: &mut Vec<TypedExpression>,
        loc: SourceLoc,
    ) -> Option<ConcreteType> {
        self.normalize_func_type_params(&mut func_type.params, loc.clone());
        let return_type = self.check_func_type_call(scope_id_opt, func_type, args, loc.clone());
        return_type
    }

    fn analyze_func_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_call: &mut TypedFuncCall,
    ) -> Option<ConcreteType> {
        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), func_call.symbol_id)
            .unwrap();

        let func_sig_opt = match &local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol.as_variable() {
                Some(resolved_var) => match &resolved_var.typed_variable.ty {
                    Some(concrete_type) => {
                        let normalized =
                            self.normalize_type(scope_id_opt, concrete_type.clone(), func_call.loc.clone())?;

                        if let Some(scope_id) = scope_id_opt {
                            update_local_symbol!(self, scope_id, func_call.symbol_id,
                                LocalSymbolKind::Variable(resolved_var) => resolved_var, {
                                    resolved_var.typed_variable.ty = Some(normalized.clone());
                                }
                            );
                        }

                        match normalized.as_func_type() {
                            Some(func_type) => {
                                return self.analyze_func_call_on_func_type(
                                    scope_id_opt,
                                    &mut func_type.clone(),
                                    &mut func_call.args,
                                    func_call.loc.clone(),
                                );
                            }
                            None => None,
                        }
                    }
                    None => None,
                },
                None => None,
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match &symbol_entry.kind {
                SymbolEntryKind::Func(resolved_function) => Some(resolved_function.func_sig.clone()),
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
                    location: Some(DiagLoc::new(func_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        self.normalize_func_params(&mut func_sig.params, func_sig.loc.clone());

        let return_type = self.check_func_call(
            scope_id_opt,
            &mut func_sig,
            &mut func_call.args,
            func_call.loc.clone(),
            false,
        );

        update_global_symbol!(self, func_sig.module_id, func_call.symbol_id,
            SymbolEntryKind::Func(resolved_func) => resolved_func, {
                resolved_func.func_sig = func_sig.clone();
            }
        );

        return_type
    }

    fn extract_object_symbol_id<'b>(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        var_type: ConcreteType,
        loc: SourceLoc,
    ) -> Option<u32> {
        let normalized = self.normalize_type(scope_id_opt, var_type, loc.clone())?;

        match normalized {
            ConcreteType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.get_symbol_id()),
            ConcreteType::Pointer(concrete_type) => self.extract_object_symbol_id(scope_id_opt, *concrete_type, loc),
            _ => None,
        }
    }

    fn analyze_method_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &mut TypedMethodCall,
        expected_type: Option<ConcreteType>,
    ) -> Option<ConcreteType> {
        let method_name = method_call.method_name.clone();
        let loc = method_call.loc.clone();

        method_call.operand.concrete_type =
            match self.analyze_typed_expr_type(scope_id_opt, &mut method_call.operand, expected_type.clone()) {
                Some(concrete_type) => Some(concrete_type),
                None => return None,
            };

        if let Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(enum_symbol_id))) =
            method_call.operand.concrete_type
        {
            let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());
            let local_or_global_symbol = self
                .resolver
                .resolve_local_or_global_symbol(local_scope_opt.clone(), enum_symbol_id)
                .unwrap();
            let resolved_enum = local_or_global_symbol.as_enum().unwrap();

            let enum_variant_opt = resolved_enum
                .enum_sig
                .variants
                .iter()
                .find(|variant| variant.get_identifier().as_string() == method_call.method_name);

            if let Some(enum_variant) = enum_variant_opt {
                return self.analyze_enum_variant(scope_id_opt, enum_symbol_id, enum_variant, method_call);
            }
        }

        let object_symbol_id = {
            let operand_type = match &method_call.operand.kind {
                TypedExpressionKind::Symbol(instance_symbol_id, ..) => {
                    let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());

                    self.mark_local_symbol_used_once(
                        local_scope_opt.clone().unwrap(),
                        self.module_id,
                        *instance_symbol_id,
                    );

                    if let Some(ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(..))) =
                        method_call.operand.concrete_type
                    {
                        method_call.operand.concrete_type.clone().unwrap()
                    } else {
                        let resolved_var_type = self
                            .resolve_var_or_global_var_type(
                                scope_id_opt,
                                local_scope_opt.clone(),
                                *instance_symbol_id,
                                method_call.loc.clone(),
                            )
                            .unwrap();

                        resolved_var_type
                    }
                }
                _ => {
                    match self.analyze_typed_expr_type(scope_id_opt, &mut method_call.operand, expected_type.clone()) {
                        Some(concrete_type) => concrete_type,
                        None => return None,
                    }
                }
            };

            match operand_type.get_const_inner() {
                ConcreteType::ResolvedSymbol(resolved_symbol) => resolved_symbol.get_symbol_id(),
                ConcreteType::Pointer(concrete_type) => {
                    if concrete_type.is_void() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                            location: Some(DiagLoc::new(method_call.loc.clone())),
                            hint: None,
                        });
                        return None;
                    }

                    self.extract_object_symbol_id(scope_id_opt, *concrete_type.clone(), loc.clone())
                        .unwrap()
                }
                _ => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                        location: Some(DiagLoc::new(method_call.loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());
        let local_or_global_symbol = match self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, object_symbol_id)
        {
            Some(local_or_global_symbol) => local_or_global_symbol,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectNotSupportsFields,
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        let struct_ids_opt = {
            if let Some(resolved_struct) = local_or_global_symbol.as_struct() {
                // static method call
                Some((resolved_struct.module_id, resolved_struct.symbol_id))
            } else if let Some(resolved_enum) = local_or_global_symbol.as_enum() {
                Some((resolved_enum.module_id, resolved_enum.symbol_id))
            } else if let Some(resolved_union) = local_or_global_symbol.as_union() {
                Some((resolved_union.module_id, resolved_union.symbol_id))
            } else {
                // instance method call
                if let Some(resolved_var) = local_or_global_symbol.as_variable() {
                    let var_type = resolved_var
                        .typed_variable
                        .ty
                        .clone()
                        .unwrap_or({
                            self.analyze_typed_expr_type(
                                scope_id_opt,
                                &mut resolved_var.typed_variable.rhs.unwrap(),
                                expected_type.clone(),
                            )
                            .unwrap()
                        })
                        .get_const_inner()
                        .clone();

                    match self.extract_object_symbol_id(scope_id_opt, var_type, loc) {
                        Some(struct_id) => Some((resolved_var.module_id, struct_id)),
                        None => None,
                    }
                } else if let Some(resolved_global_var) = local_or_global_symbol.as_global_var() {
                    let var_type = resolved_global_var.global_var_sig.ty.unwrap().get_const_inner().clone();

                    match self.extract_object_symbol_id(scope_id_opt, var_type, loc) {
                        Some(struct_id) => Some((resolved_global_var.module_id, struct_id)),
                        None => None,
                    }
                } else {
                    None
                }
            }
        };

        let (module_id, struct_id) = match struct_ids_opt {
            Some((module_id, struct_id)) => (module_id, struct_id),
            None => {
                let symbol_name = (self.symbol_formatter)(scope_id_opt)(object_symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::NonStructSymbol { symbol_name },
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: None,
                });
                return None;
            }
        };

        method_call.object_symbol_id = Some(struct_id);
        let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, struct_id).unwrap();

        let (object_name, object_methods, object_module_id) = {
            match symbol_entry.kind {
                SymbolEntryKind::Struct(resolved_struct) => (
                    resolved_struct.struct_sig.name,
                    resolved_struct.struct_sig.methods,
                    resolved_struct.module_id,
                ),
                SymbolEntryKind::Enum(resolved_enum) => (
                    resolved_enum.enum_sig.name,
                    resolved_enum.enum_sig.methods,
                    resolved_enum.module_id,
                ),
                SymbolEntryKind::Union(resolved_union) => (
                    resolved_union.union_sig.name,
                    resolved_union.union_sig.methods,
                    resolved_union.module_id,
                ),
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
                    location: Some(DiagLoc::new(method_call.loc.clone())),
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
        let is_instance_method_call = {
            match first_param_opt {
                Some(first_param) => match first_param {
                    TypedFuncParamKind::FuncParam(..) => false,
                    TypedFuncParamKind::SelfModifier(..) => true,
                },
                None => false,
            }
        };

        if !self.validate_method_call(
            scope_id_opt,
            object_symbol_id,
            method_call,
            first_param_opt,
            object_methods,
            object_name.clone(),
            &resolved_method,
        ) {
            return None;
        }

        self.check_func_call(
            scope_id_opt,
            &mut resolved_method.func_sig,
            &mut method_call.args,
            method_call.loc.clone(),
            is_instance_method_call,
        );

        Some(resolved_method.func_sig.return_type.clone())
    }

    fn validate_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        instance_symbol_id: SymbolID,
        method_call: &TypedMethodCall,
        first_param_opt: Option<&TypedFuncParamKind>,
        object_methods: HashMap<String, SymbolID>,
        object_name: String,
        resolved_method: &ResolvedMethod,
    ) -> bool {
        let mut result = true;
        let method_symbol_ids = object_methods.values().into_iter().cloned().collect::<Vec<SymbolID>>();
        let method_call_from_struct_methods = method_symbol_ids.contains(&self.current_method_symbol_id.unwrap());

        if !(method_call_from_struct_methods || resolved_method.func_sig.vis.is_public()) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InternalMethodCall {
                    method_name: resolved_method.func_sig.name.clone(),
                    object_name,
                },
                location: Some(DiagLoc::new(method_call.loc.clone())),
                hint: None,
            });
            result = false;
        }

        let is_pointer = method_call
            .operand
            .concrete_type
            .clone()
            .unwrap()
            .get_const_inner()
            .is_pointer();
        let is_struct = method_call
            .operand
            .concrete_type
            .clone()
            .unwrap()
            .get_const_inner()
            .is_resolved_symbol();

        let is_operand_const = method_call.operand.concrete_type.clone().unwrap().is_const();

        if method_call.is_fat_arrow {
            if !is_pointer {
                dbg!(method_call.operand.concrete_type.clone());

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidFatArrow,
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
                result = false;
            }
        } else {
            if !is_struct {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::UseFatArrow,
                    location: Some(DiagLoc::new(method_call.loc.clone())),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
                result = false;
            }
        }

        if let Some(first_param) = first_param_opt {
            if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = first_param {
                if typed_self_modifier.kind == SelfModifierKind::Referenced && is_operand_const {
                    let instance_name = (self.symbol_formatter)(scope_id_opt)(instance_symbol_id);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::MutationPossibleMethodCallOnConstInstance {
                            method_name: method_call.method_name.clone(),
                            instance_name: instance_name.clone(),
                        },
                        location: Some(DiagLoc::new(method_call.loc.clone())),
                        hint: Some(format!(
                            "Instance '{}' is declared as 'const' and cannot be modified.",
                            instance_name
                        )),
                    });
                    result = false;
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

        typed_array.array_type =
            match self.normalize_type(scope_id_opt, typed_array.array_type.clone(), typed_array.loc.clone()) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        for (argument_idx, argument) in typed_array.elements.iter_mut().enumerate() {
            let argument_type = match self.analyze_typed_expr_type(
                scope_id_opt,
                argument,
                Some(*typed_array.array_type.as_array_type().unwrap().element_type.clone()),
            ) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            let element_type = match self.normalize_type(
                scope_id_opt,
                *typed_array.array_type.as_array_type().unwrap().element_type.clone(),
                argument.loc.clone(),
            ) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            if !self.check_type_mismatch(scope_id_opt, argument_type.clone(), element_type, argument.loc.clone()) {
                let element_type = format_concrete_type(argument_type, &formatter_closure);
                let expected_type = format_concrete_type(
                    *typed_array.array_type.as_array_type().unwrap().element_type.clone(),
                    &formatter_closure,
                );

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ArrayElementTypeMismatch {
                        element_type,
                        element_index: argument_idx.try_into().unwrap(),
                        expected_type,
                    },
                    location: Some(DiagLoc::new(typed_array.loc.clone())),
                    hint: None,
                });
            }
        }

        let array_type = typed_array.array_type.as_array_type().unwrap();
        let array_capacity = match &array_type.capacity {
            TypedArrayCapacity::Fixed(capacity_value) => match capacity_value {
                TypedArrayFixedCapacityValue::Expr(typed_expr) => {
                    self.const_expr_as_raw_integer(scope_id_opt, typed_expr)?
                }
                TypedArrayFixedCapacityValue::Value(value) => *value as i64,
            },
            TypedArrayCapacity::Dynamic => todo!(),
        };

        if typed_array.elements.len() != array_capacity.try_into().unwrap() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::ArrayElementsCountMismatch {
                    elements: typed_array.elements.len().try_into().unwrap(),
                    expected: array_capacity.try_into().unwrap(),
                },
                location: Some(DiagLoc::new(typed_array.loc.clone())),
                hint: None,
            });
            return None;
        }

        Some(ConcreteType::Array(
            typed_array.array_type.as_array_type().unwrap().clone(),
        ))
    }

    fn analyze_cast_expr_type(&mut self, scope_id_opt: Option<ScopeID>, cast: &mut TypedCast) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let operand =
            match self.analyze_typed_expr_type(scope_id_opt, &mut cast.operand, Some(cast.target_type.clone())) {
                Some(concrete_type) => concrete_type,
                None => return None,
            };

        if !(self.check_type_mismatch(
            scope_id_opt,
            operand.clone(),
            cast.target_type.clone(),
            cast.loc.clone(),
        ) || self.check_explicit_typecast(operand.clone(), cast.target_type.clone()))
        {
            let lhs_type = format_concrete_type(cast.target_type.clone(), &formatter_closure);
            let rhs_type = format_concrete_type(operand, &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CastTypeMismatch { lhs_type, rhs_type },
                location: Some(DiagLoc::new(cast.loc.clone())),
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
        loc: SourceLoc,
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

    fn analyze_compare_enums(
        &mut self,
        local_scope_opt: Option<LocalScopeRef>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
    ) -> Option<ConcreteType> {
        let enum_symbol_id1 = lhs_type.as_enum_symbol_id().unwrap();
        let enum_symbol_id2 = rhs_type.as_enum_symbol_id().unwrap();

        let local_or_global_symbol1 = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), enum_symbol_id1)
            .unwrap();

        let local_or_global_symbol2 = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, enum_symbol_id2)
            .unwrap();

        let resolved_enum1 = local_or_global_symbol1.as_enum()?;
        let resolved_enum2 = local_or_global_symbol2.as_enum()?;

        if resolved_enum1.symbol_id == resolved_enum2.symbol_id {
            Some(ConcreteType::BasicType(BasicConcreteType::Bool))
        } else {
            None
        }
    }

    fn analyze_compare_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        cmp_eq: bool,
        loc: SourceLoc,
    ) -> Option<ConcreteType> {
        let lhs_type = lhs_type.get_const_inner();
        let rhs_type = rhs_type.get_const_inner();

        if lhs_type.is_enum() && rhs_type.is_enum() {
            let lhs_type_str = format_concrete_type(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_concrete_type(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());
            match self.analyze_compare_enums(local_scope_opt, lhs_type.clone(), rhs_type.clone()) {
                Some(concrete_type) => return Some(concrete_type),
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::InvalidInfix {
                            lhs_type: lhs_type_str,
                            rhs_type: rhs_type_str,
                        },
                        location: Some(DiagLoc::new(loc.clone())),
                        hint: None,
                    });
                    return None;
                }
            }
        } else if !self.check_type_mismatch(scope_id_opt, rhs_type.clone(), lhs_type.clone(), loc.clone()) {
            let lhs_type_str = format_concrete_type(lhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));
            let rhs_type_str = format_concrete_type(rhs_type.clone(), &(self.symbol_formatter)(scope_id_opt));

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidInfix {
                    lhs_type: lhs_type_str,
                    rhs_type: rhs_type_str,
                },
                location: Some(DiagLoc::new(loc.clone())),
                hint: None,
            });
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
                    } else if let (ConcreteType::Pointer(_), ConcreteType::BasicType(BasicConcreteType::Null)) =
                        (&lhs, &rhs)
                    {
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
        loc: SourceLoc,
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
                    loc.clone()
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
                    location: Some(DiagLoc::new(loc)),
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
        loc: SourceLoc,
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
        loc: SourceLoc,
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
        loc: SourceLoc,
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
        loc: SourceLoc,
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
                            location: Some(DiagLoc::new(loc.clone())),
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
        loc: SourceLoc,
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

        let rhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut infix_expr.rhs, Some(lhs_type.clone())) {
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
        let symbol_id = match &sizeof_expr.expr.kind {
            TypedExpressionKind::ConcreteType(concrete_type) => {
                if let ConcreteType::UnresolvedSymbol(symbol_id) = concrete_type {
                    *symbol_id
                } else {
                    self.normalize_type(scope_id_opt, concrete_type.clone(), sizeof_expr.loc.clone())?;
                    return Some(ConcreteType::BasicType(BasicConcreteType::SizeT));
                }
            }
            TypedExpressionKind::Symbol(symbol_id, ..) => *symbol_id,
            _ => {
                self.analyze_typed_expr_type(scope_id_opt, &mut sizeof_expr.expr, expected_type);
                return Some(ConcreteType::BasicType(BasicConcreteType::SizeT));
            }
        };

        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, symbol_id)
            .unwrap();

        if local_or_global_symbol.as_global_var().is_some() || local_or_global_symbol.as_variable().is_some() {
            // consider as expr
            self.analyze_typed_expr_type(scope_id_opt, &mut sizeof_expr.expr, expected_type);
        } else {
            // consider as type
            self.normalize_type(
                scope_id_opt,
                ConcreteType::UnresolvedSymbol(symbol_id),
                sizeof_expr.loc.clone(),
            )?;
        }

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
                            location: Some(DiagLoc::new(prefix_expr.loc.clone())),
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
                            location: Some(DiagLoc::new(prefix_expr.loc.clone())),
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
                            location: Some(DiagLoc::new(prefix_expr.loc.clone())),
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

        if operand_type.is_const() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CannotAssignToConstLValue,
                location: Some(DiagLoc::new(unary_expr.loc.clone())),
                hint: None,
            });
            return None;
        }

        if !self.is_integer_type(operand_type.get_const_inner().clone()) {
            let operand_type = format_concrete_type(operand_type, &formatter_closure);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::InvalidUnary { operand_type },
                location: Some(DiagLoc::new(unary_expr.loc.clone())),
                hint: None,
            });
            return None;
        }

        Some(operand_type)
    }
}
