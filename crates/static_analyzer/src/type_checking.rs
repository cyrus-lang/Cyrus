use std::collections::HashMap;

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use ast::{
    LiteralKind,
    operators::{InfixOperator, PrefixOperator},
    token::{Location, TokenKind},
};
use diagcentral::{Diag, DiagLevel, DiagLoc};
use resolver::{
    declsign::FuncSig,
    scope::{LocalOrGlobalSymbol, LocalSymbol, LocalSymbolKind, ResolvedMethod, ResolvedVariable, SymbolEntryKind},
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
        if let TypedExpressionKind::MethodCall(method_call) = &mut typed_expr.kind {
            let (concrete_type, lowered_func_call) =
                self.analyze_and_lower_method_call_expr_type(scope_id_opt, method_call)?;

            typed_expr.concrete_type = Some(concrete_type.clone());
            typed_expr.kind = TypedExpressionKind::FuncCall(lowered_func_call);
            return Some(concrete_type);
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
                        self.mark_symbol_used_once(self.module_id, symbol_entry.get_symbol_id());
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
            TypedExpressionKind::MethodCall(..) => unreachable!(), // lowered to func call
        };

        let normalized_type = self.normalize_type(scope_id_opt, concrete_type.clone()?, typed_expr.get_loc());
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

        if !operand_type.is_array() {
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

        None
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
        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt.clone(), *instance_symbol_id)
            .unwrap();

        self.mark_local_symbol_used_once(local_scope_opt.unwrap(), self.module_id, *instance_symbol_id);

        let mut resolved_var_type = match match &local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => {
                let typed_variable = &local_symbol.as_variable().unwrap().typed_variable;

                match &typed_variable.ty {
                    Some(concrete_type) => {
                        self.normalize_type(scope_id_opt, concrete_type.clone(), field_access.loc.clone())
                    }
                    None => {
                        let rhs = typed_variable.rhs.clone()?;
                        self.analyze_typed_expr_type(scope_id_opt, &mut rhs.clone(), None)
                    }
                }
            }
            LocalOrGlobalSymbol::GlobalSymbol(global_symbol) => match global_symbol.as_global_var() {
                Some(resolved_global_var) => Some(resolved_global_var.global_var_sig.ty.clone().unwrap()),
                None => None,
            },
        } {
            Some(resolved_var) => resolved_var,
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

        resolved_var_type = self.normalize_type(scope_id_opt, resolved_var_type.clone(), field_access.loc.clone())?;

        let symbol_id = match resolved_var_type {
            ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(symbol_id)) => symbol_id,
            ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(symbol_id)) => symbol_id,
            ConcreteType::Pointer(inner_concrete_type) => {
                inner_concrete_type.as_struct_symbol_id().or(inner_concrete_type.as_enum_symbol_id()).unwrap()
            },
            _ => unreachable!(),
        };

        let module_id = self.resolver.lookup_symbol_id_in_modules(symbol_id).unwrap();
        let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, symbol_id).unwrap();

        let (struct_name, struct_fields, struct_symbol_id) = match match symbol_entry.as_struct() {
            Some(resolved_struct) => Some((
                resolved_struct.struct_sig.name.clone(),
                resolved_struct.struct_sig.fields.clone(),
                resolved_struct.symbol_id,
            )),
            None => match symbol_entry.as_enum() {
                Some(..) => todo!(),
                None => None,
            },
        } {
            Some(result) => result,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ObjectNotSupportsMethods,
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
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);
        let resolved_struct =
            self.resolve_symbol_as_struct(scope_id_opt, typed_struct_init.symbol_id, typed_struct_init.loc.clone())?;

        // check duplicate field inits
        let mut field_names: Vec<String> = Vec::new();
        for field_init in &typed_struct_init.fields {
            let struct_name = formatter_closure(typed_struct_init.symbol_id);

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
                let struct_name = formatter_closure(typed_struct_init.symbol_id);

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
                let struct_name = formatter_closure(typed_struct_init.symbol_id);
                let expected_type = format_concrete_type(field_target_type, &formatter_closure);
                let found_type = format_concrete_type(field_value_type, &formatter_closure);

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
            let struct_name = formatter_closure(typed_struct_init.symbol_id);

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

        match operand_type {
            ConcreteType::Pointer(concrete_type) => Some(*concrete_type),
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
        }
    }

    fn check_func_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_sig: FuncSig,
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
                    func_name: func_sig.name,
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
                    func_name: func_sig.name,
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
            match func_sig.params.variadic.clone().unwrap() {
                TypedFuncVariadicParams::Typed(_, variadic_param_type) => {
                    let static_params_len = func_sig.params.list.len();
                    let variadic_args = &mut args[static_params_len..];

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
                            argument.get_loc(),
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
                TypedFuncVariadicParams::UntypedCStyle => {}
            }
        }

        for typed_expr in args {
            self.analyze_typed_expr_type(scope_id_opt, typed_expr, typed_expr.concrete_type.clone());
        }

        self.check_duplicate_param_names(
            &func_sig.params.list,
            func_sig.params.variadic.as_ref(),
            DiagLoc::new(self.resolver.get_current_module_file_path(), loc.clone(), 0),
        );

        Some(func_sig.return_type)
    }

    fn analyze_func_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_call: &mut TypedFuncCall,
    ) -> Option<ConcreteType> {
        let module_id = self.resolver.lookup_symbol_id_in_modules(func_call.symbol_id).unwrap();

        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt.unwrap());
        let local_or_global_symbol = {
            match self
                .resolver
                .resolve_symbol_from_local_scope(local_scope_opt.clone()?, func_call.symbol_id)
            {
                Some(local_symbol) => Some(LocalOrGlobalSymbol::LocalSymbol(local_symbol)),
                None => match self
                    .resolver
                    .lookup_symbol_entry_with_id(module_id, func_call.symbol_id)
                {
                    Some(symbol_entry) => Some(LocalOrGlobalSymbol::GlobalSymbol(symbol_entry)),
                    None => None,
                },
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

        let func_sig = match func_sig_opt {
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

        self.mark_func_used(local_scope_opt?, self.module_id, func_call.symbol_id);
        self.check_func_call(scope_id_opt, func_sig, &mut func_call.args, func_call.loc.clone())
    }

    fn resolve_object_for_method_call(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &TypedMethodCall,
    ) -> Option<(String, HashMap<String, SymbolID>, ModuleID)> {
        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt?);
        let local_or_global = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, method_call.symbol_id)
            .unwrap();

        match local_or_global {
            LocalOrGlobalSymbol::LocalSymbol(mut local_symbol) => {
                self.resolve_local_object(scope_id_opt, method_call, &mut local_symbol)
            }
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => {
                let resolved_struct = symbol_entry.as_struct().unwrap().clone();
                self.mark_symbol_used_once(symbol_entry.get_module_id(), resolved_struct.symbol_id);
                Some((
                    resolved_struct.struct_sig.name,
                    resolved_struct.struct_sig.methods,
                    symbol_entry.get_module_id(),
                ))
            }
        }
    }

    fn resolve_local_object(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &TypedMethodCall,
        local_symbol: &mut LocalSymbol,
    ) -> Option<(String, HashMap<String, SymbolID>, ModuleID)> {
        match &mut local_symbol.kind {
            LocalSymbolKind::Variable(resolved_var) => {
                let var_type = match &mut resolved_var.typed_variable.ty {
                    Some(concrete_type) => {
                        self.normalize_type(scope_id_opt, concrete_type.clone(), method_call.loc.clone())
                    }
                    None => {
                        let rhs = resolved_var.typed_variable.rhs.as_mut()?;
                        self.analyze_typed_expr_type(scope_id_opt, rhs, None)
                    }
                }
                .unwrap();

                let symbol_id = match var_type {
                    ConcreteType::ResolvedSymbol(ResolvedSymbol::NamedStruct(symbol_id)) => symbol_id,
                    ConcreteType::ResolvedSymbol(ResolvedSymbol::Enum(symbol_id)) => symbol_id,
                    _ => unreachable!(),
                };

                let module_id = self.resolver.lookup_symbol_id_in_modules(symbol_id).unwrap();
                let symbol_entry = self.resolver.lookup_symbol_entry_with_id(module_id, symbol_id).unwrap();
                match symbol_entry.as_struct() {
                    Some(resolved_struct) => Some((
                        resolved_struct.struct_sig.name.clone(),
                        resolved_struct.struct_sig.methods.clone(),
                        module_id,
                    )),
                    None => match symbol_entry.as_enum() {
                        Some(resolved_enum) => Some((
                            resolved_enum.enum_sig.name.clone(),
                            resolved_enum.enum_sig.methods.clone(),
                            module_id,
                        )),
                        None => {
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
                            None
                        }
                    },
                }
            }
            LocalSymbolKind::Struct(resolved_struct) => Some((
                resolved_struct.struct_sig.name.clone(),
                resolved_struct.struct_sig.methods.clone(),
                self.module_id,
            )),
            LocalSymbolKind::Enum(resolved_enum) => Some((
                resolved_enum.enum_sig.name.clone(),
                resolved_enum.enum_sig.methods.clone(),
                self.module_id,
            )),
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
                None
            }
        }
    }

    fn analyze_and_lower_method_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        method_call: &mut TypedMethodCall,
    ) -> Option<(ConcreteType, TypedFuncCall)> {
        let method_name = method_call.method_name.clone();
        let loc = method_call.loc.clone();

        let (object_name, object_methods, object_module_id) =
            self.resolve_object_for_method_call(scope_id_opt, method_call)?;

        let method_symbol_id = match object_methods.get(&method_name) {
            Some(symbol_id) => *symbol_id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::StructMethodNotDefined {
                        struct_name: object_name.clone(),
                        method_name: method_name.clone(),
                    },
                    location: Some(DiagLoc::new(self.resolver.get_current_module_file_path(), loc, 0)),
                    hint: None,
                });
                return None;
            }
        };

        let method_symbol_entry = self
            .resolver
            .lookup_symbol_entry_with_id(object_module_id, method_symbol_id)
            .unwrap();

        let resolved_method = method_symbol_entry.as_method().unwrap();

        if !self.validate_method_call(resolved_method, method_call) {
            return None;
        }

        let mut func_call = self.lower_to_func_call(resolved_method, method_call, method_symbol_id);

        let local_scope_opt = self.resolver.get_scope_ref(self.module_id, scope_id_opt?);
        self.mark_func_used(local_scope_opt?, self.module_id, method_symbol_id);

        let concrete_type = self.check_func_call(
            scope_id_opt,
            resolved_method.func_sig.clone(),
            &mut func_call.args,
            func_call.loc.clone(),
        )?;

        Some((concrete_type, func_call))
    }

    fn report_invalid_fat_arrow(&mut self, loc: &Location) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::InvalidFatArrow,
            location: Some(DiagLoc::new(
                self.resolver.get_current_module_file_path(),
                loc.clone(),
                0,
            )),
            hint: None,
        });
    }

    fn report_invalid_fat_arrow_pointer(&mut self, loc: &Location) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: AnalyzerDiagKind::InvalidFatArrow,
            location: Some(DiagLoc::new(
                self.resolver.get_current_module_file_path(),
                loc.clone(),
                0,
            )),
            hint: Some("The fat arrow operator '->' can only be applied to pointers.".to_string()),
        });
    }

    fn validate_method_call(&mut self, resolved_method: &ResolvedMethod, method_call: &TypedMethodCall) -> bool {
        if !resolved_method.is_instance_method() && method_call.is_fat_arrow {
            self.report_invalid_fat_arrow(&method_call.loc);
            return false;
        }

        if method_call.is_fat_arrow && !method_call.operand.concrete_type.clone().unwrap().is_pointer() {
            self.report_invalid_fat_arrow_pointer(&method_call.loc);
            return false;
        }

        true
    }

    fn lower_to_func_call(
        &self,
        resolved_method: &ResolvedMethod,
        method_call: &mut TypedMethodCall,
        method_symbol_id: SymbolID,
    ) -> TypedFuncCall {
        if resolved_method.is_instance_method() {
            // self modifier value
            method_call.args.insert(0, *method_call.operand.clone());

            TypedFuncCall {
                symbol_id: method_symbol_id,
                args: method_call.args.clone(),
                loc: method_call.loc.clone(),
            }
        } else {
            TypedFuncCall {
                symbol_id: method_symbol_id,
                args: method_call.args.clone(),
                loc: method_call.loc.clone(),
            }
        }
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
                element.get_loc(),
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
            PrefixOperator::SizeOf => Some(ConcreteType::BasicType(BasicConcreteType::SizeT)),
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
