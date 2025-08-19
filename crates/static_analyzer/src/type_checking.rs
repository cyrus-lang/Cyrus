use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use ast::{
    LiteralKind,
    operators::{InfixOperator, PrefixOperator},
    token::{Location, TokenKind},
};
use diagcentral::{Diag, DiagLevel, DiagLoc};
use resolver::scope::{LocalOrGlobalSymbol, SymbolEntryKind};
use typed_ast::{
    ScopeID, SymbolID, TypedAddressOf, TypedArray, TypedArrayIndex, TypedCast, TypedDereference, TypedExpression,
    TypedExpressionKind, TypedFuncCall, TypedFuncVariadicParams, TypedInfixExpression, TypedLiteral,
    TypedPrefixExpression, TypedStructInit, TypedUnaryExpression, TypedUnnamedStructValue,
    format::format_concrete_type,
    types::{
        BasicConcreteType::{self, *},
        ConcreteType, ResolvedSymbol, TypedArrayCapacity, TypedUnnamedStructType, TypedUnnamedStructTypeField,
    },
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_type_mismatch(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        value_type: ConcreteType,
        target_type: ConcreteType,
        loc: Location,
    ) -> bool {
        match (value_type, target_type) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                self.check_basic_type_mismatch(basic_concrete_type1, basic_concrete_type2)
            }
            (ConcreteType::Const(inner_concrete_type1), ConcreteType::Const(inner_concrete_type2)) => {
                self.check_type_mismatch(scope_id_opt, *inner_concrete_type1, *inner_concrete_type2, loc)
            }
            (ConcreteType::Const(inner_concrete_type1), concrete_type2) => {
                self.check_type_mismatch(scope_id_opt, *inner_concrete_type1, concrete_type2, loc)
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

    pub(crate) fn is_basic_concrete_type_integer(&self, basic_concrete_type: BasicConcreteType) -> bool {
        matches!(
            basic_concrete_type,
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

    pub(crate) fn is_basic_concrete_type_float(&self, basic_concrete_type: BasicConcreteType) -> bool {
        matches!(
            basic_concrete_type,
            BasicConcreteType::Float16
                | BasicConcreteType::Float32
                | BasicConcreteType::Float64
                | BasicConcreteType::Float128
        )
    }

    pub(crate) fn analyze_typed_literal_type(&mut self, typed_literal: &TypedLiteral) -> Option<ConcreteType> {
        let check_integer_invalid_suffix = |this: &mut AnalysisContext, suffix_opt: &Option<Box<TokenKind>>| {
            if let Some(suffix) = suffix_opt {
                let valid_integer_suffixes: &[TokenKind] = &[
                    TokenKind::UIntPtr,
                    TokenKind::IntPtr,
                    TokenKind::SizeT,
                    TokenKind::Int,
                    TokenKind::Int8,
                    TokenKind::Int16,
                    TokenKind::Int32,
                    TokenKind::Int64,
                    TokenKind::Int128,
                    TokenKind::UInt,
                    TokenKind::UInt8,
                    TokenKind::UInt16,
                    TokenKind::UInt32,
                    TokenKind::UInt64,
                    TokenKind::UInt128,
                ];

                if !valid_integer_suffixes.contains(&suffix) {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::InvalidIntegerLiteralSuffix,
                        location: Some(DiagLoc::new(
                            this.resolver.get_current_module_file_path(),
                            typed_literal.loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                }
            }
        };

        let check_float_invalid_suffix = |this: &mut AnalysisContext, suffix_opt: &Option<Box<TokenKind>>| {
            if let Some(suffix) = suffix_opt {
                let valid_float_suffixes: &[TokenKind] = &[
                    TokenKind::Float16,
                    TokenKind::Float32,
                    TokenKind::Float64,
                    TokenKind::Float128,
                ];

                if !valid_float_suffixes.contains(&suffix) {
                    this.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: AnalyzerDiagKind::InvalidFloatLiteralSuffix,
                        location: Some(DiagLoc::new(
                            this.resolver.get_current_module_file_path(),
                            typed_literal.loc.clone(),
                            0,
                        )),
                        hint: None,
                    });
                }
            }
        };

        match &typed_literal.kind {
            LiteralKind::Integer(_, suffix_opt) => {
                check_integer_invalid_suffix(self, suffix_opt);
            }
            LiteralKind::Float(_, suffix_opt) => {
                check_float_invalid_suffix(self, suffix_opt);
            }
            LiteralKind::Bool(_) => {}
            LiteralKind::String(_, _) => {}
            LiteralKind::Char(_) => {}
            LiteralKind::Null => {}
        };

        Some(typed_literal.ty.clone())
    }

    pub(crate) fn analyze_typed_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &mut TypedExpression,
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
                        self.mark_local_symbol_used_once(local_scope_opt, self.module_id, *symbol_id);
                    }
                    LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => {
                        self.mark_symbol_used_once(self.module_id, symbol_entry.get_symbol_id());
                    }
                }

                self.resolve_full_type_from_local_or_global_symbol(scope_id_opt, local_or_global_symbol)
            }
            TypedExpressionKind::Literal(typed_literal) => self.analyze_typed_literal_type(typed_literal),
            TypedExpressionKind::Prefix(typed_prefix_expr) => {
                self.analyze_prefix_expr_type(scope_id_opt, typed_prefix_expr)
            }
            TypedExpressionKind::Infix(typed_infix_expr) => {
                self.analyze_infix_expr_type(scope_id_opt, typed_infix_expr)
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
            TypedExpressionKind::FieldAccess(typed_field_access) => todo!(),
            TypedExpressionKind::MethodCall(typed_method_call) => todo!(),
            TypedExpressionKind::UnnamedStructValue(typed_unnamed_struct_value) => {
                self.analyze_unnamed_struct_value_expr_type(scope_id_opt, typed_unnamed_struct_value)
            }
        };

        let normalized_type = self.normalize_type(scope_id_opt, concrete_type.clone()?, typed_expr.get_loc());
        typed_expr.concrete_type = normalized_type;

        if cfg!(debug_assertions) {
            if let Some(concrete_type_clone) = typed_expr.concrete_type.clone() {
                let is_unresolved_symbol = matches!(concrete_type_clone, ConcreteType::UnresolvedSymbol(..));
                assert!(is_unresolved_symbol == false);
            }
        }

        typed_expr.concrete_type.clone()
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
            let field_value_type = match self.analyze_typed_expr_type(scope_id_opt, &mut field.field_value) {
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
        todo!(); // uncompleted due to parser errors

        // let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        // let is_operand_array = match &array_index.operand.kind {
        //     TypedExpressionKind::Symbol(symbol_id, ..) => {
        //         self.get_definite_array_concrete_type(scope_id_opt, *symbol_id, array_index.loc.clone())
        //     }
        //     _ => match self.analyze_typed_expr_type(scope_id_opt, &mut array_index.operand) {
        //         Some(concrete_type) => match concrete_type {
        //             ConcreteType::Symbol(symbol_id) => {
        //                 self.get_definite_array_concrete_type(scope_id_opt, symbol_id, array_index.loc.clone())
        //             }
        //             ConcreteType::Array(typed_array_type) => Some(typed_array_type),
        //             _ => None,
        //         },
        //         None => None,
        //     },
        // }
        // .is_some();

        // if !is_operand_array {
        //     // FIXME Show the exact type of the array-index operand.

        //     self.reporter.report(Diag {
        //         level: DiagLevel::Error,
        //         kind: AnalyzerDiagKind::ArrayIndexOnNonArrayOperand,
        //         location: Some(DiagLoc::new(
        //             self.resolver.get_current_module_file_path(),
        //             array_index.loc.clone(),
        //             0,
        //         )),
        //         hint: None,
        //     });
        // }

        // let index_concrete_type = match self.analyze_typed_expr_type(scope_id_opt, &mut array_index.index) {
        //     Some(concrete_type) => Some(concrete_type),
        //     None => None,
        // };

        // let is_index_integer_value = match &index_concrete_type {
        //     Some(concrete_type) => {
        //         match match concrete_type {
        //             ConcreteType::Symbol(symbol_id) => {
        //                 self.get_definite_basic_concrete_type(scope_id_opt, *symbol_id, array_index.loc.clone())
        //             }
        //             ConcreteType::BasicType(basic_concrete_type) => Some(basic_concrete_type.clone()),
        //             _ => None,
        //         } {
        //             Some(basic_concrete_type) => self.is_basic_concrete_type_integer(basic_concrete_type.clone()),
        //             None => false,
        //         }
        //     }
        //     None => false,
        // };

        // if !is_index_integer_value {
        //     let found_type = format_concrete_type(index_concrete_type.unwrap(), &formatter_closure);

        //     self.reporter.report(Diag {
        //         level: DiagLevel::Error,
        //         kind: AnalyzerDiagKind::ArrayNonIntegerIndex { found_type },
        //         location: Some(DiagLoc::new(
        //             self.resolver.get_current_module_file_path(),
        //             array_index.loc.clone(),
        //             0,
        //         )),
        //         hint: None,
        //     });
        // }

        // None // FIXME
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

            let field_value_type = match self.analyze_typed_expr_type(scope_id_opt, &mut field_init.value) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            let field_target_type = field.unwrap().ty.clone();

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

        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut address_of.operand) {
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
        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut dereference.operand) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        match operand_type {
            ConcreteType::Pointer(concrete_type) => Some(ConcreteType::Pointer(concrete_type)),
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
        func_call: &mut TypedFuncCall,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let local_scope_opt = {
            if let Some(scope_id) = scope_id_opt {
                Some(self.resolver.get_scope_ref(self.module_id, scope_id).unwrap())
            } else {
                None
            }
        };

        let local_or_global_symbol = self
            .resolver
            .resolve_local_or_global_symbol(local_scope_opt, func_call.symbol_id)
            .unwrap();

        let func_sig_opt = match local_or_global_symbol {
            resolver::scope::LocalOrGlobalSymbol::LocalSymbol(_) => None,
            resolver::scope::LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => match symbol_entry.kind {
                SymbolEntryKind::Func(resolved_function) => Some(resolved_function.func_sig),
                _ => None,
            },
        };

        let func_sig = match func_sig_opt {
            Some(func_sig) => func_sig,
            None => {
                let func_name = formatter_closure(func_call.symbol_id);

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

        let is_variadic = func_sig.params.variadic.is_some();

        if !is_variadic && func_call.args.len() != func_sig.params.list.len() {
            let func_name = formatter_closure(func_call.symbol_id);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: func_call.args.len() as u32,
                    expected: func_sig.params.list.len() as u32,
                    func_name,
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    func_call.loc.clone(),
                    0,
                )),
                hint: None,
            });
            return None;
        } else if is_variadic && func_call.args.len() < func_sig.params.list.len() {
            let func_name = formatter_closure(func_call.symbol_id);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: func_call.args.len() as u32,
                    expected: func_sig.params.list.len() as u32,
                    func_name,
                },
                location: Some(DiagLoc::new(
                    self.resolver.get_current_module_file_path(),
                    func_call.loc.clone(),
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
                    let variadic_args = &mut func_call.args[static_params_len..];

                    for (argument_idx, argument) in variadic_args.iter_mut().enumerate() {
                        let argument_type = match self.analyze_typed_expr_type(scope_id_opt, argument) {
                            Some(concrete_type) => concrete_type,
                            None => continue,
                        };

                        if !self.check_type_mismatch(
                            scope_id_opt,
                            argument_type.clone(),
                            variadic_param_type.clone(),
                            argument.get_loc(),
                        ) {
                            let param_type = format_concrete_type(variadic_param_type.clone(), &formatter_closure);
                            let argument_type = format_concrete_type(argument_type, &formatter_closure);

                            self.reporter.report(Diag {
                                level: DiagLevel::Error,
                                kind: AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                    param_type,
                                    argument_type,
                                    argument_idx: (argument_idx + static_params_len) as u32,
                                },
                                location: Some(DiagLoc::new(
                                    self.resolver.get_current_module_file_path(),
                                    func_call.loc.clone(),
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

        self.check_duplicate_param_names(
            &func_sig.params.list,
            func_sig.params.variadic.as_ref(),
            DiagLoc::new(self.resolver.get_current_module_file_path(), func_call.loc.clone(), 0),
        );

        Some(func_sig.return_type)
    }

    fn analyze_func_call_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        func_call: &mut TypedFuncCall,
    ) -> Option<ConcreteType> {
        self.check_func_call(scope_id_opt, func_call)
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
            let element_type = match self.analyze_typed_expr_type(scope_id_opt, element) {
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

        let operand = match self.analyze_typed_expr_type(scope_id_opt, &mut cast.operand) {
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
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let valid_concrete_type = match (lhs_type.clone(), rhs_type.clone()) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                if (self.is_basic_concrete_type_integer(basic_concrete_type1.clone())
                    && self.is_basic_concrete_type_integer(basic_concrete_type2.clone()))
                    || (self.is_basic_concrete_type_float(basic_concrete_type1.clone())
                        && self.is_basic_concrete_type_integer(basic_concrete_type2.clone()))
                {
                    Some(BasicConcreteType::bigger_type(basic_concrete_type1, basic_concrete_type2).unwrap())
                } else {
                    None
                }
            }

            _ => None,
        };

        match valid_concrete_type {
            Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type)),
            None => {
                let lhs_type = format_concrete_type(lhs_type, &formatter_closure);
                let rhs_type = format_concrete_type(rhs_type, &formatter_closure);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidInfix { lhs_type, rhs_type },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        }
    }

    fn analyze_compare_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        cmp_eq: bool,
        loc: Location,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let valid_concrete_type = match (lhs_type.clone(), rhs_type.clone()) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                if self.is_basic_concrete_type_integer(basic_concrete_type1.clone())
                    && self.is_basic_concrete_type_integer(basic_concrete_type2.clone())
                {
                    Some(BasicConcreteType::Bool)
                } else if self.is_basic_concrete_type_float(basic_concrete_type1.clone())
                    && self.is_basic_concrete_type_float(basic_concrete_type2.clone())
                {
                    Some(BasicConcreteType::Bool)
                } else {
                    None
                }
            }
            (ConcreteType::Const(inner_concrete_type1), ConcreteType::Const(inner_concrete_type2)) => {
                if *inner_concrete_type1 == *inner_concrete_type2 {
                    Some(BasicConcreteType::Bool)
                } else {
                    None
                }
            }
            (ConcreteType::Const(inner_concrete_type), concrete_type) => {
                if *inner_concrete_type == concrete_type {
                    Some(BasicConcreteType::Bool)
                } else {
                    None
                }
            }
            (concrete_type, ConcreteType::Const(inner_concrete_type)) => {
                if *inner_concrete_type == concrete_type {
                    Some(BasicConcreteType::Bool)
                } else {
                    None
                }
            }
            _ => {
                if cmp_eq {
                    match (lhs_type.clone(), rhs_type.clone()) {
                        (ConcreteType::Pointer(inner_concrete_type1), ConcreteType::Pointer(inner_concrete_type2)) => {
                            if self.check_type_mismatch(
                                scope_id_opt,
                                *inner_concrete_type1,
                                *inner_concrete_type2,
                                loc.clone(),
                            ) {
                                Some(BasicConcreteType::Bool)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
        };

        match valid_concrete_type {
            Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type)),
            None => {
                let lhs_type = format_concrete_type(lhs_type, &formatter_closure);
                let rhs_type = format_concrete_type(rhs_type, &formatter_closure);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidInfix { lhs_type, rhs_type },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
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
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let valid_concrete_type = match (lhs_type.clone(), rhs_type.clone()) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                if basic_concrete_type1.is_bool() && basic_concrete_type2.is_bool() {
                    Some(BasicConcreteType::Bool)
                } else {
                    None
                }
            }
            (ConcreteType::Const(inner_concrete_type1), ConcreteType::Const(inner_concrete_type2)) => {
                match (
                    inner_concrete_type1.as_basic_type(),
                    inner_concrete_type2.as_basic_type(),
                ) {
                    (Some(basic_concrete_type1), Some(basic_concrete_type2)) => {
                        if basic_concrete_type1.is_bool() && basic_concrete_type2.is_bool() {
                            Some(BasicConcreteType::Bool)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            (ConcreteType::Const(inner_concrete_type), concrete_type) => {
                match (inner_concrete_type.as_basic_type(), concrete_type.as_basic_type()) {
                    (Some(basic_concrete_type1), Some(basic_concrete_type2)) => {
                        if basic_concrete_type1.is_bool() && basic_concrete_type2.is_bool() {
                            Some(BasicConcreteType::Bool)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            (concrete_type, ConcreteType::Const(inner_concrete_type)) => {
                match (inner_concrete_type.as_basic_type(), concrete_type.as_basic_type()) {
                    (Some(basic_concrete_type1), Some(basic_concrete_type2)) => {
                        if basic_concrete_type1.is_bool() && basic_concrete_type2.is_bool() {
                            Some(BasicConcreteType::Bool)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            (ConcreteType::Pointer(inner_concrete_type1), ConcreteType::Pointer(inner_concrete_type2)) => {
                if self.check_type_mismatch(scope_id_opt, *inner_concrete_type1, *inner_concrete_type2, loc.clone()) {
                    Some(BasicConcreteType::Bool)
                } else {
                    None
                }
            }
            _ => None,
        };

        match valid_concrete_type {
            Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type)),
            None => {
                let lhs_type = format_concrete_type(lhs_type, &formatter_closure);
                let rhs_type = format_concrete_type(rhs_type, &formatter_closure);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidInfix { lhs_type, rhs_type },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        }
    }

    fn analyze_and_expr(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        lhs_type: ConcreteType,
        rhs_type: ConcreteType,
        loc: Location,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let valid_concrete_type = match (lhs_type.clone(), rhs_type.clone()) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                if basic_concrete_type1.is_bool() && basic_concrete_type2.is_bool() {
                    Some(BasicConcreteType::Bool)
                } else {
                    None
                }
            }
            (ConcreteType::Const(inner_concrete_type1), ConcreteType::Const(inner_concrete_type2)) => {
                match (
                    inner_concrete_type1.as_basic_type(),
                    inner_concrete_type2.as_basic_type(),
                ) {
                    (Some(basic_concrete_type1), Some(basic_concrete_type2)) => {
                        if basic_concrete_type1.is_bool() && basic_concrete_type2.is_bool() {
                            Some(BasicConcreteType::Bool)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            (ConcreteType::Const(inner_concrete_type), concrete_type) => {
                match (inner_concrete_type.as_basic_type(), concrete_type.as_basic_type()) {
                    (Some(basic_concrete_type1), Some(basic_concrete_type2)) => {
                        if basic_concrete_type1.is_bool() && basic_concrete_type2.is_bool() {
                            Some(BasicConcreteType::Bool)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            (concrete_type, ConcreteType::Const(inner_concrete_type)) => {
                match (inner_concrete_type.as_basic_type(), concrete_type.as_basic_type()) {
                    (Some(basic_concrete_type1), Some(basic_concrete_type2)) => {
                        if basic_concrete_type1.is_bool() && basic_concrete_type2.is_bool() {
                            Some(BasicConcreteType::Bool)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        match valid_concrete_type {
            Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type)),
            None => {
                let lhs_type = format_concrete_type(lhs_type, &formatter_closure);
                let rhs_type = format_concrete_type(rhs_type, &formatter_closure);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidInfix { lhs_type, rhs_type },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        }
    }

    fn analyze_infix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        infix_expr: &mut TypedInfixExpression,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let lhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut infix_expr.lhs) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let rhs_type = match self.analyze_typed_expr_type(scope_id_opt, &mut infix_expr.rhs) {
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

            InfixOperator::Equal => {
                self.analyze_compare_expr(scope_id_opt, lhs_type, rhs_type, true, infix_expr.loc.clone())
            }
            InfixOperator::NotEqual => {
                self.analyze_compare_expr(scope_id_opt, lhs_type, rhs_type, true, infix_expr.loc.clone())
            }
            InfixOperator::Or => self.analyze_or_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone()),
            InfixOperator::And => self.analyze_and_expr(scope_id_opt, lhs_type, rhs_type, infix_expr.loc.clone()),
        }
    }

    fn analyze_prefix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        prefix_expr: &mut TypedPrefixExpression,
    ) -> Option<ConcreteType> {
        let formatter_closure: Box<dyn Fn(SymbolID) -> String + 'a> = (self.symbol_formatter)(scope_id_opt);

        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut prefix_expr.operand) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        match prefix_expr.op {
            PrefixOperator::SizeOf => Some(ConcreteType::BasicType(BasicConcreteType::SizeT)),
            PrefixOperator::Bang => {
                let valid_concrete_type = match &operand_type {
                    ConcreteType::BasicType(basic_concrete_type) => match basic_concrete_type {
                        BasicConcreteType::Bool => Some(ConcreteType::BasicType(basic_concrete_type.clone())),
                        _ => None,
                    },
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(concrete_type),
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
                    ConcreteType::BasicType(basic_concrete_type) => match basic_concrete_type {
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
                        | BasicConcreteType::Float16
                        | BasicConcreteType::Float32
                        | BasicConcreteType::Float64
                        | BasicConcreteType::Float128 => Some(ConcreteType::BasicType(basic_concrete_type.clone())),
                        _ => None,
                    },
                    _ => None,
                };

                match valid_concrete_type {
                    Some(concrete_type) => Some(concrete_type),
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

        let operand_type = match self.analyze_typed_expr_type(scope_id_opt, &mut unary_expr.operand) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let valid_operand_type = match &operand_type {
            ConcreteType::BasicType(basic_concrete_type) => {
                if self.is_basic_concrete_type_integer(basic_concrete_type.clone()) {
                    Some(basic_concrete_type)
                } else {
                    None
                }
            }
            _ => None,
        };

        match valid_operand_type {
            Some(concrete_type) => Some(ConcreteType::BasicType(concrete_type.clone())),
            None => {
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
        }
    }
}
