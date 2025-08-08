use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use ast::{LiteralKind, operators::PrefixOperator};
use diagcentral::{Diag, DiagLevel, DiagLoc};
use typed_ast::{
    ScopeID, SymbolID, TypedArray, TypedCast, TypedExpression, TypedInfixExpression, TypedPrefixExpression,
    TypedUnaryExpression,
    format::format_concrete_type,
    types::{
        BasicConcreteType::{self, *},
        ConcreteType, TypedArrayCapacity, TypedArrayType,
    },
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_type_mismatch(&self, value_type: ConcreteType, target_type: ConcreteType) -> bool {
        match (value_type, target_type) {
            (ConcreteType::BasicType(basic_concrete_type1), ConcreteType::BasicType(basic_concrete_type2)) => {
                self.check_basic_type_mismatch(basic_concrete_type1, basic_concrete_type2)
            }
            (ConcreteType::Const(inner_concrete_type1), ConcreteType::Const(inner_concrete_type2)) => {
                self.check_type_mismatch(*inner_concrete_type1, *inner_concrete_type2)
            }
            (ConcreteType::Const(inner_concrete_type1), concrete_type2) => {
                self.check_type_mismatch(*inner_concrete_type1, concrete_type2)
            }
            (concrete_type1, ConcreteType::Const(inner_concrete_type2)) => {
                self.check_type_mismatch(concrete_type1, *inner_concrete_type2)
            }
            (ConcreteType::Array(array_type1), ConcreteType::Array(array_type2)) => {
                let capacity = {
                    match (array_type1.capacity, array_type2.capacity) {
                        (TypedArrayCapacity::Fixed(size1), TypedArrayCapacity::Fixed(size2)) => size1 == size2,
                        (TypedArrayCapacity::Dynamic, TypedArrayCapacity::Dynamic) => true,
                        _ => false,
                    }
                };

                capacity && self.check_type_mismatch(*array_type1.element_type, *array_type2.element_type)
            }
            (ConcreteType::Pointer(inner_concrete_type1), ConcreteType::Pointer(inner_concrete_type2)) => {
                self.check_type_mismatch(*inner_concrete_type1, *inner_concrete_type2)
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
            (ConcreteType::Symbol(symbol_id), concrete_type) => {
                todo!();
            }
            (concrete_type, ConcreteType::Symbol(symbol_id)) => {
                todo!();
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

    pub(crate) fn get_symbol_formatter(&self) -> Box<&dyn Fn(SymbolID) -> String> {
        Box::new(&move |symbol_id: SymbolID| -> String {
            todo!();
        })
    }

    pub(crate) fn get_typed_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        typed_expr: &TypedExpression,
    ) -> Option<ConcreteType> {
        let scope_id = scope_id_opt.unwrap();

        match typed_expr {
            TypedExpression::Symbol(symbol_id) => {
                let local_scope_ref = self.resolver.get_scope_ref(self.module_id, scope_id).unwrap();
                let local_or_global_symbol = self
                    .resolver
                    .resolve_local_or_global_symbol(self.module_id, Some(local_scope_ref), *symbol_id)
                    .unwrap();

                self.get_type_from_local_or_global_symbol(scope_id_opt, local_or_global_symbol)
            }
            TypedExpression::Literal(literal) => match &literal.kind {
                LiteralKind::Integer(_) => Some(ConcreteType::BasicType(BasicConcreteType::Int)),
                LiteralKind::Float(_) => Some(ConcreteType::BasicType(BasicConcreteType::Float32)),
                LiteralKind::Bool(_) => Some(ConcreteType::BasicType(BasicConcreteType::Bool)),
                LiteralKind::String(string_value, string_prefix) => {
                    if let Some(string_prefix) = string_prefix {
                        match string_prefix {
                            ast::StringPrefix::B => {
                                let len = string_value.len() + 1;
                                return Some(ConcreteType::Array(TypedArrayType {
                                    element_type: Box::new(ConcreteType::BasicType(BasicConcreteType::Char)),
                                    capacity: TypedArrayCapacity::Fixed(len.try_into().unwrap()),
                                    loc: literal.loc.clone(),
                                }));
                            }
                            ast::StringPrefix::C => {}
                        }
                    }
                    Some(ConcreteType::Pointer(Box::new(ConcreteType::BasicType(
                        BasicConcreteType::Char,
                    ))))
                }
                LiteralKind::Char(_) => Some(ConcreteType::BasicType(BasicConcreteType::Char)),
                LiteralKind::Null => Some(ConcreteType::BasicType(BasicConcreteType::Null)),
            },
            TypedExpression::Prefix(typed_prefix_expr) => self.get_prefix_expr_type(scope_id_opt, typed_prefix_expr),
            TypedExpression::Infix(typed_infix_expr) => self.get_infix_expr_type(scope_id_opt, typed_infix_expr),
            TypedExpression::Unary(typed_unary_expr) => self.get_unary_expr_type(scope_id_opt, typed_unary_expr),
            TypedExpression::Assignment(typed_assignment) => {
                self.analyze_assignment(scope_id_opt, typed_assignment);
                None
            }
            TypedExpression::Cast(typed_cast) => self.get_cast_expr_type(scope_id_opt, typed_cast),
            TypedExpression::Array(typed_array) => self.get_array_expr_type(scope_id_opt, typed_array),
            TypedExpression::ArrayIndex(typed_array_index) => todo!(),
            TypedExpression::AddressOf(typed_address_of) => todo!(),
            TypedExpression::Dereference(typed_dereference) => todo!(),
            TypedExpression::StructInit(typed_struct_init) => todo!(),
            TypedExpression::FuncCall(typed_func_call) => todo!(),
            TypedExpression::FieldAccess(typed_field_access) => todo!(),
            TypedExpression::MethodCall(typed_method_call) => todo!(),
            TypedExpression::UnnamedStructValue(typed_unnamed_struct_value) => todo!(),
        }
    }

    fn get_array_expr_type(&mut self, scope_id_opt: Option<ScopeID>, typed_array: &TypedArray) -> Option<ConcreteType> {
        let typed_array_type = match &typed_array.array_type {
            ConcreteType::Array(typed_array_type) => typed_array_type,
            _ => unreachable!(),
        };

        if let TypedArrayCapacity::Fixed(fixed_capacity) = typed_array_type.capacity {
            if typed_array.elements.len() != fixed_capacity.try_into().unwrap() {}
        }

        for (element_index, element) in typed_array.elements.iter().enumerate() {
            let element_type = match self.get_typed_expr_type(scope_id_opt, element) {
                Some(concrete_type) => concrete_type,
                None => continue,
            };

            if !self.check_type_mismatch(element_type.clone(), *typed_array_type.element_type.clone()) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::ArrayElementTypeMismatch {
                        element_type: format_concrete_type(element_type, self.get_symbol_formatter()),
                        element_index: element_index.try_into().unwrap(),
                        expected_type: format_concrete_type(
                            *typed_array_type.element_type.clone(),
                            self.get_symbol_formatter(),
                        ),
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

    fn get_cast_expr_type(&mut self, scope_id_opt: Option<ScopeID>, cast: &TypedCast) -> Option<ConcreteType> {
        let operand = match self.get_typed_expr_type(scope_id_opt, &cast.operand) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        if !self.check_type_mismatch(operand.clone(), cast.target_type.clone()) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: AnalyzerDiagKind::CastTypeMismatch {
                    lhs_type: format_concrete_type(cast.target_type.clone(), self.get_symbol_formatter()),
                    rhs_type: format_concrete_type(operand, self.get_symbol_formatter()),
                },
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

    fn get_infix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        infix_expr: &TypedInfixExpression,
    ) -> Option<ConcreteType> {
        let lhs_type = match self.get_typed_expr_type(scope_id_opt, &infix_expr.lhs) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

        let rhs_type = match self.get_typed_expr_type(scope_id_opt, &infix_expr.rhs) {
            Some(concrete_type) => concrete_type,
            None => return None,
        };

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
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidInfix {
                        lhs_type: format_concrete_type(lhs_type, self.get_symbol_formatter()),
                        rhs_type: format_concrete_type(rhs_type, self.get_symbol_formatter()),
                    },
                    location: Some(DiagLoc::new(
                        self.resolver.get_current_module_file_path(),
                        infix_expr.loc.clone(),
                        0,
                    )),
                    hint: None,
                });
                return None;
            }
        }
    }

    fn get_prefix_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        prefix_expr: &TypedPrefixExpression,
    ) -> Option<ConcreteType> {
        let operand_type = match self.get_typed_expr_type(scope_id_opt, &prefix_expr.operand) {
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
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixBangOnNonBool {
                                operand_type: format_concrete_type(operand_type, self.get_symbol_formatter()),
                            },
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
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: AnalyzerDiagKind::PrefixMinusOnNonInteger {
                                operand_type: format_concrete_type(operand_type, self.get_symbol_formatter()),
                            },
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

    fn get_unary_expr_type(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        unary_expr: &TypedUnaryExpression,
    ) -> Option<ConcreteType> {
        let operand_type = match self.get_typed_expr_type(scope_id_opt, &unary_expr.operand) {
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
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: AnalyzerDiagKind::InvalidUnary {
                        operand_type: format_concrete_type(operand_type, self.get_symbol_formatter()),
                    },
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
