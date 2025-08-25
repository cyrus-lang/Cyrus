use std::fmt;

#[derive(Debug, Clone)]
pub enum AnalyzerDiagKind {
    ObjectNotSupportsMethods, 
    InvalidFatArrow,
    StructMethodNotDefined {
        struct_name: String,
        method_name: String,
    },
    RhsOfShiftMustBeUnsignedInteger,
    CyclicTypeDefinition {
        symbol: String,
    },
    MissingReturn,
    VoidFunctionReturnsValue,
    UnreachableCode,
    InvalidBreakStatement,
    InvalidContinueStatement,
    ConditionExprMustBeOfTypeBool,
    InternalInterfaceIsNotValid,
    NamingConv {
        kind: String,
        name: String,
        expected: String,
    },
    UnusedSymbol {
        symbol_name: String,
    },
    GlobalVariableExprNotComptimeValid,
    CannotAssignToConstLValue,
    InvalidIntegerLiteralSuffix,
    InvalidFloatLiteralSuffix,
    ArrayNonIntegerIndex {
        found_type: String,
    },
    ArrayIndexOnNonArrayOperand,
    StructHasNoFieldNamed {
        struct_name: String,
        field_name: String,
    },
    StructMissingFields {
        struct_name: String,
        missing_field_names: Vec<String>,
    },
    StructFieldTypeMismatch {
        struct_name: String,
        field_name: String,
        expected_type: String,
        found_type: String,
    },
    ReturnStatementTypeMismatch {
        expected: String,
        got: String,
    },
    ReturnStatementNeedsAnArgument {
        argument_type: String,
    },
    AssignmentTypeMismatch {
        lhs_type: String,
        rhs_type: String,
    },
    ArrayElementsCountMismatch {
        elements: u32,
        expected: u32,
    },
    FuncCallArgsCountMismatch {
        args: u32,
        expected: u32,
        func_name: String,
    },
    FuncCallVariadicParamTypeMismatch {
        param_type: String,
        argument_idx: u32,
        argument_type: String,
    },
    ArrayElementTypeMismatch {
        element_type: String,
        element_index: u32,
        expected_type: String,
    },
    CastTypeMismatch {
        lhs_type: String,
        rhs_type: String,
    },
    PrefixMinusOnNonInteger {
        operand_type: String,
    },
    PrefixBangOnNonBool {
        operand_type: String,
    },
    InvalidInfix {
        lhs_type: String,
        rhs_type: String,
    },
    InvalidUnary {
        operand_type: String,
    },
    InterfaceDuplicateMethod {
        interface_name: String,
        method_name: String,
    },
    NonFunctionSymbol {
        symbol_name: String,
    },
    NonTypeSymbol {
        symbol_name: String,
    },
    NonStructSymbol {
        symbol_name: String,
    },
    DuplicateFuncParameter {
        param_name: String,
        param_idx: u32,
    },
    DuplicateFuncVariadicParameter {
        param_name: String,
    },
    DuplicateFieldName {
        struct_name: String,
        field_name: String,
    },
    DuplicateEnumVariantName {
        enum_name: String,
        variant_name: String,
    },
    DuplicateEnumFieldName {
        enum_name: String,
        field_name: String,
        variant_name: String,
    },
    AddressOfRvalue,
    DerefNonPointerValue,
    MultipleEntryPoints,
    MissingEntryPoint,
    ConstVariableMustBeInitialized,
}

impl fmt::Display for AnalyzerDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnalyzerDiagKind::ObjectNotSupportsMethods => {
                write!(f, "Invalid method call (not supported for this symbol).")
            }
            AnalyzerDiagKind::InvalidFatArrow => {
                write!(f, "Invalid usage of the fat arrow.")
            }
            AnalyzerDiagKind::StructMethodNotDefined {
                struct_name,
                method_name,
            } => {
                write!(f, "Method '{}' not defined for struct '{}'.", method_name, struct_name)
            }
            AnalyzerDiagKind::RhsOfShiftMustBeUnsignedInteger => {
                write!(f, "Rhs of the shift must be unsigned integer.")
            }
            AnalyzerDiagKind::ConstVariableMustBeInitialized => {
                write!(f, "Constant variable must be initialized with a value.")
            }
            AnalyzerDiagKind::CyclicTypeDefinition { symbol } => {
                write!(f, "Cyclic type definition found for type '{}'.", symbol)
            }
            AnalyzerDiagKind::MissingEntryPoint => {
                write!(f, "No entry point found (missing 'main' function).")
            }
            AnalyzerDiagKind::MultipleEntryPoints => {
                write!(f, "Multiple entry points are not allowed.")
            }
            AnalyzerDiagKind::MissingReturn => {
                write!(f, "Missing return statement.")
            }
            AnalyzerDiagKind::ReturnStatementNeedsAnArgument { argument_type } => {
                write!(f, "Return statement requires an argument of type '{}'.", argument_type)
            }
            AnalyzerDiagKind::ReturnStatementTypeMismatch { expected, got } => {
                write!(
                    f,
                    "Return statement argument must be a value of type '{}' but got '{}'.",
                    expected, got
                )
            }
            AnalyzerDiagKind::VoidFunctionReturnsValue => {
                write!(f, "Function with void return type cannot return a value.")
            }
            AnalyzerDiagKind::UnreachableCode => {
                write!(f, "Unreachable code.")
            }
            AnalyzerDiagKind::InvalidContinueStatement => {
                write!(
                    f,
                    "Invalid usage of the continue statement. It must be inside a loop statement."
                )
            }
            AnalyzerDiagKind::InvalidBreakStatement => {
                write!(
                    f,
                    "Invalid usage of the break statement. It must be inside a loop/switch statement."
                )
            }
            AnalyzerDiagKind::ConditionExprMustBeOfTypeBool => {
                write!(f, "Condition expression must be of type 'bool'.")
            }
            AnalyzerDiagKind::InternalInterfaceIsNotValid => {
                write!(f, "Interfaces must be declared globally.")
            }
            AnalyzerDiagKind::NamingConv { kind, name, expected } => {
                write!(f, "{} '{}' does not follow {} naming convention.", kind, name, expected)
            }
            AnalyzerDiagKind::UnusedSymbol { symbol_name } => {
                write!(f, "'{}' is declared but never used.", symbol_name)
            }
            AnalyzerDiagKind::GlobalVariableExprNotComptimeValid => {
                write!(f, "Global variable expression is not valid at compile time.")
            }
            AnalyzerDiagKind::CannotAssignToConstLValue => {
                write!(f, "Cannot assign to immutable variable.")
            }
            AnalyzerDiagKind::StructMissingFields {
                struct_name,
                missing_field_names,
            } => {
                write!(
                    f,
                    "Missing required fields {} in struct '{}'.",
                    missing_field_names
                        .iter()
                        .map(|f| format!("'{}'", f))
                        .collect::<Vec<String>>()
                        .join(", "),
                    struct_name
                )
            }
            AnalyzerDiagKind::StructHasNoFieldNamed {
                struct_name,
                field_name,
            } => {
                write!(f, "Struct '{}' has no field named '{}'.", struct_name, field_name)
            }
            AnalyzerDiagKind::InvalidIntegerLiteralSuffix => {
                write!(f, "Invalid integer literal suffix.")
            }
            AnalyzerDiagKind::InvalidFloatLiteralSuffix => {
                write!(f, "Invalid float literal suffix.")
            }
            AnalyzerDiagKind::StructFieldTypeMismatch {
                struct_name,
                field_name,
                expected_type,
                found_type,
            } => {
                write!(
                    f,
                    "Type mismatch for field '{}' in struct '{}' (expected '{}', found '{}').",
                    field_name, struct_name, expected_type, found_type
                )
            }
            AnalyzerDiagKind::ArrayNonIntegerIndex { found_type } => {
                write!(f, "Array index must be an integer (found '{}').", found_type)
            }
            AnalyzerDiagKind::ArrayIndexOnNonArrayOperand => {
                write!(f, "Cannot index non-array value.")
            }
            AnalyzerDiagKind::AddressOfRvalue => {
                write!(f, "Cannot take the address of a temporary value.")
            }
            AnalyzerDiagKind::DerefNonPointerValue => {
                write!(f, "Cannot dereference non-pointer value.")
            }
            AnalyzerDiagKind::DuplicateFuncParameter { param_idx, param_name } => {
                write!(f, "Duplicate parameter name '{}' at index {}.", param_name, param_idx)
            }
            AnalyzerDiagKind::DuplicateFuncVariadicParameter { param_name } => {
                write!(f, "Duplicate declaration of variadic parameter '{}'.", param_name)
            }
            AnalyzerDiagKind::DuplicateEnumVariantName {
                enum_name,
                variant_name,
            } => {
                write!(
                    f,
                    "Duplicate declaration of enum variant '{}' in enum '{}'.",
                    variant_name, enum_name
                )
            }
            AnalyzerDiagKind::DuplicateEnumFieldName {
                enum_name,
                field_name,
                variant_name,
            } => {
                write!(
                    f,
                    "Duplicate field name '{}' in variant '{}' of enum '{}'.",
                    field_name, variant_name, enum_name
                )
            }

            AnalyzerDiagKind::DuplicateFieldName {
                struct_name,
                field_name,
            } => {
                write!(
                    f,
                    "Duplicate declaration of field '{}' in struct '{}'.",
                    field_name, struct_name
                )
            }
            AnalyzerDiagKind::FuncCallArgsCountMismatch {
                args,
                expected,
                func_name,
            } => {
                write!(
                    f,
                    "Function '{}' expects {} arguments, but {} was provided..",
                    func_name, expected, args
                )
            }
            AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                param_type,
                argument_idx,
                argument_type,
            } => {
                write!(
                    f,
                    "Argument at index {} has type '{}', but the variadic parameter expects type '{}'.",
                    argument_idx, argument_type, param_type
                )
            }
            AnalyzerDiagKind::NonFunctionSymbol { symbol_name } => {
                write!(f, "Symbol '{}' is not a function.", symbol_name)
            }
            AnalyzerDiagKind::NonStructSymbol { symbol_name } => {
                write!(f, "Symbol '{}' is not a struct.", symbol_name)
            }
            AnalyzerDiagKind::NonTypeSymbol { symbol_name } => {
                write!(f, "Symbol '{}' is not a type.", symbol_name)
            }
            AnalyzerDiagKind::ArrayElementsCountMismatch { elements, expected } => {
                write!(f, "Cannot use {} elements in an array of size {}.", elements, expected)
            }
            AnalyzerDiagKind::ArrayElementTypeMismatch {
                element_type,
                element_index,
                expected_type,
            } => {
                write!(
                    f,
                    "Type mismatch for value of type '{}' at index {} for array of type '{}'.",
                    element_type, element_index, expected_type
                )
            }
            AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type } => {
                write!(
                    f,
                    "Cannot assign value of type '{}' to variable of type '{}'.",
                    rhs_type, lhs_type
                )
            }
            AnalyzerDiagKind::CastTypeMismatch { lhs_type, rhs_type } => {
                write!(f, "Cannot cast value of type '{}' to type '{}'.", rhs_type, lhs_type)
            }
            AnalyzerDiagKind::PrefixMinusOnNonInteger { operand_type } => {
                write!(f, "Cannot apply minus operator to value of type '{}'.", operand_type)
            }
            AnalyzerDiagKind::PrefixBangOnNonBool { operand_type } => {
                write!(
                    f,
                    "Cannot apply logical-not operator to value of type '{}'.",
                    operand_type
                )
            }
            AnalyzerDiagKind::InvalidInfix { lhs_type, rhs_type } => {
                write!(
                    f,
                    "Cannot apply infix operator between values of type '{}' and '{}'.",
                    lhs_type, rhs_type
                )
            }
            AnalyzerDiagKind::InvalidUnary { operand_type } => {
                write!(f, "Cannot apply unary operator to value of type '{}'.", operand_type)
            }
            AnalyzerDiagKind::InterfaceDuplicateMethod {
                interface_name,
                method_name,
            } => {
                write!(
                    f,
                    "Duplicate method '{}' in interface '{}'.",
                    method_name, interface_name
                )
            }
        }
    }
}
