use std::fmt;

#[derive(Debug, Clone)]
pub enum AnalyzerDiagKind {
    NegativeArrayCapacity,
    ValueIsNotACompTimeConst,
    SwitchFallthroughIntoValuedFieldCase,
    EnumVariantArgCountMismatch {
        variant_name: String,
        expected: u32,
        provided: u32,
    },
    EnumVariantDoesNotAcceptFields {
        variant_name: String,
    },
    NoSuchEnumVariant {
        enum_name: String,
        variant_name: String,
    },
    ExpressionPatternInAEnumSwitch,
    SwitchOperandIsNotEnum {
        expr_type: String,
    },
    VariantMissingFields {
        enum_name: String,
        variant_name: String,
    },
    VariantNotDefinedForEnum {
        enum_name: String,
        variant_name: String,
    },
    UnionInitWithInvalidFields,
    EmptyCaseSwitchStatement,
    TypeMismatchLiteral {
        literal_type: String,
        expected_type: String,
    },
    ObjectNotSupportsMethods,
    ObjectNotSupportsFields,
    InvalidFatArrow,
    UseFatArrow,
    InternalFieldAccess {
        field_name: String,
        struct_name: String,
    },
    InternalMethodCall {
        method_name: String,
        object_name: String,
    },
    MutationPossibleMethodCallOnConstInstance {
        method_name: String,
        instance_name: String,
    },
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
    ObjectHasNoFieldNamed {
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
    FuncCallParamTypeMismatch {
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
    TypeMismatchInCasePattern {
        operand_type: String,
        pattern_type: String,
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
    NonUnionSymbol {
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
        object_name: String,
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
    DerefVoidPointerValue,
    MultipleEntryPoints,
    MissingEntryPoint,
    ConstVariableMustBeInitialized,
    InvalidUsageOfTheConcreteType,
}

impl fmt::Display for AnalyzerDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnalyzerDiagKind::EnumVariantArgCountMismatch {
                variant_name,
                expected,
                provided,
            } => {
                write!(
                    f,
                    "Enum variant '{}' expects {} fields, but {} arguments were provided.",
                    variant_name, expected, provided
                )
            }
            AnalyzerDiagKind::NegativeArrayCapacity => {
                write!(f, "Array capacity cannot be a negative integer.")
            }
            AnalyzerDiagKind::ValueIsNotACompTimeConst => {
                write!(f, "Value is not a compile-time constant.")
            }
            AnalyzerDiagKind::SwitchFallthroughIntoValuedFieldCase => {
                write!(
                    f,
                    "Falling through into a case with fields may cause undefined behavior."
                )
            }
            AnalyzerDiagKind::EnumVariantDoesNotAcceptFields { variant_name } => {
                write!(f, "Enum variant '{}' does not accept fields.", variant_name)
            }
            AnalyzerDiagKind::NoSuchEnumVariant {
                enum_name,
                variant_name,
            } => {
                write!(
                    f,
                    "Enum '{}' does not have a variant named '{}'.",
                    enum_name, variant_name
                )
            }
            AnalyzerDiagKind::ExpressionPatternInAEnumSwitch => {
                write!(f, "Only enum variants are allowed here.")
            }
            AnalyzerDiagKind::SwitchOperandIsNotEnum { expr_type } => {
                write!(f, "Switch expression must be an enum type, but found '{}'.", expr_type)
            }
            AnalyzerDiagKind::VariantMissingFields {
                enum_name,
                variant_name,
            } => {
                write!(f, "Variant '{}.{}' is missing fields.", enum_name, variant_name)
            }
            AnalyzerDiagKind::VariantNotDefinedForEnum {
                enum_name,
                variant_name,
            } => {
                write!(f, "Enum '{}' has no variant named '{}'.", enum_name, variant_name)
            }
            AnalyzerDiagKind::UnionInitWithInvalidFields => {
                write!(f, "Union initializer must specify exactly one field.")
            }
            AnalyzerDiagKind::EmptyCaseSwitchStatement => {
                write!(
                    f,
                    "Switch statement must contain at least one case or consider to remove it or replace it with a if statement."
                )
            }
            AnalyzerDiagKind::TypeMismatchLiteral {
                literal_type,
                expected_type,
            } => {
                write!(
                    f,
                    "Literal of type {} is not assignable to expected type '{}'.",
                    literal_type, expected_type
                )
            }
            AnalyzerDiagKind::TypeMismatchInCasePattern {
                operand_type,
                pattern_type,
            } => {
                write!(
                    f,
                    "Case pattern with type '{}' is not compatible with switch operand of type '{}'.",
                    pattern_type, operand_type
                )
            }
            AnalyzerDiagKind::MutationPossibleMethodCallOnConstInstance {
                method_name,
                instance_name,
            } => {
                write!(
                    f,
                    "Cannot call method '{}' on constant instance '{}'.",
                    method_name, instance_name
                )
            }
            AnalyzerDiagKind::InternalFieldAccess {
                field_name,
                struct_name,
            } => {
                write!(
                    f,
                    "Cannot access internal field '{}' of struct '{}' from outside its definition.",
                    field_name, struct_name
                )
            }
            AnalyzerDiagKind::InternalMethodCall {
                method_name,
                object_name,
            } => {
                write!(
                    f,
                    "Cannot call internal method '{}' of object '{}' from outside its definition.",
                    method_name, object_name
                )
            }
            AnalyzerDiagKind::UseFatArrow => {
                write!(f, "Use '->' instead of '.' when accessing a member via a pointer.")
            }
            AnalyzerDiagKind::InvalidUsageOfTheConcreteType => {
                write!(f, "Invalid usage of the concrete type.")
            }
            AnalyzerDiagKind::ObjectNotSupportsFields => {
                write!(f, "Invalid field access (not supported for this symbol).")
            }
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
                write!(f, "Cannot assign to immutable lvalue.")
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
            AnalyzerDiagKind::ObjectHasNoFieldNamed {
                struct_name,
                field_name,
            } => {
                write!(f, "'{}' has no field named '{}'.", struct_name, field_name)
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
            AnalyzerDiagKind::DerefVoidPointerValue => {
                write!(f, "Cannot dereference a pointer of type 'void*'.")
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
                object_name,
                field_name,
            } => {
                write!(
                    f,
                    "Duplicate declaration of field '{}' in '{}'.",
                    field_name, object_name
                )
            }
            AnalyzerDiagKind::FuncCallArgsCountMismatch {
                args,
                expected,
                func_name,
            } => {
                write!(
                    f,
                    "Function '{}' expects {} arguments, but {} was provided.",
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
            AnalyzerDiagKind::FuncCallParamTypeMismatch {
                param_type,
                argument_idx,
                argument_type,
            } => {
                write!(
                    f,
                    "Argument at index {} has type '{}', but the expected parameter type is '{}'.",
                    argument_idx, argument_type, param_type
                )
            }
            AnalyzerDiagKind::NonFunctionSymbol { symbol_name } => {
                write!(f, "Symbol '{}' is not a function.", symbol_name)
            }
            AnalyzerDiagKind::NonStructSymbol { symbol_name } => {
                write!(f, "Symbol '{}' is not a struct.", symbol_name)
            }
            AnalyzerDiagKind::NonUnionSymbol { symbol_name } => {
                write!(f, "Symbol '{}' is not a union.", symbol_name)
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
