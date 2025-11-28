use cyrusc_diagcentral::DiagKind;
use cyrusc_strescape::diagnostics::UnescapeError;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum AnalyzerDiagKind {
    #[error("Field cannot be type 'void'.")]
    VoidFieldType,

    #[error("A function parameter cannot have type 'void'.")]
    VoidParameterType,
    
    #[error("Unary operator minus is not permitted on unsigned types.")]
    UnaryOperatorMinusOnUnsignedInteger,

    #[error("Variant '.{variant_name}' has only one field, but multiple were provided.")]
    ValuedEnumVariantCanOnlyExportOneField { variant_name: String },

    #[error("Variant '.{variant_name}' is listed more than once in the patterns.")]
    DuplicateEnumVariantInSwitchPatterns { variant_name: String },

    #[error("Range lower bound must not exceed upper bound.")]
    InvalidRange,

    #[error("Overlapping range in switch case.")]
    OverlappingSwitchCaseRange,

    #[error("Goto targets a the label '{label_name}' that is not defined in the current scope.")]
    UndefinedGotoLabel { label_name: String },

    #[error("Cannot destructure tuple in export without a value.")]
    DestructureTupleWithNoRhs,

    #[error("{0}")]
    UnescapeError(UnescapeError),

    #[error("Cannot infer all generic parameters for type '{type_name}'.")]
    ExplicitTypeArgsRequired { type_name: String },

    #[error("Type arguments supplied to a non-generic type.")]
    UnexpectedTypeArgs,

    #[error("Unknown symbol '{symbol_name}'.")]
    UnknownSymbol { symbol_name: String },

    #[error("Type '{type_name}' contains a field of its own type, causing infinite recursion.")]
    InfiniteRecursiveType { type_name: String },

    #[error(
        "Method '{method_name}' implemented in object '{object_name}' does not match the method from interface '{interface_name}'."
    )]
    InterfaceMethodTypeMismatch {
        method_name: String,
        object_name: String,
        interface_name: String,
    },

    #[error("Internal symbol '{symbol_name}' is not accessible here.")]
    InternalSymbolAccess { symbol_name: String },

    #[error("'{symbol_name}' must be an interface.")]
    SymbolMustBeAnInterface { symbol_name: String },

    #[error(
        "Object '{object_name}' does not implement required method '{method_name}' from interface '{interface_name}'."
    )]
    MissingInterfaceMethodImpl {
        object_name: String,
        method_name: String,
        interface_name: String,
    },

    #[error("Mismatch between number of exported values and tuple elements.")]
    TupleExportedValuesAndTupleElementsCountMismatch,

    #[error("Tuple member access with index {index} exceeds tuple length {length}.")]
    TupleIndexOutOfRange { index: usize, length: usize },

    #[error("Function '{name}' is private and cannot be called.")]
    PrivateFunctionCall { name: String },

    #[error("Lambda cannot be left uninitialized.")]
    UninitializedLambda,

    #[error("The type 'void' cannot be used in this context.")]
    VoidVariableType,

    #[error("Enum variant '{variant_name}' expects {expected} fields, but {provided} arguments were provided.")]
    EnumVariantArgCountMismatch {
        variant_name: String,
        expected: u32,
        provided: u32,
    },

    #[error("Array capacity cannot be a negative integer.")]
    NegativeArrayCapacity,

    #[error("Value is not a compile-time constant.")]
    ValueIsNotACompTimeConst,

    #[error("Falling through into a case with fields may cause undefined behavior.")]
    SwitchFallthroughIntoValuedFieldCase,

    #[error("Enum variant '{variant_name}' does not accept fields.")]
    EnumVariantDoesNotAcceptFields { variant_name: String },

    #[error("Enum '{enum_name}' does not have a variant named '{variant_name}'.")]
    NoSuchEnumVariant { enum_name: String, variant_name: String },

    #[error("Only enum variants are allowed here.")]
    ExpressionPatternInAEnumSwitch,

    #[error("Switch expression must be an enum type, but found '{expr_type}'.")]
    SwitchOperandIsNotEnum { expr_type: String },

    #[error("Variant '{enum_name}.{variant_name}' is missing fields.")]
    VariantMissingFields { enum_name: String, variant_name: String },

    #[error("Enum '{enum_name}' has no variant named '{variant_name}'.")]
    VariantNotDefinedForEnum { enum_name: String, variant_name: String },

    #[error("Union initializer must specify exactly one field.")]
    UnionInitWithInvalidFields,

    #[error(
        "Switch statement must contain at least one case or consider removing it or replacing it with an if statement."
    )]
    EmptyCaseSwitchStatement,

    #[error("Literal of type {literal_type} is not assignable to expected type '{expected_type}'.")]
    TypeMismatchLiteral {
        literal_type: String,
        expected_type: String,
    },

    #[error("Case pattern with type '{pattern_type}' is not compatible with switch operand of type '{operand_type}'.")]
    TypeMismatchInCasePattern { operand_type: String, pattern_type: String },

    #[error("Cannot call method '{method_name}' on constant instance '{instance_name}'.")]
    MutationPossibleMethodCallOnConstInstance { method_name: String, instance_name: String },

    #[error("Cannot access internal field '{field_name}' of struct '{struct_name}' from outside its definition.")]
    InternalFieldAccess { field_name: String, struct_name: String },

    #[error("Cannot call internal method '{method_name}' of object '{object_name}' from outside its definition.")]
    InternalMethodCall { method_name: String, object_name: String },

    #[error("Use '->' instead of '.' when accessing a member via a pointer.")]
    UseThinArrow,

    #[error("Invalid usage of the concrete type.")]
    InvalidUsageOfTheSemanticType,

    #[error("Invalid field access (not supported for this symbol).")]
    ObjectNotSupportsFields,

    #[error("Invalid method call (not supported for this symbol).")]
    ObjectNotSupportsMethods,

    #[error("Invalid usage of the thin arrow.")]
    InvalidThinArrow,

    #[error("Method '{method_name}' not defined for struct '{struct_name}'.")]
    StructMethodNotDefined { struct_name: String, method_name: String },

    #[error("Rhs of the shift must be unsigned integer.")]
    RhsOfShiftMustBeUnsignedInteger,

    #[error("Constant variable must be initialized with a value.")]
    ConstVariableMustBeInitialized,

    #[error("Cyclic type definition found for type '{symbol}'.")]
    CyclicTypeDefinition { symbol: String },

    #[error("No entry point found (missing 'main' function).")]
    MissingEntryPoint,

    #[error("Multiple entry points are not allowed.")]
    MultipleEntryPoints,

    #[error("Missing return statement.")]
    MissingReturn,

    #[error("Return statement requires an argument of type '{argument_type}'.")]
    ReturnStatementNeedsAnArgument { argument_type: String },

    #[error("Return statement argument must be a value of type '{expected}' but got '{got}'.")]
    ReturnStatementTypeMismatch { expected: String, got: String },

    #[error("Function with void return type cannot return a value.")]
    VoidFunctionReturnsValue,

    #[error("Unreachable code.")]
    UnreachableCode,

    #[error("Invalid usage of the continue statement. It must be inside a loop statement.")]
    InvalidContinueStatement,

    #[error("Invalid usage of the break statement. It must be inside a loop/switch statement.")]
    InvalidBreakStatement,

    #[error("Condition expression must be of type 'bool'.")]
    ConditionExprMustBeOfTypeBool,

    #[error("Interfaces must be declared globally.")]
    InternalInterfaceIsNotValid,

    #[error("{kind} '{name}' does not follow {expected} naming convention.")]
    NamingConv {
        kind: String,
        name: String,
        expected: String,
    },

    #[error("'{symbol_name}' is declared but never used.")]
    UnusedSymbol { symbol_name: String },

    #[error("Global variable expression is not valid at compile time.")]
    GlobalVariableExprNotComptimeValid,

    #[error("Expr is not valid at compile time.")]
    ExprNotComptimeValid,

    #[error("Cannot assign to immutable lvalue.")]
    CannotAssignToConstLValue,

    #[error("Missing required fields {missing_field_names:?} in struct '{struct_name}'.")]
    StructMissingFields {
        struct_name: String,
        missing_field_names: Vec<String>,
    },

    #[error("'{struct_name}' has no field named '{field_name}'.")]
    ObjectHasNoFieldNamed { struct_name: String, field_name: String },

    #[error("Invalid integer literal suffix.")]
    InvalidIntegerLiteralSuffix,

    #[error("Invalid float literal suffix.")]
    InvalidFloatLiteralSuffix,

    #[error("Array index must be an integer (found '{found_type}').")]
    ArrayNonIntegerIndex { found_type: String },

    #[error("Cannot index non-array value.")]
    ArrayIndexOnNonArrayOperand,

    #[error("Cannot access member of non-tuple value.")]
    TupleMemberAccessOnNonTupleOperand,

    #[error("Cannot take the address of a temporary value.")]
    AddressOfRvalue,

    #[error("Cannot dereference non-pointer value.")]
    DerefNonPointerValue,

    #[error("Cannot dereference a pointer of type 'void*'.")]
    DerefVoidPointerValue,

    #[error("Duplicate parameter name '{param_name}' at index {param_idx}.")]
    DuplicateFuncParameter { param_name: String, param_idx: u32 },

    #[error("Duplicate declaration of variadic parameter '{param_name}'.")]
    DuplicateFuncVariadicParameter { param_name: String },

    #[error("Duplicate declaration of enum variant '{variant_name}' in enum '{enum_name}'.")]
    DuplicateEnumVariantName { enum_name: String, variant_name: String },

    #[error("Duplicate field name '{field_name}' in variant '{variant_name}' of enum '{enum_name}'.")]
    DuplicateEnumFieldName {
        enum_name: String,
        field_name: String,
        variant_name: String,
    },

    #[error("Duplicate declaration of field '{field_name}' in '{object_name}'.")]
    DuplicateFieldName { object_name: String, field_name: String },

    #[error("Function '{func_name}' expects {expected} arguments, but {args} was provided.")]
    FuncCallArgsCountMismatch {
        args: u32,
        expected: u32,
        func_name: String,
    },

    #[error(
        "Argument at index {argument_idx} has type '{argument_type}', but the variadic parameter expects type '{param_type}'."
    )]
    FuncCallVariadicParamTypeMismatch {
        param_type: String,
        argument_idx: u32,
        argument_type: String,
    },

    #[error(
        "Argument at index {argument_idx} has type '{argument_type}', but the expected parameter type is '{param_type}'."
    )]
    FuncCallParamTypeMismatch {
        param_type: String,
        argument_idx: u32,
        argument_type: String,
    },

    #[error("Symbol '{symbol_name}' is not a function.")]
    NonFunctionSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not a struct.")]
    NonStructSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not a union.")]
    NonUnionSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not a type.")]
    NonTypeSymbol { symbol_name: String },

    #[error("Cannot use {elements} elements in an array of size {expected}.")]
    ArrayElementsCountMismatch { elements: u32, expected: u32 },

    #[error(
        "Type mismatch for value of type '{element_type}' at index {element_index} for array of type '{expected_type}'."
    )]
    ArrayElementTypeMismatch {
        element_type: String,
        element_index: u32,
        expected_type: String,
    },

    #[error("Cannot assign value of type '{rhs_type}' to variable of type '{lhs_type}'.")]
    AssignmentTypeMismatch { lhs_type: String, rhs_type: String },

    #[error("Cannot cast value of type '{rhs_type}' to type '{lhs_type}'.")]
    CastTypeMismatch { lhs_type: String, rhs_type: String },

    #[error("Cannot apply minus operator to value of type '{operand_type}'.")]
    PrefixMinusOnNonInteger { operand_type: String },

    #[error("Cannot apply logical-not operator to value of type '{operand_type}'.")]
    PrefixBangOnNonBool { operand_type: String },

    #[error("Cannot apply infix operator between values of type '{lhs_type}' and '{rhs_type}'.")]
    InvalidInfix { lhs_type: String, rhs_type: String },

    #[error("Cannot apply unary operator to value of type '{operand_type}'.")]
    InvalidUnary { operand_type: String },

    #[error("Duplicate method '{method_name}' in interface '{interface_name}'.")]
    InterfaceDuplicateMethod {
        interface_name: String,
        method_name: String,
    },
}

impl DiagKind for AnalyzerDiagKind {}
