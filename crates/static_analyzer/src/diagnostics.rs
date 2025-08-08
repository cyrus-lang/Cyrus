use std::fmt;

#[derive(Debug, Clone)]
pub enum AnalyzerDiagKind {
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
    DuplicateFuncParameter {
        param_name: String,
        param_idx: u32,
    },
    DuplicateFuncVariadicParameter {
        param_name: String,
    },
    AddressOfRvalue,
}

impl fmt::Display for AnalyzerDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnalyzerDiagKind::AddressOfRvalue => {
                write!(f, "Cannot take the address of a temporary value.")
            }
            AnalyzerDiagKind::DuplicateFuncParameter { param_idx, param_name } => {
                write!(f, "Duplicate parameter name '{}' at index {}.", param_name, param_idx)
            }
            AnalyzerDiagKind::DuplicateFuncVariadicParameter { param_name } => {
                write!(f, "Duplicate declaration of variadic parameter '{}'.", param_name)
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
