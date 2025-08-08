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
}

impl fmt::Display for AnalyzerDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
