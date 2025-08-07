use std::fmt;

#[derive(Debug, Clone)]
pub enum AnalyzerDiagKind {
    AssignmentTypeMismatch { lhs_type: String, rhs_type: String },
    PrefixMinusOnNonInteger { operand_type: String },
    PrefixBangOnNonBool { operand_type: String },
    InvalidInfix { lhs_type: String, rhs_type: String },
    InvalidUnary { operand_type: String },
}

impl fmt::Display for AnalyzerDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type } => {
                write!(
                    f,
                    "Cannot assign value of type '{}' to variable of type '{}'.",
                    rhs_type, lhs_type
                )
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
        }
    }
}
