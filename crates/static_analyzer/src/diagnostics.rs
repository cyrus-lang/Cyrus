use std::fmt;

#[derive(Debug, Clone)]
pub enum AnalyzerDiagKind {
    AssignmentTypeMismatch { lhs_type: String, rhs_type: String },
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
        }
    }
}
