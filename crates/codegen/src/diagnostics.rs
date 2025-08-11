use std::fmt;

#[derive(Debug, Clone)]
pub enum CodeGenDiagKind {
    FailedToParseBuildManifest { file_path: String, err: String },
}

impl fmt::Display for CodeGenDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodeGenDiagKind::FailedToParseBuildManifest { file_path, err } => {
                write!(f, "Failed to parse '{}': {}", file_path, err)
            }
        }
    }
}
