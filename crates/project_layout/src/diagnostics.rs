use std::fmt;

#[derive(Debug, Clone)]
pub enum ProjectLayoutDiagKind {
    DuplicateProjectName { name: String },
}

impl fmt::Display for ProjectLayoutDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProjectLayoutDiagKind::DuplicateProjectName { name } => {
                write!(f, "A project named '{}' already exists in this location.", name)
            }
        }
    }
}
