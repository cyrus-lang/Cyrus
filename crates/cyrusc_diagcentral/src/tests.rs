#[cfg(test)]
mod tests {
    use crate::reporter::*;
    use crate::*;
    use std::fmt;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum DummyDiagKind {
        Mismatch,
    }

    impl fmt::Display for DummyDiagKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                DummyDiagKind::Mismatch => write!(f, "Type mismatch occurred"),
            }
        }
    }

    #[test]
    fn test_diag_display_formatting() {
        let diag = Diag {
            level: DiagLevel::Error,
            kind: Box::new(DummyDiagKind::Mismatch),
            location: Some(DiagLoc::new(SourceLoc {
                line: 5,
                column: 10,
                file_path: "sample".to_string(),
            })),
            hint: Some("A simple example of hint message.".to_string()),
        };

        let output = DiagReporter::format_panel(&diag);
        assert!(output.contains("error"));
        assert!(output.contains("Type mismatch occurred"));
        eprintln!("{}", output);
    }
}
