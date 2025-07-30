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
            kind: DummyDiagKind::Mismatch,
            location: Some(DiagLoc::new("sample.cyr".to_string(), Location::new(5, 10), 10)),
            hint: Some("A simple example of hint message.".to_string()),
        };

        let reporter = DiagReporter::new().report(diag.clone());
        let output = DiagReporter::format_panel(&diag);
        assert!(output.contains("error"));
        assert!(output.contains("Type mismatch occurred"));
        eprintln!("{}", output);
    }
}
