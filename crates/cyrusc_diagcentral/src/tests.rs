// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#[cfg(test)]
mod tests {
    use cyrusc_source_loc::{FileID, SourceMap};

    use crate::reporter::*;
    use crate::*;
    use std::fmt;
    use std::sync::Arc;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum DummyDiagKind {
        Mismatch,
    }

    impl DiagKind for DummyDiagKind {}

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
            loc: Some(Loc::new(FileID(0), 0, 0, 3, 10)),
            hint: Some("A simple example of hint message.".to_string()),
        };

        let source_map = Arc::new(SourceMap::new());
        source_map.add_file("main.cyrus".to_string(), "   ABCD".to_string());

        let diag_reporter = DiagReporter::new(source_map);
        let output = diag_reporter.render(&diag);

        assert!(output.contains("error"));
        assert!(output.contains("Type mismatch occurred"));
        eprintln!("{}", output);
    }
}
