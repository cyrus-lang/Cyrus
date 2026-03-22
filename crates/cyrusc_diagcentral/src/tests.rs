/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

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
            loc: Some(Loc::new(FileID(0), 0, 3, 10)),
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
