// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use std::sync::Arc;

use crate::Parser;
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_lexer::Lexer;
use cyrusc_source_loc::SourceMap;

#[derive(Debug, PartialEq)]
struct ObservedDiagnostic {
    message: String,
    start: usize,
    end: usize,
}

struct ParseOutcome {
    succeeded: bool,
    diagnostics: Vec<ObservedDiagnostic>,
}

#[allow(clippy::arc_with_non_send_sync)]
fn parse(input: &str) -> ParseOutcome {
    let source_map = Arc::new(SourceMap::new());
    let file_id = source_map.add_file("test.cyrus", input.to_string());
    let source_file = source_map.get_file(file_id).unwrap();
    let reporter = Arc::new(DiagReporter::new(source_map));
    let tokens = Lexer::new(&reporter, &source_file).tokenize();
    let succeeded = Parser::new(reporter.clone(), &source_file, tokens).parse().is_ok();
    let diagnostics = reporter
        .diags()
        .iter()
        .map(|diag| {
            let loc = diag.loc.expect("parser diagnostics should identify their source span");

            ObservedDiagnostic {
                message: diag.kind.to_string(),
                start: loc.start,
                end: loc.end,
            }
        })
        .collect();

    ParseOutcome { succeeded, diagnostics }
}

fn assert_first_diagnostic(input: &str, expected_message: &str, marker: &str) {
    let outcome = parse(input);
    let start = input
        .find(marker)
        .expect("diagnostic marker should be present in test input");

    assert!(!outcome.succeeded, "input unexpectedly parsed successfully");
    assert_eq!(
        outcome.diagnostics.first(),
        Some(&ObservedDiagnostic {
            message: expected_message.to_string(),
            start,
            end: start + marker.len(),
        })
    );
}

#[test]
fn test_at_infix_reports_invalid_operator() {
    let input = "const invalid = 2 @ 4;";

    assert_first_diagnostic(input, "Invalid infix operator '@'.", "@");
}

#[test]
fn test_backtick_infix_reports_invalid_operator() {
    let input = "const invalid = 2 ` 4;";

    assert_first_diagnostic(input, "Invalid infix operator '`'.", "`");
}

#[test]
fn test_identifier_after_expression_keeps_statement_boundary_diagnostic() {
    for input in [
        "fn test() void {\n    const value = 2 foo;\n}",
        "fn test() void {\n    const value = 2 foo();\n}",
        "fn test() void {\n    const value = 2\n    foo;\n}",
    ] {
        let outcome = parse(input);

        assert!(!outcome.succeeded, "input unexpectedly parsed successfully");
        assert_eq!(outcome.diagnostics.first().unwrap().message, "Missing semicolon.");
        assert!(
            outcome
                .diagnostics
                .iter()
                .all(|diag| !diag.message.starts_with("Invalid infix operator"))
        );
    }
}

#[test]
fn test_at_on_following_line_keeps_statement_boundary_diagnostic() {
    let input = "fn test() void {\n    const value = 2\n    @unreachable();\n}";
    let outcome = parse(input);

    assert!(!outcome.succeeded, "input unexpectedly parsed successfully");
    assert_eq!(outcome.diagnostics.first().unwrap().message, "Missing semicolon.");
    assert!(
        outcome
            .diagnostics
            .iter()
            .all(|diag| !diag.message.starts_with("Invalid infix operator"))
    );
}

#[test]
fn test_valid_infix_and_builtin_expressions_still_parse() {
    let input = "fn test() void {\n    const sum = 2 + 4;\n    const converted = @cast(int32, sum);\n}";
    let outcome = parse(input);

    assert!(outcome.succeeded, "unexpected diagnostics: {:?}", outcome.diagnostics);
    assert!(outcome.diagnostics.is_empty());
}

#[test]
fn test_backtick_prefix_reports_unexpected_token() {
    let input = "fn test() void {\n    const invalid = `;\n}";

    assert_first_diagnostic(input, "Unexpected token: '`'.", "`");
}

#[test]
fn test_silently_consumed_backtick_is_rejected() {
    let input = "interface C {\n    fn bar(&self) uint8*;\n`}";

    assert_first_diagnostic(input, "Unexpected token: '`'.", "`");
}
