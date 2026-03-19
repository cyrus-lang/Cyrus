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
    use crate::Lexer;
    use cyrusc_diagcentral::reporter::DiagReporter;
    use cyrusc_source_loc::SourceMap;
    use cyrusc_tokens::{Token, TokenKind};

    fn lex(input: &str) -> Vec<Token> {
        let mut source_map = SourceMap::new();
        let file_id = source_map.add_file("test".to_string(), input.to_string());
        let source_file = source_map.get_file(file_id).unwrap();
        let mut reporter = DiagReporter::new(&source_map);
        let mut lexer = Lexer::new(&mut reporter, source_file);
        lexer.tokenize()
    }

    macro_rules! t {
        ($tokens:expr, $i:expr, $kind:pat, $line:expr, $start:expr, $end:expr) => {{
            let tok = &$tokens[$i];
            assert!(matches!(tok.kind, $kind), "token {} kind mismatch: {:?}", $i, tok.kind);
            assert_eq!(tok.loc.line, $line, "token {} line mismatch", $i);
            assert_eq!(tok.loc.start, $start, "token {} start mismatch", $i);
            assert_eq!(tok.loc.end, $end, "token {} end mismatch", $i);
        }};
    }

    #[test]
    fn test_integer_spans() {
        let t = lex("123 42\n789");

        t!(t, 0, TokenKind::Literal(_), 1, 0, 3);
        t!(t, 1, TokenKind::Literal(_), 1, 4, 6);
        t!(t, 2, TokenKind::Literal(_), 2, 7, 10);
    }

    #[test]
    fn test_float_spans() {
        let t = lex("3.14 0.001\n42");

        t!(t, 0, TokenKind::Literal(_), 1, 0, 4);
        t!(t, 1, TokenKind::Literal(_), 1, 5, 10);
        t!(t, 2, TokenKind::Literal(_), 2, 11, 13); // "42."
    }

    #[test]
    fn test_string_spans() {
        let t = lex("\"hello\" \"x\"");

        t!(t, 0, TokenKind::Literal(_), 1, 0, 7);
        t!(t, 1, TokenKind::Literal(_), 1, 8, 11);
    }

    #[test]
    fn test_prefixed_strings() {
        let t = lex("c\"hello\" b\"bye\"");

        t!(t, 0, TokenKind::Literal(_), 1, 0, 8);
        t!(t, 1, TokenKind::Literal(_), 1, 9, 15);
    }

    #[test]
    fn test_char_literal() {
        let t = lex("'x' '\\n'");

        t!(t, 0, TokenKind::Literal(_), 1, 0, 3);
        t!(t, 1, TokenKind::Literal(_), 1, 4, 8);
    }

    #[test]
    fn test_plus_ops() {
        let t = lex("+ ++");

        t!(t, 0, TokenKind::Plus, 1, 0, 1);
        t!(t, 1, TokenKind::Increment, 1, 2, 4);
    }

    #[test]
    fn test_minus_ops() {
        let t = lex("- -- ->");

        t!(t, 0, TokenKind::Minus, 1, 0, 1);
        t!(t, 1, TokenKind::Decrement, 1, 2, 4);
        t!(t, 2, TokenKind::ThinArrow, 1, 5, 7);
    }

    #[test]
    fn test_equality_ops() {
        let t = lex("= == !=");

        t!(t, 0, TokenKind::Assign, 1, 0, 1);
        t!(t, 1, TokenKind::Equal, 1, 2, 4);
        t!(t, 2, TokenKind::NotEqual, 1, 5, 7);
    }

    #[test]
    fn test_dot_sequences() {
        let t = lex(". .. ...");

        t!(t, 0, TokenKind::Dot, 1, 0, 1);
        t!(t, 1, TokenKind::DoubleDot, 1, 2, 4);
        t!(t, 2, TokenKind::TripleDot, 1, 5, 8);
    }

    #[test]
    fn test_logical_ops() {
        let t = lex("&& || &~");

        t!(t, 0, TokenKind::And, 1, 0, 2);
        t!(t, 1, TokenKind::Or, 1, 3, 5);
        t!(t, 2, TokenKind::AmpTilde, 1, 6, 8);
    }

    #[test]
    fn test_stmt_var_decl() {
        let input = "var x = 50;";

        let t = lex(input);

        t!(t, 0, TokenKind::Var, 1, 0, 3);
        t!(t, 1, TokenKind::Ident(_), 1, 4, 5);
        t!(t, 2, TokenKind::Assign, 1, 6, 7);
        t!(t, 3, TokenKind::Literal(_), 1, 8, 10);
        t!(t, 4, TokenKind::Semicolon, 1, 10, 11);
    }

    #[test]
    fn test_assign_with_increment() {
        let input = r#"
    y = x++ + func(3, b"hi");
    "#;

        let t = lex(input);

        t!(t, 0, TokenKind::Ident(_), 2, 5, 6);
        t!(t, 1, TokenKind::Assign, 2, 7, 8);
        t!(t, 2, TokenKind::Ident(_), 2, 9, 10);
        t!(t, 3, TokenKind::Increment, 2, 10, 12);
        t!(t, 4, TokenKind::Plus, 2, 13, 14);
    }

    #[test]
    fn test_single_line_comment() {
        let input = "x // hello\ny";

        let t = lex(input);

        t!(t, 0, TokenKind::Ident(_), 1, 0, 1); // x 
        t!(t, 1, TokenKind::Ident(_), 2, 11, 12); // y
    }

    #[test]
    fn test_comment_inside_expr() {
        let input = "a = 1 /* skip */ + 2;";

        let t = lex(input);

        t!(t, 0, TokenKind::Ident(_), 1, 0, 1);
        t!(t, 1, TokenKind::Assign, 1, 2, 3);
        t!(t, 2, TokenKind::Literal(_), 1, 4, 5);
        t!(t, 3, TokenKind::Plus, 1, 17, 18);
        t!(t, 4, TokenKind::Literal(_), 1, 19, 20);
        t!(t, 5, TokenKind::Semicolon, 1, 20, 21);
    }
}
