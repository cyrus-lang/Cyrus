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
use crate::diagnostics::LexicalDiagKind;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, display_single_diag, source_loc::SourceLoc};
use cyrusc_strescape::unescape_string;
use cyrusc_tokens::{
    Token, TokenKind,
    literals::{Literal, LiteralKind, StringPrefix},
    loc::{Location, Span},
};
use diagnostics::lexer_invalid_char_error;
use std::{fmt::Debug, process::exit};

mod diagnostics;
mod format;

#[derive(Debug, Clone)]
pub struct Lexer {
    pos: usize,
    next_pos: usize,
    ch: char,
    pub input: String,
    pub file_name: String,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: String, file_name: String) -> Self {
        let mut lexer = Self {
            input: input.to_string(),
            pos: 0,
            next_pos: 0,
            ch: '\0',
            file_name,
            line: 1,
            column: 1,
        };

        lexer.read_char();
        lexer
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        let mut token = self.next_token();
        while !token.kind.is_eof() {
            tokens.push(token.clone());
            token = self.next_token();
        }

        tokens
    }

    #[inline]
    fn peek_char(&self) -> char {
        if self.next_pos >= self.input.len() {
            '\0'
        } else {
            self.input[self.next_pos..].chars().next().unwrap_or('\0')
        }
    }

    fn read_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.ch = '\0';
            return;
        }

        let remaining_input = &self.input[self.next_pos..];
        if let Some(c) = remaining_input.chars().next() {
            self.ch = c;
            self.pos = self.next_pos;
            self.next_pos += c.len_utf8();
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        } else {
            self.ch = '\0';
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comments();

        let token_line = self.line;
        let token_column = self.column;
        let token_start = self.pos;

        if self.ch == '\0' {
            return Token {
                kind: TokenKind::EOF,
                span: Span {
                    start: self.pos,
                    end: self.pos,
                },
                loc: Location::new(self.line, self.column),
            };
        }

        let token_kind: TokenKind = match self.ch {
            '~' => TokenKind::Tilde,
            '+' => {
                if self.peek_char() == '+' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::Increment,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Plus,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            '-' => {
                if self.peek_char() == '-' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::Decrement,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else if self.peek_char() == '>' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::ThinArrow,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Minus,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            ':' => {
                if self.peek_char() == ':' {
                    let start = self.pos;
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::DoubleColon,
                        span: Span::new(start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Colon,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            ',' => TokenKind::Comma,
            '.' => {
                self.read_char();

                let mut kind = TokenKind::Dot;

                if self.ch == '.' && self.peek_char() == '.' {
                    self.read_char();
                    self.read_char();
                    kind = TokenKind::TripleDot;
                } else if self.ch == '.' {
                    self.read_char();
                    kind = TokenKind::DoubleDot;
                }

                return Token {
                    kind,
                    span: Span::new(token_start, self.pos),
                    loc: Location::new(token_line, token_column),
                };
            }
            '^' => TokenKind::Caret,
            '"' => return self.read_string(None),
            '\'' => return self.read_char_literal(),
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char(); // consume current equal sign
                    self.read_char(); // consume peeked equal sign
                    return Token {
                        kind: TokenKind::Equal,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else if self.peek_char() == '>' {
                    self.read_char(); // consume current equal sign
                    self.read_char(); // consume peeked equal sign
                    return Token {
                        kind: TokenKind::FatArrow,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char(); // consume current equal sign
                    return Token {
                        kind: TokenKind::Assign,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::NotEqual,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char(); // consume current equal sign
                    return Token {
                        kind: TokenKind::Bang,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::LessEqual,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else if self.peek_char() == '<' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::ShiftLeft,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::LessThan,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            '>' => {
                // NOTE: shift-right(>>) token due to ambiguity it has, is handled inside parser.

                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::GreaterEqual,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::GreaterThan,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            '&' => {
                if self.peek_char() == '&' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::And,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else if self.peek_char() == '~' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::AmpTilde,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Ampersand,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            '|' => {
                if self.peek_char() == '|' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::Or,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Pipe,
                        span: Span::new(token_start, self.pos),
                        loc: Location::new(token_line, token_column),
                    };
                }
            }
            ';' => TokenKind::Semicolon,
            _ => {
                if self.ch == 'c' && self.peek_char() == '"' {
                    self.read_char();
                    return self.read_string(Some(StringPrefix::C));
                }
                if self.ch == 'b' && self.peek_char() == '"' {
                    self.read_char();
                    return self.read_string(Some(StringPrefix::B));
                } else if self.ch.is_alphabetic() || self.ch == '_' {
                    return self.read_ident();
                } else if self.is_numeric(self.ch) {
                    return self.read_number();
                } else {
                    lexer_invalid_char_error(self.file_name.clone(), self.line, self.column - 1, self.ch);
                    exit(1);
                }
            }
        };

        self.read_char();

        Token {
            kind: token_kind,
            span: Span::new(token_start, self.pos),
            loc: Location::new(token_line, token_column),
        }
    }

    fn read_char_literal(&mut self) -> Token {
        let start = self.pos;
        let line = self.line;
        let column = self.column;

        self.read_char(); // skip opening quote

        let mut final_char: Option<char> = None;

        loop {
            if self.ch == '\'' {
                break;
            }

            if final_char.is_some() {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(LexicalDiagKind::CharLiteralMustBeASingleUnit),
                    location: Some(DiagLoc::new(SourceLoc {
                        line,
                        column,
                        file_path: self.file_name.clone()
                    })),
                    hint: Some("Character literals may contain exactly one character.".to_string()),
                });
            }

            // multi-byte UTF-8 is not allowed in single quotes
            if self.ch.len_utf8() != 1 {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(LexicalDiagKind::CharLiteralMustBeASingleUnit),
                    location: Some(DiagLoc::new(SourceLoc {
                        line,
                        column,
                        file_path: self.file_name.clone()
                    })),
                    hint: Some("Use a string literal for multi-byte characters or emojis.".to_string()),
                });
            }

            final_char = Some(self.ch);
            self.read_char();

            if self.is_eof() {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(LexicalDiagKind::UnterminatedStringLiteral),
                    location: Some(DiagLoc::new(SourceLoc {
                        line,
                        column,
                        file_path: self.file_name.clone()
                    })),
                    hint: None,
                });
            }
        }

        self.read_char(); // consume closing quote

        if let Some(value) = final_char {
            Token {
                kind: TokenKind::Literal(Literal {
                    kind: LiteralKind::Char(value),
                    loc: Location::new(line, column),
                    span: Span::new(start, self.pos),
                }),
                span: Span::new(start, self.pos),
                loc: Location::new(line, column),
            }
        } else {
            display_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: Box::new(LexicalDiagKind::EmptyCharLiteral),
                location: Some(DiagLoc::new(SourceLoc {
                    line,
                    column,
                    file_path: self.file_name.clone()
                })),
                hint: None,
            });
        }
    }

    fn read_string(&mut self, prefix: Option<StringPrefix>) -> Token {
        let start = self.pos; // Position of opening quote or prefix
        let line = self.line;
        let column = self.column;

        self.read_char(); // Skip opening quote (or prefix char)

        let mut raw = String::new();

        loop {
            if self.is_eof() {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(LexicalDiagKind::UnterminatedStringLiteral),
                    location: Some(DiagLoc::new(SourceLoc {
                        line,
                        column,
                        file_path: self.file_name.clone()
                    })),
                    hint: None,
                });
            }

            match self.ch {
                '\\' => {
                    raw.push('\\');
                    self.read_char();
                    raw.push(self.ch);
                    self.read_char();
                }
                '"' => {
                    break;
                }
                _ => {
                    raw.push(self.ch);
                    self.read_char();
                }
            }
        }

        // consume closing "
        self.read_char();

        let unescaped = match unescape_string(&raw) {
            Ok(s) => s,
            Err(err) => {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(LexicalDiagKind::InvalidChar(self.ch)),
                    location: Some(DiagLoc::new(SourceLoc {
                        line,
                        column,
                        file_path: self.file_name.clone()
                    })),
                    hint: Some(err.to_string()),
                });
            }
        };

        Token {
            kind: TokenKind::Literal(Literal {
                kind: LiteralKind::String(unescaped, prefix),
                span: Span::new(start, self.pos),
                loc: Location::new(line, column),
            }),
            span: Span::new(start, self.pos),
            loc: Location::new(line, column),
        }
    }

    fn read_ident(&mut self) -> Token {
        let start = self.pos;
        let line = self.line;
        let column = self.column;

        let mut final_ident = String::new();

        while self.ch.is_alphanumeric() || self.ch == '_' {
            final_ident.push(self.ch);
            self.read_char();
        }

        let end = self.pos;

        let token_kind = self.lookup_identifier(final_ident);

        Token {
            kind: token_kind,
            span: Span { start, end },
            loc: Location::new(line, column),
        }
    }

    #[inline]
    fn read_literal_suffix(&mut self) -> Option<Box<TokenKind>> {
        if matches!(self.ch, 'f' | 'u' | 'i' | 's') {
            let mut suffix = String::new();

            while self.ch.is_alphanumeric() || self.ch == '_' {
                suffix.push(self.ch);
                self.read_char();
            }

            Some(Box::new(self.lookup_identifier(suffix)))
        } else {
            None
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        let line = self.line;
        let column = self.column;

        let mut number = String::new();
        let mut is_float = false;

        let base = if self.ch == '0' {
            match self.peek_char().to_ascii_lowercase() {
                'x' => {
                    self.read_char(); // consume '0'
                    self.read_char(); // consume 'x' or 'X'
                    while self.ch.is_ascii_hexdigit() || self.ch == '_' {
                        if self.ch != '_' {
                            number.push(self.ch);
                        }
                        self.read_char();
                    }
                    16
                }
                'b' => {
                    self.read_char(); // consume '0'
                    self.read_char(); // consume 'b' or 'B'
                    while self.ch == '0' || self.ch == '1' || self.ch == '_' {
                        if self.ch != '_' {
                            number.push(self.ch);
                        }
                        self.read_char();
                    }
                    2
                }
                _ => 10,
            }
        } else {
            10
        };

        if base == 10 {
            while self.ch.is_ascii_digit() || self.ch == '_' {
                if self.ch != '_' {
                    number.push(self.ch);
                }
                self.read_char();
            }

            if self.ch == '.' && self.peek_char().is_ascii_digit() {
                is_float = true;
                number.push(self.ch);
                self.read_char();
                while self.ch.is_ascii_digit() || self.ch == '_' {
                    if self.ch != '_' {
                        number.push(self.ch);
                    }
                    self.read_char();
                }
            }

            if self.ch == 'e' || self.ch == 'E' {
                is_float = true;
                number.push(self.ch);
                self.read_char();
                if self.ch == '+' || self.ch == '-' {
                    number.push(self.ch);
                    self.read_char();
                }
                while self.ch.is_ascii_digit() {
                    number.push(self.ch);
                    self.read_char();
                }
            }
        }

        let suffix = self.read_literal_suffix();

        let token_kind = if is_float {
            match number.parse::<f64>() {
                Ok(value) => LiteralKind::Float(value, suffix),
                Err(_) => {
                    display_single_diag!(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(LexicalDiagKind::InvalidFloatLiteral),
                        location: Some(DiagLoc::new(SourceLoc {
                            line,
                            column,
                            file_path: self.file_name.clone()
                        })),
                        hint: None,
                    });
                }
            }
        } else {
            let is_unsigned = matches!(suffix, Some(ref token_kind) if token_kind.is_unsigned());
            let value: i128 = if is_unsigned || base != 10 {
                match u128::from_str_radix(&number, base) {
                    Ok(v) => v as i128,
                    Err(_) => {
                        display_single_diag!(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(LexicalDiagKind::InvalidIntegerLiteral),
                            location: Some(DiagLoc::new(SourceLoc {
                                line,
                                column,
                                file_path: self.file_name.clone()
                            })),
                            hint: None,
                        });
                    }
                }
            } else {
                match i128::from_str_radix(&number, base) {
                    Ok(v) => v,
                    Err(_) => {
                        display_single_diag!(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(LexicalDiagKind::InvalidIntegerLiteral),
                            location: Some(DiagLoc::new(SourceLoc {
                                line,
                                column,
                                file_path: self.file_name.clone()
                            })),
                            hint: None,
                        });
                    }
                }
            };

            LiteralKind::Integer(value, suffix)
        };

        Token {
            kind: TokenKind::Literal(Literal {
                kind: token_kind,
                span: Span::new(start, self.pos),
                loc: Location::new(line, column),
            }),
            span: Span::new(start, self.pos),
            loc: Location::new(line, column),
        }
    }

    #[inline]
    fn is_numeric(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }

    #[inline]
    fn is_eof(&mut self) -> bool {
        self.pos == self.input.len() || self.ch == '\0'
    }

    #[inline]
    fn is_whitespace(ch: char) -> bool {
        matches!(
            ch,
            // Usual ASCII suspects
            '\u{0009}'   // \t
            | '\u{000A}' // \n
            | '\u{000B}' // vertical tab
            | '\u{000C}' // form feed
            | '\u{000D}' // \r
            | '\u{0020}' // space
            // NEXT LINE from latin1
            | '\u{0085}'
            // Bidi markers
            | '\u{200E}' // LEFT-TO-RIGHT MARK
            | '\u{200F}' // RIGHT-TO-LEFT MARK
            // Dedicated whitespace characters from Unicode
            | '\u{2028}' // LINE SEPARATOR
            | '\u{2029}' // PARAGRAPH SEPARATOR
        )
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        while Self::is_whitespace(self.ch) && !self.is_eof() {
            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
        while self.ch == '/' && (self.peek_char() == '/' || self.peek_char() == '*') {
            let comment_start_line = self.line;
            let comment_start_column = self.column;

            if self.peek_char() == '/' {
                // handle single-line comment
                self.read_char(); // consume '/'
                self.read_char(); // consume second '/'

                while !self.is_eof() && self.ch != '\n' {
                    self.read_char();
                }

                if !self.is_eof() && self.ch == '\n' {
                    self.read_char();
                }
            } else if self.peek_char() == '*' {
                // handle multi-line (possibly nested) comment
                self.read_char(); // consume '/'
                self.read_char(); // consume '*'

                let mut depth = 1;
                while !self.is_eof() && depth > 0 {
                    if self.ch == '/' && self.peek_char() == '*' {
                        // new nested comment
                        self.read_char();
                        self.read_char();
                        depth += 1;
                    } else if self.ch == '*' && self.peek_char() == '/' {
                        // closing comment
                        self.read_char();
                        self.read_char();
                        depth -= 1;
                    } else {
                        self.read_char();
                    }
                }

                if depth > 0 {
                    display_single_diag!(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(LexicalDiagKind::UnterminatedMultiLineComment),
                        location: Some(DiagLoc::new(SourceLoc {
                            line: comment_start_line,
                            column: comment_start_column,
                            file_path: self.file_name.clone()
                        })),
                        hint: None,
                    });
                }

                // skip any newlines after comment
                while !self.is_eof() && self.ch == '\n' {
                    self.read_char();
                }
            }

            // skip whitespace that might be after comment
            self.skip_whitespace();
        }
    }

    #[inline]
    fn lookup_identifier(&mut self, ident: String) -> TokenKind {
        match ident.as_str() {
            "dynamic" => TokenKind::Dynamic,
            "var" => TokenKind::Var,
            "defer" => TokenKind::Defer,
            "goto" => TokenKind::Goto,
            "union" => TokenKind::Union,
            "interface" => TokenKind::Interface,
            "type" => TokenKind::Typedef,
            "cast" => TokenKind::Typecast,
            "fn" => TokenKind::Function,
            "switch" => TokenKind::Switch,
            "case" => TokenKind::Case,
            "default" => TokenKind::Default,
            "struct" => TokenKind::Struct,
            "bits" => TokenKind::Bits,
            "import" => TokenKind::Import,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "for" => TokenKind::For,
            "while" => TokenKind::While,
            "foreach" => TokenKind::Foreach,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            "as" => TokenKind::As,
            "in" => TokenKind::In,
            "enum" => TokenKind::Enum,
            "void" => TokenKind::Void,
            "extern" => TokenKind::Extern,
            "inline" => TokenKind::Inline,
            "alwaysinline" => TokenKind::AlwaysInline,
            "noinline" => TokenKind::NoInline,
            "pub" => TokenKind::Public,
            "const" => TokenKind::Const,
            "sizeof" => TokenKind::SizeOf,
            "weak" => TokenKind::Weak,
            "linkonce" => TokenKind::LinkOnce,
            "callconv" => TokenKind::Callconv,
            "naked" => TokenKind::Naked,
            "noreturn" => TokenKind::NoReturn,
            "hot" => TokenKind::Hot,
            "cold" => TokenKind::Cold,
            "dllimport" => TokenKind::DllImport,
            "dllexport" => TokenKind::DllExport,
            "optsize" => TokenKind::OptSize,
            "optnone" => TokenKind::OptNone,
            "no_sanitize" => TokenKind::NoSanitize,
            "nounwind" => TokenKind::NoUnwind,
            "section" => TokenKind::Section,
            "uintptr" => TokenKind::UIntPtr,
            "intptr" => TokenKind::IntPtr,
            "isize" => TokenKind::ISize,
            "usize" => TokenKind::USize,
            "int" => TokenKind::Int,
            "int8" => TokenKind::Int8,
            "int16" => TokenKind::Int16,
            "int32" => TokenKind::Int32,
            "int64" => TokenKind::Int64,
            "int128" => TokenKind::Int128,
            "uint" => TokenKind::UInt,
            "uint8" => TokenKind::UInt8,
            "uint16" => TokenKind::UInt16,
            "uint32" => TokenKind::UInt32,
            "uint64" => TokenKind::UInt64,
            "uint128" => TokenKind::UInt128,
            "float16" => TokenKind::Float16,
            "float32" => TokenKind::Float32,
            "float64" => TokenKind::Float64,
            "float128" => TokenKind::Float128,
            "char" => TokenKind::Char,
            "bool" => TokenKind::Bool,
            _ => TokenKind::Ident(ident),
        }
    }
}
