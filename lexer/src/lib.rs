use ast::{
    ast::{CharLiteral, FloatLiteral, IntegerLiteral, Literal, StringLiteral},
    token::*,
};
use core::panic;
use errors::{lexer_invalid_char_error, lexer_unknown_char_error, LexicalError};
use std::{fmt::Debug, ops::Range, process::exit};

mod errors;
mod format;
mod lexer_test;

#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
    file_name: String,
    pos: usize,
    next_pos: usize,
    ch: char,
    pub line: usize,
    pub column: usize,
}

impl Lexer {
    pub fn new(input: String, file_name: String) -> Self {
        let mut lexer = Self {
            input: input.to_string(),
            pos: 0,      // points to current position
            next_pos: 0, // points to next position
            ch: ' ',
            file_name,
            line: 0,
            column: 0,
        };

        lexer.read_char();
        lexer
    }

    /// Selects a substring from the input string within the specified range.
    ///
    /// # Parameters
    /// - `range`: A `Range<usize>` specifying the start (inclusive) and end (exclusive) indices
    ///   of the substring to extract. Both indices must be within the bounds of the input string,
    ///   and the start index must not exceed the end index.
    ///
    /// # Returns
    /// A `String` containing the substring specified by the given range.
    ///
    /// # Panics
    /// This method will panic if:
    /// - The start or end indices are out of bounds of the input string's length.
    /// - The start index is greater than the end index.
    ///
    /// # Example
    /// ```
    /// let lexer = Lexer::new("hello, world!");
    /// let substring = lexer.select(0..5);
    /// assert_eq!(substring, "hello");
    /// ```
    pub fn select(&self, range: Range<usize>) -> String {
        let len = self.input.len();

        if range.start > len || range.end > len || range.start > range.end {
            panic!("Range {:?} is out of bounds for string of length {}", range, len);
        }

        self.input[range].to_string()
    }

    fn peek_char(&self) -> char {
        if self.next_pos >= self.input.len() {
            ' '
        } else {
            match self.input.chars().nth(self.next_pos) {
                Some(ch) => return ch,
                None => {
                    lexer_unknown_char_error(self.file_name.clone(), self.line, self.column - 1);
                    std::process::exit(1);
                }
            }
        }
    }

    fn read_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.ch = ' ';
        } else {
            self.ch = self.input.chars().nth(self.next_pos).unwrap_or(' ');

            if self.ch == '\n' {
                self.line += 1;
                self.column = 0;
            }
        }

        self.pos = self.next_pos;
        self.next_pos += 1;
        self.column += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comments();

        if self.is_eof() {
            return Token {
                kind: TokenKind::EOF,
                span: Span {
                    start: self.pos,
                    end: self.pos,
                },
            };
        }

        let token_kind: TokenKind = match self.ch {
            ' ' => return self.next_token(),
            '`' => TokenKind::BackTick,
            '+' => {
                if self.peek_char() == '+' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::Increment,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Plus,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    };
                }
            }
            '-' => {
                if self.peek_char() == '-' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::Decrement,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Minus,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
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
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '#' => TokenKind::Hashtag,
            '"' => return self.read_string(),
            '\'' => return self.read_char_literal(),
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char(); // consume current equal sign
                    self.read_char(); // consume peeked equal sign
                    return Token {
                        kind: TokenKind::Equal,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    self.read_char(); // consume current equal sign
                    return Token {
                        kind: TokenKind::Assign,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    };
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::NotEqual,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    self.read_char(); // consume current equal sign
                    return Token {
                        kind: TokenKind::Bang,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    };
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::LessEqual,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::LessThan,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    };
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::GreaterEqual,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::GreaterThan,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    };
                }
            }
            '&' => {
                if self.peek_char() == '&' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::And,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Ampersand,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    };
                }
            }
            '|' => {
                if self.peek_char() == '|' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::Or,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Pipe,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    };
                }
            }
            ';' => TokenKind::Semicolon,
            ':' => TokenKind::Colon,
            _ => {
                if self.ch.is_alphabetic() {
                    return self.read_identifider();
                } else if self.is_numeric(self.ch) {
                    return self.read_integer();
                } else {
                    lexer_invalid_char_error(self.file_name.clone(), self.line, self.column - 1, self.ch);
                    exit(1);
                }
            }
        };

        self.read_char();

        Token {
            kind: token_kind,
            span: Span {
                start: self.pos - 1,
                end: self.pos - 1,
            },
        }
    }

    fn read_char_literal(&mut self) -> Token {
        let start: usize = self.pos + 1;

        let mut final_char: Option<char> = None;

        loop {
            self.read_char();

            if self.ch == '\'' {
                break;
            }

            final_char = Some(self.ch);

            if self.is_eof() {
                LexicalError {
                    line: self.line,
                    column: self.column - 1,
                    code_raw: Some(self.select(start - 1..self.pos)), // -1 for encounter "
                    etype: errors::LexicalErrorType::UnterminatedStringLiteral,
                    verbose: None,
                    caret: true,
                    file_name: Some(self.file_name.clone()),
                }
                .print();
                exit(1);
            }
        }

        if self.ch == '\'' {
            // consume the ending single quote
            self.read_char();
        }

        let end = self.pos;

        let span = Span { start: start - 1, end };

        if let Some(raw) = final_char {
            Token {
                kind: TokenKind::Literal(Literal::Char(CharLiteral {
                    raw,
                    span: span.clone(),
                })),
                span,
            }
        } else {
            LexicalError {
                line: self.line,
                column: self.column - 1,
                code_raw: Some(self.select(start - 1..self.pos)), // -1 for encounter "
                etype: errors::LexicalErrorType::EmptyCharLiteral,
                verbose: None,
                caret: true,
                file_name: Some(self.file_name.clone()),
            }
            .print();
            exit(1);
        }
    }

    fn read_string(&mut self) -> Token {
        let start: usize = self.pos + 1;

        let mut final_string = String::new();

        loop {
            self.read_char();

            if self.ch == '"' {
                break;
            }

            final_string.push(self.ch);

            if self.is_eof() {
                LexicalError {
                    line: self.line,
                    column: self.column - 1,
                    code_raw: Some(self.select(start - 1..self.pos)), // -1 for encounter "
                    etype: errors::LexicalErrorType::UnterminatedStringLiteral,
                    verbose: None,
                    caret: true,
                    file_name: Some(self.file_name.clone()),
                }
                .print();
                exit(1);
            }
        }

        if self.ch == '"' {
            // consume the ending double quote
            self.read_char();
        }

        let end = self.pos;

        let span = Span { start: start - 1, end };

        Token {
            kind: TokenKind::Literal(Literal::String(StringLiteral {
                raw: final_string,
                span: span.clone(),
            })),
            span,
        }
    }

    fn read_identifider(&mut self) -> Token {
        let start = self.pos;

        let mut final_identifier = String::new();

        while self.ch.is_alphanumeric() || self.ch == '_' {
            final_identifier.push(self.ch);
            self.read_char();
        }

        let end = self.pos;

        let token_kind = self.lookup_identifier(final_identifier);

        Token {
            kind: token_kind,
            span: Span { start, end },
        }
    }

    fn read_integer(&mut self) -> Token {
        let start = self.pos;

        let mut is_float = false;

        let mut number_str = vec![];

        while self.is_numeric(self.ch) || self.ch == '.' || self.ch == '_' {
            number_str.push(self.ch);
            self.read_char();

            if self.ch == '.' {
                is_float = true;
            }

            if self.ch == '_' {
                continue;
            }
        }

        let end = self.pos;

        let token_kind: TokenKind = {
            if is_float {
                let literal: Result<f32, _> = String::from_iter(number_str).parse();

                match literal {
                    Ok(value) => TokenKind::Literal(Literal::Float(FloatLiteral::F32(value))),
                    Err(_) => {
                        LexicalError {
                            line: self.line,
                            column: self.column - 1,
                            code_raw: Some(self.select(start..end)),
                            etype: errors::LexicalErrorType::InvalidFloatLiteral,
                            verbose: None,
                            caret: true,
                            file_name: Some(self.file_name.clone()),
                        }
                        .print();
                        exit(1);
                    }
                }
            } else {
                let literal: Result<i32, _> = String::from_iter(number_str).parse();

                match literal {
                    Ok(value) => TokenKind::Literal(Literal::Integer(IntegerLiteral::I32(value))),
                    Err(_) => {
                        LexicalError {
                            line: self.line,
                            column: self.column - 1,
                            code_raw: Some(self.select(start..end)),
                            etype: errors::LexicalErrorType::InvalidIntegerLiteral,
                            verbose: None,
                            caret: true,
                            file_name: Some(self.file_name.clone()),
                        }
                        .print();
                        exit(1);
                    }
                }
            }
        };

        Token {
            kind: token_kind,
            span: Span { start, end },
        }
    }

    fn is_numeric(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }

    fn is_eof(&mut self) -> bool {
        self.pos == self.input.len()
    }

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

    fn skip_whitespace(&mut self) {
        while Self::is_whitespace(self.ch) {
            self.read_char();

            if self.is_eof() {
                break;
            }
        }
    }

    fn skip_comments(&mut self) {
        let start = self.pos.clone();

        if self.ch == '/' && self.peek_char() == '/' {
            self.read_char();
            self.read_char();

            loop {
                if self.is_eof() || self.ch == '\n' {
                    break;
                }

                self.read_char(); // consume
            }

            loop {
                if self.ch == '\n' {
                    // consume the new line char
                    self.read_char();
                } else {
                    break;
                }
            }
        } else if self.ch == '/' && self.peek_char() == '*' {
            self.read_char();
            self.read_char();

            loop {
                if self.is_eof() || self.ch == '*' {
                    if self.peek_char() != '/' {
                        LexicalError {
                            line: self.line,
                            column: self.column - 1,
                            code_raw: Some(self.select(start..self.pos)),
                            etype: errors::LexicalErrorType::UnterminatedMultiLineComment,
                            verbose: None,
                            caret: true,
                            file_name: Some(self.file_name.clone()),
                        }
                        .print();
                        exit(1);
                    }

                    self.read_char();
                    self.read_char();

                    break;
                }

                self.read_char();
            }

            // Skip extra new lines

            while self.ch == '\n' {
                self.read_char();
            }
        }
    }

    fn lookup_identifier(&mut self, ident: String) -> TokenKind {
        match ident.as_str() {
            "fn" => TokenKind::Function,
            "match" => TokenKind::Match,
            "struct" => TokenKind::Struct,
            "import" => TokenKind::Import,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "for" => TokenKind::For,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            "i8" => TokenKind::I8,
            "i16" => TokenKind::I16,
            "i32" => TokenKind::I32,
            "i64" => TokenKind::I64,
            "i128" => TokenKind::I128,
            "u8" => TokenKind::U8,
            "u16" => TokenKind::U16,
            "u32" => TokenKind::U32,
            "u64" => TokenKind::U64,
            "u128" => TokenKind::U128,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "csize" => TokenKind::CSize,
            "char" => TokenKind::Char,
            "bool" => TokenKind::Bool,
            "void" => TokenKind::Void,
            "string" => TokenKind::String,
            "extern" => TokenKind::Extern,
            "inline" => TokenKind::Inline,
            "pub" => TokenKind::Pub,
            "decl" => TokenKind::Decl,
            _ => TokenKind::Identifier {
                name: ident.to_string(),
            },
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.kind == TokenKind::EOF {
            return None;
        }

        Some(token)
    }
}
