use ::diag::errors::CompileTimeError;
use ast::ast::Literal;
use ast::token::*;
use core::panic;
use diag::{LexicalErrorType, lexer_invalid_char_error, lexer_unknown_char_error};
use std::{fmt::Debug, ops::Range, process::exit};

mod diag;
mod format;
mod tests;

#[derive(Debug, Clone)]
pub struct Lexer {
    pos: usize,
    next_pos: usize,
    ch: char,
    pub input: String,
    pub file_name: String,
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
                Some(ch) => ch,
                None => {
                    lexer_unknown_char_error(
                        self.file_name.clone(),
                        self.line,
                        self.column - 1,
                        self.ch,
                        Box::new(self.input.clone()),
                    );
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
        }

        if self.ch == '\n' {
            self.line += 1;
            self.column = 0;
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
            ':' => {
                self.read_char();
                if self.ch == ':' {
                    self.read_char();
                    return Token {
                        kind: TokenKind::DoubleColon,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    };
                } else {
                    return Token {
                        kind: TokenKind::Colon,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
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
                    span: Span {
                        start: self.pos - 1,
                        end: self.pos - 1,
                    },
                };
            }
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
            _ => {
                if self.ch.is_alphabetic() || self.ch == '_' {
                    return self.read_identifier();
                } else if self.is_numeric(self.ch) {
                    return self.read_number();
                } else {
                    lexer_invalid_char_error(
                        self.file_name.clone(),
                        self.line,
                        self.column - 1,
                        self.ch,
                        Box::new(self.input.clone()),
                    );
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
                CompileTimeError {
                    location: Location {
                        line: self.line,
                        column: self.column,
                    },
                    source_content: Box::new(self.input.clone()),
                    etype: LexicalErrorType::UnterminatedStringLiteral,
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

        if let Some(value) = final_char {
            Token {
                kind: TokenKind::Literal(Literal::Char(value)),
                span,
            }
        } else {
            CompileTimeError {
                location: Location {
                    line: self.line,
                    column: self.column,
                },
                source_content: Box::new(self.input.clone()),
                etype: LexicalErrorType::EmptyCharLiteral,
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
                CompileTimeError {
                    location: Location {
                        line: self.line,
                        column: self.column,
                    },
                    source_content: Box::new(self.input.clone()),
                    etype: LexicalErrorType::UnterminatedStringLiteral,
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
            kind: TokenKind::Literal(Literal::String(final_string)),
            span,
        }
    }

    fn read_identifier(&mut self) -> Token {
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

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        let mut number = String::new();
        let mut is_float = false;

        // hexadecimal literals
        let token_kind = if self.ch == '0' && (self.peek_char() == 'x' || self.peek_char() == 'X') {
            number.push(self.ch);
            self.read_char(); // consume '0'
            number.push(self.ch);
            self.read_char(); // consume 'x' or 'X'

            while self.ch.is_ascii_hexdigit() || self.ch == '_' {
                if self.ch != '_' {
                    number.push(self.ch);
                }
                self.read_char();
            }

            match i64::from_str_radix(&number[2..], 16) {
                Ok(value) => TokenKind::Literal(Literal::Integer(value)),
                Err(_) => {
                    CompileTimeError {
                        location: Location {
                            line: self.line,
                            column: self.column,
                        },
                        source_content: Box::new(self.input.clone()),
                        etype: LexicalErrorType::InvalidIntegerLiteral,
                        verbose: None,
                        caret: true,
                        file_name: Some(self.file_name.clone()),
                    }
                    .print();
                    exit(1);
                }
            }
        } else {
            // Match leading digits (optional)
            while self.ch.is_ascii_digit() || self.ch == '_' {
                if self.ch != '_' {
                    number.push(self.ch);
                }
                self.read_char();
            }

            // Decimal point and fractional part
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

            // Exponent part (e.g., e+10, E-5)
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

            if matches!(self.ch, 'f' | 'F' | 'l' | 'L') {
                number.push(self.ch);
                self.read_char();
            }

            if is_float {
                match number.parse::<f64>() {
                    Ok(value) => TokenKind::Literal(Literal::Float(value)),
                    Err(_) => {
                        CompileTimeError {
                            location: Location {
                                line: self.line,
                                column: self.column,
                            },
                            source_content: Box::new(self.input.clone()),
                            etype: LexicalErrorType::InvalidFloatLiteral,
                            verbose: None,
                            caret: true,
                            file_name: Some(self.file_name.clone()),
                        }
                        .print();
                        exit(1);
                    }
                }
            } else {
                match number.parse::<i64>() {
                    Ok(value) => TokenKind::Literal(Literal::Integer(value)),
                    Err(_) => {
                        CompileTimeError {
                            location: Location {
                                line: self.line,
                                column: self.column,
                            },
                            source_content: Box::new(self.input.clone()),
                            etype: LexicalErrorType::InvalidIntegerLiteral,
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
            span: Span { start, end: self.pos },
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
            if self.is_eof() {
                break;
            }

            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
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
                        CompileTimeError {
                            location: Location {
                                line: self.line,
                                column: self.column,
                            },
                            source_content: Box::new(self.input.clone()),
                            etype: LexicalErrorType::UnterminatedMultiLineComment,
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
            "as" => TokenKind::As,
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
            "f16" => TokenKind::F16,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "f128" => TokenKind::F128,
            "size_t" => TokenKind::SizeT,
            "char" => TokenKind::Char,
            "bool" => TokenKind::Bool,
            "in" => TokenKind::In,
            "enum" => TokenKind::Enum,
            "void" => TokenKind::Void,
            "string" => TokenKind::String,
            "extern" => TokenKind::Extern,
            "inline" => TokenKind::Inline,
            "public" => TokenKind::Public,
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
