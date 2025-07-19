use ::diag::errors::CompileTimeError;
use ast::ast::{Literal, LiteralKind, StringPrefix};
use ast::token::*;
use diag::{LexicalErrorType, lexer_invalid_char_error};
use std::{fmt::Debug, process::exit};
use utils::escaping::escape_string;

mod diag;
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
            pos: 0,      // points to current position
            next_pos: 0, // points to next position
            ch: '\0',
            file_name,
            line: 1,
            column: 0,
        };

        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> char {
        self.input[self.next_pos..].chars().next().unwrap_or('\0')
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
                self.column = 0;
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

        let loc = Location::new(self.line, self.column);

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
            ' ' => {
                self.next_token(); // consume whitespace
                return self.next_token(); // recurse to get the next non-whitespace token
            }
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
                        loc,
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Plus,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                        loc,
                    };
                } else if self.peek_char() == '>' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::FatArrow,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                        loc,
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Minus,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                        loc,
                    };
                } else {
                    return Token {
                        kind: TokenKind::Colon,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                    loc,
                };
            }
            '#' => TokenKind::Hashtag,
            '"' => return self.read_string(None),
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
                        loc,
                    };
                } else {
                    self.read_char(); // consume current equal sign
                    return Token {
                        kind: TokenKind::Assign,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                        loc,
                    };
                } else {
                    self.read_char(); // consume current equal sign
                    return Token {
                        kind: TokenKind::Bang,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                        loc,
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::LessThan,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                        loc,
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::GreaterThan,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                        loc,
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Ampersand,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                        loc,
                    };
                } else {
                    self.read_char();
                    return Token {
                        kind: TokenKind::Pipe,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                        loc,
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
                }
                else if self.ch.is_alphabetic() || self.ch == '_' {
                    return self.read_identifier();
                } else if self.is_numeric(self.ch) {
                    return self.read_number();
                } else {
                    lexer_invalid_char_error(
                        self.file_name.clone(),
                        self.line,
                        self.column - 1,
                        self.ch,
                        Span::new(self.column - 1, self.column),
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
            loc,
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
                    caret: Some(Span::new(start, self.column + 1)),
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
                kind: TokenKind::Literal(Literal {
                    kind: LiteralKind::Char(value),
                    loc: Location::new(self.line, self.column),
                    span: span.clone(),
                }),
                span,
                loc: Location::new(self.line, self.column),
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
                caret: Some(span),
                file_name: Some(self.file_name.clone()),
            }
            .print();
            exit(1);
        }
    }

    fn read_string(&mut self, prefix: Option<StringPrefix>) -> Token {
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
                    caret: Some(Span::new(start, self.column + 1)),
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
            kind: TokenKind::Literal(Literal {
                kind: LiteralKind::String(escape_string(&final_string), prefix),
                span: span.clone(),
                loc: Location::new(self.line, self.column),
            }),
            span,
            loc: Location::new(self.line, self.column),
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
            loc: Location::new(self.line, self.column),
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        let mut number = String::new();
        let mut is_float = false;

        // hexadecimal literals
        let token_kind = if self.ch == '0' && (self.peek_char().to_ascii_lowercase() == 'x') {
            number.push('0');
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
                Ok(value) => TokenKind::Literal(Literal {
                    kind: LiteralKind::Integer(value),
                    loc: Location::new(self.line, self.column),
                    span: Span::new(start, self.column),
                }),
                Err(_) => {
                    CompileTimeError {
                        location: Location {
                            line: self.line,
                            column: self.column,
                        },
                        source_content: Box::new(self.input.clone()),
                        etype: LexicalErrorType::InvalidIntegerLiteral,
                        verbose: None,
                        caret: Some(Span::new(start, self.column + 1)),
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
                    Ok(value) => TokenKind::Literal(Literal {
                        kind: LiteralKind::Float(value),
                        loc: Location::new(self.line, self.column),
                        span: Span::new(start, self.column),
                    }),
                    Err(_) => {
                        CompileTimeError {
                            location: Location {
                                line: self.line,
                                column: self.column,
                            },
                            source_content: Box::new(self.input.clone()),
                            etype: LexicalErrorType::InvalidFloatLiteral,
                            verbose: None,
                            caret: Some(Span::new(start, self.column + 1)),
                            file_name: Some(self.file_name.clone()),
                        }
                        .print();
                        exit(1);
                    }
                }
            } else {
                match number.parse::<i64>() {
                    Ok(value) => TokenKind::Literal(Literal {
                        kind: LiteralKind::Integer(value),
                        loc: Location::new(self.line, self.column),
                        span: Span::new(start, self.column),
                    }),
                    Err(_) => {
                        CompileTimeError {
                            location: Location {
                                line: self.line,
                                column: self.column,
                            },
                            source_content: Box::new(self.input.clone()),
                            etype: LexicalErrorType::InvalidIntegerLiteral,
                            verbose: None,
                            caret: Some(Span::new(start, self.column + 1)),
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
            loc: Location::new(self.line, self.column),
        }
    }

    fn is_numeric(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }

    fn is_eof(&mut self) -> bool {
        self.pos == self.input.len() || self.ch == '\0'
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
        while Self::is_whitespace(self.ch) && !self.is_eof() {
            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
        let start = self.pos;

        while self.ch == '/' && (self.peek_char() == '/' || self.peek_char() == '*') {
            if self.peek_char() == '/' {
                // handle single-line comment
                self.read_char();
                self.read_char();

                while !self.is_eof() && self.ch != '\n' {
                    self.read_char();
                }

                // Consume the newline character, if present
                if !self.is_eof() && self.ch == '\n' {
                    self.read_char();
                }
            } else if self.peek_char() == '*' {
                // handle multi-line comment

                self.read_char();
                self.read_char();
                while self.peek_char() != '\0' {
                    if self.ch == '*' && self.peek_char() == '/' {
                        break;
                    }
                    self.read_char();
                }

                if !(self.ch == '*' && self.peek_char() == '/') {
                    CompileTimeError {
                        location: Location {
                            line: self.line,
                            column: self.column,
                        },
                        source_content: Box::new(self.input.clone()),
                        etype: LexicalErrorType::UnterminatedMultiLineComment,
                        verbose: None,
                        caret: Some(Span::new(start, self.column + 1)),
                        file_name: Some(self.file_name.clone()),
                    }
                    .print();
                    exit(1);
                }
                self.read_char();
                self.read_char();

                // skip any trailing newlines after the comment
                while !self.is_eof() && self.ch == '\n' {
                    self.read_char();
                }
            }

            // skip whitespace to advance to the next valid character
            self.skip_whitespace();
        }
    }

    fn lookup_identifier(&mut self, ident: String) -> TokenKind {
        match ident.as_str() {
            "type" => TokenKind::Typedef,
            "func" => TokenKind::Function,
            "match" => TokenKind::Match,
            "struct" => TokenKind::Struct,
            "bits" => TokenKind::Bits,
            "import" => TokenKind::Import,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "for" => TokenKind::For,
            "foreach" => TokenKind::Foreach,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            "as" => TokenKind::As,
            "uintptr" => TokenKind::UIntPtr,
            "intptr" => TokenKind::IntPtr,
            "size_t" => TokenKind::SizeT,
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
            "in" => TokenKind::In,
            "enum" => TokenKind::Enum,
            "void" => TokenKind::Void,
            "extern" => TokenKind::Extern,
            "inline" => TokenKind::Inline,
            "public" => TokenKind::Public,
            "const" => TokenKind::Const,
            "sizeof" => TokenKind::SizeOf,
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

pub fn new_lexer_debugger(input: String) {
    let mut lexer = Lexer::new(input, "debug.cyr".to_string());
    loop {
        let token = lexer.next_token();
        if token.kind == TokenKind::EOF {
            break;
        }

        println!(
            "{:?} Span({}, {}) Line({}) Column({})",
            token.kind, token.span.start, token.span.end, token.loc.line, token.loc.column
        );
    }
}
