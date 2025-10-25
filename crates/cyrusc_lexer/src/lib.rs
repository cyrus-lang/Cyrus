use ast::source_loc::SourceLoc;
use ast::token::*;
use ast::{Literal, LiteralKind, StringPrefix};
use diagcentral::{Diag, DiagLevel, DiagLoc, display_single_diag};
use diagnostics::{LexicalDiagKind, lexer_invalid_char_error};
use std::{fmt::Debug, process::exit};
use strescape::escape_string;

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

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        let mut token = self.next_token();
        while token.kind != TokenKind::EOF {
            tokens.push(token.clone());
            token = self.next_token();
        }

        tokens
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
            '~' => TokenKind::Tilde,
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
            '^' => TokenKind::Caret,
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
                }
                if self.peek_char() == '<' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::ShiftLeft,
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
                }
                if self.peek_char() == '>' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::ShiftRight,
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
                } else if self.peek_char() == '~' {
                    self.read_char();
                    self.read_char();
                    return Token {
                        kind: TokenKind::AmpTilde,
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
                } else if self.ch.is_alphabetic() || self.ch == '_' {
                    return self.read_identifier();
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

            // only single unit char is supported
            if self.ch.len_utf8() != 1 {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: LexicalDiagKind::CharLiteralMustBeASingleUnit,
                    location: Some(DiagLoc::new(SourceLoc {
                        line: self.line,
                        column: self.column,
                        file_path: self.file_name.clone()
                    })),
                    hint: Some("Use a string literal for multi-byte characters or emojis.".to_string()),
                });
            }

            final_char = Some(self.ch);

            if self.is_eof() {
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: LexicalDiagKind::UnterminatedStringLiteral,
                    location: Some(DiagLoc::new(SourceLoc {
                        line: self.line,
                        column: self.column,
                        file_path: self.file_name.clone()
                    })),
                    hint: None,
                });
            }
        }

        if self.ch == '\'' {
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
            display_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: LexicalDiagKind::EmptyCharLiteral,
                location: Some(DiagLoc::new(SourceLoc {
                    line: self.line,
                    column: self.column,
                    file_path: self.file_name.clone()
                })),
                hint: None,
            });
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
                display_single_diag!(Diag {
                    level: DiagLevel::Error,
                    kind: LexicalDiagKind::UnterminatedStringLiteral,
                    location: Some(DiagLoc::new(SourceLoc {
                        line: self.line,
                        column: self.column,
                        file_path: self.file_name.clone()
                    })),
                    hint: None,
                });
            }
        }

        if self.ch == '"' {
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

    fn read_literal_suffix(&mut self) -> Option<Box<TokenKind>> {
        if matches!(self.ch, 'f' | 'u' | 'i' | 's') {
            let suffix_token_kind = self.read_identifier().kind;

            Some(Box::new(suffix_token_kind))
        } else {
            None
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

            let suffix = self.read_literal_suffix();

            match i64::from_str_radix(&number[2..], 16) {
                Ok(value) => TokenKind::Literal(Literal {
                    kind: LiteralKind::Integer(value, suffix),
                    loc: Location::new(self.line, self.column),
                    span: Span::new(start, self.column),
                }),
                Err(_) => {
                    display_single_diag!(Diag {
                        level: DiagLevel::Error,
                        kind: LexicalDiagKind::InvalidIntegerLiteral,
                        location: Some(DiagLoc::new(SourceLoc {
                            line: self.line,
                            column: self.column,
                            file_path: self.file_name.clone()
                        })),
                        hint: None,
                    });
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

            let suffix = self.read_literal_suffix();

            if is_float {
                match number.parse::<f64>() {
                    Ok(value) => TokenKind::Literal(Literal {
                        kind: LiteralKind::Float(value, suffix),
                        loc: Location::new(self.line, self.column),
                        span: Span::new(start, self.column),
                    }),
                    Err(_) => {
                        display_single_diag!(Diag {
                            level: DiagLevel::Error,
                            kind: LexicalDiagKind::InvalidFloatLiteral,
                            location: Some(DiagLoc::new(SourceLoc {
                                line: self.line,
                                column: self.column,
                                file_path: self.file_name.clone()
                            })),
                            hint: None,
                        });
                    }
                }
            } else {
                match number.parse::<i64>() {
                    Ok(value) => TokenKind::Literal(Literal {
                        kind: LiteralKind::Integer(value, suffix),
                        loc: Location::new(self.line, self.column),
                        span: Span::new(start, self.column),
                    }),
                    Err(_) => {
                        display_single_diag!(Diag {
                            level: DiagLevel::Error,
                            kind: LexicalDiagKind::InvalidIntegerLiteral,
                            location: Some(DiagLoc::new(SourceLoc {
                                line: self.line,
                                column: self.column,
                                file_path: self.file_name.clone()
                            })),
                            hint: None,
                        });
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
        while self.ch == '/' && (self.peek_char() == '/' || self.peek_char() == '*') {
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
                        kind: LexicalDiagKind::UnterminatedMultiLineComment,
                        location: Some(DiagLoc::new(SourceLoc {
                            line: self.line,
                            column: self.column,
                            file_path: self.file_name.clone()
                        })),
                        hint: None,
                    });
                }

                while !self.is_eof() && self.ch == '\n' {
                    self.read_char();
                }
            }

            self.skip_whitespace();
        }
    }

    fn lookup_typename(&self, ident: String) -> Option<TokenKind> {
        match ident.as_str() {
            "uintptr" => Some(TokenKind::UIntPtr),
            "intptr" => Some(TokenKind::IntPtr),
            "size_t" => Some(TokenKind::SizeT),
            "int" => Some(TokenKind::Int),
            "int8" => Some(TokenKind::Int8),
            "int16" => Some(TokenKind::Int16),
            "int32" => Some(TokenKind::Int32),
            "int64" => Some(TokenKind::Int64),
            "int128" => Some(TokenKind::Int128),
            "uint" => Some(TokenKind::UInt),
            "uint8" => Some(TokenKind::UInt8),
            "uint16" => Some(TokenKind::UInt16),
            "uint32" => Some(TokenKind::UInt32),
            "uint64" => Some(TokenKind::UInt64),
            "uint128" => Some(TokenKind::UInt128),
            "float16" => Some(TokenKind::Float16),
            "float32" => Some(TokenKind::Float32),
            "float64" => Some(TokenKind::Float64),
            "float128" => Some(TokenKind::Float128),
            "char" => Some(TokenKind::Char),
            "bool" => Some(TokenKind::Bool),
            _ => None,
        }
    }

    fn lookup_identifier(&mut self, ident: String) -> TokenKind {
        match ident.as_str() {
            "var" => TokenKind::Var,
            "defer" => TokenKind::Defer,
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
            "pub" => TokenKind::Public,
            "const" => TokenKind::Const,
            "sizeof" => TokenKind::SizeOf,
            _ => self.lookup_typename(ident.clone()).unwrap_or(TokenKind::Identifier {
                name: ident.to_string(),
            }),
        }
    }
}
