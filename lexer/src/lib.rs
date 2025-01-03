use ast::{
    ast::{FloatLiteral, IntegerLiteral, Literal, StringLiteral},
    token::*,
};
use std::{
    borrow::{Borrow, BorrowMut},
    fmt::Debug,
};
use unicode_segmentation::UnicodeSegmentation;
use utils::lexer_error;

mod format;
mod lexer_test;

#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
    pos: usize,
    next_pos: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.trim().to_string(),
            pos: 0,      // points to current position
            next_pos: 0, // points to next position
            ch: ' ',
        };

        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> char {
        if self.next_pos >= self.input.len() {
            ' '
        } else {
            match self.input.chars().nth(self.next_pos) {
                Some(ch) => return ch,
                None => lexer_error!("Failed to read the char with peeked position."),
            }
        }
    }

    fn read_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.ch = ' ';
        } else {
            self.ch = self.input.chars().nth(self.next_pos).unwrap_or(' ');
        }

        self.pos = self.next_pos;
        self.next_pos += 1;
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
            '#' => TokenKind::Hashtag,
            '"' => return self.read_string(),
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
                    lexer_error!(format!("Invalid character detected '{}'", self.ch));
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
                lexer_error!(format!("Expected closing double quote but got EOF."));
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

        while self.ch.is_alphanumeric() || self.ch == '_' {
            self.read_char();
        }

        let end = self.pos;

        let identifier = self.input[start..end].to_string();

        let token_kind = self.lookup_identifier(identifier);

        Token {
            kind: token_kind,
            span: Span { start, end },
        }
    }

    fn read_integer(&mut self) -> Token {
        let start = self.pos;

        let mut is_float = false;

        while self.is_numeric(self.ch) || self.ch == '.' || self.ch == '_' {
            if self.ch == '.' {
                is_float = true;
            }

            self.read_char();
        }

        let end = self.pos;

        let token_kind: TokenKind = {
            if is_float {
                let literal: Result<f32, _> = self.input[start..end].to_string().parse();

                match literal {
                    Ok(value) => TokenKind::Literal(Literal::Float(FloatLiteral::F32(value))),
                    Err(_) => {
                        lexer_error!(format!("Expected number but got {}.", self.ch));
                    }
                }
            } else {
                let literal: Result<i32, _> = self.input[start..end].to_string().parse();

                match literal {
                    Ok(value) => TokenKind::Literal(Literal::Integer(IntegerLiteral::I32(value))),
                    Err(_) => {
                        lexer_error!(format!("Expected number but got {}.", self.ch));
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

    fn consume_emoji(&mut self) -> String {
        let ch = self.input.pop().unwrap();
        ch.to_string()
    }

    fn is_emoji(ch: char) -> bool {
        // Emojis are generally within these ranges in Unicode
        (ch >= '\u{1F600}' && ch <= '\u{1F64F}')         // Emoticons
            || (ch >= '\u{1F300}' && ch <= '\u{1F5FF}')  // Miscellaneous Symbols and Pictographs
            || (ch >= '\u{1F680}' && ch <= '\u{1F6FF}')  // Transport and Map Symbols
            || (ch >= '\u{1F700}' && ch <= '\u{1F77F}')  // Alchemical Symbols
            || (ch >= '\u{1F780}' && ch <= '\u{1F7FF}')  // Geometric Shapes Extended
            || (ch >= '\u{1F800}' && ch <= '\u{1F8FF}')  // Supplemental Arrows-C
            || (ch >= '\u{1F900}' && ch <= '\u{1F9FF}')  // Supplemental Symbols and Pictographs
            || (ch >= '\u{1FA00}' && ch <= '\u{1FA6F}')  // Chess Symbols
            || (ch >= '\u{1FA70}' && ch <= '\u{1FAFF}')  // Symbols and Pictographs Extended-A
            || (ch >= '\u{2600}' && ch <= '\u{26FF}')    // Miscellaneous Symbols
            || (ch >= '\u{2700}' && ch <= '\u{27BF}')    // Dingbats
            || (ch >= '\u{2300}' && ch <= '\u{23FF}')    // Miscellaneous Technical
            || (ch >= '\u{2B50}' && ch <= '\u{2B50}')    // Star emoji (included for general cases)
            || (ch == '\u{1F004}' || ch == '\u{1F0CF}')  // Card game symbols
            || (ch == '\u{1F003}')                       // Mahjong tile emoji
            || (ch == '\u{1F3C1}' || ch == '\u{1F3C2}')  // Racing flags
            || (ch == '\u{2764}' || ch == '\u{1F49C}') // Heart emojis
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

            if self.ch == '\n' {
                // consume the new line char
                self.read_char();
            }
        } else if self.ch == '/' && self.peek_char() == '*' {
            self.read_char();
            self.read_char();

            loop {
                if self.is_eof() || self.ch == '*' {
                    if self.peek_char() != '/' {
                        lexer_error!("Expected to close the multi-line comment with slash.");
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
            "package" => TokenKind::Package,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "return" => TokenKind::Return,
            "for" => TokenKind::For,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "i32" => TokenKind::I32,
            "i64" => TokenKind::I64,
            "u32" => TokenKind::U32,
            "u64" => TokenKind::U64,
            "f32" => TokenKind::F32,
            "f64" => TokenKind::F64,
            "void" => TokenKind::Void,
            "array" => TokenKind::Array,
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
