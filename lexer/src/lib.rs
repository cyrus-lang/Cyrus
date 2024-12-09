use std::fmt::{self, Debug};
use token::{Span, Token, TokenKind};

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
            self.input
                .chars()
                .nth(self.next_pos)
                .expect("Failed to read the char with peeked position")
        }
    }

    // The most significant section of the lexer is this method
    // that reads the char with next_pos and returns it.
    fn read_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.ch = ' ';
        } else {
            self.ch = self
                .input
                .chars()
                .nth(self.next_pos)
                .expect("Failed to read the char with current position");
        }

        self.pos = self.next_pos;
        self.next_pos += 1;
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace();
        self.skip_comments();

        if self.is_eof() {
            return Ok(Token {
                kind: TokenKind::EOF,
                span: Span {
                    start: self.pos,
                    end: self.pos,
                },
            });
        }

        let token_kind = match self.ch {
            ' ' => return self.next_token(),
            '+' => {
                if self.peek_char() == '+' {
                    self.read_char();
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::Increment,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    });  
                } else {
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::Plus,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    });
                }
            }
            '-' => {
                if self.peek_char() == '-' {
                    self.read_char();
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::Decrement,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    });                    
                } else {
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::Minus,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    });
                }
            }
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Modulo,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            ',' => TokenKind::Comma,
            '#' => TokenKind::Hashtag,
            '"' => {
                let (start, end, content) = self.read_string()?;
                return Ok(Token {
                    kind: TokenKind::String(content),
                    span: Span { start, end },
                });
            }
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char(); // consume current equal sign
                    self.read_char(); // consume peeked equal sign
                    return Ok(Token {
                        kind: TokenKind::Equal,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    });
                } else {
                    self.read_char(); // consume current equal sign
                    return Ok(Token {
                        kind: TokenKind::Assign,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    });
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::NotEqual,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    });
                } else {
                    self.read_char(); // consume current equal sign
                    return Ok(Token {
                        kind: TokenKind::Bang,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    });
                }
            }
            '<' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::LessEqual,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    });
                } else {
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::LessThan,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    });
                }
            }
            '>' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::GreaterEqual,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    });
                } else {
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::GreaterThan,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    });
                }
            }
            '&' => {
                if self.peek_char() == '&' {
                    self.read_char();
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::And,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    });
                } else {
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::Ampersand,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    });
                }
            }
            '|' => {
                if self.peek_char() == '|' {
                    self.read_char();
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::Or,
                        span: Span {
                            start: self.pos - 2,
                            end: self.pos - 1,
                        },
                    });
                } else {
                    self.read_char();
                    return Ok(Token {
                        kind: TokenKind::Pipe,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    });
                }
            }
            ';' => TokenKind::Semicolon,
            _ => {
                // Reading identifiers and integers is happening here
                let start = self.pos;

                if self.ch.is_alphabetic() {
                    return Ok(Token {
                        kind: self.read_identifider(),
                        span: Span {
                            start,
                            end: self.pos - 1,
                        },
                    });
                } else if self.is_numeric(self.ch) {
                    return Ok(Token {
                        kind: self.read_integer(),
                        span: Span {
                            start,
                            end: self.pos - 1,
                        },
                    });
                } else {
                    #[cfg(test)]
                    dbg!(self.clone());

                    return Err(format!("Illegal character detected '{}'", self.ch));
                }
            }
        };

        self.read_char();
        Ok(Token {
            kind: token_kind,
            span: Span {
                start: self.pos - 1,
                end: self.pos - 1,
            },
        })
    }

    fn read_string(&mut self) -> Result<(usize, usize, String), String> {
        let start: usize = self.pos + 1; // plus one to skip the first double quote
        loop {
            self.read_char();

            if self.ch == '"' || self.ch == '\u{0}' {
                break;
            }

            if self.is_eof() {
                return Err(
                    "expected closing string with double quotation but got nothing".to_string(),
                );
            }
        }

        let content = self.input[start..self.pos].to_string();

        if self.ch == '"' {
            // consume the ending double quote
            self.read_char();
        }

        let end = self.pos;

        Ok((start - 1, end, content))
    }

    fn read_identifider(&mut self) -> TokenKind {
        let start = self.pos;

        while self.ch.is_alphanumeric() || self.ch == '_' {
            self.read_char();
        }

        let end = self.pos;

        let identifier = self.input[start..end].to_string();

        self.lookup_identifier(identifier)
    }

    fn read_integer(&mut self) -> TokenKind {
        let start = self.pos;

        while self.is_numeric(self.ch) {
            self.read_char();
        }

        let end = self.pos;

        let identifier: i64 = self.input[start..end]
            .to_string()
            .parse()
            .expect("expected identifier to be a number but it's something unknown");

        TokenKind::Integer(identifier)
    }

    fn is_numeric(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }

    fn is_eof(&mut self) -> bool {
        self.pos == self.input.len()
    }

    fn is_whitespace(ch: char) -> bool {
        ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
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
            self.read_char(); // consume double slash

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
        }
    }

    fn lookup_identifier(&mut self, ident: String) -> TokenKind {
        match ident.as_str() {
            "fn" => TokenKind::Function,
            "match" => TokenKind::Match,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "ret" => TokenKind::Return,
            "for" => TokenKind::For,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Identifier {
                name: ident.to_string(),
            },
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Err(str) => panic!("{}", str),
            Ok(token) => {
                if token.kind == TokenKind::EOF {
                    return None;
                }

                Some(token)
            }
        }
    }
}

impl fmt::Display for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "pos: {}, next_pos: {}, char: {}",
            self.pos, self.next_pos, self.ch
        )
    }
}
