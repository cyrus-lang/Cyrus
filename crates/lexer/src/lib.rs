use ::diag::errors::CompileTimeError;
use ast::ast::Literal;
use ast::token::*;
use core::panic;
use diag::{LexicalErrorType, lexer_invalid_char_error, lexer_unknown_char_error};
use std::{fmt::Debug, ops::Range, process::exit};

mod diag;
mod format;
mod tests;

macro_rules! single_char_token {
    ($self:ident, $kind:expr) => {{
        $self.read_char();
        Token {
            kind: $kind,
            span: Span {
                start: $self.pos - 1,
                end: $self.pos - 1,
            },
        }
    }};
}

macro_rules! double_char_token {
    ($self:ident, $first_char:expr, $second_char:expr, $kind:expr) => {{
        $self.read_char(); // consume first char
        $self.read_char(); // consume second char
        Token {
            kind: $kind,
            span: Span {
                start: $self.pos - 2,
                end: $self.pos - 1,
            },
        }
    }};
}

macro_rules! lexer_error {
    ($self:ident, $error_type:expr) => {{
        CompileTimeError {
            location: Location {
                line: $self.line,
                column: $self.column,
            },
            source_content: Box::new($self.input.clone()),
            etype: $error_type,
            verbose: None,
            caret: true,
            file_name: Some($self.file_name.clone()),
        }
        .print();
        exit(1);
    }};
}

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
            ch: '\0',
            file_name,
            line: 1,
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
        self.input.chars().nth(self.next_pos).unwrap_or('\0')
    }

    fn read_char(&mut self) {
        self.ch = self.input.chars().nth(self.next_pos).unwrap_or('\0');
        self.pos = self.next_pos;
        self.next_pos += self.ch.len_utf8();
        if self.ch == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        if self.ch == '\0' {
            return Token {
                kind: TokenKind::EOF,
                span: Span {
                    start: self.pos,
                    end: self.pos,
                },
            };
        }

        let token = match self.ch {
            '`' => single_char_token!(self, TokenKind::BackTick),
            '+' => {
                if self.peek_char() == '+' {
                    double_char_token!(self, '+', '+', TokenKind::Increment)
                } else {
                    single_char_token!(self, TokenKind::Plus)
                }
            }
            '-' => {
                if self.peek_char() == '-' {
                    double_char_token!(self, '-', '-', TokenKind::Decrement)
                } else {
                    single_char_token!(self, TokenKind::Minus)
                }
            }
            '*' => single_char_token!(self, TokenKind::Asterisk),
            '/' => single_char_token!(self, TokenKind::Slash),
            '%' => single_char_token!(self, TokenKind::Percent),
            '(' => single_char_token!(self, TokenKind::LeftParen),
            ')' => single_char_token!(self, TokenKind::RightParen),
            '{' => single_char_token!(self, TokenKind::LeftBrace),
            '}' => single_char_token!(self, TokenKind::RightBrace),
            '[' => single_char_token!(self, TokenKind::LeftBracket),
            ']' => single_char_token!(self, TokenKind::RightBracket),
            ',' => single_char_token!(self, TokenKind::Comma),
            '#' => single_char_token!(self, TokenKind::Hashtag),
            ';' => single_char_token!(self, TokenKind::Semicolon),
            
            ':' => {
                if self.peek_char() == ':' {
                    double_char_token!(self, ':', ':', TokenKind::DoubleColon)
                } else {
                    single_char_token!(self, TokenKind::Colon)
                }
            }
            
            '.' => {
                self.read_char();
                if self.ch == '.' && self.peek_char() == '.' {
                    double_char_token!(self, '.', '.', TokenKind::TripleDot)
                } else if self.ch == '.' {
                    single_char_token!(self, TokenKind::DoubleDot)
                } else {
                    Token {
                        kind: TokenKind::Dot,
                        span: Span {
                            start: self.pos - 1,
                            end: self.pos - 1,
                        },
                    }
                }
            }
            
            '=' => {
                if self.peek_char() == '=' {
                    double_char_token!(self, '=', '=', TokenKind::Equal)
                } else {
                    single_char_token!(self, TokenKind::Assign)
                }
            }
            
            '!' => {
                if self.peek_char() == '=' {
                    double_char_token!(self, '!', '=', TokenKind::NotEqual)
                } else {
                    single_char_token!(self, TokenKind::Bang)
                }
            }
            
            '<' => {
                if self.peek_char() == '=' {
                    double_char_token!(self, '<', '=', TokenKind::LessEqual)
                } else {
                    single_char_token!(self, TokenKind::LessThan)
                }
            }
            
            '>' => {
                if self.peek_char() == '=' {
                    double_char_token!(self, '>', '=', TokenKind::GreaterEqual)
                } else {
                    single_char_token!(self, TokenKind::GreaterThan)
                }
            }
            
            '&' => {
                if self.peek_char() == '&' {
                    double_char_token!(self, '&', '&', TokenKind::And)
                } else {
                    single_char_token!(self, TokenKind::Ampersand)
                }
            }
            
            '|' => {
                if self.peek_char() == '|' {
                    double_char_token!(self, '|', '|', TokenKind::Or)
                } else {
                    single_char_token!(self, TokenKind::Pipe)
                }
            }
            
            '"' => return self.read_string(),
            '\'' => return self.read_char_literal(),
            
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

        token
    }

    fn read_char_literal(&mut self) -> Token {
        let (value, start, end) = self.read_quoted('\'', LexicalErrorType::UnterminatedStringLiteral);
        
        if value.len() != 1 {
            lexer_error!(self, LexicalErrorType::EmptyCharLiteral);
        }
        
        Token {
            kind: TokenKind::Literal(Literal::Char(value.chars().next().unwrap())),
            span: Span { start, end },
        }
    }

    fn read_string(&mut self) -> Token {
        let (value, start, end) = self.read_quoted('"', LexicalErrorType::UnterminatedStringLiteral);
        Token {
            kind: TokenKind::Literal(Literal::String(value)),
            span: Span { start, end },
        }
    }

    fn read_quoted(&mut self, quote_char: char, error_type: LexicalErrorType) -> (String, usize, usize) {
        let start = self.pos + 1;
        let mut value = String::new();
    
        loop {
            self.read_char();
            
            if self.ch == quote_char {
                break;
            }
            
            if self.is_eof() {
                lexer_error!(self, error_type);
            }
            
            value.push(self.ch);
        }
    
        self.read_char(); // consume closing quote
        let end = self.pos;
        
        (value, start - 1, end)
    }

    fn read_identifier(&mut self) -> Token {
        let start = self.pos;
        let mut ident = String::new();
    
        while self.ch.is_alphanumeric() || self.ch == '_' {
            ident.push(self.ch);
            self.read_char();
        }
    
        Token {
            kind: self.lookup_identifier(ident),
            span: Span { start, end: self.pos },
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        
        match (self.ch, self.peek_char().to_ascii_lowercase()) {
            ('0', 'x') => self.read_hex_number(start),
            ('0', 'b') => self.read_binary_number(start),
            ('0', 'o') => self.read_octal_number(start),
            _ => self.read_decimal_number(start),
        }
    }

    fn read_octal_number(&mut self, start: usize) -> Token {
        let mut number = String::new();
        number.push('0');
        self.read_char(); // consume '0'
        number.push(self.ch);
        self.read_char(); // consume 'o' or 'O'
    
        while self.ch.is_digit(8) || self.ch == '_' {
            if self.ch != '_' {
                number.push(self.ch);
            }
            self.read_char();
        }
    
        match i64::from_str_radix(&number[2..], 8) {
            Ok(value) => Token {
                kind: TokenKind::Literal(Literal::Integer(value)),
                span: Span { start, end: self.pos },
            },
            Err(_) => self.number_parse_error(start),
        }
    }

    fn read_binary_number(&mut self, start: usize) -> Token {
        let mut number = String::new();
        number.push('0');
        self.read_char(); // consume '0'
        number.push(self.ch);
        self.read_char(); // consume 'b' or 'B'
    
        while self.ch == '0' || self.ch == '1' || self.ch == '_' {
            if self.ch != '_' {
                number.push(self.ch);
            }
            self.read_char();
        }
    
        match i64::from_str_radix(&number[2..], 2) {
            Ok(value) => Token {
                kind: TokenKind::Literal(Literal::Integer(value)),
                span: Span { start, end: self.pos },
            },
            Err(_) => self.number_parse_error(start),
        }
    }

    fn read_hex_number(&mut self, start: usize) -> Token {
        let mut number = String::new();
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
            Ok(value) => Token {
                kind: TokenKind::Literal(Literal::Integer(value)),
                span: Span { start, end: self.pos },
            },
            Err(_) => self.number_parse_error(start),
        }
    }
    
    fn read_decimal_number(&mut self, start: usize) -> Token {
        let mut number = String::new();
        let mut is_float = false;
    
        // Integer part
        while self.ch.is_ascii_digit() || self.ch == '_' {
            if self.ch != '_' {
                number.push(self.ch);
            }
            self.read_char();
        }
    
        // Decimal part
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
    
        // Exponent part
        if (self.ch == 'e' || self.ch == 'E') && (self.peek_char().is_ascii_digit() || 
            (self.peek_char() == '+' || self.peek_char() == '-')) {
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
    
        // Suffixes
        if matches!(self.ch, 'f' | 'F' | 'l' | 'L') {
            is_float = matches!(self.ch, 'f' | 'F');
            number.push(self.ch);
            self.read_char();
        }
    
        if is_float {
            match number.parse::<f64>() {
                Ok(value) => Token {
                    kind: TokenKind::Literal(Literal::Float(value)),
                    span: Span { start, end: self.pos },
                },
                Err(_) => self.number_parse_error(start),
            }
        } else {
            match number.parse::<i64>() {
                Ok(value) => Token {
                    kind: TokenKind::Literal(Literal::Integer(value)),
                    span: Span { start, end: self.pos },
                },
                Err(_) => self.number_parse_error(start),
            }
        }
    }
    
    fn number_parse_error(&self, start: usize) -> ! {
        CompileTimeError {
            location: Location {
                line: self.line,
                column: self.column,
            },
            source_content: Box::new(self.input.clone()),
            etype: if start == self.pos {
                LexicalErrorType::InvalidIntegerLiteral
            } else {
                LexicalErrorType::InvalidFloatLiteral
            },
            verbose: None,
            caret: true,
            file_name: Some(self.file_name.clone()),
        }
        .print();
        exit(1);
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
        while Self::is_whitespace(self.ch) && !self.is_eof() {
            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
        while self.ch == '/' && (self.peek_char() == '/' || self.peek_char() == '*') {
            if self.peek_char() == '/' {
                // Handle single-line comment
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
                // Handle multi-line comment
                self.read_char();
                self.read_char();
    
                while !self.is_eof() {
                    if self.ch == '*' && self.peek_char() == '/' {
                        self.read_char();
                        self.read_char();
                        break;
                    }
                    self.read_char();
                }
                
                // Skip any trailing newlines after the comment
                while !self.is_eof() && self.ch == '\n' {
                    self.read_char();
                }
            }
            
            // Skip whitespace to advance to the next valid character
            // self.skip_whitespace();
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            self.skip_whitespace();
            let start = self.pos;
            self.skip_comments();
            if self.pos == start {
                break;
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
            "string" => TokenKind::String,
            "extern" => TokenKind::Extern,
            "inline" => TokenKind::Inline,
            "public" => TokenKind::Public,
            "const" => TokenKind::Const,
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