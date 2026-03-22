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
use cyrusc_diagcentral::{Diag, DiagLevel, exit_with_single_diag, reporter::DiagReporter};
use cyrusc_source_loc::{FileID, Loc, SourceFile};
use cyrusc_strescape::unescape_string;
use cyrusc_tokens::{
    Token, TokenKind,
    literals::{ASTLiteralExpr, LiteralKind, StringPrefix},
};

mod diagnostics;
mod format;
mod tests;

pub struct Lexer<'diag, 'source_file> {
    source_file: &'source_file SourceFile,
    reporter: &'diag DiagReporter,
    pos: usize,
    next_pos: usize,
    ch: char,
    line: usize,
}

impl<'diag, 'source_file> Lexer<'diag, 'source_file> {
    /// Creates a new lexer that reads a single characters from a SourceFile immediately.
    pub fn new(reporter: &'diag DiagReporter, source_file: &'source_file SourceFile) -> Self {
        let mut lexer = Self {
            source_file,
            reporter,
            pos: 0,
            next_pos: 0,
            ch: '\0',
            line: 1,
        };

        lexer.read_char();
        lexer
    }

    /// Returns the FileID of the source file being tokenized.
    #[inline]
    pub fn file_id(&self) -> FileID {
        self.source_file.id
    }

    /// Returns the input source string.
    #[inline]
    pub fn input(&self) -> &str {
        &self.source_file.content
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
        if self.next_pos >= self.input().len() {
            '\0'
        } else {
            self.input()[self.next_pos..].chars().next().unwrap_or('\0')
        }
    }

    fn read_char(&mut self) {
        if self.next_pos >= self.input().len() {
            self.pos = self.input().len();
            self.ch = '\0';
            return;
        }

        let remaining_input = &self.input()[self.next_pos..];
        if let Some(ch) = remaining_input.chars().next() {
            self.ch = ch;
            self.pos = self.next_pos;
            self.next_pos += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
            }
        } else {
            self.ch = '\0';
        }
    }

    #[inline]
    fn single(&mut self, kind: TokenKind) -> TokenKind {
        self.read_char();
        kind
    }

    fn invalid_char(&mut self) -> TokenKind {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(LexicalDiagKind::InvalidChar(self.ch)),
            loc: Some(Loc::new(self.file_id(), self.line, self.pos, self.pos)),
            hint: None,
        });
        self.read_char();
        TokenKind::Invalid
    }

    #[inline]
    fn is_eof(&mut self) -> bool {
        self.pos == self.input().len() || self.ch == '\0'
    }
}

impl<'source_map, 'source_file> Lexer<'source_map, 'source_file> {
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        self.skip_comments();

        let line = self.line;
        let start = self.pos;

        let kind = self.read_token_kind();

        Token {
            kind,
            loc: Loc::new(self.file_id(), line, start, self.pos),
        }
    }

    #[inline]
    fn read_token_kind(&mut self) -> TokenKind {
        match self.ch {
            '\0' => TokenKind::EOF,

            'a'..='z' | 'A'..='Z' | '_' => return self.read_ident_or_prefix(),
            '0'..='9' => return self.read_integer_literal(),

            '"' => return self.read_string_literal(None),
            '\'' => return self.read_char_literal(),

            '.' => return self.read_dot(),

            '+' => return self.read_plus(),
            '-' => return self.read_minus(),
            ':' => return self.read_colon(),
            '=' => return self.read_equal(),
            '!' => return self.read_bang(),
            '<' => return self.read_less(),
            '>' => return self.read_greater(),
            '&' => return self.read_amp(),
            '|' => return self.read_pipe(),

            '~' => self.single(TokenKind::Tilde),
            '*' => self.single(TokenKind::Asterisk),
            '/' => self.single(TokenKind::Slash),
            '%' => self.single(TokenKind::Percent),
            '^' => self.single(TokenKind::Caret),

            '(' => self.single(TokenKind::LeftParen),
            ')' => self.single(TokenKind::RightParen),
            '{' => self.single(TokenKind::LeftBrace),
            '}' => self.single(TokenKind::RightBrace),
            '[' => self.single(TokenKind::LeftBracket),
            ']' => self.single(TokenKind::RightBracket),

            ',' => self.single(TokenKind::Comma),
            ';' => self.single(TokenKind::Semicolon),
            '@' => self.single(TokenKind::At),

            _ => self.invalid_char(),
        }
    }

    fn read_ident_or_prefix(&mut self) -> TokenKind {
        if self.ch == 'c' && self.peek_char() == '"' {
            self.read_char();
            return self.read_string_literal(Some(StringPrefix::C));
        }

        if self.ch == 'b' && self.peek_char() == '"' {
            self.read_char();
            return self.read_string_literal(Some(StringPrefix::B));
        }

        self.read_ident()
    }

    fn read_colon(&mut self) -> TokenKind {
        if self.peek_char() == ':' {
            self.read_char(); // consume first :
            self.read_char(); // consume second :
            TokenKind::DoubleColon
        } else {
            self.read_char(); // consume :
            TokenKind::Colon
        }
    }

    fn read_equal(&mut self) -> TokenKind {
        match self.peek_char() {
            '=' => {
                self.read_char();
                self.read_char();
                TokenKind::Equal
            }
            '>' => {
                self.read_char();
                self.read_char();
                TokenKind::FatArrow
            }
            _ => {
                self.read_char();
                TokenKind::Assign
            }
        }
    }

    fn read_bang(&mut self) -> TokenKind {
        if self.peek_char() == '=' {
            self.read_char();
            self.read_char();
            TokenKind::NotEqual
        } else {
            self.read_char();
            TokenKind::Bang
        }
    }

    fn read_less(&mut self) -> TokenKind {
        match self.peek_char() {
            '=' => {
                self.read_char();
                self.read_char();
                TokenKind::LessEqual
            }
            '<' => {
                self.read_char();
                self.read_char();
                TokenKind::ShiftLeft
            }
            _ => {
                self.read_char();
                TokenKind::LessThan
            }
        }
    }

    fn read_greater(&mut self) -> TokenKind {
        // NOTE: '>>' ambiguity is entrusted to the parser (your original design)
        if self.peek_char() == '=' {
            self.read_char();
            self.read_char();
            TokenKind::GreaterEqual
        } else {
            self.read_char();
            TokenKind::GreaterThan
        }
    }

    fn read_amp(&mut self) -> TokenKind {
        match self.peek_char() {
            '&' => {
                self.read_char();
                self.read_char();
                TokenKind::And
            }
            '~' => {
                self.read_char();
                self.read_char();
                TokenKind::AmpTilde
            }
            _ => {
                self.read_char();
                TokenKind::Ampersand
            }
        }
    }

    fn read_pipe(&mut self) -> TokenKind {
        if self.peek_char() == '|' {
            self.read_char();
            self.read_char();
            TokenKind::Or
        } else {
            self.read_char();
            TokenKind::Pipe
        }
    }

    fn read_dot(&mut self) -> TokenKind {
        self.read_char();

        if self.ch == '.' && self.peek_char() == '.' {
            self.read_char();
            self.read_char();
            TokenKind::TripleDot
        } else if self.ch == '.' {
            self.read_char();
            TokenKind::DoubleDot
        } else {
            TokenKind::Dot
        }
    }

    fn read_minus(&mut self) -> TokenKind {
        match self.peek_char() {
            '-' => {
                self.read_char();
                self.read_char();
                TokenKind::Decrement
            }
            '>' => {
                self.read_char();
                self.read_char();
                TokenKind::ThinArrow
            }
            _ => {
                self.read_char();
                TokenKind::Minus
            }
        }
    }

    fn read_plus(&mut self) -> TokenKind {
        if self.peek_char() == '+' {
            self.read_char();
            self.read_char();
            TokenKind::Increment
        } else {
            self.read_char();
            TokenKind::Plus
        }
    }

    fn read_char_literal(&mut self) -> TokenKind {
        let start = self.pos;
        let line = self.line;

        self.read_char(); // skip opening quote

        if self.is_eof() {
            exit_with_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: Box::new(LexicalDiagKind::UnterminatedStringLiteral),
                loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                hint: None,
            });
        }

        let value = if self.ch == '\\' {
            // escape sequence
            self.read_char();

            let escaped = match self.ch {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '\'' => '\'',
                '0' => '\0',
                _ => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(LexicalDiagKind::InvalidEscapeSequence),
                        loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                        hint: Some("Valid escapes include \\n, \\t, \\\\, \\'.".to_string()),
                    });
                    self.read_char();
                    return TokenKind::Invalid;
                }
            };

            self.read_char();
            escaped
        } else {
            // normal char
            if self.ch.len_utf8() != 1 {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(LexicalDiagKind::CharLiteralMustBeASingleUnit),
                    loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                    hint: Some("Use a string literal for multi-byte characters or emojis.".to_string()),
                });
                self.read_char();
                return TokenKind::Invalid;
            }

            let c = self.ch;
            self.read_char();
            c
        };

        if self.ch != '\'' {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(LexicalDiagKind::CharLiteralMustBeASingleUnit),
                loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                hint: Some("Character literals may contain exactly one character.".to_string()),
            });
            self.read_char();
            return TokenKind::Invalid;
        }

        self.read_char(); // consume closing quote

        TokenKind::Literal(ASTLiteralExpr {
            kind: LiteralKind::Char(value),
            loc: Loc::new(self.file_id(), line, start, self.pos),
        })
    }

    fn read_string_literal(&mut self, prefix: Option<StringPrefix>) -> TokenKind {
        let start = self.pos;
        let line = self.line;

        self.read_char(); // skip opening quote (or prefix char)

        let mut raw = String::new();

        loop {
            if self.is_eof() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(LexicalDiagKind::UnterminatedStringLiteral),
                    loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                    hint: None,
                });
                self.read_char();
                return TokenKind::Invalid;
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
            Ok(str) => str,
            Err(err) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(LexicalDiagKind::InvalidChar(self.ch)),
                    loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                    hint: Some(err.to_string()),
                });
                self.read_char();
                return TokenKind::Invalid;
            }
        };

        TokenKind::Literal(ASTLiteralExpr {
            kind: LiteralKind::String(unescaped, prefix),
            loc: Loc::new(self.file_id(), line, start, self.pos),
        })
    }

    fn read_integer_literal(&mut self) -> TokenKind {
        let start = self.pos;
        let line = self.line;

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
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(LexicalDiagKind::InvalidFloatLiteral),
                        loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                        hint: None,
                    });
                    self.read_char();
                    return TokenKind::Invalid;
                }
            }
        } else {
            let is_unsigned = matches!(suffix, Some(ref token_kind) if token_kind.is_unsigned());
            let value: i128 = if is_unsigned || base != 10 {
                match u128::from_str_radix(&number, base) {
                    Ok(v) => v as i128,
                    Err(_) => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(LexicalDiagKind::InvalidIntegerLiteral),
                            loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                            hint: None,
                        });
                        self.read_char();
                        return TokenKind::Invalid;
                    }
                }
            } else {
                match i128::from_str_radix(&number, base) {
                    Ok(v) => v,
                    Err(_) => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(LexicalDiagKind::InvalidIntegerLiteral),
                            loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                            hint: None,
                        });
                        self.read_char();
                        return TokenKind::Invalid;
                    }
                }
            };

            LiteralKind::Integer(value, suffix)
        };

        TokenKind::Literal(ASTLiteralExpr {
            kind: token_kind,
            loc: Loc::new(self.file_id(), line, start, self.pos),
        })
    }

    fn read_ident(&mut self) -> TokenKind {
        let mut final_ident = String::new();

        while self.ch.is_alphanumeric() || self.ch == '_' {
            final_ident.push(self.ch);
            self.read_char();
        }

        lookup_identifier(final_ident)
    }

    #[inline]
    fn read_literal_suffix(&mut self) -> Option<Box<TokenKind>> {
        if matches!(self.ch, 'f' | 'u' | 'i' | 's') {
            let mut suffix = String::new();

            while self.ch.is_alphanumeric() || self.ch == '_' {
                suffix.push(self.ch);
                self.read_char();
            }

            Some(Box::new(lookup_identifier(suffix)))
        } else {
            None
        }
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        while is_whitespace(self.ch) && !self.is_eof() {
            self.read_char();
        }
    }

    fn skip_comments(&mut self) {
        while self.ch == '/' && (self.peek_char() == '/' || self.peek_char() == '*') {
            let start = self.pos;
            let line = self.line;

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
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(LexicalDiagKind::UnterminatedMultiLineComment),
                        loc: Some(Loc::new(self.file_id(), line, start, self.pos)),
                        hint: None,
                    });
                    self.read_char();
                    return;
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
}

fn lookup_identifier(ident: String) -> TokenKind {
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
        "repr" => TokenKind::Repr,
        "align" => TokenKind::Align,
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
