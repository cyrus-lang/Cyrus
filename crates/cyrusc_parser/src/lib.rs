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
use crate::diagnostics::ParserDiagKind;
use cyrusc_ast::*;
use cyrusc_diagcentral::Diag;
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::read_file;
use cyrusc_lexer::*;
use cyrusc_tokens::Token;
use cyrusc_tokens::TokenKind;
use cyrusc_tokens::loc::Location;
use cyrusc_tokens::loc::Span;
use std::rc::Rc;

mod common;
mod diagnostics;
mod exprs;
mod modifiers;
mod prec;
mod stmts;

/// Parses the program from the given file path.
///
/// # Parameters
/// - `file_path`: The path to the file containing the program code.
///
/// # Returns
/// A tuple containing:
/// - `ProgramTree`: The parsed program representation.
/// - `String`: The name of the file.
pub fn parse_program<'source_file>(source_file: &'source_file SourceFile, file_path: String) -> (ProgramTree, String) {
    let file = read_file(file_path.clone());
    let code = file.0;

    let mut lexer = Lexer::new(source_file, code, file_path.clone());
    let mut parser = Parser::new(lexer.tokenize(), file_path);

    let program = match parser.parse() {
        Ok(program) => program,
        Err(errors) => {
            parser.display_parser_errors(errors);
            panic!();
        }
    };

    (program, file.1)
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    file_name: String,
    errors: Vec<Diag>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file_name: String) -> Self {
        Parser {
            tokens,
            pos: 0,
            file_name,
            errors: Vec::new(),
        }
    }

    #[inline]
    pub fn parse(&mut self) -> Result<ProgramTree, Vec<Diag>> {
        self.parse_program()
    }

    /// Parses the program by repeatedly parsing statements until the end of file (EOF) token is encountered.
    ///
    /// It processes each statement and adds it to the program body. If any errors occur during parsing,
    /// they are accumulated and returned after the entire program has been parsed.
    pub fn parse_program(&mut self) -> Result<ProgramTree, Vec<Diag>> {
        let mut body: Vec<Stmt> = Vec::new();

        while self.current_token().kind != TokenKind::EOF {
            match self.parse_stmt(None, true) {
                Ok(stmts) => body.extend(stmts),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }

        let program = ProgramTree { body: Rc::new(body) };
        self.finalize_program_parse(program)
    }

    #[inline]
    pub fn display_parser_errors(&mut self, errors: Vec<Diag>) {
        if errors.len() > 0 {
            let diag = errors.first().unwrap().clone();
            DiagReporter::display(diag);
        }
    }

    #[inline]
    /// Finalizes the program parse by checking for errors.
    fn finalize_program_parse(&self, program: ProgramTree) -> Result<ProgramTree, Vec<Diag>> {
        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors.clone())
        }
    }

    #[inline]
    fn next_token(&mut self) -> Option<Token> {
        let peek_token = match self.tokens.get(self.pos) {
            Some(token) => token,
            None => {
                self.error_at_peek(ParserDiagKind::InvalidToken(self.peek_token().kind));
                return None;
            }
        };
        self.pos += 1;
        Some(peek_token.clone())
    }

    #[inline]
    fn current_token_is(&self, token_kind: TokenKind) -> bool {
        let current_token = match self.tokens.get(self.pos) {
            Some(token) => token,
            None => {
                if token_kind == TokenKind::EOF {
                    return true;
                } else {
                    return false;
                }
            }
        };
        current_token.kind == token_kind
    }

    #[inline]
    fn peek_token_is(&self, token_kind: TokenKind) -> bool {
        let peek_token = match self.tokens.get(self.pos + 1) {
            Some(token) => token,
            None => {
                if token_kind == TokenKind::EOF {
                    return true;
                } else {
                    return false;
                }
            }
        };
        peek_token.kind == token_kind
    }

    #[inline]
    fn current_token(&self) -> Token {
        match self.tokens.get(self.pos).cloned() {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                span: Span::default(),
                loc: Location::default(),
            },
        }
    }

    #[inline]
    fn peek_token(&self) -> Token {
        match self.tokens.get(self.pos + 1).cloned() {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                span: Span::default(),
                loc: Location::default(),
            },
        }
    }

    #[inline]
    fn peek_n_token(&self, n: usize) -> Option<Token> {
        self.tokens.get(self.pos + n).cloned()
    }

    /// This function peeks at the next token without advancing the lexer. If the token matches
    /// the expected kind, it consumes the token and returns `Ok`. Otherwise, it returns an error
    /// with a message indicating the mismatch.
    fn expect_peek(&mut self, token_kind: TokenKind) -> Result<(), Diag> {
        if self.peek_token_is(token_kind.clone()) {
            self.next_token(); // consume current token
            return Ok(());
        }

        Err(self.error_at_token(&self.peek_token(), ParserDiagKind::ExpectedToken(token_kind)))
    }

    /// This function checks the current token and consumes it if it matches the expected kind.
    /// If the current token does not match, it returns an error indicating the mismatch.
    fn expect_current(&mut self, token_kind: TokenKind) -> Result<(), Diag> {
        if self.current_token_is(token_kind.clone()) {
            self.next_token(); // consume current token
            return Ok(());
        }

        Err(self.error_at_token(&self.peek_token(), ParserDiagKind::ExpectedToken(token_kind)))
    }

    /// Check if current token is a left parenthesis '(' and report error if not
    pub(crate) fn must_be_left_paren(&self) -> Result<(), Diag> {
        if !self.current_token_is(TokenKind::LeftParen) {
            Err(self.error_at_token(&self.prev_token(), ParserDiagKind::MissingOpeningParen))
        } else {
            Ok(())
        }
    }

    /// Check if current token is a right parenthesis ')' and report error if not
    pub(crate) fn must_be_right_paren(&self) -> Result<(), Diag> {
        if !self.current_token_is(TokenKind::RightParen) {
            Err(self.error_at_token(&self.prev_token(), ParserDiagKind::MissingClosingParen))
        } else {
            Ok(())
        }
    }

    /// Check if current token is a left brace '{' and report error if not
    pub(crate) fn must_be_left_brace(&self) -> Result<(), Diag> {
        if !self.current_token_is(TokenKind::LeftBrace) {
            Err(self.error_at_token(&self.prev_token(), ParserDiagKind::MissingOpeningBrace))
        } else {
            Ok(())
        }
    }

    /// Check if current token is a right brace '}' and report error if not
    pub(crate) fn must_be_right_brace(&self) -> Result<(), Diag> {
        if !self.current_token_is(TokenKind::RightBrace) {
            Err(self.error_at_token(&self.prev_token(), ParserDiagKind::MissingClosingBrace))
        } else {
            Ok(())
        }
    }

    /// Check if current token is a semicolon ';' and report error if not
    pub(crate) fn must_be_semicolon(&self) -> Result<(), Diag> {
        if !self.current_token_is(TokenKind::Semicolon) {
            Err(self.error_at_token(&self.prev_token(), ParserDiagKind::MissingSemicolon))
        } else {
            Ok(())
        }
    }

    /// Check if peek token is a semicolon ';' and report error if not
    pub(crate) fn peek_must_be_semicolon(&self) -> Result<(), Diag> {
        if !self.peek_token_is(TokenKind::Semicolon) {
            Err(self.error_at_token(&self.current_token(), ParserDiagKind::MissingSemicolon))
        } else {
            Ok(())
        }
    }

    pub(crate) fn expect_semicolon(&mut self) -> Result<(), Diag> {
        self.must_be_semicolon()?;
        self.next_token();
        Ok(())
    }

    pub(crate) fn expect_peek_semicolon(&mut self) -> Result<(), Diag> {
        self.peek_must_be_semicolon()?;
        self.next_token();
        Ok(())
    }

    fn prev_token(&self) -> Token {
        self.tokens[self.pos - 1].clone()
    }
}
