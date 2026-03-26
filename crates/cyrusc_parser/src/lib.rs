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
use cyrusc_lexer::Lexer;
use cyrusc_source_loc::{FileID, Loc, SourceFile};
use cyrusc_tokens::{Token, TokenKind};
use std::{fmt, rc::Rc, sync::Arc};

mod common;
mod diagnostics;
mod exprs;
mod modifiers;
mod prec;
mod stmts;

const DISPLAY_DIAG_KIND: usize = 1;

pub struct Parser<'source_file> {
    source_file: &'source_file SourceFile,
    reporter: Arc<DiagReporter>,
    tokens: Vec<Token>,
    pos: usize,
    last_loc: Loc,
}

pub struct SourceParser {
    reporter: Arc<DiagReporter>,
}

impl SourceParser {
    #[inline]
    pub fn parse_program(&self, source_file: &SourceFile) -> Result<ProgramTree, ()> {
        let mut lexer = Lexer::new(&self.reporter, source_file);
        let tokens = lexer.tokenize();

        let mut parser = Parser::new(self.reporter.clone(), source_file, tokens);

        let Ok(program_tree) = parser.parse() else {
            self.display_errors();
            return Err(());
        };

        Ok(program_tree)
    }

    #[inline]
    pub fn display_errors(&self) {
        self.reporter.display_first(DISPLAY_DIAG_KIND);
    }
}

impl SourceParser {
    pub fn new(reporter: Arc<DiagReporter>) -> Self {
        Self { reporter }
    }
}

impl<'source_file> Parser<'source_file> {
    pub fn new(reporter: Arc<DiagReporter>, source_file: &'source_file SourceFile, tokens: Vec<Token>) -> Self {
        let initial_loc = Loc::default(source_file.id);

        Parser {
            source_file,
            reporter,
            tokens,
            pos: 0,
            last_loc: initial_loc,
        }
    }

    /// Returns the FileID of the source file being tokenized.
    #[inline]
    pub fn file_id(&self) -> FileID {
        self.source_file.id
    }

    /// Parses the program by repeatedly parsing statements until the end of file (EOF) token is encountered.
    ///
    /// It processes each statement and adds it to the program body. If any errors occur during parsing,
    /// they are accumulated and returned after the entire program has been parsed.
    pub fn parse(&mut self) -> Result<ProgramTree, ()> {
        let mut body: Vec<ASTStmt> = Vec::new();

        while self.current_token().kind != TokenKind::EOF {
            match self.parse_stmt(None, true) {
                Ok(stmts) => body.extend(stmts),
                Err(diag) => {
                    self.reporter.report(diag);
                }
            }
            self.next_token();
        }

        let program = ProgramTree { body: Rc::new(body) };
        self.finalize_program_parse(program)
    }

    /// Finalizes the program parse by checking for errors.
    #[inline]
    fn finalize_program_parse(&self, program: ProgramTree) -> Result<ProgramTree, ()> {
        if !self.reporter.has_errors() {
            Ok(program)
        } else {
            Err(())
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

        self.last_loc = self.current_token().loc;
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
            None => {
                // generate proper EOF using last known location
                Token {
                    kind: TokenKind::EOF,
                    loc: Loc {
                        file_id: self.last_loc.file_id,
                        line: self.last_loc.line,
                        column: self.last_loc.column,
                        start: self.last_loc.end,
                        end: self.last_loc.end,
                    },
                }
            }
        }
    }

    #[inline]
    fn peek_token(&self) -> Token {
        match self.tokens.get(self.pos + 1).cloned() {
            Some(token) => token,
            None => {
                // generate proper EOF using last known location
                Token {
                    kind: TokenKind::EOF,
                    loc: Loc {
                        file_id: self.last_loc.file_id,
                        line: self.last_loc.line,
                        column: self.last_loc.column,
                        start: self.last_loc.end,
                        end: self.last_loc.end,
                    },
                }
            }
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

impl<'source_file> fmt::Debug for Parser<'source_file> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("source_file", &self.source_file)
            .field("tokens", &self.tokens)
            .field("pos", &self.pos)
            .field("last_loc", &self.last_loc)
            .finish()
    }
}

impl fmt::Debug for SourceParser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceParser").finish()
    }
}

unsafe impl<'source_file> Send for Parser<'source_file> {}
unsafe impl<'source_file> Sync for Parser<'source_file> {}
