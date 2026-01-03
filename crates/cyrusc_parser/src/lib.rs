/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_ast::token::*;
use cyrusc_ast::*;
use cyrusc_diagcentral::Diag;
use cyrusc_diagcentral::DiagLevel;
use cyrusc_diagcentral::DiagLoc;
use cyrusc_diagcentral::display_single_diag;
use cyrusc_diagcentral::reporter::DiagReporter;
use cyrusc_fs_utils::read_file;
use cyrusc_lexer::*;
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
pub fn parse_program(file_path: String) -> (ProgramTree, String) {
    let file = read_file(file_path.clone());
    let code = file.0;

    let mut lexer = Lexer::new(code, file_path.clone());
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
    cur_token_idx: usize,
    file_name: String,
    errors: Vec<Diag>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file_name: String) -> Self {
        Parser {
            tokens,
            cur_token_idx: 0,
            file_name,
            errors: Vec::new(),
        }
    }

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
            match self.parse_statement(true) {
                Ok(statement) => body.push(statement),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }

        let program = ProgramTree { body: Rc::new(body) };
        self.finalize_program_parse(program)
    }

    pub fn display_parser_errors(&mut self, errors: Vec<Diag>) {
        let len = errors.len();
        if len > 0 {
            let diag = errors.first().unwrap().clone();
            DiagReporter::display_single(diag);
        }
    }

    /// Finalizes the program parse by checking for errors.
    fn finalize_program_parse(&self, program: ProgramTree) -> Result<ProgramTree, Vec<Diag>> {
        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors.clone())
        }
    }

    fn next_token(&mut self) -> Token {
        let peek_token = match self.tokens.get(self.cur_token_idx) {
            Some(token) => token,
            None => {
                display_single_diag!(Diag {
                    kind: Box::new(ParserDiagKind::InvalidToken(self.peek_token().kind)),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        self.current_token().loc.clone(),
                        self.file_name.clone(),
                    ))),
                    hint: None,
                });
            }
        };
        self.cur_token_idx += 1;
        peek_token.clone()
    }

    fn current_token_is(&self, token_kind: TokenKind) -> bool {
        let current_token = match self.tokens.get(self.cur_token_idx) {
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

    fn peek_token_is(&self, token_kind: TokenKind) -> bool {
        let peek_token = match self.tokens.get(self.cur_token_idx + 1) {
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

    fn current_token(&self) -> Token {
        match self.tokens.get(self.cur_token_idx).cloned() {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                span: Span::default(),
                loc: Location::default(),
            },
        }
    }

    fn peek_token(&self) -> Token {
        match self.tokens.get(self.cur_token_idx + 1).cloned() {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                span: Span::default(),
                loc: Location::default(),
            },
        }
    }

    fn peek_n_token(&self, n: usize) -> Option<Token> {
        self.tokens.get(self.cur_token_idx + n).cloned()
    }

    /// This function peeks at the next token without advancing the lexer. If the token matches
    /// the expected kind, it consumes the token and returns `Ok`. Otherwise, it returns an error
    /// with a message indicating the mismatch.
    fn expect_peek(&mut self, token_kind: TokenKind) -> Result<(), Diag> {
        if self.peek_token_is(token_kind.clone()) {
            self.next_token(); // consume current token
            return Ok(());
        }

        Err(Diag {
            kind: Box::new(ParserDiagKind::ExpectedToken(token_kind)),
            level: DiagLevel::Error,
            location: Some(DiagLoc::new(SourceLoc::from_loc(
                self.current_token().loc,
                self.file_name.clone(),
            ))),
            hint: None,
        })
    }

    /// This function checks the current token and consumes it if it matches the expected kind.
    /// If the current token does not match, it returns an error indicating the mismatch.
    fn expect_current(&mut self, token_kind: TokenKind) -> Result<(), Diag> {
        if self.current_token_is(token_kind.clone()) {
            self.next_token(); // consume current token
            return Ok(());
        }

        Err(Diag {
            kind: Box::new(ParserDiagKind::UnexpectedToken(self.current_token().kind, token_kind)),
            level: DiagLevel::Error,
            location: Some(DiagLoc::new(SourceLoc::from_loc(
                self.current_token().loc,
                self.file_name.clone(),
            ))),
            hint: None,
        })
    }
}
