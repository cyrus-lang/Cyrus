use ast::source_loc::SourceLoc;
use ast::token::*;
use ast::*;
use diagcentral::Diag;
use diagcentral::DiagLevel;
use diagcentral::DiagLoc;
use diagcentral::display_single_diag;
use diagcentral::reporter::DiagReporter;
use lexer::*;
use std::rc::Rc;
use utils::fs::read_file;

use crate::diagnostics::ParserDiagKind;

mod common;
mod diagnostics;
mod exprs;
mod prec;
mod stmts;

pub type ParserError = Diag<ParserDiagKind>;

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
        Ok(result) => {
            if let Node::ProgramTree(program) = result {
                program
            } else {
                panic!("Expected a program given as input to the compiler but got unknown.");
            }
        }
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
    errors: Vec<ParserError>,
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

    /// Parses the entire input program and returns it as a `Node::ProgramTree`.
    pub fn parse(&mut self) -> Result<Node, Vec<ParserError>> {
        let program = self.parse_program()?;
        Ok(Node::ProgramTree(program))
    }

    /// Parses the program by repeatedly parsing statements until the end of file (EOF) token is encountered.
    ///
    /// It processes each statement and adds it to the program body. If any errors occur during parsing,
    /// they are accumulated and returned after the entire program has been parsed.
    pub fn parse_program(&mut self) -> Result<ProgramTree, Vec<ParserError>> {
        let mut body: Vec<Statement> = Vec::new();

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

    pub fn display_parser_errors(&mut self, errors: Vec<ParserError>) {
        let len = errors.len();
        if len > 0 {
            let diag = errors.first().unwrap().clone();
            DiagReporter::display_single(diag);
        }
    }

    /// Finalizes the program parse by checking for errors.
    pub fn finalize_program_parse(&self, program: ProgramTree) -> Result<ProgramTree, Vec<ParserError>> {
        if self.errors.is_empty() {
            Ok(program)
        } else {
            Err(self.errors.clone())
        }
    }

    pub fn next_token(&mut self) -> Token {
        let peek_token = match self.tokens.get(self.cur_token_idx) {
            Some(token) => token,
            None => {
                display_single_diag!(Diag {
                    kind: ParserDiagKind::InvalidToken(self.peek_token().kind),
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

    pub fn current_token_is(&self, token_kind: TokenKind) -> bool {
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

    pub fn peek_token_is(&self, token_kind: TokenKind) -> bool {
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

    pub fn current_token(&self) -> Token {
        match self.tokens.get(self.cur_token_idx).cloned() {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                span: Span::default(),
                loc: Location::default(),
            },
        }
    }

    pub fn peek_token(&self) -> Token {
        match self.tokens.get(self.cur_token_idx + 1).cloned() {
            Some(token) => token,
            None => Token {
                kind: TokenKind::EOF,
                span: Span::default(),
                loc: Location::default(),
            },
        }
    }

    pub fn peek_peek_token_is(&self, token_kind: TokenKind) -> Option<bool> {
        match self.tokens.get(self.cur_token_idx + 2) {
            Some(token) => Some(token.kind == token_kind),
            None => None,
        }
    }

    pub fn peek_n_token(&self, n: usize) -> Option<Token> {
        self.tokens.get(self.cur_token_idx + n).cloned()
    }

    /// This function peeks at the next token without advancing the lexer. If the token matches
    /// the expected kind, it consumes the token and returns `Ok`. Otherwise, it returns an error
    /// with a message indicating the mismatch.
    pub fn expect_peek(&mut self, token_kind: TokenKind) -> Result<(), ParserError> {
        if self.peek_token_is(token_kind.clone()) {
            self.next_token(); // consume current token
            return Ok(());
        }

        Err(Diag {
            kind: ParserDiagKind::ExpectedToken(token_kind),
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
    pub fn expect_current(&mut self, token_kind: TokenKind) -> Result<(), ParserError> {
        if self.current_token_is(token_kind.clone()) {
            self.next_token(); // consume current token
            return Ok(());
        }

        Err(Diag {
            kind: ParserDiagKind::UnexpectedToken(self.current_token().kind, token_kind),
            level: DiagLevel::Error,
            location: Some(DiagLoc::new(SourceLoc::from_loc(
                self.current_token().loc,
                self.file_name.clone(),
            ))),
            hint: None,
        })
    }
}
