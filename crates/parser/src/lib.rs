use crate::diag::ParserDiagKind;
use ast::ast::*;
use ast::token::*;
use diagcentral::Diag;
use diagcentral::DiagLevel;
use diagcentral::DiagLoc;
use diagcentral::reporter::DiagReporter;
use lexer::*;
use utils::fs::read_file;

mod common;
mod diag;
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
    let mut parser = Parser::new(&mut lexer);

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

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
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
        let mut program = ProgramTree::new();

        while self.current_token.kind != TokenKind::EOF {
            self.parse_and_add_statement(&mut program);
            self.next_token();
        }

        self.finalize_program_parse(program)
    }

    /// Parses a statement and adds it to the program, accumulating errors if any.
    pub fn parse_and_add_statement(&mut self, program: &mut ProgramTree) {
        match self.parse_statement(true) {
            Ok(statement) => program.body.push(statement),
            Err(error) => self.errors.push(error),
        }
    }

    pub fn display_parser_errors(&mut self, errors: Vec<ParserError>) {
        if errors.len() > 0 {
            let output = DiagReporter::format_panel(&errors[0]);
            eprintln!("{}", output);
            std::process::exit(1);
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
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
        self.peek_token.clone()
    }

    pub fn current_token_is(&self, token_kind: TokenKind) -> bool {
        self.current_token.kind == token_kind
    }

    pub fn peek_token_is(&self, token_kind: TokenKind) -> bool {
        self.peek_token.kind == token_kind
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
            kind: ParserDiagKind::InvalidToken(self.current_token.kind.clone()),
            level: DiagLevel::Error,
            location: Some(DiagLoc::new(
                self.lexer.file_name.clone(),
                self.current_token.loc.clone(),
                self.current_token.span.end,
            )),
            hint: Some(String::from("Invalid token inside an anonymous struct definition.")),
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
            kind: ParserDiagKind::UnexpectedToken(self.current_token.kind.clone(), token_kind),
            level: DiagLevel::Error,
            location: Some(DiagLoc::new(
                self.lexer.file_name.clone(),
                self.current_token.loc.clone(),
                self.current_token.span.end,
            )),
            hint: None,
        })
    }
}
