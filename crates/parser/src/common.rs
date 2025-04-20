use crate::ParseError;
use crate::Parser;
use ast::ast::*;
use ast::token::*;
use utils::compile_time_errors::errors::CompileTimeError;
use utils::compile_time_errors::parser_errors::ParserErrorType;

impl<'a> Parser<'a> {
    pub fn parse_identifier(&mut self) -> Result<Identifier, CompileTimeError<ParserErrorType>> {
        match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => Ok(Identifier {
                name,
                span: self.current_token.span.clone(),
                loc: self.current_location(),
            }),
            _ => {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::ExpectedIdentifier,
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(
                        self.lexer
                            .select(self.current_token.span.start..self.current_token.span.end),
                    ),
                    verbose: None,
                    caret: true,
                });
            }
        }
    }

    pub fn match_type_token(&mut self) -> bool {
        if PRIMITIVE_TYPES.contains(&self.current_token.kind.clone()) {
            return true;
        } else if let TokenKind::Identifier { .. } = self.current_token.kind.clone() {
            return true;
        } else {
            matches!(
                self.current_token.kind.clone(),
                TokenKind::Asterisk | TokenKind::Ampersand
            )
        }
    }

    pub fn parse_type_token(&mut self) -> Result<TokenKind, ParseError> {
        let location = self.current_location();

        match self.current_token.kind.clone() {
            token_kind if PRIMITIVE_TYPES.contains(&token_kind) => Ok(token_kind),
            TokenKind::Asterisk => {
                self.next_token();
                Ok(TokenKind::Dereference(Box::new(self.parse_type_token()?)))
            }
            TokenKind::Ampersand => {
                self.next_token();
                Ok(TokenKind::AddressOf(Box::new(self.parse_type_token()?)))
            }
            TokenKind::Identifier { name: type_name } => Ok(TokenKind::UserDefinedType(Identifier {
                name: type_name.clone(),
                span: self.current_token.span.clone(),
                loc: self.current_location(),
            })),
            TokenKind::LeftBracket => {
                let mut dimensions: Vec<TokenKind> = Vec::new();

                while self.current_token_is(TokenKind::LeftBracket) {
                    self.next_token(); // consume left bracket

                    if let TokenKind::Literal(_) = self.current_token.kind.clone() {
                        let capacity = self.current_token.kind.clone();
                        dimensions.push(capacity);
                        self.next_token();
                    } else if TokenKind::Dyn == self.current_token.kind.clone() {
                        dimensions.push(TokenKind::Dyn);
                        self.next_token();
                    }

                    self.expect_current(TokenKind::RightBracket)?;
                }

                let data_type = self.parse_type_token()?;
                self.next_token();

                Ok(TokenKind::Array(Box::new(data_type), dimensions))
            }
            token_kind => Err(CompileTimeError {
                location,
                etype: ParserErrorType::InvalidTypeToken(token_kind.clone()),
                file_name: Some(self.lexer.file_name.clone()),
                code_raw: Some(
                    self.lexer
                        .select(self.current_token.span.start..self.current_token.span.end),
                ),
                verbose: None,
                caret: true,
            }),
        }
    }

    /// Parses a visibility or type modifier token and returns its corresponding `VisType`.
    ///
    /// This function checks if the given token represents a valid function visibility or type modifier
    /// (e.g., `Inline`, `Extern`, or `Pub`) and converts it to the appropriate `VisType`.
    /// If the token is invalid, it returns a parsing error.
    pub fn parse_vis_type(&mut self, token: Token) -> Result<VisType, ParseError> {
        let vis_type = match token.kind {
            TokenKind::Inline => VisType::Inline,
            TokenKind::Extern => VisType::Extern,
            TokenKind::Pub => VisType::Pub,
            _ => {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(
                        self.lexer
                            .select(self.current_token.span.start..self.current_token.span.end),
                    ),
                    verbose: Some(String::from(
                        "Expected one of: 'inline', 'extern', 'pub' as function visibility.",
                    )),
                    caret: true,
                });
            }
        };
        self.next_token(); // consume vis_type token
        Ok(vis_type)
    }
}
