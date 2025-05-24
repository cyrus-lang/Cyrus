use crate::ParseError;
use crate::Parser;
use crate::diag::ParserErrorType;
use ast::ast::*;
use ast::token::*;
use diag::errors::CompileTimeError;

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
                    source_content: Box::new(self.lexer.input.clone()),
                    verbose: None,
                    caret: true,
                });
            }
        }
    }

    pub fn match_type_token(&mut self, token_kind: TokenKind) -> bool {
        if PRIMITIVE_TYPES.contains(&token_kind.clone()) {
            return true;
        } else if let TokenKind::Identifier { .. } = token_kind.clone() {
            return true;
        } else {
            matches!(token_kind.clone(), TokenKind::Asterisk | TokenKind::Ampersand)
        }
    }

    pub fn parse_primitive_type_token(&mut self) -> Result<TokenKind, ParseError> {
        let token_kind = self.current_token.kind.clone();
        if PRIMITIVE_TYPES.contains(&token_kind) {
            Ok(token_kind)
        } else {
            Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::InvalidTypeToken(token_kind.clone()),
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: true,
            })
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
            identifier @ TokenKind::Identifier { .. } => Ok(identifier),
            TokenKind::LeftBracket => self.parse_array_type(),
            token_kind => Err(CompileTimeError {
                location,
                etype: ParserErrorType::InvalidTypeToken(token_kind.clone()),
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: true,
            }),
        }
    }

    pub fn parse_storage_class(&mut self, token: Token) -> Result<StorageClass, ParseError> {
        let storage_class = {
            if self.current_token_is(TokenKind::Inline) {
                self.next_token();
                StorageClass::Inline
            } else if self.current_token_is(TokenKind::Extern) {
                self.next_token();
                StorageClass::Extern
            } else if self.current_token_is(TokenKind::Public) {
                self.next_token();
                if self.current_token_is(TokenKind::Inline) {
                    self.next_token();
                    StorageClass::PublicInline
                } else if self.current_token_is(TokenKind::Extern) {
                    self.next_token();
                    StorageClass::PublicExtern
                } else {
                    StorageClass::Public
                }
            } else {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::InvalidToken(token.kind),
                    file_name: Some(self.lexer.file_name.clone()),
                    source_content: Box::new(self.lexer.input.clone()),
                    verbose: None,
                    caret: true,
                });
            }
        };

        Ok(storage_class)
    }
}
