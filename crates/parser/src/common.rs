use crate::ParseError;
use crate::Parser;
use crate::diag::ParserErrorType;
use crate::prec::Precedence;
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

    pub fn parse_type_token(&mut self) -> Result<TokenKind, ParseError> {
        let mut base_type = self.parse_base_type_token()?;

        loop {
            if self.peek_token_is(TokenKind::Asterisk) {
                self.next_token();
                base_type = TokenKind::Dereference(Box::new(base_type));
            } else if self.peek_token_is(TokenKind::Ampersand) {
                self.next_token();
                base_type = TokenKind::AddressOf(Box::new(base_type));
            } else if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume base_type
                base_type = self.parse_array_type(base_type)?;
            } else {
                break;
            }
        }

        Ok(base_type)
    }

    fn parse_base_type_token(&mut self) -> Result<TokenKind, ParseError> {
        let current_kind = self.current_token.kind.clone();

        let parsed_kind = match current_kind {
            token_kind if PRIMITIVE_TYPES.contains(&token_kind) => Ok(token_kind),
            TokenKind::Const => {
                self.next_token(); // consume const
                let inner_type = self.parse_base_type_token()?;
                Ok(TokenKind::ConstOf(Box::new(inner_type)))
            }
            TokenKind::Identifier { .. } => Ok(current_kind),
            _ => Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::InvalidTypeToken(current_kind.clone()),
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: true,
            }),
        };

        parsed_kind
    }

    pub fn parse_array_type(&mut self, data_type: TokenKind) -> Result<TokenKind, ParseError> {
        let mut dimensions: Vec<ArrayCapacity> = Vec::new();

        while self.current_token_is(TokenKind::LeftBracket) {
            let array_capacity = self.parse_single_array_capacity()?;
            // prevent consuming the latest token_kind here
            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume right bracket
            }
            dimensions.push(array_capacity);
        }

        dbg!(self.current_token.kind.clone());

        Ok(TokenKind::Array(Box::new(data_type), dimensions))
    }

    pub fn parse_single_array_capacity(&mut self) -> Result<ArrayCapacity, ParseError> {
        self.expect_current(TokenKind::LeftBracket)?;
        if self.current_token_is(TokenKind::RightBracket) {
            return Ok(ArrayCapacity::Dynamic);
        }
        let capacity = self.current_token.kind.clone();
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(ArrayCapacity::Static(capacity))
    }

    pub fn parse_single_array_index(&mut self) -> Result<Expression, ParseError> {
        self.expect_current(TokenKind::LeftBracket)?;
        if self.current_token_is(TokenKind::RightBracket) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: true,
            });
        }
        let index = self.parse_expression(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(index)
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
