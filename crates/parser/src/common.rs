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
                    caret: Some(self.current_token.span.clone()),
                });
            }
        }
    }

    pub fn matches_type_token(&mut self, token_kind: TokenKind) -> bool {
        if PRIMITIVE_TYPES.contains(&token_kind.clone()) {
            return true;
        } else if let TokenKind::Identifier { .. } = token_kind.clone() {
            return true;
        } else {
            matches!(
                token_kind.clone(),
                TokenKind::Asterisk | TokenKind::Ampersand | TokenKind::Const
            )
        }
    }

    pub fn parse_type_specifier(&mut self) -> Result<TypeSpecifier, ParseError> {
        let mut base_type = self.parse_base_type_token()?;

        loop {
            if self.peek_token_is(TokenKind::Asterisk) {
                self.next_token();
                base_type = TypeSpecifier::Dereference(Box::new(base_type));
            } else if self.peek_token_is(TokenKind::Ampersand) {
                self.next_token();
                base_type = TypeSpecifier::AddressOf(Box::new(base_type));
            } else if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume base_type
                base_type = self.parse_array_type(base_type)?;
            } else {
                break;
            }
        }

        Ok(base_type)
    }

    fn parse_base_type_token(&mut self) -> Result<TypeSpecifier, ParseError> {
        let current = self.current_token.clone();

        let parsed_kind = match current.kind.clone() {
            token_kind if PRIMITIVE_TYPES.contains(&token_kind) => Ok(TypeSpecifier::TypeToken(current)),
            TokenKind::Struct => self.parse_struct_type(),
            TokenKind::Const => {
                self.next_token(); // consume const
                let inner_type = self.parse_base_type_token()?;
                Ok(TypeSpecifier::Const(Box::new(inner_type)))
            }
            TokenKind::Identifier { .. } => {
                if self.peek_token_is(TokenKind::DoubleColon) {
                    let module_import = self.parse_module_import()?;
                    dbg!(module_import.clone());
                    dbg!(self.current_token.kind.clone());
                    Ok(TypeSpecifier::ModuleImport(module_import))
                } else {
                    Ok(TypeSpecifier::Identifier(Identifier {
                        name: {
                            if let TokenKind::Identifier { name } = current.kind {
                                name
                            } else {
                                unreachable!()
                            }
                        },
                        span: current.span,
                        loc: self.current_location(),
                    }))
                }
            }
            _ => Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::InvalidTypeToken(current.kind.clone()),
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: Some(Span::new(current.span.start, self.current_token.span.end)),
            }),
        };

        parsed_kind
    }

    pub fn parse_array_type(&mut self, base_type_specifier: TypeSpecifier) -> Result<TypeSpecifier, ParseError> {
        let mut dimensions: Vec<ArrayCapacity> = Vec::new();

        while self.current_token_is(TokenKind::LeftBracket) {
            let array_capacity = self.parse_single_array_capacity()?;
            // prevent consuming the latest token_kind here
            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume right bracket
            }
            dimensions.push(array_capacity);
        }

        let mut type_specifier = base_type_specifier.clone();
        for dimension in dimensions.iter().rev() {
            type_specifier = TypeSpecifier::Array(ArrayTypeSpecifier {
                size: dimension.clone(),
                element_type: Box::new(type_specifier),
            });
        }

        Ok(type_specifier)
    }

    pub fn parse_single_array_capacity(&mut self) -> Result<ArrayCapacity, ParseError> {
        self.expect_current(TokenKind::LeftBracket)?;
        if self.current_token_is(TokenKind::RightBracket) {
            return Ok(ArrayCapacity::Dynamic);
        }
        let capacity = self.current_token.kind.clone();
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(ArrayCapacity::Fixed(capacity))
    }

    pub fn parse_single_array_index(&mut self) -> Result<Expression, ParseError> {
        self.expect_current(TokenKind::LeftBracket)?;
        let start = self.current_token.span.start;

        if self.current_token_is(TokenKind::RightBracket) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: Some(Span::new(start, self.current_token.span.end)),
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
                    caret: Some(token.span.clone()),
                });
            }
        };

        Ok(storage_class)
    }

    pub fn parse_struct_type(&mut self) -> Result<TypeSpecifier, ParseError> {
        let struct_start = self.current_token.span.start;

        self.expect_current(TokenKind::Struct)?;
        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<Field> = Vec::new();

        loop {
            match self.current_token.kind.clone() {
                TokenKind::RightBrace => {
                    break;
                }
                TokenKind::EOF => {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::MissingClosingBrace,
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: None,
                        caret: Some(Span::new(struct_start, self.current_token.span.end)),
                    });
                }
                TokenKind::Identifier { name: field_name } => {
                    let start = self.current_token.span.start;
                    self.next_token(); // consume identifier

                    self.expect_current(TokenKind::Colon)?;

                    let type_token = self.parse_type_specifier()?;
                    self.next_token();

                    fields.push(Field {
                        name: field_name,
                        ty: type_token,
                        loc: self.current_location(),
                        span: Span {
                            start,
                            end: self.current_token.span.end,
                        },
                    });

                    if self.current_token_is(TokenKind::RightBrace) {
                        break;
                    } else {
                        self.expect_current(TokenKind::Comma)?;
                    }
                }
                _ => {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: Some(String::from("Invalid token inside a anonymous struct definition.")),
                        caret: Some(Span::new(struct_start, self.current_token.span.end)),
                    });
                }
            }
        }

        Ok(TypeSpecifier::UnnamedStruct(UnnamedStruct { fields }))
    }
}
