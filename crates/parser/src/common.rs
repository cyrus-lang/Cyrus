use crate::Parser;
use crate::ParserError;
use crate::diagnostics::ParserDiagKind;
use crate::prec::Precedence;
use ast::token::*;
use ast::*;
use diagcentral::Diag;
use diagcentral::DiagLevel;
use diagcentral::DiagLoc;

impl<'a> Parser<'a> {
    pub fn parse_identifier(&mut self) -> Result<Identifier, Diag<ParserDiagKind>> {
        match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => Ok(Identifier {
                name,
                span: self.current_token.span.clone(),
                loc: self.current_token.loc.clone(),
            }),
            _ => {
                return Err(Diag {
                    kind: ParserDiagKind::ExpectedIdentifier,
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(
                        self.lexer.file_name.clone(),
                        self.current_token.loc.clone(),
                        self.current_token.span.end,
                    )),
                    hint: None,
                });
            }
        }
    }

    pub fn matches_type_token(&mut self, token_kind: TokenKind) -> bool {
        if PRIMITIVE_TYPES.contains(&token_kind.clone()) {
            true
        } else if let TokenKind::Identifier { .. } = token_kind.clone() {
            true
        } else {
            matches!(
                token_kind.clone(),
                TokenKind::Asterisk | TokenKind::Ampersand | TokenKind::Const
            )
        }
    }

    pub fn parse_type_specifier(&mut self) -> Result<TypeSpecifier, ParserError> {
        let mut base_type = self.parse_base_type_token()?;

        loop {
            if self.peek_token_is(TokenKind::Asterisk) {
                self.next_token();
                base_type = TypeSpecifier::Dereference(Box::new(base_type));
            } else if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token(); // consume base_type
                base_type = self.parse_array_type(base_type)?;
            } else {
                break;
            }
        }

        Ok(base_type)
    }

    fn parse_base_type_token(&mut self) -> Result<TypeSpecifier, ParserError> {
        let current = self.current_token.clone();

        let parsed_kind = match current.kind.clone() {
            token_kind if PRIMITIVE_TYPES.contains(&token_kind) => Ok(TypeSpecifier::TypeToken(current)),
            TokenKind::Struct | TokenKind::Bits => self.parse_struct_type(),
            TokenKind::Const => {
                self.next_token(); // consume const
                let inner_type = self.parse_base_type_token()?;
                Ok(TypeSpecifier::Const(Box::new(inner_type)))
            }
            TokenKind::Identifier { .. } => {
                if self.peek_token_is(TokenKind::DoubleColon) {
                    let module_import = self.parse_module_import()?;
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
                        loc: self.current_token.loc.clone(),
                    }))
                }
            }
            _ => Err(Diag {
                kind: ParserDiagKind::InvalidTypeToken(current.kind.clone()),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(
                    self.lexer.file_name.clone(),
                    self.current_token.loc.clone(),
                    self.current_token.span.end,
                )),
                hint: None,
            }),
        };

        parsed_kind
    }

    pub fn parse_array_type(&mut self, base_type_specifier: TypeSpecifier) -> Result<TypeSpecifier, ParserError> {
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

    pub fn parse_single_array_capacity(&mut self) -> Result<ArrayCapacity, ParserError> {
        self.expect_current(TokenKind::LeftBracket)?;
        if self.current_token_is(TokenKind::RightBracket) {
            return Ok(ArrayCapacity::Dynamic);
        }
        let capacity = self.current_token.kind.clone();
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(ArrayCapacity::Fixed(capacity))
    }

    pub fn parse_single_array_index(&mut self) -> Result<Expression, ParserError> {
        self.expect_current(TokenKind::LeftBracket)?;

        if self.current_token_is(TokenKind::RightBracket) {
            return Err(Diag {
                kind: ParserDiagKind::InvalidToken(self.current_token.kind.clone()),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(
                    self.lexer.file_name.clone(),
                    self.current_token.loc.clone(),
                    self.current_token.span.end,
                )),
                hint: None,
            });
        }
        let index = self.parse_expression(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(index)
    }

    pub fn parse_access_specifier(&mut self, token: Token) -> Result<AccessSpecifier, ParserError> {
        let access_specifier: AccessSpecifier = {
            if self.current_token_is(TokenKind::Inline) {
                self.next_token();
                AccessSpecifier::Inline
            } else if self.current_token_is(TokenKind::Extern) {
                self.next_token();
                AccessSpecifier::Extern
            } else if self.current_token_is(TokenKind::Public) {
                self.next_token();
                if self.current_token_is(TokenKind::Inline) {
                    self.next_token();
                    AccessSpecifier::PublicInline
                } else if self.current_token_is(TokenKind::Extern) {
                    self.next_token();
                    AccessSpecifier::PublicExtern
                } else {
                    AccessSpecifier::Public
                }
            } else {
                return Err(Diag {
                    kind: ParserDiagKind::InvalidToken(token.kind),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(self.lexer.file_name.clone(), token.loc, token.span.end)),
                    hint: None,
                });
            }
        };

        Ok(access_specifier)
    }

    pub fn parse_struct_type(&mut self) -> Result<TypeSpecifier, ParserError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let packed = {
            if self.current_token_is(TokenKind::Bits) {
                self.next_token();
                true
            } else if self.current_token_is(TokenKind::Struct) {
                self.next_token();
                false
            } else {
                return Err(Diag {
                    kind: ParserDiagKind::InvalidToken(self.current_token.kind.clone()),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(
                        self.lexer.file_name.clone(),
                        self.current_token.loc.clone(),
                        self.current_token.span.end,
                    )),
                    hint: None,
                });
            }
        };
        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<UnnamedStructTypeField> = Vec::new();

        loop {
            match self.current_token.kind.clone() {
                TokenKind::RightBrace => {
                    break;
                }
                TokenKind::EOF => {
                    return Err(Diag {
                        kind: ParserDiagKind::MissingClosingBrace,
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(
                            self.lexer.file_name.clone(),
                            self.current_token.loc.clone(),
                            self.current_token.span.end,
                        )),
                        hint: None,
                    });
                }
                TokenKind::Identifier { .. } => {
                    let start = self.current_token.span.start;
                    let loc = self.current_token.loc.clone();

                    let field_name = self.parse_identifier()?;
                    self.next_token(); // consume identifier

                    self.expect_current(TokenKind::Colon)?;

                    let field_type_specifier = self.parse_type_specifier()?;
                    self.next_token();

                    fields.push(UnnamedStructTypeField {
                        field_name,
                        field_type: field_type_specifier,
                        loc,
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
                    return Err(Diag {
                        kind: ParserDiagKind::InvalidToken(self.current_token.kind.clone()),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(
                            self.lexer.file_name.clone(),
                            self.current_token.loc.clone(),
                            self.current_token.span.end,
                        )),
                        hint: None,
                    });
                }
            }
        }

        Ok(TypeSpecifier::UnnamedStruct(UnnamedStructType {
            fields,
            packed,
            loc,
            span: Span::new(start, self.current_token.span.end),
        }))
    }
}
