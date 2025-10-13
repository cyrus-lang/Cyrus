use crate::Parser;
use crate::ParserError;
use crate::diagnostics::ParserDiagKind;
use crate::prec::Precedence;
use ast::source_loc::SourceLoc;
use ast::token::*;
use ast::*;
use diagcentral::Diag;
use diagcentral::DiagLevel;
use diagcentral::DiagLoc;
use partialmatch::partial_match;

impl Parser {
    pub fn parse_identifier(&mut self) -> Result<Identifier, Diag<ParserDiagKind>> {
        match self.current_token().kind {
            TokenKind::Identifier { name } => Ok(Identifier {
                name,
                span: self.current_token().span.clone(),
                loc: self.current_token().loc.clone(),
            }),
            _ => {
                return Err(Diag {
                    kind: ParserDiagKind::ExpectedIdentifier,
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        self.current_token().loc.clone(),
                        self.file_name.clone(),
                    ))),
                    hint: None,
                });
            }
        }
    }

    pub fn is_array_constructor_start(&mut self) -> bool {
        if !self.matches_type_token(self.current_token().kind) {
            return false;
        }

        let mut i = 1; // token after type
        let mut bracket_count = 0;

        while let Some(tok) = self.peek_n_token(i) {
            partial_match!(&tok.kind, {
                TokenKind::LeftBracket => {
                    bracket_count += 1;
                },
                TokenKind::RightBracket => {
                    if bracket_count == 0 {
                        return false; // unmatched
                    }
                    bracket_count -= 1;
                    if bracket_count == 0 {
                        // next token after last closing bracket
                        if let Some(next_tok) = self.peek_n_token(i + 1) {
                            return next_tok.kind == TokenKind::LeftBrace;
                        } else {
                            return false;
                        }
                    }
                },
            });
            i += 1;
        }

        false
    }

    pub fn matches_type_token(&mut self, token_kind: TokenKind) -> bool {
        if PRIMITIVE_TYPES.contains(&token_kind) {
            true
        } else if let TokenKind::Identifier { .. } = token_kind {
            true
        } else {
            matches!(
                token_kind,
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

        if self.peek_token_is(TokenKind::LessThan) {
            let start = self.peek_token().span.start;
            let loc = self.peek_token().loc.clone();

            self.next_token();
            let type_args = self.parse_type_arg_list()?;

            Ok(TypeSpecifier::GenericInst(GenericInst {
                base: Box::new(base_type),
                type_args,
                loc,
                span: Span::new(start, self.current_token().span.end),
            }))
        } else {
            Ok(base_type)
        }
    }

    pub fn parse_func_type_params(&mut self) -> Result<FuncTypeParams, ParserError> {
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::LeftParen)?;

        let mut variadic: Option<FuncTypeVariadicParams> = None;
        let mut list: Vec<TypeSpecifier> = Vec::new();

        while self.current_token().kind != TokenKind::RightParen {
            if self.current_token_is(TokenKind::TripleDot) {
                self.next_token(); // consume triple dot

                if self.current_token_is(TokenKind::RightParen) {
                    variadic = Some(FuncTypeVariadicParams::UntypedCStyle);
                    break;
                } else {
                    let variadic_data_type = self.parse_type_specifier()?;
                    self.next_token();

                    variadic = Some(FuncTypeVariadicParams::Typed(variadic_data_type));
                    break;
                }
            } else {
                let var_type = self.parse_type_specifier()?;
                self.next_token();
                list.push(var_type);
            }

            match &self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                }
                TokenKind::RightParen => {
                    break;
                }
                _ => {
                    return Err(Diag {
                        kind: ParserDiagKind::MissingComma,
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(loc, self.file_name.clone()))),
                        hint: None,
                    });
                }
            }
        }

        self.expect_current(TokenKind::RightParen)?;

        Ok(FuncTypeParams { list, variadic })
    }

    fn parse_func_type(&mut self) -> Result<TypeSpecifier, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume function

        let params = self.parse_func_type_params()?;
        let ret = self.parse_type_specifier()?;

        Ok(TypeSpecifier::FuncType(Box::new(FuncType {
            params,
            return_type: Box::new(ret),
            span: Span::new(start, self.current_token().span.end),
            vis_opt: None,
            loc,
        })))
    }

    fn parse_tuple(&mut self) -> Result<TypeSpecifier, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::LeftParen)?;

        let mut type_list: Vec<TypeSpecifier> = Vec::new();

        loop {
            let type_specifier = self.parse_type_specifier()?;
            self.next_token();

            type_list.push(type_specifier);

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        if !self.current_token_is(TokenKind::RightParen) {
            return Err(Diag {
                kind: ParserDiagKind::MissingClosingParen,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: None,
            });
        }

        if type_list.len() <= 1 {
            return Err(Diag {
                kind: ParserDiagKind::SingleElementTupleType,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: Some(
                    "If you only need a single element, remove the tuple syntax and use the type directly.".to_string(),
                ),
            });
        }

        Ok(TypeSpecifier::Tuple(TupleType {
            type_list,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_base_type_token(&mut self) -> Result<TypeSpecifier, ParserError> {
        let current = self.current_token().clone();

        let parsed_kind = match current.kind {
            ref token_kind if PRIMITIVE_TYPES.contains(&token_kind) => Ok(TypeSpecifier::TypeToken(current)),
            TokenKind::LeftParen => self.parse_tuple(),
            TokenKind::Function => self.parse_func_type(),
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
                        loc: self.current_token().loc.clone(),
                    }))
                }
            }
            _ => Err(Diag {
                kind: ParserDiagKind::InvalidTypeToken(current.kind),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
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
        let capacity = self.parse_expression(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(ArrayCapacity::Fixed(Box::new(capacity)))
    }

    pub fn parse_single_array_index(&mut self) -> Result<Expression, ParserError> {
        self.expect_current(TokenKind::LeftBracket)?;

        if self.current_token_is(TokenKind::RightBracket) {
            return Err(Diag {
                kind: ParserDiagKind::InvalidToken(self.current_token().kind),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
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
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        token.loc.clone(),
                        self.file_name.clone(),
                    ))),
                    hint: None,
                });
            }
        };

        Ok(access_specifier)
    }

    pub fn parse_struct_type(&mut self) -> Result<TypeSpecifier, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let packed = {
            if self.current_token_is(TokenKind::Bits) {
                self.next_token();
                true
            } else if self.current_token_is(TokenKind::Struct) {
                self.next_token();
                false
            } else {
                return Err(Diag {
                    kind: ParserDiagKind::InvalidToken(self.current_token().kind),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        self.current_token().loc.clone(),
                        self.file_name.clone(),
                    ))),
                    hint: None,
                });
            }
        };
        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<UnnamedStructTypeField> = Vec::new();

        loop {
            match self.current_token().kind {
                TokenKind::RightBrace => {
                    break;
                }
                TokenKind::EOF => {
                    return Err(Diag {
                        kind: ParserDiagKind::MissingClosingBrace,
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc.clone(),
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
                TokenKind::Identifier { .. } => {
                    let start = self.current_token().span.start;
                    let loc = self.current_token().loc.clone();

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
                            end: self.current_token().span.end,
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
                        kind: ParserDiagKind::InvalidToken(self.current_token().kind),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc.clone(),
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
            }
        }

        Ok(TypeSpecifier::UnnamedStruct(UnnamedStructType {
            fields,
            packed,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_bounds(&mut self) -> Result<Vec<Bound>, ParserError> {
        self.expect_current(TokenKind::Colon)?;

        let mut list: Vec<Bound> = Vec::new();

        loop {
            let symbol = self.parse_identifier()?;
            self.next_token();

            list.push(Bound {
                symbol,
                type_args: Vec::new(),
            });

            match self.current_token().kind {
                TokenKind::Plus => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        Ok(list)
    }

    fn parse_generic_param(&mut self) -> Result<GenericParam, ParserError> {
        let param_name = self.parse_identifier()?;
        self.next_token();

        let bounds = if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume identifier
            Some(self.parse_bounds()?)
        } else {
            None
        };

        let default = if self.current_token_is(TokenKind::Assign) {
            self.next_token(); // consume assign
            let type_specifier = self.parse_type_specifier()?;
            self.next_token();
            Some(type_specifier)
        } else {
            None
        };

        Ok(GenericParam {
            param_name,
            bounds,
            default,
        })
    }

    pub fn parse_generic_params(&mut self) -> Result<GenericParamsList, ParserError> {
        self.expect_current(TokenKind::LessThan)?;

        let mut generic_params: GenericParamsList = GenericParamsList::new();

        loop {
            generic_params.push(self.parse_generic_param()?);

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        self.expect_current(TokenKind::GreaterThan)?;
        Ok(generic_params)
    }

    pub(crate) fn parse_type_arg_list(&mut self) -> Result<Vec<TypeArg>, ParserError> {
        self.expect_current(TokenKind::LessThan)?;

        let mut args = Vec::new();

        loop {
            if let TokenKind::Identifier { .. } = self.current_token().kind {
                if self.peek_token_is(TokenKind::Assign) {
                    let key = self.parse_identifier()?;
                    self.next_token(); // consume identifier
                    self.expect_current(TokenKind::Assign)?;

                    let val = self.parse_type_specifier()?;
                    self.next_token();

                    args.push(TypeArg::Named { key, value: val });
                } else {
                    // positional type arg
                    let ty = self.parse_type_specifier()?;
                    args.push(TypeArg::Positional(ty));
                }
            } else {
                // positional type arg that is not an identifier
                let ty = self.parse_type_specifier()?;
                self.next_token();
                args.push(TypeArg::Positional(ty));
            }

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                TokenKind::GreaterThan => {
                    // self.next_token();
                    break;
                }
                _ => {
                    return Err(Diag {
                        kind: ParserDiagKind::InvalidToken(self.current_token().kind),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc.clone(),
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
            }
        }

        Ok(args)
    }

    fn current_expr_is_path_like(&self, last_parsed_expression: Expression) -> bool {
        matches!(
            last_parsed_expression,
            Expression::Identifier(..) | Expression::ModuleImport(..)
        )
    }

    pub fn is_type_arg_start(&mut self, last_parsed_expression: Expression) -> bool {
        if !self.current_expr_is_path_like(last_parsed_expression) {
            return false;
        }

        if !self.peek_token_is(TokenKind::LessThan) {
            return false;
        }

        let mut i = 1; // token after '<'
        let mut depth = 0;

        while let Some(tok) = self.peek_n_token(i) {
            partial_match!(tok.kind, {
                TokenKind::LessThan => {
                    depth += 1;
                },
                TokenKind::GreaterThan => {
                    if depth == 0 {
                        return false; // unmatched `>`
                    }
                    depth -= 1;

                    if depth == 0 {
                        // reached the matching '>' for the first '<'
                        if let Some(next_tok) = self.peek_n_token(i + 1) {
                            return matches!(
                                next_tok.kind,
                                TokenKind::LeftParen     // foo<T>()
                                | TokenKind::LeftBrace   // Foo<T> {}
                                | TokenKind::DoubleColon // Foo<T>::bar
                            );
                        } else {
                            return false;
                        }
                    }
                },
                TokenKind::Semicolon | TokenKind::EOF => {
                    return false; // abort if new statement starts
                },
            });

            i += 1;
        }

        false
    }
}
