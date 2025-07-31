use crate::Parser;
use crate::ParserError;
use crate::diag::ParserDiagKind;
use crate::prec::*;
use ast::operators::InfixOperator;
use ast::operators::PrefixOperator;
use ast::operators::UnaryOperator;
use ast::token::*;
use ast::*;
use diagcentral::Diag;
use diagcentral::DiagLevel;
use diagcentral::DiagLoc;

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<(Expression, Span), ParserError> {
        let mut left_start = self.current_token.span.start;
        let mut left = self.parse_prefix_expression()?;

        while self.peek_token_is(TokenKind::Dot) || self.peek_token_is(TokenKind::FatArrow) {
            self.next_token(); // consume the left expression
            left = self.parse_field_access(left)?;
        }

        if self.peek_token_is(TokenKind::Assign) {
            self.next_token();
            let expr = self.parse_assignment(left, left_start)?;
            return Ok((expr, Span::new(left_start, self.current_token.span.end)));
        }

        while self.current_token.kind != TokenKind::EOF
            && precedence < token_precedence_of(self.peek_token.kind.clone())
        {
            match self.parse_infix_expression(left.clone(), left_start) {
                Some(infix) => {
                    left = infix?;

                    if let Expression::Infix(b) = left.clone() {
                        left_start = b.span.start;
                    }
                }
                None => {
                    return Ok((
                        left,
                        Span {
                            start: left_start,
                            end: self.current_token.span.end,
                        },
                    ));
                }
            }
        }

        let end = self.current_token.span.end;
        Ok((left, Span { start: left_start, end }))
    }

    pub fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let expr = match &self.current_token.clone().kind {
            TokenKind::Struct | TokenKind::Bits => self.parse_unnamed_struct_value()?,
            TokenKind::Identifier { .. } => {
                let module_import = self.parse_module_import()?;

                if self.current_token_is(TokenKind::LeftBrace) {
                    self.parse_struct_init(module_import)?
                } else if self.peek_token_is(TokenKind::Increment) {
                    self.next_token();
                    Expression::Unary(UnaryExpression {
                        operand: Box::new(Expression::ModuleImport(module_import.clone())),
                        op: UnaryOperator::PostIncrement,
                        span: Span::new(start, self.current_token.span.end),
                        loc: loc.clone(),
                    })
                } else if self.peek_token_is(TokenKind::Decrement) {
                    self.next_token();
                    Expression::Unary(UnaryExpression {
                        operand: Box::new(Expression::ModuleImport(module_import.clone())),
                        op: UnaryOperator::PostDecrement,
                        span: Span::new(start, self.current_token.span.end),
                        loc: loc.clone(),
                    })
                } else {
                    Expression::ModuleImport(module_import)
                }
            }
            TokenKind::Ampersand => {
                let start = self.current_token.span.start;
                let loc = self.current_token.loc.clone();
                self.next_token();
                Expression::AddressOf(AddressOf {
                    expr: Box::new(self.parse_prefix_expression()?),
                    span: Span::new(start, self.current_token.span.end),
                    loc,
                })
            }
            TokenKind::Asterisk => {
                let start = self.current_token.span.start;
                let loc = self.current_token.loc.clone();
                self.next_token();
                Expression::Dereference(Dereference {
                    expr: Box::new(self.parse_prefix_expression()?),
                    span: Span::new(start, self.current_token.span.end),
                    loc,
                })
            }
            TokenKind::Null => Expression::Literal(Literal {
                kind: LiteralKind::Null,
                span: Span::new(start, self.current_token.span.end),
                loc: loc.clone(),
            }),
            token_kind @ TokenKind::Increment | token_kind @ TokenKind::Decrement => {
                let unary_operator = match token_kind {
                    TokenKind::Increment => UnaryOperator::PreIncrement,
                    TokenKind::Decrement => UnaryOperator::PreDecrement,
                    _ => {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidToken(token_kind.clone()),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(
                                self.lexer.file_name.clone(),
                                loc,
                                self.current_token.span.end,
                            )),
                            hint: Some(String::from("Wanted increment or decrement operator.")),
                        });
                    }
                };

                self.next_token(); // consume the operator

                match self.current_token.kind.clone() {
                    TokenKind::Identifier { .. } => {
                        let module_import = self.parse_module_import()?;

                        Expression::Unary(UnaryExpression {
                            operand: Box::new(Expression::ModuleImport(module_import)),
                            op: unary_operator,
                            span: Span {
                                start,
                                end: self.current_token.span.end,
                            },
                            loc: loc.clone(),
                        })
                    }
                    _ => {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidToken(token_kind.clone()),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(
                                self.lexer.file_name.clone(),
                                loc,
                                self.current_token.span.end,
                            )),
                            hint: None,
                        });
                    }
                }
            }
            bool_token @ TokenKind::True | bool_token @ TokenKind::False => {
                let value = match bool_token {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => panic!(),
                };

                Expression::Literal(Literal {
                    kind: LiteralKind::Bool(value),
                    span: Span::new(start, self.current_token.span.end),
                    loc: loc.clone(),
                })
            }
            TokenKind::Literal(value) => Expression::Literal(value.clone()),
            token_kind @ TokenKind::Minus | token_kind @ TokenKind::Bang | token_kind @ TokenKind::SizeOf => {
                let start = self.current_token.span.start;
                let prefix_operator = match token_kind {
                    TokenKind::Minus => PrefixOperator::Minus,
                    TokenKind::Bang => PrefixOperator::Bang,
                    TokenKind::SizeOf => PrefixOperator::SizeOf,
                    _ => {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidPrefixOperator(token_kind.clone()),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(
                                self.lexer.file_name.clone(),
                                loc,
                                self.current_token.span.end,
                            )),
                            hint: None,
                        });
                    }
                };
                self.next_token(); // consume the prefix operator
                let (expr, span) = self.parse_expression(Precedence::Prefix)?;
                Expression::Prefix(PrefixExpression {
                    op: prefix_operator,
                    operand: Box::new(expr),
                    span: Span { start, end: span.end },
                    loc: loc.clone(),
                })
            }
            TokenKind::LeftParen => {
                // c-style casting
                if self.matches_type_token(self.peek_token.kind.clone())
                    && !(self.peek_token_is(TokenKind::Ampersand) || self.peek_token_is(TokenKind::Asterisk))
                {
                    self.parse_cast_expression(start)?
                } else {
                    // grouped expression
                    self.next_token(); // consume left paren
                    let expr = self.parse_expression(Precedence::Lowest)?.0;
                    self.expect_peek(TokenKind::RightParen)?;
                    expr
                }
            }
            _ => {
                if self.matches_type_token(self.current_token.kind.clone()) {
                    let type_specifier = self.parse_type_specifier()?;

                    if self.peek_token_is(TokenKind::LeftBrace) {
                        self.next_token();
                        return Ok(self.parse_array(type_specifier)?);
                    }

                    Expression::TypeSpecifier(type_specifier)
                } else {
                    return Err(Diag {
                        kind: ParserDiagKind::InvalidToken(self.current_token.kind.clone()),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(
                            self.lexer.file_name.clone(),
                            loc,
                            self.current_token.span.end,
                        )),
                        hint: None,
                    });
                }
            }
        };

        if self.peek_token_is(TokenKind::LeftBrace) {
            if let Expression::ModuleImport(module_import) = expr.clone() {
                self.next_token(); // consume struct name
                let struct_init = self.parse_struct_init(module_import)?;
                return Ok(struct_init);
            } else {
                return Err(Diag {
                    kind: ParserDiagKind::InvalidToken(self.current_token.kind.clone()),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(
                        self.lexer.file_name.clone(),
                        loc,
                        self.current_token.span.end,
                    )),
                    hint: None,
                });
            }
        } else if self.peek_token_is(TokenKind::LeftBracket) {
            self.next_token();
            return Ok(self.parse_array_index(expr)?);
        } else if self.current_token_is(TokenKind::LeftBracket) {
            return Ok(self.parse_array_index(expr)?);
        } else if self.peek_token_is(TokenKind::LeftParen) {
            return self.parse_func_call(expr);
        } else {
            Ok(expr)
        }
    }

    pub fn parse_infix_expression(
        &mut self,
        left: Expression,
        left_start: usize,
    ) -> Option<Result<Expression, ParserError>> {
        let loc = self.current_token.loc.clone();

        match self.peek_token.kind.clone() {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Asterisk
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::LessEqual
            | TokenKind::LessThan
            | TokenKind::GreaterEqual
            | TokenKind::GreaterThan
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Identifier { .. } => {
                self.next_token(); // consume left expression
                let op_token = self.current_token.kind.clone();
                let precedence = token_precedence_of(op_token.clone());
                self.next_token(); // consume the operator

                let op = match op_token {
                    TokenKind::Plus => InfixOperator::Add,
                    TokenKind::Minus => InfixOperator::Sub,
                    TokenKind::Asterisk => InfixOperator::Mul,
                    TokenKind::Slash => InfixOperator::Div,
                    TokenKind::Percent => InfixOperator::Rem,
                    TokenKind::LessThan => InfixOperator::LessThan,
                    TokenKind::LessEqual => InfixOperator::LessEqual,
                    TokenKind::GreaterThan => InfixOperator::GreaterThan,
                    TokenKind::GreaterEqual => InfixOperator::GreaterEqual,
                    TokenKind::Equal => InfixOperator::Equal,
                    TokenKind::NotEqual => InfixOperator::NotEqual,
                    TokenKind::Or => InfixOperator::Or,
                    TokenKind::And => InfixOperator::And,
                    _ => {
                        return Some(Err(Diag {
                            kind: ParserDiagKind::InvalidInfixOperator(self.current_token.kind.clone()),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(
                                self.lexer.file_name.clone(),
                                loc,
                                self.current_token.span.end,
                            )),
                            hint: None,
                        }));
                    }
                };
                let (right, span) = self.parse_expression(precedence).ok()?;

                Some(Ok(Expression::Infix(InfixExpression {
                    op,
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                    span: Span {
                        start: left_start,
                        end: span.end,
                    },
                    loc,
                })))
            }
            _ => None,
        }
    }

    pub fn parse_expression_series(&mut self, end: TokenKind) -> Result<(Vec<Expression>, Span), ParserError> {
        let start = self.current_token.span.start;
        let mut series: Vec<Expression> = Vec::new();

        // detect empty series of expressions
        if self.peek_token_is(end.clone()) {
            self.next_token();
            return Ok((series, Span::new(start, self.current_token.span.end)));
        } else if self.current_token_is(end.clone()) {
            return Ok((series, Span::new(start, self.current_token.span.end)));
        }
        self.next_token();

        series.push(self.parse_expression(Precedence::Lowest)?.0);

        while self.peek_token_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();
            series.push(self.parse_expression(Precedence::Lowest)?.0);
        }
        self.next_token(); // consume latest token of the expression

        Ok((
            series,
            Span {
                start,
                end: self.current_token.span.end,
            },
        ))
    }

    pub fn parse_module_import(&mut self) -> Result<ModuleImport, ParserError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let mut segments = match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => {
                vec![ModuleSegment::SubModule(Identifier {
                    name,
                    span: Span {
                        start,
                        end: self.current_token.span.end - 1,
                    },
                    loc: loc.clone(),
                })]
            }
            _ => {
                return Err(Diag {
                    kind: ParserDiagKind::ExpectedIdentifier,
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(
                        self.lexer.file_name.clone(),
                        loc.clone(),
                        self.current_token.span.end,
                    )),
                    hint: None,
                });
            }
        };

        if !self.peek_token_is(TokenKind::DoubleColon) {
            return Ok(ModuleImport {
                segments,
                span: Span::new(start, self.current_token.span.end),
                loc,
            });
        }

        self.next_token(); // consume first identifier

        loop {
            if self.current_token_is(TokenKind::DoubleColon) {
                self.next_token(); // consume double colon
            } else if let TokenKind::Identifier { name } = self.current_token.kind.clone() {
                segments.push(ModuleSegment::SubModule(Identifier {
                    name,
                    span: Span {
                        start,
                        end: self.peek_token.span.end - 1,
                    },
                    loc: loc.clone(),
                }));

                if self.peek_token_is(TokenKind::DoubleColon) {
                    self.next_token();
                    continue;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(ModuleImport {
            segments,
            span: Span::new(start, self.current_token.span.end),
            loc,
        })
    }

    pub fn parse_module_path(&mut self) -> Result<ModulePath, ParserError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let mut module_path = ModulePath {
            alias: None,
            segments: Vec::new(),
            loc: loc.clone(),
            span: Span::default(),
        };

        while !self.current_token_is(TokenKind::Semicolon) {
            match self.current_token.kind.clone() {
                TokenKind::Identifier { name: identifier } => {
                    let span = self.current_token.span.clone();
                    self.next_token(); // consume identifier

                    if self.current_token_is(TokenKind::Colon) {
                        if module_path.alias.is_none() {
                            self.next_token();
                            module_path.alias = Some(identifier);
                            continue;
                        } else {
                            return Err(Diag {
                                kind: ParserDiagKind::UnexpectedToken(
                                    TokenKind::DoubleColon,
                                    self.current_token.kind.clone(),
                                ),
                                level: DiagLevel::Error,
                                location: Some(DiagLoc::new(
                                    self.lexer.file_name.clone(),
                                    loc.clone(),
                                    self.current_token.span.end,
                                )),
                                hint: None,
                            });
                        }
                    }

                    module_path.segments.push(ModuleSegment::SubModule(Identifier {
                        name: identifier.clone(),
                        span: span.clone(),
                        loc: loc.clone(),
                    }));

                    if self.current_token_is(TokenKind::DoubleColon) {
                        continue;
                    } else if self.current_token_is(TokenKind::Semicolon) {
                        return Ok(module_path);
                    } else {
                        break;
                    }
                }
                TokenKind::DoubleColon => {
                    self.next_token();
                    continue;
                }
                _ => {
                    return Err(Diag {
                        kind: ParserDiagKind::ExpectedIdentifier,
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(
                            self.lexer.file_name.clone(),
                            loc.clone(),
                            self.current_token.span.end,
                        )),
                        hint: None,
                    });
                }
            }
        }

        module_path.span = Span::new(start, self.current_token.span.end);

        Ok(module_path)
    }

    pub fn parse_cast_expression(&mut self, start: usize) -> Result<Expression, ParserError> {
        let loc = self.current_token.loc.clone();

        self.expect_current(TokenKind::LeftParen)?;
        let target_type = self.parse_type_specifier()?;
        self.next_token(); // consume target_type
        self.expect_current(TokenKind::RightParen)?;

        let expr = self.parse_expression(Precedence::Lowest)?.0;

        Ok(Expression::Cast(Cast {
            expr: Box::new(expr),
            target_type,
            span: Span::new(start, self.current_token.span.end),
            loc,
        }))
    }

    pub fn parse_method_call(
        &mut self,
        operand: Expression,
        method_name: Identifier,
        is_fat_arrow: bool,
        start: usize,
        loc: Location,
    ) -> Result<Expression, ParserError> {
        if !self.current_token_is(TokenKind::LeftParen) {
            return Err(Diag {
                kind: ParserDiagKind::MissingOpeningParen,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(
                    self.lexer.file_name.clone(),
                    loc.clone(),
                    self.current_token.span.end,
                )),
                hint: None,
            });
        }

        let arguments = self.parse_expression_series(TokenKind::RightParen)?.0;
        if !self.current_token_is(TokenKind::RightParen) {
            return Err(Diag {
                kind: ParserDiagKind::MissingClosingParen,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(
                    self.lexer.file_name.clone(),
                    loc.clone(),
                    self.current_token.span.end,
                )),
                hint: None,
            });
        }

        Ok(Expression::MethodCall(MethodCall {
            is_fat_arrow,
            operand: Box::new(operand),
            method_name,
            arguments,
            span: Span::new(start, self.current_token.span.end),
            loc,
        }))
    }

    pub fn parse_field_access(&mut self, operand: Expression) -> Result<Expression, ParserError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let is_fat_arrow = {
            if self.current_token_is(TokenKind::FatArrow) {
                self.next_token();
                true
            } else if self.current_token_is(TokenKind::Dot) {
                self.next_token();
                false
            } else {
                return Err(Diag {
                    kind: ParserDiagKind::InvalidToken(self.current_token.kind.clone()),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(
                        self.lexer.file_name.clone(),
                        loc.clone(),
                        self.current_token.span.end,
                    )),
                    hint: None,
                });
            }
        };
        let identifier = self.parse_identifier()?;

        if self.peek_token_is(TokenKind::LeftParen) {
            self.next_token(); // consume identifier
            return self.parse_method_call(operand, identifier, is_fat_arrow, start, loc);
        }

        Ok(Expression::FieldAccess(FieldAccess {
            is_fat_arrow,
            operand: Box::new(operand),
            field_name: identifier,
            span: Span::new(start, self.current_token.span.end),
            loc,
        }))
    }

    pub fn parse_func_call(&mut self, operand: Expression) -> Result<Expression, ParserError> {
        let loc = self.current_token.loc.clone();
        let start = self.current_token.span.start;

        self.expect_peek(TokenKind::LeftParen)?;

        let arguments = self.parse_expression_series(TokenKind::RightParen)?.0;
        if !(self.current_token_is(TokenKind::RightParen)) {
            return Err(Diag {
                kind: ParserDiagKind::InvalidToken(self.current_token.kind.clone()),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(
                    self.lexer.file_name.clone(),
                    loc.clone(),
                    self.current_token.span.end,
                )),
                hint: None,
            });
        }

        Ok(Expression::FuncCall(FuncCall {
            operand: Box::new(operand),
            arguments,
            span: Span::new(start, self.current_token.span.end),
            loc: loc,
        }))
    }

    pub fn parse_struct_init(&mut self, struct_name: ModuleImport) -> Result<Expression, ParserError> {
        let loc = self.current_token.loc.clone();
        let start = self.current_token.span.start;

        let mut field_inits: Vec<FieldInit> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Expression::StructInit(StructInit {
                struct_name: struct_name.clone(),
                field_inits: Vec::new(),
                loc: loc.clone(),
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
            }));
        }

        loop {
            let field_name = self.parse_identifier()?.name;
            let field_loc = self.current_token.loc.clone();

            self.next_token(); // consume identifier
            self.expect_current(TokenKind::Colon)?;

            let value = self.parse_expression(Precedence::Lowest)?.0;
            self.next_token();

            field_inits.push(FieldInit {
                name: field_name,
                value,
                loc: field_loc,
            });

            match self.current_token.kind.clone() {
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
                TokenKind::Comma => {
                    self.next_token();
                    if self.current_token_is(TokenKind::RightBrace) {
                        break;
                    }
                }
                TokenKind::RightBrace => {
                    break;
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

        return Ok(Expression::StructInit(StructInit {
            struct_name,
            field_inits,
            loc,
            span: Span {
                start,
                end: self.current_token.span.end,
            },
        }));
    }

    pub fn parse_assignment(&mut self, lhs: Expression, start: usize) -> Result<Expression, ParserError> {
        let loc = self.current_token.loc.clone();

        self.expect_current(TokenKind::Assign)?;
        let rhs = self.parse_expression(Precedence::Lowest)?.0;
        let end = self.current_token.span.end;
        Ok(Expression::Assignment(Box::new(Assignment {
            lhs,
            rhs,
            span: Span { start, end },
            loc,
        })))
    }

    pub fn parse_array_index(&mut self, expr: Expression) -> Result<Expression, ParserError> {
        let loc = self.current_token.loc.clone();
        let start = self.current_token.span.start;

        let mut base_index = Expression::ArrayIndex(ArrayIndex {
            operand: Box::new(expr),
            index: Box::new(self.parse_single_array_index()?),
            span: Span::new(start, self.current_token.span.end),
            loc: loc.clone(),
        });

        if self.peek_token_is(TokenKind::LeftBracket) {
            self.next_token();
        }

        while self.current_token_is(TokenKind::LeftBracket) {
            base_index = Expression::ArrayIndex(ArrayIndex {
                operand: Box::new(base_index),
                index: Box::new(self.parse_single_array_index()?),
                span: Span::new(start, self.current_token.span.end),
                loc: loc.clone(),
            });

            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token();
            }
        }

        Ok(base_index)
    }

    pub fn parse_array(&mut self, data_type: TypeSpecifier) -> Result<Expression, ParserError> {
        let loc = self.current_token.loc.clone();
        let start = self.current_token.span.start;

        if !self.current_token_is(TokenKind::LeftBrace) {
            return Err(Diag {
                kind: ParserDiagKind::MissingOpeningBrace,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(
                    self.lexer.file_name.clone(),
                    loc.clone(),
                    self.current_token.span.end,
                )),
                hint: None,
            });
        }

        let mut elements: Vec<Expression> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Expression::Array(Array {
                elements,
                data_type,
                span: Span::new(start, self.current_token.span.end),
                loc: loc.clone(),
            }));
        }

        loop {
            if self.current_token_is(TokenKind::LeftBrace) {
                let untyped_array_start = self.current_token.span.start;
                let mut untyped_array: Vec<Expression> = Vec::new();
                self.next_token(); // consume left brace

                loop {
                    if self.current_token_is(TokenKind::RightBrace) {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidUntypedArrayConstructor,
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(
                                self.lexer.file_name.clone(),
                                loc.clone(),
                                self.current_token.span.end,
                            )),
                            hint: None,
                        });
                    }

                    untyped_array.push(self.parse_expression(Precedence::Lowest)?.0);

                    if self.peek_token_is(TokenKind::Comma) {
                        self.next_token(); // consume last token of the expression
                        self.next_token(); // consume comma
                    } else if self.peek_token_is(TokenKind::RightBrace) {
                        self.next_token();
                        break;
                    } else {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidToken(self.peek_token.kind.clone()),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(
                                self.lexer.file_name.clone(),
                                loc.clone(),
                                self.current_token.span.end,
                            )),
                            hint: None,
                        });
                    }
                }

                if let TypeSpecifier::Array(inner_type_specifier, ..) = data_type.clone() {
                    elements.push(Expression::Array(Array {
                        data_type: TypeSpecifier::Array(ArrayTypeSpecifier {
                            size: ArrayCapacity::Fixed(TokenKind::Literal(Literal {
                                kind: LiteralKind::Integer(untyped_array.len().try_into().unwrap()),
                                span: Span::new(untyped_array_start, self.current_token.span.end),
                                loc: loc.clone(),
                            })),
                            element_type: inner_type_specifier.element_type,
                        }),
                        elements: untyped_array,
                        span: Span::new(untyped_array_start, self.current_token.span.end),
                        loc: loc.clone(),
                    }));
                } else {
                    unreachable!()
                }
            } else {
                elements.push(self.parse_expression(Precedence::Lowest)?.0);
            }

            if self.peek_token_is(TokenKind::Comma) {
                self.next_token(); // consume last token of the expression

                if self.peek_token_is(TokenKind::RightBrace) {
                    break;
                }

                self.next_token(); // consume comma
                continue;
            } else {
                break;
            }
        }

        self.expect_peek(TokenKind::RightBrace)?;

        Ok(Expression::Array(Array {
            elements,
            data_type,
            span: Span::new(start, self.current_token.span.end),
            loc,
        }))
    }

    pub fn parse_unnamed_struct_value(&mut self) -> Result<Expression, ParserError> {
        let struct_start = self.current_token.span.start;

        let packed = {
            if self.current_token_is(TokenKind::Struct) {
                self.next_token(); // consume struct
                false
            } else if self.current_token_is(TokenKind::Bits) {
                self.next_token(); // consume bits
                true
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

        let mut fields: Vec<UnnamedStructValueField> = Vec::new();

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
                TokenKind::Identifier { name: field_name } => {
                    let start = self.current_token.span.start;
                    let loc = self.current_token.loc.clone();

                    self.next_token(); // consume identifier

                    let mut field_type: Option<TypeSpecifier> = None;
                    if self.current_token_is(TokenKind::Colon) {
                        self.next_token();

                        let type_specifier = self.parse_type_specifier()?;
                        self.next_token();

                        field_type = Some(type_specifier);
                    }

                    self.expect_current(TokenKind::Assign)?;
                    let field_value = self.parse_expression(Precedence::Lowest)?.0;
                    self.next_token();

                    fields.push(UnnamedStructValueField {
                        field_name: Identifier {
                            name: field_name.clone(),
                            span: Span {
                                start,
                                end: self.current_token.span.end,
                            },
                            loc: loc.clone(),
                        },
                        field_type,
                        field_value: Box::new(field_value),
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

        Ok(Expression::UnnamedStructValue(UnnamedStructValue {
            fields,
            packed,
            loc: self.current_token.loc.clone(),
            span: Span::new(struct_start, self.current_token.span.end),
        }))
    }
}
