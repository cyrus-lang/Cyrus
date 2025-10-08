use crate::Parser;
use crate::ParserError;
use crate::diagnostics::ParserDiagKind;
use crate::prec::*;
use ast::operators::InfixOperator;
use ast::operators::PrefixOperator;
use ast::operators::UnaryOperator;
use ast::source_loc::SourceLoc;
use ast::token::*;
use ast::*;
use diagcentral::Diag;
use diagcentral::DiagLevel;
use diagcentral::DiagLoc;

impl Parser {
    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<(Expression, Span), ParserError> {
        let mut left_start = self.current_token().span.start;
        let mut left = self.parse_prefix_expression()?;

        loop {
            if self.peek_token_is(TokenKind::Assign) {
                self.next_token();
                let expr = self.parse_assignment(left, AssignmentKind::Default, left_start)?;
                return Ok((expr, Span::new(left_start, self.current_token().span.end)));
            }

            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token();
                left = self.parse_array_index(left)?;
                continue;
            }

            if self.peek_token_is(TokenKind::Dot) || self.peek_token_is(TokenKind::FatArrow) {
                self.next_token();
                left = self.parse_field_access(left)?;
                continue;
            }

            if self.peek_token_is(TokenKind::Increment) {
                self.next_token();
                let loc = self.current_token().loc.clone();
                let span = Span::new(left_start, self.current_token().span.end);
                return Ok((
                    Expression::Unary(UnaryExpression {
                        operand: Box::new(left),
                        op: UnaryOperator::PostIncrement,
                        span: span.clone(),
                        loc,
                    }),
                    span,
                ));
            } else if self.peek_token_is(TokenKind::Decrement) {
                self.next_token();
                let loc = self.current_token().loc.clone();
                let span = Span::new(left_start, self.current_token().span.end);
                return Ok((
                    Expression::Unary(UnaryExpression {
                        operand: Box::new(left),
                        op: UnaryOperator::PostDecrement,
                        span: span.clone(),
                        loc,
                    }),
                    span,
                ));
            }

            // infix handling (respect precedence)
            let peek_prec = token_precedence_of(self.peek_token().kind);
            if self.peek_token().kind != TokenKind::EOF && precedence < peek_prec {
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
                                end: self.current_token().span.end,
                            },
                        ));
                    }
                }
            } else {
                break;
            }
        }

        let end = self.current_token().span.end;
        Ok((left, Span { start: left_start, end }))
    }

    pub fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let expr = match &self.current_token().clone().kind {
            TokenKind::Function => self.parse_lambda_expr()?,
            TokenKind::SizeOf => self.parse_sizeof_expression()?,
            TokenKind::Typecast => self.parse_cast_expression()?,
            TokenKind::Struct | TokenKind::Bits => self.parse_unnamed_struct_value(false)?,
            TokenKind::Const => {
                if self.peek_token_is(TokenKind::Struct) || self.peek_token_is(TokenKind::Bits) {
                    self.next_token();
                    self.parse_unnamed_struct_value(true)?
                } else if let TokenKind::Identifier { .. } = self.peek_token().kind {
                    self.next_token(); // consume const
                    let module_import = self.parse_module_import()?;
                    self.next_token();
                    let mut struct_init = self.parse_struct_init(module_import)?;
                    struct_init.is_const = true;
                    Expression::StructInit(struct_init)
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
            }
            TokenKind::Identifier { .. } => {
                let module_import = self.parse_module_import()?;

                if self.current_token_is(TokenKind::LeftBrace) {
                    Expression::StructInit(self.parse_struct_init(module_import)?)
                } else {
                    Expression::ModuleImport(module_import)
                }
            }
            TokenKind::Ampersand => {
                let start = self.current_token().span.start;
                let loc = self.current_token().loc.clone();
                self.next_token();
                Expression::AddressOf(AddressOf {
                    expr: Box::new(self.parse_expression(Precedence::Lowest)?.0),
                    span: Span::new(start, self.current_token().span.end),
                    loc,
                })
            }
            TokenKind::Asterisk => {
                let start = self.current_token().span.start;
                let loc = self.current_token().loc.clone();
                self.next_token();
                Expression::Dereference(Dereference {
                    expr: Box::new(self.parse_prefix_expression()?),
                    span: Span::new(start, self.current_token().span.end),
                    loc,
                })
            }
            TokenKind::Null => Expression::Literal(Literal {
                kind: LiteralKind::Null,
                span: Span::new(start, self.current_token().span.end),
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
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                self.current_token().loc.clone(),
                                self.file_name.clone(),
                            ))),
                            hint: Some(String::from("Wanted increment or decrement operator.")),
                        });
                    }
                };

                self.next_token(); // consume the operator

                match self.current_token().kind {
                    TokenKind::Identifier { .. } => {
                        let module_import = self.parse_module_import()?;

                        Expression::Unary(UnaryExpression {
                            operand: Box::new(Expression::ModuleImport(module_import)),
                            op: unary_operator,
                            span: Span {
                                start,
                                end: self.current_token().span.end,
                            },
                            loc: loc.clone(),
                        })
                    }
                    _ => {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidToken(token_kind.clone()),
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
            bool_token @ TokenKind::True | bool_token @ TokenKind::False => {
                let value = match bool_token {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => panic!(),
                };

                Expression::Literal(Literal {
                    kind: LiteralKind::Bool(value),
                    span: Span::new(start, self.current_token().span.end),
                    loc: loc.clone(),
                })
            }
            TokenKind::Literal(value) => Expression::Literal(value.clone()),
            token_kind @ TokenKind::Minus | token_kind @ TokenKind::Bang | token_kind @ TokenKind::Tilde => {
                let start = self.current_token().span.start;
                let prefix_operator = match token_kind {
                    TokenKind::Minus => PrefixOperator::Minus,
                    TokenKind::Bang => PrefixOperator::Bang,
                    TokenKind::Tilde => PrefixOperator::BitwiseNot,
                    _ => {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidPrefixOperator(token_kind.clone()),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
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
                // grouped expression
                self.next_token();
                let expr = self.parse_expression(Precedence::Lowest)?.0;
                self.next_token(); // consume last token of expr

                if self.current_token_is(TokenKind::Comma) {
                    // considered as tuple construction, not grouped expr
                    self.next_token();
                    self.parse_tuple_value(expr)?
                } else {
                    expr
                }
            }
            _ => {
                let type_specifier = self.parse_type_specifier()?;

                if self.peek_token_is(TokenKind::LeftBrace) {
                    self.next_token();
                    return Ok(self.parse_array(type_specifier)?);
                }

                Expression::TypeSpecifier(type_specifier)
            }
        };

        if self.peek_token_is(TokenKind::LeftBrace) {
            if let Expression::ModuleImport(module_import) = expr.clone() {
                self.next_token(); // consume struct name
                let struct_init = self.parse_struct_init(module_import)?;

                return Ok(Expression::StructInit(struct_init));
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
        } else if self.peek_token_is(TokenKind::LeftParen) {
            return self.parse_func_call(expr);
        } else {
            Ok(expr)
        }
    }

    fn parse_tuple_value(&mut self, first_expr: Expression) -> Result<Expression, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let mut expr_list: Vec<Expression> = vec![first_expr];

        loop {
            let expr = self.parse_expression(Precedence::Lowest)?.0;
            self.next_token(); // consume last token

            expr_list.push(expr);

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

        Ok(Expression::Tuple(TupleValue {
            expr_list,
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    fn parse_lambda_expr(&mut self) -> Result<Expression, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume function
        let params = self.parse_func_params()?;
        let return_type = self.parse_type_specifier()?;
        self.next_token(); // last token of return type

        let body = self.parse_block_statement()?;

        Ok(Expression::Lambda(Lambda {
            params,
            body: Box::new(body),
            return_type,
            span: Span::new(start, self.current_token().span.end),
            loc,
        }))
    }

    fn parse_sizeof_expression(&mut self) -> Result<Expression, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc;

        self.next_token(); // consume sizeof

        if !self.current_token_is(TokenKind::LeftParen) {
            return Err(Diag {
                kind: ParserDiagKind::MissingOpeningParen,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                hint: None,
            });
        }

        self.next_token(); // consume left paren

        let expr = if let Some(token) = self.peek_n_token(1) {
            if self.matches_type_token(self.current_token().kind) && self.matches_type_token(token.kind) {
                let type_specifier = self.parse_type_specifier()?;
                Expression::TypeSpecifier(type_specifier)
            } else {
                self.parse_expression(Precedence::Lowest)?.0
            }
        } else {
            self.parse_expression(Precedence::Lowest)?.0
        };

        if !self.peek_token_is(TokenKind::RightParen) {
            return Err(Diag {
                kind: ParserDiagKind::MissingClosingParen,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                hint: None,
            });
        }
        self.next_token();

        Ok(Expression::SizeOfExpression(SizeOfExpression {
            expr: Box::new(expr),
            loc,
            span: Span::new(start, self.current_token().span.end),
        }))
    }

    pub fn parse_infix_expression(
        &mut self,
        left: Expression,
        left_start: usize,
    ) -> Option<Result<Expression, ParserError>> {
        let loc = self.current_token().loc.clone();

        match self.peek_token().kind {
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
            | TokenKind::Ampersand
            | TokenKind::Pipe
            | TokenKind::Tilde
            | TokenKind::AmpTilde
            | TokenKind::Caret
            | TokenKind::ShiftLeft
            | TokenKind::ShiftRight
            | TokenKind::Identifier { .. } => {
                self.next_token(); // consume left expression
                let op_token = self.current_token().kind;
                if self.peek_token_is(TokenKind::Assign) {
                    self.next_token();
                    let kind = self.parse_assignment_kind(op_token, loc).ok()?;
                    return Some(self.parse_assignment(left, kind, left_start));
                }
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
                    TokenKind::Ampersand => InfixOperator::BitwiseAnd,
                    TokenKind::Pipe => InfixOperator::BitwiseOr,
                    TokenKind::Caret => InfixOperator::BitwiseXor,
                    TokenKind::AmpTilde => InfixOperator::BitwiseAndNot,
                    TokenKind::ShiftLeft => InfixOperator::ShiftLeft,
                    TokenKind::ShiftRight => InfixOperator::ShiftRight,
                    _ => {
                        return Some(Err(Diag {
                            kind: ParserDiagKind::InvalidInfixOperator(self.current_token().kind),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
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
        let start = self.current_token().span.start;
        let mut series: Vec<Expression> = Vec::new();

        // detect empty series of expressions
        if self.peek_token_is(end.clone()) {
            self.next_token();
            return Ok((series, Span::new(start, self.current_token().span.end)));
        } else if self.current_token_is(end.clone()) {
            return Ok((series, Span::new(start, self.current_token().span.end)));
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
                end: self.current_token().span.end,
            },
        ))
    }

    pub fn parse_module_import(&mut self) -> Result<ModuleImport, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let mut segments = match self.current_token().kind {
            TokenKind::Identifier { name } => {
                vec![ModuleSegment::SubModule(Identifier {
                    name,
                    span: Span {
                        start,
                        end: self.current_token().span.end - 1,
                    },
                    loc: loc.clone(),
                })]
            }
            _ => {
                return Err(Diag {
                    kind: ParserDiagKind::ExpectedIdentifier,
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                    hint: None,
                });
            }
        };

        if !self.peek_token_is(TokenKind::DoubleColon) {
            return Ok(ModuleImport {
                segments,
                span: Span::new(start, self.current_token().span.end),
                loc,
            });
        }

        self.next_token(); // consume first identifier

        loop {
            if self.current_token_is(TokenKind::DoubleColon) {
                self.next_token(); // consume double colon
            } else if let TokenKind::Identifier { name } = self.current_token().kind {
                segments.push(ModuleSegment::SubModule(Identifier {
                    name,
                    span: Span {
                        start,
                        end: self.peek_token().span.end - 1,
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
            span: Span::new(start, self.current_token().span.end),
            loc,
        })
    }

    pub fn parse_module_path(&mut self) -> Result<ModulePath, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let mut module_path = ModulePath {
            alias: None,
            segments: Vec::new(),
            loc: loc.clone(),
            span: Span::default(),
        };

        while !self.current_token_is(TokenKind::Semicolon) {
            match self.current_token().kind {
                TokenKind::Identifier { name: identifier } => {
                    let span = self.current_token().span.clone();
                    self.next_token(); // consume identifier

                    module_path.segments.push(ModuleSegment::SubModule(Identifier {
                        name: identifier.clone(),
                        span: span.clone(),
                        loc: loc.clone(),
                    }));

                    if self.current_token_is(TokenKind::DoubleColon) {
                        continue;
                    } else if self.current_token_is(TokenKind::Semicolon) {
                        return Ok(module_path);
                    } else if self.current_token_is(TokenKind::As) {
                        self.next_token(); // consume as

                        let alias = self.parse_identifier()?;
                        self.next_token();

                        module_path.alias = Some(alias.as_string());
                        break;
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
                        location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                        hint: None,
                    });
                }
            }
        }

        module_path.span = Span::new(start, self.current_token().span.end);

        Ok(module_path)
    }

    fn parse_cast_expression(&mut self) -> Result<Expression, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        self.next_token(); // consume typecast token
        self.expect_current(TokenKind::LeftParen)?;
        let target_type = self.parse_type_specifier()?;
        self.next_token(); // consume target_type
        self.expect_current(TokenKind::Comma)?;
        let expr = self.parse_expression(Precedence::Lowest)?.0;
        self.next_token();

        if !self.current_token_is(TokenKind::RightParen) {
            return Err(Diag {
                kind: ParserDiagKind::MissingClosingParen,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                hint: None,
            });
        }

        Ok(Expression::Cast(Cast {
            expr: Box::new(expr),
            target_type,
            span: Span::new(start, self.current_token().span.end),
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
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                hint: None,
            });
        }

        let args = self.parse_expression_series(TokenKind::RightParen)?.0;
        if !self.current_token_is(TokenKind::RightParen) {
            return Err(Diag {
                kind: ParserDiagKind::MissingClosingParen,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                hint: None,
            });
        }

        Ok(Expression::MethodCall(MethodCall {
            is_fat_arrow,
            operand: Box::new(operand),
            method_name,
            args,
            span: Span::new(start, self.current_token().span.end),
            loc,
        }))
    }

    pub fn parse_field_access(&mut self, operand: Expression) -> Result<Expression, ParserError> {
        let start = self.current_token().span.start;
        let loc = self.current_token().loc.clone();

        let is_fat_arrow = {
            if self.current_token_is(TokenKind::FatArrow) {
                self.next_token();
                true
            } else if self.current_token_is(TokenKind::Dot) {
                self.next_token();
                false
            } else {
                return Err(Diag {
                    kind: ParserDiagKind::InvalidToken(self.current_token().kind),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
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
            span: Span::new(start, self.current_token().span.end),
            loc,
        }))
    }

    pub fn parse_func_call(&mut self, operand: Expression) -> Result<Expression, ParserError> {
        let loc = self.current_token().loc.clone();
        let start = self.current_token().span.start;

        self.expect_peek(TokenKind::LeftParen)?;

        let args = self.parse_expression_series(TokenKind::RightParen)?.0;
        if !(self.current_token_is(TokenKind::RightParen)) {
            return Err(Diag {
                kind: ParserDiagKind::InvalidToken(self.current_token().kind),
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                hint: None,
            });
        }

        Ok(Expression::FuncCall(FuncCall {
            operand: Box::new(operand),
            args,
            span: Span::new(start, self.current_token().span.end),
            loc: loc,
        }))
    }

    pub fn parse_struct_init(&mut self, struct_name: ModuleImport) -> Result<StructInit, ParserError> {
        let loc = self.current_token().loc.clone();
        let start = self.current_token().span.start;

        let mut field_inits: Vec<FieldInit> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(StructInit {
                struct_name,
                field_inits,
                is_const: false,
                loc,
                span: Span {
                    start,
                    end: self.current_token().span.end,
                },
            });
        }

        loop {
            let field_name = self.parse_identifier()?;
            let field_loc = self.current_token().loc.clone();

            self.next_token(); // consume identifier
            self.expect_current(TokenKind::Colon)?;

            let value = self.parse_expression(Precedence::Lowest)?.0;
            self.next_token();

            field_inits.push(FieldInit {
                identifier: field_name,
                value,
                loc: field_loc,
            });

            match self.current_token().kind {
                TokenKind::EOF => {
                    return Err(Diag {
                        kind: ParserDiagKind::MissingClosingBrace,
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc,
                            self.file_name.clone(),
                        ))),
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
                        kind: ParserDiagKind::InvalidToken(self.current_token().kind),
                        level: DiagLevel::Error,
                        location: Some(DiagLoc::new(SourceLoc::from_loc(
                            self.current_token().loc,
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
            }
        }

        Ok(StructInit {
            struct_name,
            field_inits,
            is_const: false,
            loc,
            span: Span {
                start,
                end: self.current_token().span.end,
            },
        })
    }

    pub fn parse_assignment_kind(
        &mut self,
        token_kind: TokenKind,
        loc: Location,
    ) -> Result<AssignmentKind, ParserError> {
        match token_kind {
            TokenKind::Plus => Ok(AssignmentKind::AddAssign),
            TokenKind::Minus => Ok(AssignmentKind::SubAssign),
            TokenKind::Asterisk => Ok(AssignmentKind::MulAssign),
            TokenKind::Slash => Ok(AssignmentKind::DivAssign),
            TokenKind::Percent => Ok(AssignmentKind::ModAssign),
            TokenKind::Ampersand => Ok(AssignmentKind::BitwiseAndAssign),
            TokenKind::Tilde => Ok(AssignmentKind::BitwiseXorAssign),
            TokenKind::AmpTilde => Ok(AssignmentKind::BitwiseAndNotAssign),
            TokenKind::ShiftLeft => Ok(AssignmentKind::LeftShiftAssign),
            TokenKind::ShiftRight => Ok(AssignmentKind::RightShiftAssign),
            _ => {
                return Err(Diag {
                    kind: ParserDiagKind::InvalidAssignOperator(token_kind),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                    hint: None,
                });
            }
        }
    }

    pub fn parse_assignment(
        &mut self,
        lhs: Expression,
        kind: AssignmentKind,
        start: usize,
    ) -> Result<Expression, ParserError> {
        let loc = self.current_token().loc.clone();

        self.expect_current(TokenKind::Assign)?;
        let rhs = self.parse_expression(Precedence::Lowest)?.0;
        let end = self.current_token().span.end;
        Ok(Expression::Assignment(Box::new(Assignment {
            lhs,
            rhs,
            kind,
            span: Span { start, end },
            loc,
        })))
    }

    pub fn parse_array_index(&mut self, expr: Expression) -> Result<Expression, ParserError> {
        let loc = self.current_token().loc.clone();
        let start = self.current_token().span.start;

        let index_expr = self.parse_single_array_index()?;
        let mut indexes_backup: Vec<Expression> = vec![index_expr.clone()];

        let mut base_index = Expression::ArrayIndex(ArrayIndex {
            operand: Box::new(expr.clone()),
            index: Box::new(index_expr.clone()),
            span: Span::new(start, self.current_token().span.end),
            loc: loc.clone(),
        });

        // handle chained indices ident[a][b][c]
        if self.peek_token_is(TokenKind::LeftBracket) {
            self.next_token();
        }

        while self.current_token_is(TokenKind::LeftBracket) {
            let index_expr = self.parse_single_array_index()?;

            base_index = Expression::ArrayIndex(ArrayIndex {
                operand: Box::new(base_index),
                index: Box::new(index_expr.clone()),
                span: Span::new(start, self.current_token().span.end),
                loc: loc.clone(),
            });

            indexes_backup.push(index_expr);

            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token();
            }
        }

        // ambiguity resolution!
        if self.peek_token_is(TokenKind::LeftBrace) {
            let element_type = {
                match expr.clone() {
                    Expression::Identifier(identifier) => TypeSpecifier::Identifier(identifier),
                    Expression::ModuleImport(module_import) => TypeSpecifier::ModuleImport(module_import),
                    Expression::TypeSpecifier(type_specifier) => type_specifier,
                    _ => {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidToken(self.peek_token().kind),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                            hint: None,
                        });
                    }
                }
            };

            let mut base_array = ArrayTypeSpecifier {
                size: ArrayCapacity::Fixed(Box::new(indexes_backup.remove(0))),
                element_type: Box::new(element_type),
            };

            for index in &indexes_backup {
                base_array = ArrayTypeSpecifier {
                    size: ArrayCapacity::Fixed(Box::new(index.clone())),
                    element_type: Box::new(TypeSpecifier::Array(base_array)),
                };
            }

            // parse the initializer block
            self.next_token();
            let array_const = self.parse_array(TypeSpecifier::Array(base_array))?;

            return Ok(array_const);
        }

        Ok(base_index)
    }

    pub fn parse_array(&mut self, data_type: TypeSpecifier) -> Result<Expression, ParserError> {
        let loc = self.current_token().loc.clone();
        let start = self.current_token().span.start;

        if !matches!(data_type, TypeSpecifier::Array(..)) {
            return Err(Diag {
                kind: ParserDiagKind::NonArrayDataTypeForArrayConstruction,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: None,
            });
        }

        if !self.current_token_is(TokenKind::LeftBrace) {
            return Err(Diag {
                kind: ParserDiagKind::MissingOpeningBrace,
                level: DiagLevel::Error,
                location: Some(DiagLoc::new(SourceLoc::from_loc(
                    self.current_token().loc.clone(),
                    self.file_name.clone(),
                ))),
                hint: None,
            });
        }

        let mut elements: Vec<Expression> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Expression::Array(Array {
                elements,
                data_type,
                span: Span::new(start, self.current_token().span.end),
                loc: loc.clone(),
            }));
        }

        loop {
            if self.current_token_is(TokenKind::LeftBrace) {
                let untyped_array_start = self.current_token().span.start;
                let mut untyped_array: Vec<Expression> = Vec::new();
                self.next_token(); // consume left brace

                loop {
                    if self.current_token_is(TokenKind::RightBrace) {
                        return Err(Diag {
                            kind: ParserDiagKind::InvalidUntypedArrayConstructor,
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(
                                self.current_token().loc.clone(),
                                self.file_name.clone(),
                            ))),
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
                            kind: ParserDiagKind::InvalidToken(self.peek_token().kind),
                            level: DiagLevel::Error,
                            location: Some(DiagLoc::new(SourceLoc::from_loc(loc.clone(), self.file_name.clone()))),
                            hint: None,
                        });
                    }
                }

                if let TypeSpecifier::Array(inner_type_specifier, ..) = data_type.clone() {
                    let data_type = TypeSpecifier::Array(ArrayTypeSpecifier {
                        size: ArrayCapacity::Fixed(Box::new(Expression::Literal(Literal {
                            kind: LiteralKind::Integer(untyped_array.len().try_into().unwrap(), None),
                            span: Span::new(untyped_array_start, self.current_token().span.end),
                            loc: loc.clone(),
                        }))),
                        element_type: inner_type_specifier.element_type,
                    });
                    elements.push(Expression::Array(Array {
                        data_type,
                        elements: untyped_array,
                        span: Span::new(untyped_array_start, self.current_token().span.end),
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
            span: Span::new(start, self.current_token().span.end),
            loc,
        }))
    }

    pub fn parse_unnamed_struct_value(&mut self, is_const: bool) -> Result<Expression, ParserError> {
        let struct_start = self.current_token().span.start;

        let packed = {
            if self.current_token_is(TokenKind::Struct) {
                self.next_token(); // consume struct
                false
            } else if self.current_token_is(TokenKind::Bits) {
                self.next_token(); // consume bits
                true
            } else {
                return Err(Diag {
                    kind: ParserDiagKind::InvalidToken(self.current_token().kind),
                    level: DiagLevel::Error,
                    location: Some(DiagLoc::new(SourceLoc::from_loc(
                        self.current_token().loc,
                        self.file_name.clone(),
                    ))),
                    hint: None,
                });
            }
        };
        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<UnnamedStructValueField> = Vec::new();

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
                            self.current_token().loc,
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
                TokenKind::Identifier { name: field_name } => {
                    let start = self.current_token().span.start;
                    let loc = self.current_token().loc.clone();

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
                                end: self.current_token().span.end,
                            },
                            loc: loc.clone(),
                        },
                        field_type,
                        field_value: Box::new(field_value),
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
                            self.current_token().loc,
                            self.file_name.clone(),
                        ))),
                        hint: None,
                    });
                }
            }
        }

        Ok(Expression::UnnamedStructValue(UnnamedStructValue {
            fields,
            packed,
            is_const,
            loc: self.current_token().loc.clone(),
            span: Span::new(struct_start, self.current_token().span.end),
        }))
    }
}
