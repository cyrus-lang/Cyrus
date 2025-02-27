use crate::ParseError;
use crate::Parser;
use crate::precedences::*;
use ast::ast::*;
use ast::token::*;
use utils::compile_time_errors::errors::CompileTimeError;
use utils::compile_time_errors::parser_errors::ParserErrorType;

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<(Expression, Span), ParseError> {
        let mut left_start = self.current_token.span.start;
        let mut left = self.parse_prefix_expression()?;

        if self.current_token_is(TokenKind::As) || self.peek_token_is(TokenKind::As) {
            return self.parse_cast_as_expression(left, left_start);
        }

        // dbg!(left.clone());
        // dbg!(self.peek_token.kind.clone());

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

        // REVIEW
        if !(self.current_token_is(TokenKind::Semicolon)
            || self.current_token_is(TokenKind::LeftBrace)
            || self.current_token_is(TokenKind::RightBracket))
        {
            self.next_token();
        }

        Ok((left, Span { start: left_start, end }))
    }

    pub fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let span: Span = self.current_token.span.clone();

        let expr = match &self.current_token.clone().kind {
            TokenKind::Identifier { .. } => {
                let from_package = self.parse_from_package()?;
                
                if self.peek_token_is(TokenKind::Increment) {
                    self.next_token();
                    Expression::UnaryOperator(UnaryOperator {
                        identifier: from_package.clone(),
                        ty: UnaryOperatorType::PostIncrement,
                        span,
                        loc: self.current_location(),
                    })
                } else if self.peek_token_is(TokenKind::Decrement) {
                    self.next_token();
                    Expression::UnaryOperator(UnaryOperator {
                        identifier: from_package.clone(),
                        ty: UnaryOperatorType::PostDecrement,
                        span,
                        loc: self.current_location(),
                    })
                } else if self.current_token_is(TokenKind::Assign) {
                    return self.parse_assignment(from_package);
                } else if self.current_token_is(TokenKind::LeftBracket) {
                    let array_index = self.parse_array_index(from_package)?;

                    if self.peek_token_is(TokenKind::Assign) {
                        return self.parse_array_index_assign(array_index);
                    }

                    Expression::ArrayIndex(array_index)
                } else if self.current_token_is(TokenKind::LeftBrace) {
                    return self.parse_struct_init(from_package);
                } else if self.current_token_is(TokenKind::LeftParen) {
                    return self.parse_field_access_or_method_call(
                        ast::ast::Expression::FromPackage(from_package.clone()),
                        from_package.span.start,
                    );
                } else {
                    self.next_token();
                    Expression::FromPackage(from_package)
                }
            }
            TokenKind::Ampersand => {
                self.next_token();
                Expression::AddressOf(Box::new(self.parse_prefix_expression()?))
            }
            TokenKind::Asterisk => {
                self.next_token();
                Expression::Dereference(Box::new(self.parse_prefix_expression()?))
            }
            TokenKind::Null => Expression::Literal(Literal::Null),
            token_kind @ TokenKind::Increment | token_kind @ TokenKind::Decrement => {
                let unary_operator_type = match token_kind {
                    TokenKind::Increment => UnaryOperatorType::PreIncrement,
                    TokenKind::Decrement => UnaryOperatorType::PreDecrement,
                    _ => {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::InvalidToken(token_kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            code_raw: Some(self.lexer.select(span.start..self.current_token.span.end)),
                            verbose: Some(String::from("Expected increment (++) or decrement (--) operator.")),
                            caret: true,
                        });
                    }
                };

                self.next_token(); // consume the operator

                match self.current_token.kind.clone() {
                    TokenKind::Identifier { .. } => {
                        let from_package = self.parse_from_package()?;

                        Expression::UnaryOperator(UnaryOperator {
                            identifier: from_package,
                            ty: unary_operator_type,
                            span: Span {
                                start: span.start,
                                end: self.current_token.span.end,
                            },
                            loc: self.current_location(),
                        })
                    }
                    _ => {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::InvalidToken(token_kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            code_raw: Some(self.lexer.select(span.start..self.current_token.span.end)),
                            verbose: Some(String::from("Expected an identifier.")),
                            caret: true,
                        });
                    }
                }
            }
            bool_token @ TokenKind::True | bool_token @ TokenKind::False => {
                let raw = match bool_token {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => panic!(),
                };

                Expression::Literal(Literal::Bool(BoolLiteral { raw, span }))
            }
            TokenKind::Literal(value) => Expression::Literal(value.clone()),
            TokenKind::Minus | TokenKind::Bang => {
                let start = self.current_token.span.start;
                let prefix_operator = self.current_token.clone();

                self.next_token(); // consume the prefix operator

                let (expr, span) = self.parse_expression(Precedence::Prefix)?;

                Expression::Prefix(UnaryExpression {
                    operator: prefix_operator,
                    operand: Box::new(expr),
                    span: Span { start, end: span.end },
                    loc: self.current_location(),
                })
            }
            TokenKind::LeftParen => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Lowest)?.0;
                self.expect_peek(TokenKind::RightParen)?;
                expr
            }
            TokenKind::LeftBracket => self.parse_array_items()?,
            _ => {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(self.lexer.select(span.start..self.current_token.span.end)),
                    verbose: Some(String::from(format!(
                        "Unexpected token '{}'. Expected a number, variable, or operator.",
                        self.current_token.kind.clone()
                    ))),
                    caret: true,
                });
            }
        };

        // FIXME
        if self.current_token_is(TokenKind::Dot) {
            return self.parse_struct_member(Box::new(expr));
        }

        Ok(expr)
    }

    pub fn parse_infix_expression(
        &mut self,
        left: Expression,
        left_start: usize,
    ) -> Option<Result<Expression, ParseError>> {
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
            | TokenKind::Identifier { .. } => {
                self.next_token(); // consume left expression
                let operator = self.current_token.clone();
                let precedence = token_precedence_of(operator.kind.clone());
                self.next_token(); // consume the operator

                let (right, span) = self.parse_expression(precedence).ok()?;

                Some(Ok(Expression::Infix(BinaryExpression {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: Span {
                        start: left_start,
                        end: span.end,
                    },
                    loc: self.current_location(),
                })))
            }
            TokenKind::LeftParen => {
                self.next_token(); // consume the identifier token
                let expr = self.parse_field_access_or_method_call(left, left_start);
                Some(expr)
            }
            _ => None,
        }
    }

    /// Parses a series of expressions terminated by a specified token.
    ///
    /// This function collects a series of comma-separated expressions and ensures that the series
    /// ends with the specified terminating token. It also handles empty expression series and
    /// trailing commas before the end token.
    ///
    /// # Parameters
    /// - `end`: The `TokenKind` that signifies the end of the expression series (e.g., `RightParen`, `RightBracket`).
    ///
    /// # Returns
    /// - `Ok((Vec<Expression>, Span))`:
    ///   - A vector of parsed expressions.
    ///   - A `Span` indicating the start and end positions of the entire series.
    /// - `Err(ParseError)`: If the series is not properly terminated or contains syntax errors.
    ///
    /// # Behavior
    /// - If the series is empty (i.e., immediately followed by the `end` token), it returns an empty vector and the corresponding span.
    /// - Parses each expression in the series, separated by commas (`TokenKind::Comma`).
    /// - Allows trailing commas before the terminating token.
    /// - Advances the lexer as it consumes tokens for expressions, commas, and the terminating token.
    ///
    /// # Errors
    /// - Returns an error if the terminating token is missing or mismatched.
    /// - Returns an error if there are syntax issues while parsing individual expressions.
    pub fn parse_expression_series(&mut self, end: TokenKind) -> Result<(Vec<Expression>, Span), ParseError> {
        let start = self.current_token.span.start;
        let mut series: Vec<Expression> = Vec::new();

        // Detect empty series of expressions
        if self.peek_token_is(end.clone()) {
            self.next_token();

            return Ok((
                series,
                Span {
                    start,
                    end: self.current_token.span.end,
                },
            ));
        }
        self.next_token(); // consume left brace

        let first_argument = self.parse_expression(Precedence::Lowest)?.0;
        series.push(first_argument);

        while self.current_token_is(TokenKind::Comma) {
            self.next_token();

            if self.current_token_is(TokenKind::RightBracket) {
                break;
            }

            let expr = self.parse_expression(Precedence::Lowest)?.0;
            series.push(expr);
        }

        if !(self.current_token_is(end.clone()) || self.current_token_is(TokenKind::EOF)) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::UnexpectedToken(self.current_token.kind.clone(), end),
                file_name: Some(self.lexer.file_name.clone()),
                code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                verbose: None,
                caret: true,
            });
        }

        Ok((
            series,
            Span {
                start,
                end: self.current_token.span.end,
            },
        ))
    }

    pub fn parse_cast_as_expression(
        &mut self,
        left: Expression,
        left_start: usize,
    ) -> Result<(Expression, Span), ParseError> {
        if self.peek_token_is(TokenKind::As) {
            self.next_token(); // consume left expression
            self.next_token(); // consume as expression
        } else if self.current_token_is(TokenKind::As) {
            self.next_token(); // consume as expression
        } else {
            panic!("Unexpected behaviour when trying to parse cast_as_expression.");
        }

        match self.parse_type_token() {
            Ok(cast_as) => {
                return Ok((
                    Expression::CastAs(CastAs {
                        expr: Box::new(left),
                        cast_as,
                        span: Span {
                            start: left_start,
                            end: self.current_token.span.end.clone(),
                        },
                        loc: self.current_location(),
                    }),
                    Span {
                        start: left_start,
                        end: self.current_token.span.end.clone(),
                    },
                ));
            }
            Err(err) => return Err(err),
        }
    }

    // FIXME
    pub fn parse_field_access_or_method_call(
        &mut self,
        left: Expression,
        left_start: usize,
    ) -> Result<Expression, ParseError> {

        let mut chains: Vec<FieldAccessOrMethodCall> = vec![FieldAccessOrMethodCall {
            method_call: Some(self.parse_func_call(left, left_start)?),
            field_access: None,
        }];

        while self.current_token_is(TokenKind::Dot) {
            self.next_token(); // consume dot

            let identifier = match self.current_token.kind.clone() {
                TokenKind::Identifier { name } => Identifier {
                    name,
                    span: self.current_token.span.clone(),
                    loc: self.current_location(),
                },
                _ => {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::ExpectedIdentifier,
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(left_start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
                    });
                }
            };
            self.next_token();

            if self.current_token_is(TokenKind::LeftParen) {
                chains.push(FieldAccessOrMethodCall {
                    method_call: Some(self.parse_func_call(ast::ast::Expression::Identifier(identifier), left_start)?),
                    field_access: None,
                });
            } else {
                chains.push(FieldAccessOrMethodCall {
                    method_call: None,
                    field_access: Some(FieldAccess {
                        identifier,
                        span: self.current_token.span.clone(),
                        loc: self.current_location(),
                    }),
                });
            }
        }

        Ok(Expression::FieldAccessOrMethodCall(chains))
    }

    pub fn parse_func_call(&mut self, left: Expression, left_start: usize) -> Result<FuncCall, ParseError> {
        let arguments = self.parse_expression_series(TokenKind::RightParen)?;
        let start = self.current_token.span.start;
        match left {
            Expression::FromPackage(from_package) => {
                self.next_token();

                Ok(FuncCall {
                    func_name: from_package,
                    arguments: arguments.0,
                    span: Span {
                        start: left_start,
                        end: self.current_token.span.end,
                    },
                    loc: self.current_location(),
                })
            }
            Expression::Identifier(identifier) => {
                self.next_token();

                Ok(FuncCall {
                    func_name: FromPackage {
                        sub_packages: vec![],
                        identifier,
                        span: self.current_token.span.clone(),
                        loc: self.current_location(),
                    },
                    arguments: arguments.0,
                    span: Span {
                        start: left_start,
                        end: self.current_token.span.end,
                    },
                    loc: self.current_location(),
                })
            }
            _ => {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::ExpectedIdentifier,
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                    verbose: None,
                    caret: true,
                });
            }
        }
    }

    pub fn parse_struct_member(&mut self, object_expr: Box<Expression>) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start.clone();

        let mut chains: Vec<FieldAccessOrMethodCall> = Vec::new();
        self.next_token();

        loop {
            let member_start = self.current_token.span.start.clone();
            let method_name = match self.current_token.kind.clone() {
                TokenKind::Identifier { name } => Identifier {
                    name,
                    span: self.current_token.span.clone(),
                    loc: self.current_location(),
                },
                _ => {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::ExpectedIdentifier,
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
                    });
                }
            };

            if self.peek_token_is(TokenKind::LeftParen) {
                // parse struct method call

                self.next_token(); // consume method name

                let arguments = self.parse_expression_series(TokenKind::RightParen)?.0;
                self.expect_current(TokenKind::RightParen)?;

                let method_call = FuncCall {
                    func_name: FromPackage {
                        sub_packages: vec![],
                        identifier: method_name,
                        span: Span {
                            start: member_start,
                            end: self.current_token.span.end,
                        },
                        loc: self.current_location(),
                    },
                    arguments,
                    span: Span {
                        start: member_start,
                        end: self.current_token.span.end,
                    },
                    loc: self.current_location(),
                };

                chains.push(FieldAccessOrMethodCall {
                    method_call: Some(method_call),
                    field_access: None,
                });

                match self.current_token.kind {
                    TokenKind::Dot => {
                        self.next_token();
                        continue;
                    }
                    TokenKind::RightBracket => {
                        break;
                    }
                    TokenKind::RightParen => {
                        break;
                    }
                    TokenKind::Semicolon => {
                        break;
                    }
                    _ => {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::MissingSemicolon,
                            file_name: Some(self.lexer.file_name.clone()),
                            code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                            verbose: None,
                            caret: true,
                        });
                    }
                }
            } else {
                // parse struct field access

                let field_name = match self.current_token.kind.clone() {
                    TokenKind::Identifier { name } => Identifier {
                        name,
                        span: self.current_token.span.clone(),
                        loc: self.current_location(),
                    },
                    _ => {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::ExpectedIdentifier,
                            file_name: Some(self.lexer.file_name.clone()),
                            code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                            verbose: None,
                            caret: true,
                        });
                    }
                };
                self.next_token();

                chains.push(FieldAccessOrMethodCall {
                    method_call: None,
                    field_access: Some(FieldAccess {
                        identifier: field_name,
                        span: self.current_token.span.clone(),
                        loc: self.current_location(),
                    }),
                });

                match self.current_token.kind {
                    TokenKind::Dot => {
                        self.next_token(); // consume field_name
                        continue;
                    }
                    _ => {
                        break;
                    }
                }
            }
        }

        return Ok(Expression::StructFieldAccess(Box::new(StructFieldAccess {
            expr: *object_expr,
            chains,
            span: Span {
                start,
                end: self.current_token.span.end,
            },
            loc: self.current_location(),
        })));
    }

    pub fn parse_struct_init(&mut self, struct_name: FromPackage) -> Result<Expression, ParseError> {
        let mut field_inits: Vec<FieldInit> = Vec::new();
        let start = self.current_token.span.start;

        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Expression::StructInit(StructInit {
                struct_name: struct_name.clone(),
                field_inits: Vec::new(),
                loc: self.current_location(),
            }));
        }

        loop {
            let field_name = match self.current_token.kind.clone() {
                TokenKind::Identifier { name } => {
                    self.next_token(); // consume identifier
                    name
                }
                _ => {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::ExpectedIdentifier,
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
                    });
                }
            };
            self.expect_current(TokenKind::Colon)?;

            let value = self.parse_expression(Precedence::Lowest)?.0;
            self.next_token(); // consume expr

            field_inits.push(FieldInit {
                name: field_name,
                value,
                loc: self.current_location(),
            });

            match self.current_token.kind.clone() {
                TokenKind::EOF => {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::MissingClosingBrace,
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
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
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
                    });
                }
            }
        }

        return Ok(Expression::StructInit(StructInit {
            struct_name,
            field_inits,
            loc: self.current_location(),
        }));
    }

    pub fn parse_assignment(&mut self, from_package: FromPackage) -> Result<Expression, ParseError> {
        let start: usize = self.current_token.span.start;

        self.next_token(); // consume assign

        let expr = self.parse_expression(Precedence::Lowest)?.0;

        let end = self.current_token.span.end;

        Ok(Expression::Assignment(Box::new(Assignment {
            identifier: from_package,
            expr,
            span: Span { start, end },
            loc: self.current_location(),
        })))
    }

    pub fn parse_array_index_assign(&mut self, array_index: ArrayIndex) -> Result<Expression, ParseError> {
        self.next_token(); // consume right bracket

        self.expect_current(TokenKind::Assign)?;

        let expr = self.parse_expression(Precedence::Lowest)?.0;

        Ok(Expression::ArrayIndexAssign(Box::new(ArrayIndexAssign {
            from_package: array_index.from_package,
            dimensions: array_index.dimensions,
            span: Span {
                start: array_index.span.start,
                end: self.current_token.span.end,
            },
            loc: self.current_location(),
            expr,
        })))
    }

    pub fn parse_array_index(&mut self, from_package: FromPackage) -> Result<ArrayIndex, ParseError> {
        let start = self.current_token.span.start;

        let mut dimensions: Vec<Expression> = Vec::new();

        while self.current_token_is(TokenKind::LeftBracket) {
            let expr = self.parse_array_items()?;
            dimensions.push(expr);
        }

        let end = self.current_token.span.end;

        Ok(ArrayIndex {
            dimensions,
            span: Span { start, end },
            from_package: from_package,
            loc: self.current_location(),
        })
    }

    pub fn parse_array_items(&mut self) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;

        let elements = self.parse_expression_series(TokenKind::RightBracket)?;

        Ok(Expression::Array(Array {
            elements: elements.0,
            span: Span {
                start,
                end: elements.1.end,
            },
            loc: self.current_location(),
        }))
    }
}
