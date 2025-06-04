use crate::ParseError;
use crate::Parser;
use crate::diag::ParserErrorType;
use crate::prec::*;
use ast::ast::*;
use ast::token::*;
use diag::errors::CompileTimeError;

impl<'a> Parser<'a> {
    pub fn parse_expression(&mut self, precedence: Precedence) -> Result<(Expression, Span), ParseError> {
        let mut left_start = self.current_token.span.start;
        let mut left = self.parse_prefix_expression()?;

        while self.peek_token_is(TokenKind::Dot) {
            self.next_token(); // consume the left expression
            self.expect_current(TokenKind::Dot)?;
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

    pub fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;

        let expr = match &self.current_token.clone().kind {
            TokenKind::Identifier { .. } => {
                let module_import = self.parse_module_import()?;

                if self.current_token_is(TokenKind::LeftBrace) {
                    self.parse_struct_init(module_import)?
                } else if self.peek_token_is(TokenKind::Increment) {
                    self.next_token();
                    Expression::UnaryOperator(UnaryOperator {
                        module_import: module_import.clone(),
                        ty: UnaryOperatorType::PostIncrement,
                        span: Span::new(start, self.current_token.span.end),
                        loc: self.current_location(),
                    })
                } else if self.peek_token_is(TokenKind::Decrement) {
                    self.next_token();
                    Expression::UnaryOperator(UnaryOperator {
                        module_import: module_import.clone(),
                        ty: UnaryOperatorType::PostDecrement,
                        span: Span::new(start, self.current_token.span.end),
                        loc: self.current_location(),
                    })
                } else {
                    Expression::ModuleImport(module_import)
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
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: Some(String::from("Expected increment (++) or decrement (--) operator.")),
                            caret: true,
                        });
                    }
                };

                self.next_token(); // consume the operator

                match self.current_token.kind.clone() {
                    TokenKind::Identifier { .. } => {
                        let module_import = self.parse_module_import()?;

                        Expression::UnaryOperator(UnaryOperator {
                            module_import: module_import,
                            ty: unary_operator_type,
                            span: Span {
                                start,
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
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: Some(String::from("Expected an identifier.")),
                            caret: true,
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

                Expression::Literal(Literal::Bool(value))
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
                // c-style casting
                if self.matches_type_token(self.peek_token.kind.clone()) {
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
                    let start = self.current_token.span.start;
                    let type_specifier = self.parse_type_specifier()?;

                    if self.peek_token_is(TokenKind::LeftBrace) {
                        self.next_token();
                        return Ok(self.parse_array(type_specifier)?);
                    } else if self.current_token_is(TokenKind::LeftBrace) {
                        todo!();
                    }

                    Expression::TypeSpecifier(type_specifier)
                } else {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: None,
                        caret: true,
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
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                    file_name: Some(self.lexer.file_name.clone()),
                    source_content: Box::new(self.lexer.input.clone()),
                    verbose: None,
                    caret: true,
                });
            }
        } else if self.peek_token_is(TokenKind::LeftBracket) {
            self.next_token();
            return Ok(Expression::ArrayIndex(self.parse_array_index(expr)?));
        } else if self.current_token_is(TokenKind::LeftBracket) {
            return Ok(Expression::ArrayIndex(self.parse_array_index(expr)?));
        } else if self.peek_token_is(TokenKind::LeftParen) {
            return self.parse_func_call(expr);
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
            | TokenKind::And
            | TokenKind::Or
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
            _ => None,
        }
    }

    pub fn parse_expression_series(&mut self, end: TokenKind) -> Result<(Vec<Expression>, Span), ParseError> {
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

    pub fn parse_module_import(&mut self) -> Result<ModuleImport, ParseError> {
        let start = self.current_token.span.start;
        let mut segments = match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => {
                vec![ModuleSegment::SubModule(Identifier {
                    name,
                    span: Span {
                        start,
                        end: self.current_token.span.end - 1,
                    },
                    loc: self.current_location(),
                })]
            }
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
        };

        if !self.peek_token_is(TokenKind::DoubleColon) {
            return Ok(ModuleImport {
                segments,
                span: Span::new(start, self.current_token.span.end),
                loc: self.current_location(),
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
                    loc: self.current_location(),
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
            loc: self.current_location(),
        })
    }

    pub fn parse_module_path(&mut self) -> Result<ModulePath, ParseError> {
        let mut module_path = ModulePath {
            alias: None,
            segments: Vec::new(),
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
                            return Err(CompileTimeError {
                                location: self.current_location(),
                                etype: ParserErrorType::UnexpectedToken(
                                    TokenKind::DoubleColon,
                                    self.current_token.kind.clone(),
                                ),
                                file_name: Some(self.lexer.file_name.clone()),
                                source_content: Box::new(self.lexer.input.clone()),
                                verbose: None,
                                caret: true,
                            });
                        }
                    }

                    module_path.segments.push(ModuleSegment::SubModule(Identifier {
                        name: identifier.clone(),
                        span: span.clone(),
                        loc: self.current_location(),
                    }));

                    if self.current_token_is(TokenKind::DoubleColon) {
                        continue;
                    } else if self.current_token_is(TokenKind::Semicolon) {
                        return Ok(module_path);
                    } else {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: None,
                            caret: true,
                        });
                    }
                }
                TokenKind::DoubleColon => {
                    self.next_token();
                    continue;
                }
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

        Ok(module_path)
    }

    pub fn parse_cast_expression(&mut self, start: usize) -> Result<Expression, ParseError> {
        self.expect_current(TokenKind::LeftParen)?;
        let target_type = self.parse_type_specifier()?;
        self.next_token(); // consume target_type
        self.expect_current(TokenKind::RightParen)?;

        let expr = self.parse_expression(Precedence::Lowest)?.0;

        Ok(Expression::Cast(Cast {
            expr: Box::new(expr),
            target_type,
            span: Span::new(start, self.current_token.span.end),
            loc: self.current_location(),
        }))
    }

    pub fn parse_method_call(
        &mut self,
        operand: Expression,
        method_name: Identifier,
        start: usize,
        loc: Location,
    ) -> Result<Expression, ParseError> {
        if !self.current_token_is(TokenKind::LeftParen) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingOpeningParen,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: true,
            });
        }

        let arguments = self.parse_expression_series(TokenKind::RightParen)?.0;
        if !self.current_token_is(TokenKind::RightParen) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingClosingParen,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: true,
            });
        }

        Ok(Expression::MethodCall(MethodCall {
            operand: Box::new(operand),
            method_name,
            arguments,
            span: Span::new(start, self.current_token.span.end),
            loc,
        }))
    }

    pub fn parse_field_access(&mut self, operand: Expression) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_location();

        let identifier = self.parse_identifier()?;

        if self.peek_token_is(TokenKind::LeftParen) {
            self.next_token(); // consume identifier
            return self.parse_method_call(operand, identifier, start, loc);
        }

        Ok(Expression::FieldAccess(FieldAccess {
            operand: Box::new(operand),
            field_name: identifier,
            span: Span::new(start, self.current_token.span.end),
            loc,
        }))
    }

    pub fn parse_func_call(&mut self, operand: Expression) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;

        self.expect_peek(TokenKind::LeftParen)?;

        let arguments = self.parse_expression_series(TokenKind::RightParen)?.0;
        if !(self.current_token_is(TokenKind::RightParen)) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingClosingParen,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: true,
            });
        }

        Ok(Expression::FuncCall(FuncCall {
            operand: Box::new(operand),
            arguments,
            span: Span::new(start, self.current_token.span.end),
            loc: self.current_location(),
        }))
    }

    pub fn parse_struct_init(&mut self, struct_name: ModuleImport) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;
        let mut field_inits: Vec<FieldInit> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Expression::StructInit(StructInit {
                struct_name: struct_name.clone(),
                field_inits: Vec::new(),
                loc: self.current_location(),
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
            }));
        }

        loop {
            let field_name = self.parse_identifier()?.name;
            self.next_token(); // consume identifier
            self.expect_current(TokenKind::Colon)?;

            let value = self.parse_expression(Precedence::Lowest)?.0;
            self.next_token();

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
                        source_content: Box::new(self.lexer.input.clone()),
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
                        source_content: Box::new(self.lexer.input.clone()),
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
            span: Span {
                start,
                end: self.current_token.span.end,
            },
        }));
    }

    pub fn parse_assignment(&mut self, assign_to: Expression, start: usize) -> Result<Expression, ParseError> {
        self.expect_current(TokenKind::Assign)?;
        let expr = self.parse_expression(Precedence::Lowest)?.0;
        let end = self.current_token.span.end;
        Ok(Expression::Assignment(Box::new(Assignment {
            assign_to,
            expr,
            span: Span { start, end },
            loc: self.current_location(),
        })))
    }

    pub fn parse_array_index(&mut self, expr: Expression) -> Result<ArrayIndex, ParseError> {
        let start = self.current_token.span.start;

        let mut dimensions: Vec<Expression> = Vec::new();

        while self.current_token_is(TokenKind::LeftBracket) {
            let expr = self.parse_single_array_index()?;
            dimensions.push(expr);
            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token();
            }
        }

        let end = self.current_token.span.end;

        Ok(ArrayIndex {
            dimensions,
            span: Span { start, end },
            expr: Box::new(expr),
            loc: self.current_location(),
        })
    }

    pub fn parse_array(&mut self, data_type: TypeSpecifier) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;

        if !self.current_token_is(TokenKind::LeftBrace) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingOpeningBrace,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: true,
            });
        }

        let mut elements: Vec<Expression> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Expression::Array(Array {
                elements,
                data_type,
                span: Span::new(start, self.current_token.span.end),
                loc: self.current_location(),
            }));
        }

        loop {
            if self.current_token_is(TokenKind::LeftBrace) {
                let untyped_array_start = self.current_token.span.start;
                let mut untyped_array: Vec<Expression> = Vec::new();
                self.next_token(); // consume left brace

                loop {
                    if self.current_token_is(TokenKind::RightBrace) {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::InvalidUntypedArrayConstructor,
                            file_name: Some(self.lexer.file_name.clone()),
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: None,
                            caret: true,
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
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::InvalidToken(self.peek_token.kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: None,
                            caret: true,
                        });
                    }
                }

                if let TypeSpecifier::Array(inner_type_specifier, ..) = data_type.clone() {
                    elements.push(Expression::Array(Array {
                        data_type: TypeSpecifier::Array(ArrayTypeSpecifier {
                            size: ArrayCapacity::Fixed(TokenKind::Literal(Literal::Integer(
                                untyped_array.len().try_into().unwrap(),
                            ))),
                            element_type: inner_type_specifier.element_type,
                        }),
                        elements: untyped_array,
                        span: Span::new(untyped_array_start, self.current_token.span.end),
                        loc: self.current_location(),
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
            loc: self.current_location(),
        }))
    }
}
