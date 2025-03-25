use crate::ParseError;
use crate::Parser;
use crate::prec::Precedence;
use ast::ast::*;
use ast::token::*;
use utils::compile_time_errors::errors::CompileTimeError;
use utils::compile_time_errors::parser_errors::ParserErrorType;

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start.clone();

        match self.current_token.kind {
            TokenKind::If => self.parse_if(),
            TokenKind::Function | TokenKind::Decl | TokenKind::Extern | TokenKind::Pub | TokenKind::Inline => {
                if self.current_token_is(TokenKind::Function) || self.peek_token_is(TokenKind::Function) {
                    self.parse_func()
                } else if self.current_token_is(TokenKind::Struct) || self.peek_token_is(TokenKind::Struct) {
                    self.parse_struct()
                } else {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: Some(String::from("Expected 'struct' or 'fn' after visibility token!")),
                        caret: false,
                    });
                }
            }
            TokenKind::Return => self.parse_return(),
            TokenKind::Hashtag => self.parse_variable(),
            TokenKind::For => self.parse_for_loop(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::LeftBrace => Ok(Statement::BlockStatement(self.parse_block_statement()?)),
            TokenKind::Import => self.parse_import(),
            TokenKind::Struct => self.parse_struct(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression(Precedence::Lowest, None)?.0;

        if self.peek_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(expr))
    }

    pub fn parse_struct(&mut self) -> Result<Statement, ParseError> {
        let loc = self.current_location();
        let start = self.current_token.span.start.clone();

        let vis_type = self
            .parse_vis_type(self.current_token.clone())
            .unwrap_or(VisType::Internal);
        if vis_type == VisType::Inline {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::ExpectedIdentifier,
                file_name: Some(self.lexer.file_name.clone()),
                code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                verbose: Some(String::from("Token 'inline' is not a valid vistype for structs.")),
                caret: true,
            });
        }

        self.next_token();

        let struct_name = match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => name,
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
        self.next_token(); // consume struct name

        let mut inherits: Vec<Identifier> = Vec::new();

        if self.current_token_is(TokenKind::Extends) {
            self.next_token();

            loop {
                match self.current_token.kind.clone() {
                    TokenKind::LeftBrace => {
                        self.next_token();
                        break;
                    }
                    TokenKind::EOF => {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::MissingOpeningBrace,
                            file_name: Some(self.lexer.file_name.clone()),
                            code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                            verbose: None,
                            caret: true,
                        });
                    }
                    TokenKind::Identifier { name: inherit_struct } => {
                        self.next_token();
                        inherits.push(Identifier {
                            name: inherit_struct,
                            span: self.current_token.span.clone(),
                            loc: self.current_location(),
                        });
                    }
                    TokenKind::Comma => {
                        self.next_token();
                        continue;
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
        } else {
            self.expect_current(TokenKind::LeftBrace)?;
        }

        let mut fields: Vec<Field> = Vec::new();
        let mut methods: Vec<FuncDef> = Vec::new();

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
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
                    });
                }
                TokenKind::Function | TokenKind::Decl | TokenKind::Extern | TokenKind::Pub | TokenKind::Inline => {
                    if let Statement::FuncDef(method) = self.parse_func()? {
                        methods.push(method);
                        self.next_token(); // consume the right brace
                    } else {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                            verbose: Some(format!(
                                "Expected method definition inside struct '{}'",
                                struct_name.clone()
                            )),
                            caret: true,
                        });
                    }
                }
                TokenKind::Identifier { name: field_name } => {
                    self.next_token(); // consume identifier
                    self.expect_current(TokenKind::Colon)?;
                    let type_token = self.parse_type_token()?;
                    self.expect_current(TokenKind::Semicolon)?;

                    let field = Field {
                        name: field_name,
                        ty: type_token,
                        loc: self.current_location(),
                    };

                    fields.push(field);
                }
                _ => {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: Some(String::from("Invalid token inside a struct definition.")),
                        caret: true,
                    });
                }
            }
        }

        Ok(Statement::Struct(Struct {
            vis_type,
            name: struct_name,
            inherits,
            fields,
            methods,
            loc,
        }))
    }

    pub fn parse_break(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        self.next_token();
        if !self.current_token_is(TokenKind::Semicolon) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingSemicolon,
                file_name: Some(self.lexer.file_name.clone()),
                code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                verbose: None,
                caret: true,
            });
        } else {
            Ok(Statement::Break(self.current_location()))
        }
    }

    pub fn parse_continue(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        self.next_token();
        if !self.current_token_is(TokenKind::Semicolon) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingSemicolon,
                file_name: Some(self.lexer.file_name.clone()),
                code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                verbose: None,
                caret: true,
            });
        } else {
            Ok(Statement::Continue(self.current_location()))
        }
    }

    pub fn parse_module_import(&mut self) -> Result<ModuleImport, ParseError> {
        let start = self.current_token.span.start;
        let mut sub_modules: Vec<ModulePath> = match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => vec![ModulePath::SubModule(Identifier {
                name,
                span: Span {
                    start,
                    end: self.current_token.span.end - 1,
                },
                loc: self.current_location(),
            })],
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

        while self.peek_token_is(TokenKind::Dot) {
            let start = self.current_token.span.end + 1;
            self.next_token();

            match self.peek_token.kind.clone() {
                TokenKind::Identifier { name } => {
                    sub_modules.push(ModulePath::SubModule(Identifier {
                        name,
                        span: Span {
                            start,
                            end: self.peek_token.span.end - 1,
                        },
                        loc: self.current_location(),
                    }));
                }
                _ => {
                    break;
                }
            }

            self.next_token();
        }

        if sub_modules.len() == 1 {
            if let ModulePath::SubModule(identifier) = sub_modules[0].clone() {
                Ok(ModuleImport {
                    identifier,
                    sub_modules: vec![],
                    span: Span {
                        start,
                        end: self.current_token.span.end,
                    },
                    loc: self.current_location(),
                })
            } else {
                unreachable!();
            }
        } else {
            if let ModulePath::SubModule(identifier) = sub_modules.last().unwrap().clone() {
                sub_modules.pop();
                Ok(ModuleImport {
                    identifier,
                    sub_modules,
                    span: Span {
                        start,
                        end: self.current_token.span.end,
                    },
                    loc: self.current_location(),
                })
            } else {
                unreachable!();
            }
        }
    }

    pub fn parse_module_paths(&mut self, parse_start: usize) -> Result<Vec<ModulePath>, ParseError> {
        let mut module_paths: Vec<ModulePath> = vec![];

        while !self.current_token_is(TokenKind::Semicolon) {
            match self.current_token.kind.clone() {
                TokenKind::Identifier { name: identifier } => {
                    let span = self.current_token.span.clone();
                    module_paths.push(ModulePath::SubModule(Identifier {
                        name: identifier,
                        span: span.clone(),
                        loc: self.current_location(),
                    }));
                    self.next_token(); // consume identifier

                    if self.current_token_is(TokenKind::Dot) {
                        continue;
                    } else if self.current_token_is(TokenKind::Semicolon) {
                        return Ok(module_paths);
                    } else {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            code_raw: Some(self.lexer.select(parse_start..self.current_token.span.end)),
                            verbose: None,
                            caret: true,
                        });
                    }
                }
                TokenKind::Asterisk => {
                    module_paths.push(ModulePath::Wildcard);
                    self.next_token();
                }
                TokenKind::Dot => {
                    self.next_token();
                    continue;
                }
                _ => {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::ExpectedIdentifier,
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(parse_start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
                    });
                }
            }
        }

        Ok(module_paths)
    }

    pub fn parse_import(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        if self.current_token_is(TokenKind::Import) {
            self.next_token(); // consume import keyword

            let module_paths = self.parse_module_paths(start)?;

            return Ok(Statement::Import(Import {
                module_paths,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc: self.current_location(),
            }));
        } else {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
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

    pub fn parse_func_params(&mut self, func_def_start: usize) -> Result<FuncParams, ParseError> {
        let params_start = self.current_token.span.start;

        self.expect_current(TokenKind::LeftParen)?;

        let mut variadic: Option<TokenKind> = None;
        let mut list: Vec<FuncParam> = Vec::new();

        while self.current_token.kind != TokenKind::RightParen {
            match self.current_token.kind.clone() {
                TokenKind::TripleDot => {
                    self.next_token(); // consume triple_dot

                    let variadic_data_type = self.parse_type_token()?;
                    variadic = Some(variadic_data_type);

                    if self.current_token_is(TokenKind::Comma) {
                        return Err(CompileTimeError {
                            location: self.current_location(),
                            etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            code_raw: Some(self.lexer.select(func_def_start..self.current_token.span.end + 1)),
                            verbose: Some(String::from("Define all parameters before the variadic argument.")),
                            caret: false,
                        });
                    } else {
                        break;
                    }
                }
                TokenKind::Identifier { name } => {
                    self.next_token(); // consume the identifier

                    let start = self.current_token.span.start;

                    // get the var type

                    let mut var_type: Option<TokenKind> = None;
                    if self.current_token_is(TokenKind::Colon) {
                        self.next_token(); // consume the colon

                        var_type = Some(self.parse_type_token()?);
                    }

                    let mut default_value: Option<Expression> = None;

                    if self.current_token_is(TokenKind::Assign) {
                        self.next_token(); // consume the assign

                        default_value = Some(self.parse_expression(Precedence::Lowest, None)?.0);

                        self.next_token(); // consume the expression
                    }

                    list.push(FuncParam {
                        identifier: Identifier {
                            name: name,
                            span: self.current_token.span.clone(),
                            loc: self.current_location(),
                        },
                        ty: var_type,
                        default_value: default_value,
                        span: Span {
                            start: start,
                            end: self.current_token.span.end,
                        },
                        loc: self.current_location(),
                    });

                    // after reading
                    match &self.current_token.kind {
                        TokenKind::Comma => {
                            self.next_token();
                        }
                        TokenKind::RightParen => {
                            break;
                        }
                        _ => {
                            return Err(CompileTimeError {
                                location: self.current_location(),
                                etype: ParserErrorType::MissingComma,
                                file_name: Some(self.lexer.file_name.clone()),
                                code_raw: Some(self.lexer.select(params_start..self.current_token.span.end)),
                                verbose: None,
                                caret: true,
                            });
                        }
                    }
                }
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

        self.expect_current(TokenKind::RightParen)?;

        Ok(FuncParams { list, variadic })
    }

    pub fn parse_for_loop_body(&mut self, expr_start: usize) -> Result<Box<BlockStatement>, ParseError> {
        let body: Box<BlockStatement>;
        if self.current_token_is(TokenKind::LeftBrace) {
            body = Box::new(self.parse_block_statement()?);

            if !self.current_token_is(TokenKind::RightBrace) {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::MissingClosingBrace,
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(self.lexer.select(expr_start..self.current_token.span.end)),
                    verbose: None,
                    caret: true,
                });
            }

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            }
        } else {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingOpeningBrace,
                file_name: Some(self.lexer.file_name.clone()),
                code_raw: Some(
                    self.lexer
                        .select(self.current_token.span.start..self.current_token.span.end),
                ),
                verbose: None,
                caret: true,
            });
        }
        Ok(body)
    }

    pub fn parse_for_loop(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        self.next_token(); // consume for token

        // Check for non-conditional for loop
        if self.current_token_is(TokenKind::LeftBrace) {
            let body: Box<BlockStatement>;
            if self.current_token_is(TokenKind::LeftBrace) {
                body = Box::new(self.parse_block_statement()?);

                if !self.current_token_is(TokenKind::RightBrace) {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::MissingClosingBrace,
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
                    });
                }

                if self.peek_token_is(TokenKind::Semicolon) {
                    self.next_token();
                }
            } else {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::MissingOpeningBrace,
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                    verbose: None,
                    caret: true,
                });
            }

            return Ok(Statement::For(For {
                initializer: None,
                condition: None,
                increment: None,
                body,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc: self.current_location(),
            }));
        }

        let mut initializer: Option<Variable> = None;
        if !self.current_token_is(TokenKind::Semicolon) {
            if let Statement::Variable(var) = self.parse_variable()? {
                initializer = Some(var);
            }
        }
        self.expect_current(TokenKind::Semicolon)?;

        // for loop with only initializer expression
        if self.current_token_is(TokenKind::LeftBrace) {
            let body = self.parse_for_loop_body(start)?;
            return Ok(Statement::For(For {
                initializer,
                condition: None,
                increment: None,
                body,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc: self.current_location(),
            }));
        }

        let condition = self.parse_expression(Precedence::Lowest, None)?.0;
        if !self.current_token_is(TokenKind::LeftBrace) {
            self.next_token();
            self.expect_current(TokenKind::Semicolon)?;
        }

        let mut increment: Option<Expression> = None;
        if !self.current_token_is(TokenKind::LeftBrace) {
            increment = Some(self.parse_expression(Precedence::Lowest, None)?.0);
            self.next_token(); // consume increment token
        }
        
        let body = self.parse_for_loop_body(start)?;

        Ok(Statement::For(For {
            initializer,
            condition: Some(condition),
            increment,
            body,
            span: Span {
                start,
                end: self.current_token.span.end,
            },
            loc: self.current_location(),
        }))
    }

    pub fn parse_variable(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token(); // consume sharp token

        let identifier = self.current_token.clone(); // export the name of the identifier
        self.next_token(); // consume identifier

        let name = match identifier.kind {
            TokenKind::Identifier { name } => name,
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

        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Statement::Variable(Variable {
                name,
                ty: None,
                expr: None,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc: self.current_location(),
            }));
        }

        let mut variable_type: Option<TokenKind> = None;
        if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume the colon

            variable_type = Some(self.parse_type_token()?);
        }

        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Statement::Variable(Variable {
                name,
                ty: variable_type,
                expr: None,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc: self.current_location(),
            }));
        }
        self.expect_current(TokenKind::Assign)?;

        let (expr, span) = self.parse_expression(Precedence::Lowest, None)?;

        if self.peek_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Variable(Variable {
            name,
            expr: Some(expr),
            span: Span { start, end: span.end },
            ty: variable_type,
            loc: self.current_location(),
        }))
    }

    pub fn parse_func(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        let mut vis_type = VisType::Internal; // internal by default

        if !self.current_token_is(TokenKind::Function) {
            // Parse visibility type
            vis_type = self.parse_vis_type(self.current_token.clone())?;
        }

        self.next_token(); // consume the fn token

        let func_name = match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => name,
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
        }; // export the name of the function
        self.next_token(); // consume the name of the identifier

        let params = self.parse_func_params(start)?;

        let mut return_type: Option<Token> = None;

        // parse return type
        if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume colon

            if self.current_token_is(TokenKind::LeftBrace) {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                    verbose: Some(String::from("Return type required before closing brace '{'")),
                    caret: true,
                });
            }

            let return_type_start = self.current_token.span.start.clone();
            return_type = Some(Token {
                kind: self.parse_type_token()?,
                span: Span {
                    start: return_type_start,
                    end: self.current_token.span.end,
                },
            });
        }

        // parse as func decl
        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Statement::FuncDecl(FuncDecl {
                name: func_name,
                params,
                return_type,
                vis_type,
                renamed_as: None,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc: self.current_location(),
            }));
        } else if self.current_token_is(TokenKind::As) {
            self.next_token();

            // parse renamed func decl
            let renamed_as = match self.current_token.kind.clone() {
                TokenKind::Identifier { name } => name,
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
            }; // export the name of the function

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            } else if self.peek_token_is(TokenKind::LeftBrace) {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::InvalidToken(self.peek_token.kind.clone()),
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                    verbose: Some(String::from(
                        "FuncDecl does not accept a body. Use a semicolon `;` instead of a body `{ ... }`.",
                    )),
                    caret: true,
                });
            }

            return Ok(Statement::FuncDecl(FuncDecl {
                name: func_name,
                params,
                return_type,
                vis_type,
                renamed_as: Some(renamed_as),
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc: self.current_location(),
            }));
        }

        // we used current_token_is because we don't want to consume it,
        // we pass this statement that is inside a brace to parse_block_statement.
        if self.current_token_is(TokenKind::LeftBrace) {
            let body = Box::new(self.parse_block_statement()?);

            if !(self.current_token_is(TokenKind::RightBrace) || self.current_token_is(TokenKind::EOF)) {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::MissingClosingBrace,
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                    verbose: None,
                    caret: true,
                });
            }

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            }

            let end = self.current_token.span.end;

            return Ok(Statement::FuncDef(FuncDef {
                name: func_name,
                params,
                body,
                return_type,
                vis_type,
                span: Span { start, end },
                loc: self.current_location(),
            }));
        }

        return Err(CompileTimeError {
            location: self.current_location(),
            etype: ParserErrorType::MissingClosingBrace,
            file_name: Some(self.lexer.file_name.clone()),
            code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
            verbose: None,
            caret: true,
        });
    }

    pub fn parse_return(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token(); // consume return token

        let argument = self.parse_expression(Precedence::Lowest, None)?.0;

        self.expect_current(TokenKind::Semicolon)?;

        let end = self.current_token.span.end;

        Ok(Statement::Return(Return {
            argument,
            span: Span { start, end },
            loc: self.current_location(),
        }))
    }

    pub fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let start = self.current_token.span.start;

        if self.peek_token_is(TokenKind::LeftBrace) {
            self.next_token(); // consume current
        } else if !self.current_token_is(TokenKind::LeftBrace) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingOpeningBrace,
                file_name: Some(self.lexer.file_name.clone()),
                code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                verbose: None,
                caret: true,
            });
        }
        self.next_token(); // consume left brace

        let mut block_statement: Vec<Statement> = Vec::new();

        while !self.current_token_is(TokenKind::RightBrace) && !self.current_token_is(TokenKind::EOF) {
            let statement = self.parse_statement()?;
            block_statement.push(statement);
            self.next_token();
        }

        let end = self.current_token.span.end;

        Ok(BlockStatement {
            exprs: block_statement,
            span: Span { start, end },
            loc: self.current_location(),
        })
    }

    pub fn parse_if(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let mut branches: Vec<If> = Vec::new();
        let mut alternate: Option<Box<BlockStatement>> = None;

        self.expect_current(TokenKind::If)?;

        let condition = self
            .parse_expression(Precedence::Lowest, Some(TokenKind::LeftBrace))?
            .0;
        let consequent = Box::new(self.parse_block_statement()?);

        if !(self.current_token_is(TokenKind::RightBrace) || self.current_token_is(TokenKind::EOF)) {
            return Err(CompileTimeError {
                location: self.current_location(),
                etype: ParserErrorType::MissingClosingBrace,
                file_name: Some(self.lexer.file_name.clone()),
                code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                verbose: None,
                caret: true,
            });
        }
        self.next_token(); // consume right brace

        while self.current_token_is(TokenKind::Else) {
            self.next_token(); // consume else token

            if self.current_token_is(TokenKind::If) {
                let start = self.current_token.span.start;
                self.next_token(); // consume if token

                let (condition, _) = self.parse_expression(Precedence::Lowest, None)?;

                let consequent = Box::new(self.parse_block_statement()?);
                self.expect_current(TokenKind::RightBrace)?;

                let end = self.current_token.span.end;

                branches.push(If {
                    condition,
                    consequent,
                    branches: vec![],
                    alternate: None,
                    span: Span { start, end },
                    loc: self.current_location(),
                });
            } else {
                // parse alternate
                alternate = Some(Box::new(self.parse_block_statement()?));

                if !(self.current_token_is(TokenKind::RightBrace) || self.current_token_is(TokenKind::EOF)) {
                    return Err(CompileTimeError {
                        location: self.current_location(),
                        etype: ParserErrorType::MissingClosingBrace,
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                        verbose: None,
                        caret: true,
                    });
                }
            }
        }

        let end = self.current_token.span.end;

        Ok(Statement::If(If {
            condition,
            consequent,
            branches,
            alternate,
            span: Span { start, end },
            loc: self.current_location(),
        }))
    }
}
