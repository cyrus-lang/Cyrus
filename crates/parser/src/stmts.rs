use crate::ParseError;
use crate::Parser;
use crate::diag::ParserErrorType;
use crate::prec::Precedence;
use ast::ast::*;
use ast::token::*;
use diag::errors::CompileTimeError;

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if self.current_token_is(TokenKind::Extern)
            || self.current_token_is(TokenKind::Inline)
            || self.current_token_is(TokenKind::Public)
        {
            let storage_class = self.parse_storage_class(self.current_token.clone())?;

            if self.current_token_is(TokenKind::Function) {
                return self.parse_func(Some(storage_class));
            } else if self.current_token_is(TokenKind::Struct) {
                return self.parse_struct(Some(storage_class), false);
            } else if self.current_token_is(TokenKind::Bits) {
                return self.parse_struct(Some(storage_class), true);
            } else if self.current_token_is(TokenKind::Enum) {
                return self.parse_enum(Some(storage_class));
            }
        } else if self.current_token_is(TokenKind::Function) {
            return self.parse_func(None);
        } else if self.current_token_is(TokenKind::Struct) {
            return self.parse_struct(None, false);
        } else if self.current_token_is(TokenKind::Bits) {
            return self.parse_struct(None, true);
        } else if self.current_token_is(TokenKind::Enum) {
            return self.parse_enum(None);
        }

        match self.current_token.kind {
            TokenKind::If => self.parse_if(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Hashtag => self.parse_variable(),
            TokenKind::For => self.parse_for_loop(),
            TokenKind::Foreach => self.parse_foreach(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Import => self.parse_import(),
            TokenKind::LeftBrace => {
                let block_statement = self.parse_block_statement()?;
                Ok(Statement::BlockStatement(block_statement))
            }
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::Semicolon)?;
        Ok(Statement::Expression(expr))
    }

    pub fn parse_enum_field(&mut self) -> Result<EnumField, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let variant_name = self.parse_identifier()?;
        self.next_token();

        let mut variant_fields: Vec<EnumValuedField> = Vec::new();

        if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
            return Ok(EnumField::OnlyIdentifier(variant_name));
        } else if self.current_token_is(TokenKind::Assign) {
            self.next_token(); // consume assign 
            let value = self.parse_expression(Precedence::Lowest)?.0;
            self.next_token(); // consume last token of the expression 
            return Ok(EnumField::Valued(variant_name, Box::new(value)));
        } else if self.current_token_is(TokenKind::LeftParen) {
            self.next_token(); // consume left paren

            loop {
                if self.current_token_is(TokenKind::RightParen) {
                    return Err(CompileTimeError {
                        location: loc,
                        etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: Some(String::from(
                            "Consider to add a field to enum variant or remove the parenthesis.",
                        )),
                        caret: Some(Span::new(start, self.current_token.span.end)),
                    });
                }

                let field_name = self.parse_identifier()?;
                self.next_token(); // consume field name

                let field_type = self.parse_type_specifier()?;
                self.next_token();

                variant_fields.push(EnumValuedField {
                    name: field_name,
                    field_type: field_type,
                });

                if self.current_token_is(TokenKind::RightParen) {
                    self.next_token();
                    break;
                } else {
                    self.expect_current(TokenKind::Comma)?;
                }
            }
        }

        return Ok(EnumField::Variant(variant_name, variant_fields));
    }

    pub fn parse_enum(&mut self, storage_class: Option<StorageClass>) -> Result<Statement, ParseError> {
        let loc = self.current_token.loc.clone();
        let storage_class = storage_class.unwrap_or(StorageClass::Inline);

        self.next_token(); // parse enum keyword

        let enum_name = self.parse_identifier()?;
        self.next_token(); // consume enum name

        self.expect_current(TokenKind::LeftBrace)?;

        let mut enum_fields: Vec<EnumField> = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            return Ok(Statement::Enum(Enum {
                name: enum_name,
                variants: enum_fields,
                storage_class,
                loc,
            }));
        }

        enum_fields.push(self.parse_enum_field()?);

        while self.current_token_is(TokenKind::Comma) {
            self.next_token(); // consume comma

            if self.current_token_is(TokenKind::RightBrace) {
                break;
            }

            enum_fields.push(self.parse_enum_field()?);
            if !self.current_token_is(TokenKind::RightBrace) {
                self.expect_current(TokenKind::Comma)?;
            }
        }

        // consume optional comma at the end of the variant
        if self.current_token_is(TokenKind::Comma) {
            self.next_token();
        }

        self.expect_current(TokenKind::RightBrace)?;

        Ok(Statement::Enum(Enum {
            name: enum_name,
            variants: enum_fields,
            storage_class,
            loc,
        }))
    }

    pub fn parse_struct(&mut self, storage_class: Option<StorageClass>, packed: bool) -> Result<Statement, ParseError> {
        let loc = self.current_token.loc.clone();
        let struct_start = self.current_token.span.start.clone();

        let storage_class = storage_class.unwrap_or(StorageClass::Internal);
        self.next_token(); // consume struct token

        let struct_name = self.parse_identifier()?.name;
        self.next_token(); // consume struct name

        let mut inherits: Vec<Identifier> = Vec::new();

        if self.current_token_is(TokenKind::Colon) {
            self.next_token();

            loop {
                match self.current_token.kind.clone() {
                    TokenKind::LeftBrace => {
                        self.next_token();
                        break;
                    }
                    TokenKind::EOF => {
                        return Err(CompileTimeError {
                            location: loc,
                            etype: ParserErrorType::MissingOpeningBrace,
                            file_name: Some(self.lexer.file_name.clone()),
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: None,
                            caret: Some(Span::new(struct_start, self.current_token.span.end)),
                        });
                    }
                    TokenKind::Identifier { name: inherit_struct } => {
                        self.next_token();
                        inherits.push(Identifier {
                            name: inherit_struct,
                            span: self.current_token.span.clone(),
                            loc: loc.clone(),
                        });
                    }
                    TokenKind::Comma => {
                        self.next_token();
                        continue;
                    }
                    _ => {
                        return Err(CompileTimeError {
                            location: loc,
                            etype: ParserErrorType::ExpectedIdentifier,
                            file_name: Some(self.lexer.file_name.clone()),
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: None,
                            caret: Some(Span::new(struct_start, self.current_token.span.end)),
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
                        location: self.current_token.loc.clone(),
                        etype: ParserErrorType::MissingClosingBrace,
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: None,
                        caret: Some(Span::new(struct_start, self.current_token.span.end)),
                    });
                }
                TokenKind::Extern | TokenKind::Public | TokenKind::Inline => {
                    let storage_class = self.parse_storage_class(self.current_token.clone())?;
                    if let Statement::FuncDef(method) = self.parse_func(Some(storage_class))? {
                        self.next_token(); // consume right brace
                        methods.push(method);
                    } else {
                        unreachable!();
                    }
                }
                TokenKind::Function => {
                    if let Statement::FuncDef(method) = self.parse_func(None)? {
                        self.next_token(); // consume right brace
                        methods.push(method);
                    } else {
                        unreachable!();
                    }
                }
                TokenKind::Identifier { name: field_name } => {
                    let start = self.current_token.span.start;
                    let loc = self.current_token.loc.clone();

                    self.next_token(); // consume identifier

                    self.expect_current(TokenKind::Colon)?;

                    let type_token = self.parse_type_specifier()?;
                    self.next_token();

                    let field = Field {
                        name: field_name,
                        ty: type_token,
                        loc,
                        span: Span {
                            start,
                            end: self.current_token.span.end,
                        },
                    };

                    self.expect_current(TokenKind::Semicolon)?;
                    fields.push(field);
                }
                _ => {
                    return Err(CompileTimeError {
                        location: self.current_token.loc.clone(),
                        etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: Some(String::from("Invalid token inside a struct definition.")),
                        caret: Some(Span::new(struct_start, self.current_token.span.end)),
                    });
                }
            }
        }

        Ok(Statement::Struct(Struct {
            storage_class,
            name: struct_name,
            inherits,
            fields,
            methods,
            packed,
            loc,
            span: Span {
                start: struct_start,
                end: self.current_token.span.end,
            },
        }))
    }

    pub fn parse_break(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        self.next_token();
        if !self.current_token_is(TokenKind::Semicolon) {
            return Err(CompileTimeError {
                location: loc,
                etype: ParserErrorType::MissingSemicolon,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: Some(Span::new(start, self.current_token.span.end)),
            });
        } else {
            Ok(Statement::Break(Break {
                loc,
                span: Span::new(start, self.current_token.span.end),
            }))
        }
    }

    pub fn parse_continue(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        self.next_token();
        if !self.current_token_is(TokenKind::Semicolon) {
            return Err(CompileTimeError {
                location: loc,
                etype: ParserErrorType::MissingSemicolon,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: Some(Span::new(start, self.current_token.span.end)),
            });
        } else {
            Ok(Statement::Continue(Continue {
                loc,
                span: Span::new(start, self.current_token.span.end),
            }))
        }
    }

    pub fn parse_import_module_path(&mut self, module_path: ModulePath) -> Result<ModulePath, ParseError> {
        if self.current_token_is(TokenKind::LeftBrace) {
            self.next_token();

            let mut singles: Vec<ModuleSegmentSingle> = Vec::new();

            while !self.current_token_is(TokenKind::RightBrace) {
                // enable aliasing features for a single
                let first_identifier = self.parse_identifier()?;
                self.next_token();

                if self.current_token_is(TokenKind::Colon) {
                    self.next_token(); // consume colon

                    let second_identifier = self.parse_identifier()?;
                    self.next_token();

                    let renamed_single = ModuleSegmentSingle {
                        identifier: second_identifier,
                        renamed: Some(first_identifier),
                    };
                    singles.push(renamed_single);
                } else {
                    let single = ModuleSegmentSingle {
                        identifier: first_identifier,
                        renamed: None,
                    };
                    singles.push(single);
                }

                if !self.current_token_is(TokenKind::RightBrace) {
                    self.expect_current(TokenKind::Comma)?;
                }
            }

            self.expect_current(TokenKind::RightBrace)?;

            let mut updated_module_path = module_path.clone();
            updated_module_path.segments.push(ModuleSegment::Single(singles));
            Ok(updated_module_path)
        } else {
            Ok(module_path)
        }
    }

    pub fn parse_import(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        self.next_token(); // consume import keyword

        let mut paths: Vec<ModulePath> = Vec::new();

        if self.current_token_is(TokenKind::LeftParen) {
            self.expect_current(TokenKind::LeftParen)?;

            loop {
                let mut module_path = self.parse_module_path()?;
                module_path = self.parse_import_module_path(module_path.clone())?;
                paths.push(module_path);

                match self.current_token.kind {
                    TokenKind::RightParen => {
                        break;
                    }
                    TokenKind::Comma => {
                        self.next_token();

                        if self.current_token_is(TokenKind::RightParen) {
                            break;
                        } else {
                            continue;
                        }
                    }
                    _ => {
                        return Err(CompileTimeError {
                            location: self.current_token.loc.clone(),
                            etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: None,
                            caret: Some(Span::new(start, self.current_token.span.end)),
                        });
                    }
                }
            }

            self.expect_current(TokenKind::RightParen)?;
        } else {
            let mut module_path = self.parse_module_path()?;
            module_path = self.parse_import_module_path(module_path.clone())?;
            paths = vec![module_path];
        }

        return Ok(Statement::Import(Import {
            paths,
            span: Span {
                start,
                end: self.current_token.span.end,
            },
            loc,
        }));
    }

    pub fn parse_func_params(&mut self) -> Result<FuncParams, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        self.expect_current(TokenKind::LeftParen)?;

        let mut variadic: Option<FuncVariadicParams> = None;
        let mut list: Vec<FuncParam> = Vec::new();

        while self.current_token.kind != TokenKind::RightParen {
            match self.current_token.kind.clone() {
                TokenKind::TripleDot => {
                    self.next_token(); // consume triple_dot

                    if self.current_token_is(TokenKind::Comma) {
                        return Err(CompileTimeError {
                            location: self.current_token.loc.clone(),
                            etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                            file_name: Some(self.lexer.file_name.clone()),
                            source_content: Box::new(self.lexer.input.clone()),
                            verbose: Some(String::from("Fixed parameters must be defined before the vargs.")),
                            caret: Some(Span::new(start, self.current_token.span.end)),
                        });
                    }

                    variadic = Some(FuncVariadicParams::UntypedCStyle);
                    break;
                }
                TokenKind::Identifier { name } => {
                    let param_loc = self.current_token.loc.clone();
                    let identifier = self.parse_identifier()?;
                    self.next_token(); // consume the identifier

                    let start = self.current_token.span.start;

                    // get the var type

                    let mut var_type: Option<TypeSpecifier> = None;
                    if self.current_token_is(TokenKind::Colon) {
                        self.next_token(); // consume the colon

                        if self.current_token_is(TokenKind::TripleDot) {
                            self.next_token(); // consume triple dot

                            let variadic_data_type = self.parse_type_specifier()?;
                            self.next_token();

                            variadic = Some(FuncVariadicParams::Typed(identifier, variadic_data_type));
                            continue;
                        } else {
                            var_type = Some(self.parse_type_specifier()?);
                            self.next_token();
                        }
                    }

                    let mut default_value: Option<Expression> = None;

                    if self.current_token_is(TokenKind::Assign) {
                        self.next_token(); // consume the assign

                        default_value = Some(self.parse_expression(Precedence::Lowest)?.0);

                        self.next_token(); // consume the expression
                    }

                    list.push(FuncParam {
                        identifier: Identifier {
                            name: name,
                            span: self.current_token.span.clone(),
                            loc: param_loc.clone(),
                        },
                        ty: var_type,
                        default_value: default_value,
                        span: Span {
                            start: start,
                            end: self.current_token.span.end,
                        },
                        loc: param_loc,
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
                                location: loc,
                                etype: ParserErrorType::MissingComma,
                                file_name: Some(self.lexer.file_name.clone()),
                                source_content: Box::new(self.lexer.input.clone()),
                                verbose: None,
                                caret: Some(Span::new(start, self.current_token.span.end)),
                            });
                        }
                    }
                }
                _ => {
                    return Err(CompileTimeError {
                        location: loc,
                        etype: ParserErrorType::ExpectedIdentifier,
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: None,
                        caret: Some(Span::new(start, self.current_token.span.end)),
                    });
                }
            }
        }

        self.expect_current(TokenKind::RightParen)?;

        Ok(FuncParams { list, variadic })
    }

    pub fn parse_for_loop_body(&mut self) -> Result<Box<BlockStatement>, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let body: Box<BlockStatement>;
        if self.current_token_is(TokenKind::LeftBrace) {
            body = Box::new(self.parse_block_statement()?);

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            }
        } else {
            return Err(CompileTimeError {
                location: loc,
                etype: ParserErrorType::MissingOpeningBrace,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: Some(Span::new(start, self.current_token.span.end)),
            });
        }
        Ok(body)
    }

    pub fn parse_foreach(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.end;
        let loc = self.current_token.loc.clone();

        self.next_token(); // consume foreach
        self.expect_current(TokenKind::LeftParen)?;

        let item_identifier = self.parse_identifier()?;
        self.next_token();

        let mut index_identifier: Option<Identifier> = None;

        if self.current_token_is(TokenKind::Comma) {
            self.next_token(); // consume comma
            index_identifier = Some(self.parse_identifier()?);
            self.next_token();
        }

        self.expect_current(TokenKind::In)?;

        let expr = self.parse_expression(Precedence::Lowest)?.0;
        self.next_token();

        self.expect_current(TokenKind::RightParen)?;

        let body = self.parse_block_statement()?;

        Ok(Statement::Foreach(Foreach {
            item: item_identifier,
            index: index_identifier,
            expr,
            body: Box::new(body),
            span: Span::new(start, self.current_token.span.end),
            loc,
        }))
    }

    pub fn parse_for_loop(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        self.next_token(); // consume for token

        // Check for non-conditional for loop
        if self.current_token_is(TokenKind::LeftBrace) {
            let body: Box<BlockStatement>;
            if self.current_token_is(TokenKind::LeftBrace) {
                body = Box::new(self.parse_block_statement()?);

                if self.peek_token_is(TokenKind::Semicolon) {
                    self.next_token();
                }
            } else {
                return Err(CompileTimeError {
                    location: loc,
                    etype: ParserErrorType::MissingOpeningBrace,
                    file_name: Some(self.lexer.file_name.clone()),
                    source_content: Box::new(self.lexer.input.clone()),
                    verbose: None,
                    caret: Some(Span::new(start, self.current_token.span.end)),
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
                loc,
            }));
        }

        self.expect_current(TokenKind::LeftParen)?;

        let mut initializer: Option<Variable> = None;
        if !self.current_token_is(TokenKind::Semicolon) {
            if let Statement::Variable(var) = self.parse_variable()? {
                initializer = Some(var);
            }
        }
        self.expect_current(TokenKind::Semicolon)?;

        // for loop with only initializer expression
        if self.peek_token_is(TokenKind::LeftBrace) {
            self.expect_current(TokenKind::RightParen)?;

            let body = self.parse_for_loop_body()?;
            return Ok(Statement::For(For {
                initializer,
                condition: None,
                increment: None,
                body,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc,
            }));
        }

        let condition = self.parse_expression(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::Semicolon)?;
        self.next_token();

        let mut increment: Option<Expression> = None;
        if !self.current_token_is(TokenKind::RightParen) {
            increment = Some(self.parse_expression(Precedence::Lowest)?.0);
            self.next_token(); // consume increment token
        }

        self.expect_current(TokenKind::RightParen)?;
        let body = self.parse_for_loop_body()?;

        Ok(Statement::For(For {
            initializer,
            condition: Some(condition),
            increment,
            body,
            span: Span {
                start,
                end: self.current_token.span.end,
            },
            loc,
        }))
    }

    pub fn parse_variable(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        self.next_token(); // consume sharp token

        let name = self.parse_identifier()?.name;
        self.next_token();

        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Statement::Variable(Variable {
                name,
                ty: None,
                expr: None,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc,
            }));
        }

        let mut variable_type: Option<TypeSpecifier> = None;
        if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume the colon

            variable_type = Some(self.parse_type_specifier()?);
            self.next_token();
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
                loc,
            }));
        }
        self.expect_current(TokenKind::Assign)?;

        let (expr, span) = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::Semicolon)?;

        Ok(Statement::Variable(Variable {
            name,
            expr: Some(expr),
            span: Span { start, end: span.end },
            ty: variable_type,
            loc,
        }))
    }

    pub fn parse_func(&mut self, storage_class: Option<StorageClass>) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let storage_class = storage_class.unwrap_or(StorageClass::Internal);

        self.next_token(); // consume the fn token

        let func_name = match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => name,
            _ => {
                return Err(CompileTimeError {
                    location: loc,
                    etype: ParserErrorType::ExpectedIdentifier,
                    file_name: Some(self.lexer.file_name.clone()),
                    source_content: Box::new(self.lexer.input.clone()),
                    verbose: None,
                    caret: Some(Span::new(start, self.current_token.span.end)),
                });
            }
        }; // export the name of the function
        self.next_token(); // consume the name of the identifier

        // self.expect_current(token_kind)
        let params = self.parse_func_params()?;

        let mut return_type: Option<TypeSpecifier> = None;

        // parse return type
        if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume colon

            if self.current_token_is(TokenKind::LeftBrace) {
                return Err(CompileTimeError {
                    location: loc,
                    etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                    file_name: Some(self.lexer.file_name.clone()),
                    source_content: Box::new(self.lexer.input.clone()),
                    verbose: Some(String::from("Return type required before closing brace '{'")),
                    caret: Some(Span::new(start, self.current_token.span.end)),
                });
            }

            return_type = Some(self.parse_type_specifier()?);
            self.next_token();
        }

        // parse as func decl
        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Statement::FuncDecl(FuncDecl {
                name: func_name,
                params,
                return_type,
                storage_class,
                renamed_as: None,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc,
            }));
        } else if self.current_token_is(TokenKind::As) {
            self.next_token();

            // parse renamed func decl
            let renamed_as = match self.current_token.kind.clone() {
                TokenKind::Identifier { name } => name,
                _ => {
                    return Err(CompileTimeError {
                        location: loc,
                        etype: ParserErrorType::ExpectedIdentifier,
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: None,
                        caret: Some(Span::new(start, self.current_token.span.end)),
                    });
                }
            }; // export the name of the function

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            } else if self.peek_token_is(TokenKind::LeftBrace) {
                return Err(CompileTimeError {
                    location: loc,
                    etype: ParserErrorType::InvalidToken(self.peek_token.kind.clone()),
                    file_name: Some(self.lexer.file_name.clone()),
                    source_content: Box::new(self.lexer.input.clone()),
                    verbose: Some(String::from(
                        "FuncDecl does not accept a body. Use a semicolon `;` instead of a body `{ ... }`.",
                    )),
                    caret: Some(Span::new(start, self.current_token.span.end)),
                });
            }

            return Ok(Statement::FuncDecl(FuncDecl {
                name: func_name,
                params,
                return_type,
                storage_class,
                renamed_as: Some(renamed_as),
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc,
            }));
        }

        let body = Box::new(self.parse_block_statement()?);
        let end = self.current_token.span.end;

        return Ok(Statement::FuncDef(FuncDef {
            name: func_name,
            params,
            body,
            return_type,
            storage_class,
            span: Span { start, end },
            loc,
        }));
    }

    pub fn parse_return(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        self.next_token(); // consume return token

        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Statement::Return(Return {
                argument: None,
                span: Span::new(start, self.current_token.span.end),
                loc,
            }));
        }

        let argument = self.parse_expression(Precedence::Lowest)?.0;
        self.next_token();

        if !self.current_token_is(TokenKind::Semicolon) {
            return Err(CompileTimeError {
                location: loc,
                etype: ParserErrorType::MissingSemicolon,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: Some(Span::new(start, self.current_token.span.end)),
            });
        }

        let end = self.peek_token.span.end;

        Ok(Statement::Return(Return {
            argument: Some(argument),
            span: Span { start, end },
            loc,
        }))
    }

    pub fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();
        self.expect_current(TokenKind::LeftBrace)?;

        let mut block_statement: Vec<Statement> = Vec::new();

        if self.current_token_is(TokenKind::RightBrace) {
            // detected empty block statement
            return Ok(BlockStatement {
                exprs: block_statement,
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc,
            });
        }

        loop {
            let statement = self.parse_statement()?;

            // Some statements are valid to be inside a block statement, like
            // struct definitions that works both globally and locally.
            // Global statements does not need an semicolon, but local statements
            // must be ended with a semicolon. That's the reason, we add some extra rules here.
            if matches!(statement, Statement::Struct(_) | Statement::Enum(_)) {
                self.expect_peek(TokenKind::Semicolon)?;
            }

            block_statement.push(statement);

            match self.peek_token.kind {
                TokenKind::RightBrace => break,
                _ => {
                    self.next_token();
                }
            }
        }

        self.expect_peek(TokenKind::RightBrace)?;
        let end = self.current_token.span.end;

        Ok(BlockStatement {
            exprs: block_statement,
            span: Span { start, end },
            loc,
        })
    }

    pub fn parse_if(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        let loc = self.current_token.loc.clone();

        let mut branches: Vec<If> = Vec::new();
        let mut alternate: Option<Box<BlockStatement>> = None;

        self.expect_current(TokenKind::If)?;
        self.expect_current(TokenKind::LeftParen)?;

        let condition = self.parse_expression(Precedence::Lowest)?.0;
        self.expect_peek(TokenKind::RightParen)?;
        self.next_token(); // consume right paren

        let consequent = Box::new(self.parse_block_statement()?);

        if self.peek_token_is(TokenKind::Else) {
            self.next_token(); // consume right brace
        }

        while self.current_token_is(TokenKind::Else) {
            self.next_token(); // consume else token

            if self.current_token_is(TokenKind::If) {
                let start = self.current_token.span.start;
                self.next_token(); // consume if token

                self.expect_current(TokenKind::LeftParen)?;
                let (condition, _) = self.parse_expression(Precedence::Lowest)?;
                self.next_token(); // consume last token of the expression
                self.expect_current(TokenKind::RightParen)?;

                let consequent = Box::new(self.parse_block_statement()?);

                if self.peek_token_is(TokenKind::Else) {
                    self.next_token(); // consume else token
                }

                let end = self.current_token.span.end;

                branches.push(If {
                    condition,
                    consequent,
                    branches: vec![],
                    alternate: None,
                    span: Span { start, end },
                    loc: loc.clone(),
                });
            } else {
                // parse alternate
                alternate = Some(Box::new(self.parse_block_statement()?));

                if !(self.current_token_is(TokenKind::RightBrace) || self.current_token_is(TokenKind::EOF)) {
                    return Err(CompileTimeError {
                        location: loc,
                        etype: ParserErrorType::MissingClosingBrace,
                        file_name: Some(self.lexer.file_name.clone()),
                        source_content: Box::new(self.lexer.input.clone()),
                        verbose: None,
                        caret: Some(Span::new(start, self.current_token.span.end)),
                    });
                }
            }
        }

        if !self.current_token_is(TokenKind::RightBrace) {
            return Err(CompileTimeError {
                location: loc,
                etype: ParserErrorType::MissingClosingBrace,
                file_name: Some(self.lexer.file_name.clone()),
                source_content: Box::new(self.lexer.input.clone()),
                verbose: None,
                caret: Some(Span::new(start, self.current_token.span.end)),
            });
        }

        let end = self.current_token.span.end;

        Ok(Statement::If(If {
            condition,
            consequent,
            branches,
            alternate,
            span: Span { start, end },
            loc,
        }))
    }
}
