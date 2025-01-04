use ast::ast::*;
use ast::token::*;
use lexer::*;
use precedences::{token_precedence_of, Precedence};

mod parser_test;
mod precedences;

type ParseError = String;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
        }
    }

    pub fn parse(&mut self) -> Result<Node, Vec<ParseError>> {
        let program = self.parse_program()?;
        Ok(Node::Program(program))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.current_token.kind {
            TokenKind::If => self.parse_if_statement(),
            TokenKind::Function | TokenKind::Decl | TokenKind::Extern | TokenKind::Pub | TokenKind::Inline => {
                self.parse_function_statement()
            }
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Hashtag => self.parse_variable_declaration(),
            TokenKind::For => self.parse_for_statement(),
            TokenKind::Package => self.parse_package_declaration(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_program(&mut self) -> Result<Program, Vec<ParseError>> {
        let mut program = Program::new();

        while self.current_token.kind != TokenKind::EOF {
            match self.parse_statement() {
                Ok(statement) => program.body.push(statement),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(program)
    }

    fn next_token(&mut self) -> Token {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
        self.peek_token.clone()
    }

    fn current_token_is(&self, token_kind: TokenKind) -> bool {
        self.current_token.kind == token_kind
    }

    fn peek_token_is(&self, token_kind: TokenKind) -> bool {
        self.peek_token.kind == token_kind
    }

    fn expect_peek(&mut self, token_kind: TokenKind) -> Result<(), ParseError> {
        if self.peek_token_is(token_kind.clone()) {
            self.next_token(); // consume current token
            return Ok(());
        }

        Err(format!(
            "Expected token: {} but got {}.",
            token_kind, self.peek_token.kind
        ))
    }

    fn expect_current(&mut self, token_kind: TokenKind) -> Result<(), ParseError> {
        if self.current_token_is(token_kind.clone()) {
            self.next_token(); // consume current token
            return Ok(());
        }

        Err(format!(
            "Expected token: {} but got {}.",
            token_kind, self.current_token.kind
        ))
    }

    fn parse_package_declaration(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        if self.current_token_is(TokenKind::Package) {
            self.next_token(); // consume package keyword

            let mut sub_packages: Vec<Identifier> = vec![];

            while !self.current_token_is(TokenKind::Semicolon) {
                match self.current_token.kind.clone() {
                    TokenKind::Identifier { name } => {
                        sub_packages.push(Identifier {
                            name,
                            span: Span {
                                start,
                                end: self.current_token.span.end,
                            },
                        });

                        self.next_token(); // consume identifier
                    }
                    TokenKind::Colon => {
                        self.next_token();
                        continue;
                    }
                    _ => {
                        return Err(format!(
                            "Expected an identifier as package name but got {}.",
                            self.current_token.kind
                        ));
                    }
                }
            }

            let span = Span {
                start,
                end: self.current_token.span.end,
            };

            return Ok(Statement::Package(Package { sub_packages, span }));
        } else {
            Err(format!(
                "Invalid token '{}' found in package declaration.",
                self.current_token.kind
            ))
        }
    }

    fn parse_function_params(&mut self) -> Result<FunctionParams, ParseError> {
        self.expect_current(TokenKind::LeftParen)?;

        let mut params: Vec<FunctionParam> = Vec::new();

        while self.current_token.kind != TokenKind::RightParen {
            match self.current_token.kind.clone() {
                TokenKind::Identifier { name } => {
                    self.next_token(); // consume the identifier

                    let start = self.current_token.span.start;

                    // get the var type

                    let mut varty: Option<TokenKind> = None;
                    if self.current_token_is(TokenKind::Colon) {
                        self.next_token(); // consume the colon

                        varty = Some(self.parse_type_token()?);

                        self.next_token(); // consume the type
                    }

                    let mut default_value: Option<Expression> = None;

                    if self.current_token_is(TokenKind::Assign) {
                        self.next_token(); // consume the assign

                        default_value = Some(self.parse_expression(Precedence::Lowest)?.0);

                        self.next_token(); // consume the expression
                    }

                    params.push(FunctionParam {
                        identifier: Identifier {
                            name: name,
                            span: self.current_token.span.clone(),
                        },
                        ty: varty,
                        default_value: default_value,
                        span: Span {
                            start: start,
                            end: self.current_token.span.end,
                        },
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
                            return Err(format!(
                                "Expected ',' or end of parameters but found '{}'.",
                                self.current_token.kind
                            ))
                        }
                    }
                }
                _ => {
                    return Err(format!(
                        "Expected identifier for function parameter but got '{}'.",
                        self.current_token.kind
                    ))
                }
            }
        }

        self.expect_current(TokenKind::RightParen)?;

        Ok(params)
    }

    // Parse statements
    fn parse_for_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token(); // consume for token

        let mut initializer: Option<Variable> = None;
        if let Statement::Variable(var) = self.parse_variable_declaration()? {
            initializer = Some(var);
        }

        self.expect_current(TokenKind::Semicolon)?;

        let condition: Option<Expression>;
        match self.parse_expression(Precedence::Lowest) {
            Ok(result) => {
                condition = Some(result.0);
            }
            Err(e) => {
                return Err(e);
            }
        }

        self.expect_peek(TokenKind::Semicolon)?;
        self.next_token();

        let increment: Option<Expression>;
        match self.parse_expression(Precedence::Lowest) {
            Ok(result) => {
                increment = Some(result.0);
            }
            Err(e) => {
                return Err(e);
            }
        }

        self.next_token(); // consume increment token

        let body: Box<BlockStatement>;
        if self.current_token_is(TokenKind::LeftBrace) {
            body = Box::new(self.parse_block_statement()?);

            if !self.current_token_is(TokenKind::RightBrace) {
                return Err("Missing closing brace '}'.".to_string());
            }

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            }
        } else {
            return Err(format!("Missing opening brace '{{'."));
        }

        Ok(Statement::For(For {
            initializer,
            condition,
            increment,
            body,
            span: Span {
                start,
                end: self.current_token.span.end,
            },
        }))
    }

    fn parse_type_token(&mut self) -> Result<TokenKind, ParseError> {
        match self.current_token.kind {
            | TokenKind::I8
            | TokenKind::I16
            | TokenKind::I32
            | TokenKind::I64
            | TokenKind::I128
            | TokenKind::U8
            | TokenKind::U16
            | TokenKind::U32
            | TokenKind::U64
            | TokenKind::U128
            | TokenKind::F32
            | TokenKind::F64
            | TokenKind::F128
            | TokenKind::Char
            | TokenKind::Bool
            | TokenKind::Void
            | TokenKind::String
            | TokenKind::Array => Ok(self.current_token.kind.clone()),
            _ => Err(format!("Invalid type for variable: {}.", self.current_token.kind)),
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token(); // consume sharp token

        let identifier = self.current_token.clone(); // export the name of the identifier
        self.next_token(); // consume thte identifier

        let name = match identifier.kind {
            TokenKind::Identifier { name } => name,
            _ => return Err(format!("Invalid variable name: {}.", identifier.kind)),
        };

        let mut varty: Option<TokenKind> = None;
        if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume the colon

            varty = Some(self.parse_type_token()?);

            self.next_token(); // consume the type token
        }

        self.expect_current(TokenKind::Assign)?;

        let (expr, span) = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Variable(Variable {
            name,
            expr,
            span: Span { start, end: span.end },
            ty: varty,
        }))
    }

    fn parse_expression_series(&mut self, end: TokenKind) -> Result<(Vec<Expression>, Span), ParseError> {
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

        self.next_token(); // consume the starting token

        series.push(self.parse_expression(Precedence::Lowest)?.0); // parse the first expression

        // !self.peek_token_is(end.clone())
        while self.peek_token_is(TokenKind::Comma) {
            self.next_token(); // consume the current expression

            if self.current_token_is(TokenKind::Comma) && self.peek_token_is(end.clone()) {
                self.next_token(); // consume last comma
                break;
            }

            self.next_token(); // consume the comma

            series.push(self.parse_expression(Precedence::Lowest)?.0);
        }

        if self.peek_token_is(end.clone()) {
            self.next_token(); // consume the latest expression
        }

        if !self.current_token_is(end.clone()) {
            return Err(format!(
                "Expected '{}' to terminate the expression series, but found: '{}' instead.",
                end, self.current_token.kind
            ));
        }

        Ok((
            series,
            Span {
                start,
                end: self.current_token.span.end,
            },
        ))
    }

    fn parse_func_vis_type(&mut self, token: Token) -> Result<FuncVisType, ParseError> {
        let vis_type = match token.kind {
            TokenKind::Inline => FuncVisType::Inline,
            TokenKind::Extern => FuncVisType::Extern,
            TokenKind::Pub => FuncVisType::Pub,
            _ => return Err(format!("Invalid function visibility detected: '{}'", token.kind)),
        };
        self.next_token(); // consume vis_type token
        Ok(vis_type)
    }

    fn parse_function_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        let mut vis_type = FuncVisType::Internal; // internal by default

        if !self.current_token_is(TokenKind::Function) {
            // Parse visiblity type
            vis_type = self.parse_func_vis_type(self.current_token.clone())?;
        }

        self.next_token(); // consume the fn token

        let function_name = match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => name,
            _ => return Err(format!("Invalid function name: {}.", self.current_token.kind)),
        }; // export the name of the function
        self.next_token(); // consume the name of the identifier

        let params = self.parse_function_params()?;

        let mut return_type: Option<Token> = None;

        // parse return type
        if self.current_token_is(TokenKind::Colon) {
            self.next_token(); // consume colon

            return_type = Some(self.current_token.clone());

            if self.current_token_is(TokenKind::LeftBrace) {
                return Err("Expected return type before '{'.".to_string());
            }

            self.next_token();
        }

        // we used current_token_is because we don't want to consume it,
        // we pass this statement that is inside a brace to parse_block_statement.
        if self.current_token_is(TokenKind::LeftBrace) {
            let body = Box::new(self.parse_block_statement()?);

            if !self.current_token_is(TokenKind::RightBrace) {
                return Err("Missing closing brace '}'".to_string());
            }

            if self.peek_token_is(TokenKind::Semicolon) {
                self.next_token();
            }

            let end = self.current_token.span.end;

            return Ok(Statement::FuncDef(FuncDef {
                name: function_name,
                params,
                body,
                return_type,
                vis_type,
                span: Span { start, end },
            }));
        }

        Err("Missing closing brace '}'".to_string())
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token(); // consume return token

        let argument = self.parse_expression(Precedence::Lowest)?.0;

        if self.peek_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        let end = self.current_token.span.end;

        Ok(Statement::Return(Return {
            argument,
            span: Span { start, end },
        }))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token();

        let mut block_statement: Vec<Statement> = Vec::new();

        while !self.current_token_is(TokenKind::RightBrace) && !self.current_token_is(TokenKind::EOF) {
            let statement = self.parse_statement()?;
            block_statement.push(statement);
            self.next_token();
        }

        let end = self.current_token.span.end;

        Ok(BlockStatement {
            body: block_statement,
            span: Span { start, end },
        })
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        self.expect_current(TokenKind::If)?;
        self.expect_current(TokenKind::LeftParen)?;

        let (condition, _) = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::RightParen)?;
        self.expect_peek(TokenKind::LeftBrace)?;

        let mut branches: Vec<If> = Vec::new();
        let mut alternate: Option<Box<BlockStatement>> = None;

        let consequent = Box::new(self.parse_block_statement()?);

        self.expect_current(TokenKind::RightBrace)?;

        while self.current_token_is(TokenKind::Else) {
            self.next_token(); // consume else token

            // lets parse branches
            if self.current_token_is(TokenKind::If) {
                self.next_token(); // consume if token
                let start = self.current_token.span.start;
                self.expect_current(TokenKind::LeftParen)?;
                let (condition, _) = self.parse_expression(Precedence::Lowest)?;
                self.expect_peek(TokenKind::RightParen)?; // biggening of the block
                self.expect_peek(TokenKind::LeftBrace)?; // biggening of the block
                let consequent = Box::new(self.parse_block_statement()?);
                self.expect_current(TokenKind::RightBrace)?; // end of the block
                let end = self.current_token.span.end;

                branches.push(If {
                    condition,
                    consequent,
                    branches: vec![],
                    alternate: None,
                    span: Span { start, end },
                });
            } else {
                // parse alternate

                if !self.current_token_is(TokenKind::LeftBrace) {
                    return Err("Missing opening brace '{'.".to_string());
                }

                alternate = Some(Box::new(self.parse_block_statement()?));

                if !self.current_token_is(TokenKind::RightBrace) {
                    return Err("Missing closing brace '}'".to_string());
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
        }))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression(Precedence::Lowest)?.0;

        if self.peek_token_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Ok(Statement::Expression(expr))
    }

    fn parse_function_call_expression(
        &mut self,
        left: Expression,
        left_start: usize,
    ) -> Result<Expression, ParseError> {
        let arguments = self.parse_expression_series(TokenKind::RightParen)?;

        let end = self.current_token.span.end;

        match left {
            Expression::Identifier(identifier) => Ok(Expression::FunctionCall(FunctionCall {
                function_name: identifier,
                arguments: arguments.0,
                span: Span { start: left_start, end },
            })),
            _ => return Err(format!("Expected identifier in function call but found: {}.", left)),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<(Expression, Span), ParseError> {
        let mut left_start = self.current_token.span.start;
        let mut left = self.parse_prefix_expression()?;

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

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let span = self.current_token.span.clone();

        let expr = match &self.current_token.clone().kind {
            token_kind @ TokenKind::Increment | token_kind @ TokenKind::Decrement => {
                let ty = match token_kind {
                    TokenKind::Increment => UnaryOperatorType::PreIncrement,
                    TokenKind::Decrement => UnaryOperatorType::PreDecrement,
                    _ => {
                        return Err(format!(
                            "Expected increment (++) or decrement (--) operator, but found: {}.",
                            self.current_token.kind
                        ))
                    }
                };

                self.next_token(); // consume the operator

                match self.current_token.kind.clone() {
                    TokenKind::Identifier { name } => {
                        return Ok(Expression::UnaryOperator(UnaryOperator {
                            identifer: {
                                Identifier {
                                    name,
                                    span: span.clone(),
                                }
                            },
                            ty,
                            span: Span {
                                start: span.start,
                                end: self.current_token.span.end,
                            },
                        }));
                    }
                    _ => {
                        return Err(format!("Expected identifier but got {}.", self.current_token.kind));
                    }
                }
            }
            TokenKind::Identifier { name } => {
                let identifier = Identifier {
                    name: name.clone(),
                    span: span.clone(),
                };

                if self.peek_token_is(TokenKind::Increment) {
                    self.next_token();
                    return Ok(Expression::UnaryOperator(UnaryOperator {
                        identifer: identifier.clone(),
                        ty: UnaryOperatorType::PostIncrement,
                        span,
                    }));
                } else if self.peek_token_is(TokenKind::Decrement) {
                    self.next_token();
                    return Ok(Expression::UnaryOperator(UnaryOperator {
                        identifer: identifier.clone(),
                        ty: UnaryOperatorType::PostDecrement,
                        span,
                    }));
                } else if self.peek_token_is(TokenKind::LeftBracket) {
                    return self.parse_array_index();
                } else {
                    return Ok(Expression::Identifier(Identifier {
                        name: identifier.name,
                        span,
                    }));
                }
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
                })
            }
            TokenKind::LeftParen => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Lowest)?.0;
                self.expect_peek(TokenKind::RightParen)?;
                return Ok(expr);
            }
            TokenKind::LeftBracket => self.parse_array_items()?,
            _ => {
                return Err(format!(
                    "No corresponding prefix function defined for '{}'.",
                    self.current_token.kind
                ));
            }
        };

        Ok(expr)
    }

    fn parse_infix_expression(
        &mut self,
        left: Expression,
        left_start: usize,
    ) -> Option<Result<Expression, ParseError>> {
        match &self.peek_token.kind {
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
                self.next_token(); // consume the first part of the expression

                let operator = self.current_token.clone();

                let precedence = token_precedence_of(self.current_token.kind.clone());

                self.next_token(); // consume the operator

                let (right, span) = self.parse_expression(precedence).unwrap();

                Some(Ok(Expression::Infix(BinaryExpression {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                    span: Span {
                        start: left_start,
                        end: span.end,
                    },
                })))
            }
            TokenKind::LeftParen => {
                self.next_token(); // consume the identifier token
                Some(self.parse_function_call_expression(left, left_start))
            }
            _ => None,
        }
    }

    pub fn parse_array_index(&mut self) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;

        let identifer = self.current_token.clone();

        if let TokenKind::Identifier { name } = identifer.kind {
            let mut dimensions: Vec<Array> = Vec::new();

            while self.peek_token_is(TokenKind::LeftBracket) {
                let expr = self.parse_array_items()?;

                if let Expression::Array(elements) = expr {
                    dimensions.push(elements);
                } else {
                    return Err(format!(
                        "Expected array expression to add to the array index dimensions but got '{}'.",
                        expr
                    ));
                }
            }

            let end = self.current_token.span.end;

            Ok(Expression::ArrayIndex(ArrayIndex {
                dimensions,
                span: Span { start, end },
                identifier: Identifier {
                    name,
                    span: identifer.span,
                },
            }))
        } else {
            return Err(format!(
                "Expected identifier for array index evaluation but got '{}'.",
                self.current_token.kind
            ));
        }
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
        }))
    }
}
