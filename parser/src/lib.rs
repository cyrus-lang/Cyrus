use ast::ast::*;
use ast::token::*;
use lexer::*;
use precedences::{token_precedence_of, Precedence};
use utils::compiler_error;
use utils::fs::read_file;

mod parser_test;
mod precedences;

type ParseError = String;

pub fn parse_program(file_path: String) -> (Program, String) {
    let file = read_file(file_path.clone());
    let code = file.0;

    let mut lexer = Lexer::new(code, file_path);
    let mut parser = Parser::new(&mut lexer);

    let program = match parser.parse() {
        Ok(result) => {
            if let Node::Program(program) = result {
                program
            } else {
                compiler_error!("Expected a program given as input to the compiler but got unknown.");
            }
        }
        Err(errors) => {
            for error in errors {
                println!("{}", error);
            }

            std::process::exit(1);
        }
    };

    (program, file.1)
}

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
            TokenKind::Break => self.parse_break_statement(),
            TokenKind::Continue => self.parse_continue_statement(),
            TokenKind::LeftBrace => Ok(Statement::BlockStatement(self.parse_block_statement()?)),
            TokenKind::Import => self.parse_import_statment(),
            TokenKind::Struct => self.parse_struct_statement(),
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

    fn current_location(&mut self) -> Location {
        Location {
            line: self.lexer.line,
            column: self.lexer.column,
        }
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

    fn parse_struct_statement(&mut self) -> Result<Statement, ParseError> {
        let loc = self.current_location();

        self.next_token();

        match self.current_token.kind.clone() {
            TokenKind::Identifier { name: struct_name } => {
                self.next_token(); // consume identifier

                let mut inherits: Vec<Identifier> = Vec::new();

                if self.current_token_is(TokenKind::Colon) {
                    self.next_token();

                    loop {
                        dbg!(self.current_token.kind.clone());

                        match self.current_token.kind.clone() {
                            TokenKind::LeftBrace => {
                                self.next_token();
                                break;
                            }
                            TokenKind::EOF => {
                                return Err("Missing opening brace '{'.".to_string());
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
                                return Err(format!(
                                    "Inherit struct expects an identifier but got {}",
                                    self.current_token.kind.clone()
                                ))
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
                            return Err("Missing closing brace '}'.".to_string());
                        }
                        TokenKind::Function
                        | TokenKind::Decl
                        | TokenKind::Extern
                        | TokenKind::Pub
                        | TokenKind::Inline => {
                            if let Statement::FuncDef(method) = self.parse_function_statement()? {
                                methods.push(method);
                                self.next_token(); // consume the right brace
                            } else {
                                return Err(format!(
                                    "Invalid func definition given as method to struct '{}'",
                                    struct_name.clone()
                                ));
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
                            return Err(format!(
                                "Invalid statement definition inside struct '{}'",
                                struct_name.clone()
                            ))
                        }
                    }
                }

                Ok(Statement::Struct(Struct {
                    name: struct_name,
                    inherits,
                    fields,
                    methods,
                    loc,
                }))
            }
            _ => {
                return Err(format!(
                    "Struct name must be an identifier but got {}",
                    self.current_token.kind.clone()
                ))
            }
        }
    }

    fn parse_break_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();
        if !self.current_token_is(TokenKind::Semicolon) {
            Err(format!("Missing semicolon"))
        } else {
            Ok(Statement::Break(self.current_location()))
        }
    }

    fn parse_continue_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();
        if !self.current_token_is(TokenKind::Semicolon) {
            Err(format!("Missing semicolon"))
        } else {
            Ok(Statement::Continue(self.current_location()))
        }
    }

    fn parse_import_statment(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;

        if self.current_token_is(TokenKind::Import) {
            self.next_token(); // consume import keyword

            let mut package_paths: Vec<PackagePath> = vec![];

            while !self.current_token_is(TokenKind::Semicolon) {
                match self.current_token.kind.clone() {
                    TokenKind::Identifier { name } => {
                        package_paths.push(PackagePath {
                            package_name: Identifier {
                                name,
                                span: self.current_token.span.clone(),
                                loc: self.current_location(),
                            },
                            is_relative: false,
                            span: Span {
                                start,
                                end: self.current_token.span.end,
                            },
                            loc: self.current_location(),
                        });

                        self.next_token(); // consume identifier
                    }
                    TokenKind::Literal(Literal::String(value)) => {
                        package_paths.push(PackagePath {
                            package_name: Identifier {
                                name: value.raw,
                                span: value.span,
                                loc: self.current_location(),
                            },
                            is_relative: true,
                            span: Span {
                                start,
                                end: self.current_token.span.end,
                            },
                            loc: self.current_location(),
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

            return Ok(Statement::Import(Import {
                sub_packages: package_paths,
                span,
                loc: self.current_location(),
            }));
        } else {
            Err(format!(
                "Invalid token '{}' found in import statement.",
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
                            loc: self.current_location(),
                        },
                        ty: varty,
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

        // Check for non-conditional for loop
        if self.current_token_is(TokenKind::LeftBrace) {
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

        self.expect_current(TokenKind::LeftParen)?;

        let mut initializer: Option<Variable> = None;
        if let Statement::Variable(var) = self.parse_variable_declaration()? {
            initializer = Some(var);
        }

        self.expect_current(TokenKind::Semicolon)?;

        if self.current_token_is(TokenKind::LeftBrace) {
            return Err(format!("Defined a conditional for loop without any condition."));
        }

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

        let mut increment: Option<Expression> = None;
        if !self.current_token_is(TokenKind::LeftBrace) {
            match self.parse_expression(Precedence::Lowest) {
                Ok(result) => {
                    increment = Some(result.0);
                }
                Err(e) => {
                    return Err(e);
                }
            }

            self.next_token(); // consume increment token
        }

        self.expect_current(TokenKind::RightParen)?;

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
            loc: self.current_location(),
        }))
    }

    fn parse_type_token(&mut self) -> Result<TokenKind, ParseError> {
        let data_type = match &self.current_token.kind {
            TokenKind::I8
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
            | TokenKind::CSize
            | TokenKind::Char
            | TokenKind::Bool
            | TokenKind::Void
            | TokenKind::String => self.current_token.kind.clone(),
            token_kind => match token_kind {
                TokenKind::Asterisk => {
                    self.next_token();
                    return Ok(TokenKind::Dereference(Box::new(self.parse_type_token()?)));
                }
                TokenKind::Ampersand => {
                    self.next_token();
                    return Ok(TokenKind::AddressOf(Box::new(self.parse_type_token()?)));
                }
                TokenKind::Identifier { name: type_name } => TokenKind::UserDefinedType(Identifier {
                    name: type_name.clone(),
                    span: self.current_token.span.clone(),
                    loc: self.current_location(),
                }),
                _ => return Err(format!("Invalid type: {}.", self.current_token.kind)),
            },
        };

        self.next_token(); // consume the data type

        // Check for array data type
        if self.current_token_is(TokenKind::LeftBracket) {
            let mut dimensions: Vec<Option<TokenKind>> = Vec::new();

            while self.current_token_is(TokenKind::LeftBracket) {
                let mut capacity: Option<TokenKind> = None;

                self.next_token();

                if let TokenKind::Literal(_) = self.current_token.kind.clone() {
                    capacity = Some(self.current_token.kind.clone());
                    self.next_token();
                }

                self.expect_current(TokenKind::RightBracket)?;

                dimensions.push(capacity);
            }

            Ok(TokenKind::Array(Box::new(data_type), dimensions))
        } else {
            Ok(data_type)
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let start = self.current_token.span.start;
        self.next_token(); // consume sharp token

        let identifier = self.current_token.clone(); // export the name of the identifier
        self.next_token(); // consume identifier

        let name = match identifier.kind {
            TokenKind::Identifier { name } => name,
            _ => return Err(format!("Invalid variable name: {}.", identifier.kind)),
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

        let (expr, span) = self.parse_expression(Precedence::Lowest)?;

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

        if self.current_token_is(TokenKind::Semicolon) {
            return Ok(Statement::FuncDecl(FuncDecl {
                name: function_name,
                params,
                return_type,
                vis_type,
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
                loc: self.current_location(),
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
            loc: self.current_location(),
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
            loc: self.current_location(),
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

        if !self.current_token_is(TokenKind::RightBrace) {
            return Err("Missing closing brace '}'".to_string());
        }

        if self.peek_token_is(TokenKind::Else) {
            self.next_token(); // consume closing brace
        }

        while self.current_token_is(TokenKind::Else) {
            self.next_token(); // consume else token

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
                    loc: self.current_location(),
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
            loc: self.current_location(),
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
            Expression::Identifier(identifier) => {
                self.next_token();

                if !self.current_token_is(TokenKind::Semicolon) {
                    return Err(format!("Expected to end with semicolon"));
                }

                Ok(Expression::FunctionCall(FunctionCall {
                    function_name: identifier,
                    arguments: arguments.0,
                    span: Span { start: left_start, end },
                    loc: self.current_location(),
                }))
            }
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

    fn parse_array_index_assign(&mut self, array_index: ArrayIndex) -> Result<Expression, ParseError> {
        self.next_token(); // consume right bracket

        self.expect_current(TokenKind::Assign)?;

        let expr = self.parse_expression(Precedence::Lowest)?.0;

        Ok(Expression::ArrayIndexAssign(Box::new(ArrayIndexAssign {
            identifier: array_index.identifier,
            dimensions: array_index.dimensions,
            span: Span {
                start: array_index.span.start,
                end: self.current_token.span.end,
            },
            loc: self.current_location(),
            expr,
        })))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let span = self.current_token.span.clone();

        let expr = match &self.current_token.clone().kind {
            TokenKind::Ampersand => {
                self.next_token();
                return Ok(Expression::AddressOf(Box::new(self.parse_prefix_expression()?)));
            }
            TokenKind::Asterisk => {
                self.next_token();
                return Ok(Expression::Dereference(Box::new(self.parse_prefix_expression()?)));
            }
            TokenKind::Null => return Ok(Expression::Literal(Literal::Null)),
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
                                    loc: self.current_location(),
                                }
                            },
                            ty,
                            span: Span {
                                start: span.start,
                                end: self.current_token.span.end,
                            },
                            loc: self.current_location(),
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
                    loc: self.current_location(),
                };

                if self.peek_token_is(TokenKind::Increment) {
                    self.next_token();
                    return Ok(Expression::UnaryOperator(UnaryOperator {
                        identifer: identifier.clone(),
                        ty: UnaryOperatorType::PostIncrement,
                        span,
                        loc: self.current_location(),
                    }));
                } else if self.peek_token_is(TokenKind::Decrement) {
                    self.next_token();
                    return Ok(Expression::UnaryOperator(UnaryOperator {
                        identifer: identifier.clone(),
                        ty: UnaryOperatorType::PostDecrement,
                        span,
                        loc: self.current_location(),
                    }));
                } else if self.peek_token_is(TokenKind::Assign) {
                    return self.parse_assignment();
                } else if self.peek_token_is(TokenKind::LeftBracket) {
                    let array_index = self.parse_array_index()?;

                    if self.peek_token_is(TokenKind::Assign) {
                        return self.parse_array_index_assign(array_index);
                    }

                    return Ok(Expression::ArrayIndex(array_index));
                } else if self.peek_token_is(TokenKind::LeftBrace) {
                    return self.parse_struct_init();
                } else if self.peek_token_is(TokenKind::Dot) {
                    return self.parse_method_call();
                } else {
                    return Ok(Expression::Identifier(Identifier {
                        name: identifier.name,
                        span,
                        loc: self.current_location(),
                    }));
                }
            }
            bool_token @ TokenKind::True | bool_token @ TokenKind::False => {
                let raw = match bool_token {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => panic!(),
                };

                return Ok(Expression::Literal(Literal::Bool(BoolLiteral { raw, span })));
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

    fn parse_method_call(&mut self) -> Result<Expression, ParseError> {
        match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => {
                let method_call_start = self.current_token.span.start.clone();

                let identifier = Identifier {
                    name,
                    span: self.current_token.span.clone(),
                    loc: self.current_location(),
                };
                let mut chains: Vec<FunctionCall> = Vec::new();
                self.next_token(); // consume identifer
                self.expect_current(TokenKind::Dot)?;

                loop {
                    let start = self.current_token.span.start.clone();
                    let method_name = match self.current_token.kind.clone() {
                        TokenKind::Identifier { name } => Identifier {
                            name,
                            span: self.current_token.span.clone(),
                            loc: self.current_location(),
                        },
                        _ => {
                            return Err(format!(
                                "Expected identifier as method name but got '{}'",
                                self.current_token.kind.clone()
                            ))
                        }
                    };

                    self.next_token(); // consume method name
                    if !self.current_token_is(TokenKind::LeftParen) {
                        return Err(format!(
                            "Expected to be a method call with opening paren but got '{}'",
                            self.current_token.kind.clone()
                        ));
                    }
                    let arguments = self.parse_expression_series(TokenKind::RightParen)?.0;
                    self.expect_current(TokenKind::RightParen)?;

                    let method_call = FunctionCall {
                        function_name: method_name,
                        arguments,
                        span: Span {
                            start,
                            end: self.current_token.span.end,
                        },
                        loc: self.current_location(),
                    };

                    chains.push(method_call);

                    match self.current_token.kind {
                        TokenKind::Dot => {
                            self.next_token();
                            continue;
                        }
                        TokenKind::Semicolon => {
                            break;
                        }
                        _ => {
                            return Err(format!(
                                "Expected to end method call chain with semicolon but got '{}'",
                                self.current_token.kind.clone()
                            ))
                        }
                    }
                }

                return Ok(Expression::MethodCall(MethodCall {
                    identifier,
                    chains,
                    span: Span {
                        start: method_call_start,
                        end: self.current_token.span.end,
                    },
                    loc: self.current_location(),
                }));
            }
            _ => {
                return Err(format!(
                    "Expected identifier but got '{}'",
                    self.current_token.kind.clone()
                ))
            }
        }
    }

    fn parse_struct_init(&mut self) -> Result<Expression, ParseError> {
        let mut field_inits: Vec<FieldInit> = Vec::new();

        match self.current_token.kind.clone() {
            TokenKind::Identifier { name: struct_name } => {
                self.next_token(); // consume struct name
                self.expect_current(TokenKind::LeftBrace)?;

                if self.current_token_is(TokenKind::RightBrace) {
                    return Ok(Expression::StructInit(StructInit {
                        name: struct_name,
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
                            return Err(format!(
                                "Expected identifier as field name but got '{}'",
                                self.current_token.kind
                            ))
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
                            return Err(format!("Missing closing brace"));
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
                        _ => return Err(format!("Invalid token recognized in struct init")),
                    }
                }

                return Ok(Expression::StructInit(StructInit {
                    name: struct_name,
                    field_inits,
                    loc: self.current_location(),
                }));
            }
            _ => {
                return Err(format!(
                    "Expected identifer to struct init but got '{}'",
                    self.current_token.kind
                ))
            }
        }
    }

    fn parse_assignment(&mut self) -> Result<Expression, ParseError> {
        let start = self.current_token.span.start;

        if let TokenKind::Identifier { name } = self.current_token.kind.clone() {
            let identifier = Identifier {
                name,
                span: self.current_token.span.clone(),
                loc: self.current_location(),
            };
            self.next_token(); // consume identifier
            self.next_token(); // consume assign

            let expr = self.parse_expression(Precedence::Lowest)?.0;

            let end = self.current_token.span.end;

            Ok(Expression::Assignment(Box::new(Assignment {
                identifier,
                expr,
                span: Span { start, end },
                loc: self.current_location(),
            })))
        } else {
            return Err(format!("Invalid identifier given for assignment."));
        }
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
                    loc: self.current_location(),
                })))
            }
            TokenKind::LeftParen => {
                self.next_token(); // consume the identifier token
                Some(self.parse_function_call_expression(left, left_start))
            }
            _ => None,
        }
    }

    pub fn parse_array_index(&mut self) -> Result<ArrayIndex, ParseError> {
        let start = self.current_token.span.start;

        let identifer = self.current_token.clone();

        if let TokenKind::Identifier { name } = identifer.kind {
            let mut dimensions: Vec<Expression> = Vec::new();

            while self.peek_token_is(TokenKind::LeftBracket) {
                let expr = self.parse_array_items()?;
                dimensions.push(expr);
            }

            let end = self.current_token.span.end;

            Ok(ArrayIndex {
                dimensions,
                span: Span { start, end },
                identifier: Identifier {
                    name,
                    span: identifer.span,
                    loc: self.current_location(),
                },
                loc: self.current_location(),
            })
        } else {
            return Err(format!(
                "Expected identifier for array index evaluation but got '{}'.",
                self.current_token.kind
            ));
        }
    }

    fn parse_array_items(&mut self) -> Result<Expression, ParseError> {
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
