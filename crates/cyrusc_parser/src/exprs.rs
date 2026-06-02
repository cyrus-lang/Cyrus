/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::Diag;
use crate::Parser;
use crate::diagnostics::ParserDiagKind;
use crate::prec::*;
use cyrusc_ast::abi::ReprAttr;
use cyrusc_ast::operators::InfixOperator;
use cyrusc_ast::operators::PrefixOperator;
use cyrusc_ast::operators::UnaryOperator;
use cyrusc_ast::*;
use cyrusc_diagcentral::DiagLevel;
use cyrusc_source_loc::Loc;
use cyrusc_tokens::Token;
use cyrusc_tokens::TokenKind;
use cyrusc_tokens::literals::ASTLiteralExpr;
use cyrusc_tokens::literals::IntLiteralKind;
use cyrusc_tokens::literals::LiteralKind;

impl<'source_file> Parser<'source_file> {
    pub(crate) fn parse_expr(&mut self, precedence: Precedence) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (mut lhs_line, mut lhs_column, mut lhs_start) = (loc.line, loc.column, loc.start);

        let mut lhs = self.parse_prefix_expr()?;

        loop {
            if self.peek_token_is(TokenKind::LeftBracket) {
                self.next_token();
                lhs = self.parse_array_index(lhs)?;
                continue;
            }

            if self.peek_token_is(TokenKind::Dot) || self.peek_token_is(TokenKind::ThinArrow) {
                self.next_token();
                lhs = self.parse_field_access(lhs)?;
                continue;
            }

            if self.peek_token_is(TokenKind::Increment) {
                self.next_token();

                let end = self.current_token().loc.end;

                return Ok(ASTExpr::Unary(ASTUnaryExpr {
                    operand: Box::new(lhs),
                    op: UnaryOperator::PostIncrement,
                    loc: Loc::new(self.file_id(), lhs_line, lhs_column, lhs_start, end),
                }));
            } else if self.peek_token_is(TokenKind::Decrement) {
                self.next_token();

                let end = self.current_token().loc.end;

                return Ok(ASTExpr::Unary(ASTUnaryExpr {
                    operand: Box::new(lhs),
                    op: UnaryOperator::PostDecrement,
                    loc: Loc::new(self.file_id(), lhs_line, lhs_column, lhs_start, end),
                }));
            }

            // infix handling (respect precedence)
            let peek_prec = token_precedence_of(self.peek_token().kind);
            if self.peek_token().kind != TokenKind::EOF && precedence < peek_prec {
                match self.parse_infix_expr(lhs.clone(), lhs_line, lhs_column, lhs_start) {
                    Some(infix) => {
                        lhs = infix?;

                        if let ASTExpr::Infix(infix_expr) = lhs.clone() {
                            lhs_start = infix_expr.loc.start;
                            lhs_line = infix_expr.loc.line;
                            lhs_column = infix_expr.loc.column;
                        }
                    }
                    None => return Ok(lhs),
                }
            } else {
                break;
            }
        }

        Ok(lhs)
    }

    fn parse_prefix_expr(&mut self) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let mut expr = match &self.current_token().kind {
            TokenKind::Repr => {
                let repr_attr = self.parse_repr_attr(self.current_token())?.unwrap();

                if self.current_token_is(TokenKind::Struct) {
                    self.parse_unnamed_struct_value(Some(repr_attr))?
                } else if self.current_token_is(TokenKind::Union) {
                    let end = self.current_token().loc.end;

                    return Err(Diag {
                        kind: Box::new(ParserDiagKind::InvalidModifier(
                            "Repr attribute cannot be applied to unnamed union values.".to_string(),
                        )),
                        level: DiagLevel::Error,
                        loc: Some(Loc::new(self.file_id(), line, column, start, end)),
                        hint: None,
                    });
                } else {
                    return Err(self.error_invalid_token());
                }
            }

            TokenKind::Struct => self.parse_unnamed_struct_value(None)?,

            TokenKind::Union => self.parse_unnamed_union_value()?,

            TokenKind::Inline | TokenKind::Function => {
                let is_inline = {
                    if self.current_token_is(TokenKind::Inline) {
                        self.next_token();
                        true
                    } else {
                        false
                    }
                };

                if self.current_token_is(TokenKind::Function) {
                    self.parse_lambda_expr(is_inline)?
                } else {
                    return Err(self.error_invalid_token());
                }
            }

            TokenKind::At => {
                return Ok(ASTExpr::Builtin(self.parse_builtin(false)?));
            }

            TokenKind::Dot => self.parse_unnamed_enum_value()?,

            TokenKind::Dynamic => self.parse_dynamic_expr()?,

            TokenKind::Ident { .. } => {
                let module_import = self.parse_module_import()?;

                if self.current_token_is(TokenKind::LeftBrace) {
                    ASTExpr::StructInit(self.parse_struct_init(TypeSpecifier::ModuleImport(module_import))?)
                } else {
                    ASTExpr::ModuleImport(module_import)
                }
            }

            TokenKind::Ampersand => {
                self.next_token();

                let end = self.current_token().loc.end;

                ASTExpr::AddrOf(ASTAddrOfExpr {
                    expr: Box::new(self.parse_expr(Precedence::Prefix)?),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                })
            }

            TokenKind::Asterisk => {
                self.next_token();

                let end = self.current_token().loc.end;

                ASTExpr::Deref(ASTDerefExpr {
                    expr: Box::new(self.parse_expr(Precedence::Prefix)?),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                })
            }

            TokenKind::Literal(value) => ASTExpr::Literal(value.clone()),

            TokenKind::Null => {
                let end = self.current_token().loc.end;

                ASTExpr::Literal(ASTLiteralExpr {
                    kind: LiteralKind::Null,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                })
            }

            bool_token @ TokenKind::True | bool_token @ TokenKind::False => {
                let value = match bool_token {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => unreachable!(),
                };

                let end = self.current_token().loc.end;

                ASTExpr::Literal(ASTLiteralExpr {
                    kind: LiteralKind::Bool(value),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                })
            }

            token_kind @ TokenKind::Minus | token_kind @ TokenKind::Bang | token_kind @ TokenKind::Tilde => {
                let prefix_operator = match token_kind {
                    TokenKind::Minus => PrefixOperator::Minus,
                    TokenKind::Bang => PrefixOperator::Bang,
                    TokenKind::Tilde => PrefixOperator::BitwiseNot,
                    _ => {
                        return Err(self.error_at_current(ParserDiagKind::InvalidPrefixOperator(token_kind.clone())));
                    }
                };

                self.next_token(); // consume the prefix operator

                let expr = self.parse_expr(Precedence::Prefix)?;

                let end = self.current_token().loc.end;

                ASTExpr::Prefix(ASTPrefixExpr {
                    op: prefix_operator,
                    operand: Box::new(expr),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                })
            }

            TokenKind::LeftParen => {
                // grouped expression
                self.next_token();

                let expr = self.parse_expr(Precedence::Lowest)?;
                self.next_token(); // consume last token of expr

                if self.current_token_is(TokenKind::Comma) {
                    // considered as tuple construction, not grouped expr
                    self.next_token();
                    self.parse_tuple_value(expr)?
                } else {
                    expr
                }
            }

            TokenKind::LeftBrace => self.parse_untyped_array()?,

            TokenKind::Increment => {
                self.next_token();

                let expr = self.parse_expr(Precedence::Prefix)?;

                let end = self.current_token().loc.end;

                ASTExpr::Unary(ASTUnaryExpr {
                    op: UnaryOperator::PreIncrement,
                    operand: Box::new(expr),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                })
            }

            TokenKind::Decrement => {
                self.next_token();

                let expr = self.parse_expr(Precedence::Prefix)?;

                let end = self.current_token().loc.end;

                ASTExpr::Unary(ASTUnaryExpr {
                    op: UnaryOperator::PreDecrement,
                    operand: Box::new(expr),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                })
            }

            _ => {
                let type_spec = self.parse_type_specifier()?;

                if self.peek_token_is(TokenKind::LeftBrace) {
                    self.next_token();
                    return self.parse_array(type_spec);
                }

                ASTExpr::TypeSpecifier(type_spec)
            }
        };

        let mut type_args = TypeArgs::new();

        let type_arg_start_detail = self.is_type_arg_start(expr.clone());

        if type_arg_start_detail.includes_type_args {
            if !type_arg_start_detail.is_array_init {
                self.next_token(); // consume current token of expr
                type_args = self.parse_type_args()?;
            } else {
                // handle generic array init
                let type_spec = self.parse_type_specifier()?;
                self.next_token();
                return Ok(self.parse_array(type_spec)?);
            }
        }

        if self.peek_token_is(TokenKind::LeftBrace) {
            if let ASTExpr::ModuleImport(module_import) = expr.clone() {
                self.next_token(); // consume struct name

                let struct_init = {
                    let operand = {
                        if let Some(ident) = module_import.as_ident() {
                            TypeSpecifier::Ident(ident)
                        } else {
                            TypeSpecifier::ModuleImport(module_import)
                        }
                    };

                    if type_args.is_empty() {
                        self.parse_struct_init(operand)?
                    } else {
                        let end = self.current_token().loc.end;

                        self.parse_struct_init(TypeSpecifier::GenericInst(GenericInst {
                            base: Box::new(operand),
                            type_args,
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        }))?
                    }
                };

                Ok(ASTExpr::StructInit(struct_init))
            } else {
                return Err(self.error_invalid_token());
            }
        } else if self.peek_token_is(TokenKind::LeftParen) {
            return self.parse_func_call(expr, type_args);
        }
        // Case: Option<int>.Some(...) (Method or static member access)
        // We redirect to parse_field_access, passing the base expr and the
        // type arguments that qualify it.
        else if self.peek_token_is(TokenKind::Dot) || self.peek_token_is(TokenKind::ThinArrow) {
            if let ASTExpr::ModuleImport(module_import) = expr.clone() {
                if !type_args.is_empty() {
                    self.next_token(); // consume `>`

                    expr = ASTExpr::TypeSpecifier(TypeSpecifier::GenericInst(GenericInst {
                        base: Box::new(TypeSpecifier::ModuleImport(module_import)),
                        type_args,
                        loc,
                    }));

                    self.parse_field_access(expr)
                } else {
                    Ok(expr)
                }
            } else {
                Ok(expr)
            }
        } else {
            // type args are given to the wrong expression
            if !type_args.is_empty() {
                return Err(self.error_invalid_token());
            }

            Ok(expr)
        }
    }

    fn parse_infix_expr(
        &mut self,
        lhs: ASTExpr,
        lhs_line: usize,
        lhs_column: usize,
        lhs_start: usize,
    ) -> Option<Result<ASTExpr, Diag>> {
        // NOTE: disambiguate confusion when facing `>>`. when it used as expressions the token must be lowered
        // into shift-right but otherwise, it's interpreted as separate greater-than tokens. For instance in generic types args:
        // Generic<A, Generic<B, C>>
        if let (Some(token1), Some(token2)) = (self.peek_n_token(1), self.peek_n_token(2)) {
            if token1.kind == TokenKind::GreaterThan && token2.kind == TokenKind::GreaterThan {
                let end = self.current_token().loc.end;

                let peek_token_idx = self.pos + 1;

                self.tokens.remove(peek_token_idx);
                self.tokens.remove(peek_token_idx);
                self.tokens.insert(
                    peek_token_idx,
                    Token {
                        kind: TokenKind::ShiftLeft,
                        loc: Loc::new(self.file_id(), lhs_line, lhs_column, lhs_start, end),
                    },
                );
            }
        }

        match self.peek_token().kind {
            TokenKind::Assign => {
                self.next_token();

                match self.parse_assign(lhs, AssignKind::Default, lhs_start) {
                    Ok(expr) => Some(Ok(expr)),
                    Err(err) => Some(Err(err)),
                }
            }
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
            | TokenKind::Ident { .. } => {
                self.next_token(); // consume lhs expr

                let operator_token = self.current_token().kind;
                if self.peek_token_is(TokenKind::Assign) {
                    self.next_token();
                    let Some(assign_kind) = map_assign_kind(operator_token.clone()) else {
                        return Some(Err(
                            self.error_at_current(ParserDiagKind::InvalidAssignOperator(operator_token))
                        ));
                    };

                    return Some(self.parse_assign(lhs, assign_kind, lhs_start));
                }

                let precedence = token_precedence_of(operator_token.clone());
                self.next_token(); // consume the operator

                let op = match operator_token {
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
                        return Some(Err(self.error_at_current(ParserDiagKind::InvalidInfixOperator(
                            self.current_token().kind,
                        ))));
                    }
                };

                let rhs = self.parse_expr(precedence).ok()?;

                let end = self.current_token().loc.end;

                Some(Ok(ASTExpr::Infix(ASTInfixExpr {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    loc: Loc::new(self.file_id(), lhs_line, lhs_column, lhs_start, end),
                })))
            }
            _ => None,
        }
    }

    pub(crate) fn parse_expr_series(&mut self, ending_token: TokenKind) -> Result<Vec<ASTExpr>, Diag> {
        let mut series: Vec<ASTExpr> = Vec::new();

        // detect empty series of expressions
        if self.peek_token_is(ending_token.clone()) {
            self.next_token();
            return Ok(series);
        } else if self.current_token_is(ending_token.clone()) {
            return Ok(series);
        }
        self.next_token();

        series.push(self.parse_expr(Precedence::Lowest)?);

        while self.peek_token_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();
            series.push(self.parse_expr(Precedence::Lowest)?);
        }
        self.next_token(); // consume latest token of the expression

        Ok(series)
    }

    pub(crate) fn parse_module_import(&mut self) -> Result<ASTModuleImport, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let mut segments = {
            let end = self.current_token().loc.end;

            match &self.current_token().kind {
                TokenKind::Ident(ident) => {
                    vec![ModuleSegment::SubModule(Ident {
                        value: ident.clone(),
                        loc: Loc::new(self.file_id(), line, column, start, end),
                    })]
                }
                _ => {
                    return Err(self.error_at_current(ParserDiagKind::ExpectedIdentifier {
                        got: self.current_token().kind.to_string(),
                    }));
                }
            }
        };

        // if no '::' follows, this is a single identifier import
        if !self.peek_token_is(TokenKind::DoubleColon) {
            let end = self.current_token().loc.end;

            return Ok(ASTModuleImport {
                segments,
                loc: Loc::new(self.file_id(), line, column, start, end),
            });
        }

        self.next_token(); // consume first identifier

        loop {
            if !self.current_token_is(TokenKind::DoubleColon) {
                break;
            }

            self.next_token(); // consume '::'

            let ident_token = self.current_token();

            match &ident_token.kind {
                TokenKind::Ident(name) => {
                    segments.push(ModuleSegment::SubModule(Ident {
                        value: name.clone(),
                        loc: ident_token.loc,
                    }));
                }
                _ => {
                    // if we find '::' but no identifier after it
                    return Err(self.error_at_current(ParserDiagKind::ExpectedIdentifier {
                        got: ident_token.kind.to_string(),
                    }));
                }
            }

            // check if there's another '::' after this identifier
            if !self.peek_token_is(TokenKind::DoubleColon) {
                break;
            }

            // Prepare for next iteration
            self.next_token();
        }

        let end = self.current_token().loc.end;

        Ok(ASTModuleImport {
            segments,
            loc: Loc::new(self.file_id(), line, column, start, end),
        })
    }

    pub(crate) fn parse_module_path(&mut self) -> Result<ModulePath, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let mut segments = Vec::new();
        let mut alias = None;

        let ident = self.parse_ident()?;
        segments.push(ModuleSegment::SubModule(ident));
        self.next_token();

        while self.current_token_is(TokenKind::DoubleColon) {
            self.next_token(); // consume '::'

            let ident = self.parse_ident()?;
            segments.push(ModuleSegment::SubModule(ident));
            self.next_token();
        }

        // check for optional 'as' alias
        if self.current_token_is(TokenKind::As) {
            self.next_token();

            let alias_ident = self.parse_ident()?;
            alias = Some(alias_ident.value);
            self.next_token(); // consume the alias identifier
        }

        let end = self.current_token().loc.end;

        Ok(ModulePath {
            alias,
            segments,
            loc: Loc::new(self.file_id(), line, column, start, end),
        })
    }

    fn parse_dynamic_expr(&mut self) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;

        let end = self.current_token().loc.end;

        Ok(ASTExpr::Dynamic(ASTDynamicExpr {
            operand: Box::new(expr),
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_tuple_value(&mut self, first_expr: ASTExpr) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let mut elements: Vec<ASTExpr> = vec![first_expr];

        loop {
            let expr = self.parse_expr(Precedence::Lowest)?;
            self.next_token();

            elements.push(expr);

            match self.current_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                    continue;
                }
                _ => break,
            }
        }

        self.must_be_right_paren()?;

        let end = self.current_token().loc.end;

        Ok(ASTExpr::Tuple(ASTTupleValueExpr {
            elements,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_lambda_expr(&mut self, inline: bool) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume function
        let params = self.parse_func_params()?;
        let ret_type = self.parse_type_specifier()?;
        self.next_token(); // last token of return type

        let body = self.parse_block()?;

        let end = self.current_token().loc.end;

        Ok(ASTExpr::Lambda(ASTLambdaExpr {
            params,
            body: Box::new(body),
            ret_type,
            inline,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_method_call(
        &mut self,
        operand: ASTExpr,
        method_name: Ident,
        is_thin_arrow: bool,
        type_args: TypeArgs,
        line: usize,
        column: usize,
        start: usize,
    ) -> Result<ASTExpr, Diag> {
        self.must_be_left_paren()?;
        let args = self.parse_expr_series(TokenKind::RightParen)?;
        self.must_be_right_paren()?;

        let end = self.current_token().loc.end;

        Ok(ASTExpr::MethodCall(ASTMethodCallExpr {
            is_thin_arrow,
            operand: Box::new(operand),
            method_name,
            type_args,
            args,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_field_access(&mut self, operand: ASTExpr) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let is_thin_arrow = {
            if self.current_token_is(TokenKind::ThinArrow) {
                self.next_token();
                true
            } else if self.current_token_is(TokenKind::Dot) {
                self.next_token();
                false
            } else {
                self.expect_current(TokenKind::Dot)?;
                unreachable!()
            }
        };

        if matches!(self.current_token().kind, TokenKind::Ident { .. }) {
            let ident = self.parse_ident()?;

            let type_arg_start_detail = self.is_type_arg_start(ASTExpr::Ident(ident.clone()));

            let type_args = {
                if type_arg_start_detail.includes_type_args {
                    self.next_token(); // consume ident
                    self.parse_type_args()?
                } else {
                    TypeArgs::new()
                }
            };

            if self.peek_token_is(TokenKind::LeftParen) {
                self.next_token(); // consume ident

                self.parse_method_call(operand, ident, is_thin_arrow, type_args, line, column, start)
            } else if self.peek_token_is(TokenKind::LeftBrace) {
                if is_thin_arrow || !type_args.is_empty() {
                    return Err(self.error_invalid_token());
                }

                self.next_token(); // consume ident

                self.parse_enum_struct_variant_init(operand, ident, line, column, start)
            } else {
                let end = self.current_token().loc.end;

                Ok(ASTExpr::FieldAccess(ASTFieldAccessExpr {
                    is_thin_arrow,
                    operand: Box::new(operand),
                    field_name: ident,
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }))
            }
        } else {
            let index = self.parse_integer_without_suffix()?;
            let end = self.current_token().loc.end;

            Ok(ASTExpr::TupleAccess(ASTTupleAccessExpr {
                operand: Box::new(operand),
                index: index.try_into().unwrap(),
                loc: Loc::new(self.file_id(), line, column, start, end),
            }))
        }
    }

    fn parse_func_call(&mut self, operand: ASTExpr, type_args: TypeArgs) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.expect_peek(TokenKind::LeftParen)?;

        let args = self.parse_expr_series(TokenKind::RightParen)?;
        self.must_be_right_paren()?;

        let end = self.current_token().loc.end;

        Ok(ASTExpr::FuncCall(ASTFuncCallExpr {
            operand: Box::new(operand),
            args,
            type_args,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_struct_init(&mut self, operand: TypeSpecifier) -> Result<ASTStructInitExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let mut field_inits: Vec<FieldInit> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(ASTStructInitExpr {
                operand,
                field_inits,
                loc: Loc::new(self.file_id(), line, column, start, end),
            });
        }

        loop {
            let field_name = self.parse_ident()?;
            let field_loc = self.current_token().loc;

            self.next_token(); // consume ident

            if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
                // syntax shorthand for `ident:ident`
                let ident_expr = ASTExpr::Ident(field_name.clone());

                field_inits.push(FieldInit {
                    ident: field_name,
                    value: ident_expr,
                    loc: field_loc,
                });
            } else {
                self.expect_current(TokenKind::Colon)?;

                let value = self.parse_expr(Precedence::Lowest)?;
                self.next_token();

                field_inits.push(FieldInit {
                    ident: field_name,
                    value,
                    loc: field_loc,
                });
            }

            match self.current_token().kind {
                TokenKind::EOF => {
                    return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
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
                    return Err(self.error_invalid_token());
                }
            }
        }

        let end = self.current_token().loc.end;

        Ok(ASTStructInitExpr {
            operand,
            field_inits,
            loc: Loc::new(self.file_id(), line, column, start, end),
        })
    }

    fn parse_assign(&mut self, lhs: ASTExpr, kind: AssignKind, start: usize) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column) = (loc.line, loc.column);

        self.expect_current(TokenKind::Assign)?;
        let rhs = self.parse_expr(Precedence::Lowest)?;

        let end = self.current_token().loc.end;

        Ok(ASTExpr::Assign(Box::new(ASTAssignExpr {
            lhs,
            rhs,
            kind,
            loc: Loc::new(self.file_id(), line, column, start, end),
        })))
    }

    pub(crate) fn parse_single_array_index(&mut self) -> Result<ASTExpr, Diag> {
        self.expect_current(TokenKind::LeftBracket)?;

        if self.current_token_is(TokenKind::RightBracket) {
            return Err(self.error_invalid_token());
        }

        let index = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(index)
    }

    fn parse_array_index(&mut self, expr: ASTExpr) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let index_expr = self.parse_single_array_index()?;
        let mut indexes_backup: Vec<ASTExpr> = vec![index_expr.clone()];

        let mut base_index = {
            let end = self.current_token().loc.end;

            ASTExpr::ArrayIndex(ASTArrayIndexExpr {
                operand: Box::new(expr.clone()),
                index: Box::new(index_expr.clone()),
                loc: Loc::new(self.file_id(), line, column, start, end),
            })
        };

        // handle chained indices ident[a][b][c]
        if self.peek_token_is(TokenKind::LeftBracket) {
            self.next_token();
        }

        while self.current_token_is(TokenKind::LeftBracket) {
            let index_expr = self.parse_single_array_index()?;

            let end = self.current_token().loc.end;

            base_index = ASTExpr::ArrayIndex(ASTArrayIndexExpr {
                operand: Box::new(base_index),
                index: Box::new(index_expr.clone()),
                loc: Loc::new(self.file_id(), line, column, start, end),
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
                    ASTExpr::Ident(ident) => TypeSpecifier::Ident(ident),
                    ASTExpr::ModuleImport(module_import) => TypeSpecifier::ModuleImport(module_import),
                    ASTExpr::TypeSpecifier(type_spec) => type_spec,
                    _ => {
                        return Err(self.error_invalid_token());
                    }
                }
            };

            let mut base_array = {
                let end = self.current_token().loc.end;

                ArrayType {
                    size: ArrayCapacity::Fixed(Box::new(indexes_backup.remove(0))),
                    element_type: Box::new(element_type),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                }
            };

            for index in &indexes_backup {
                let end = self.current_token().loc.end;

                base_array = ArrayType {
                    size: ArrayCapacity::Fixed(Box::new(index.clone())),
                    element_type: Box::new(TypeSpecifier::Array(base_array)),
                    loc: Loc::new(self.file_id(), line, column, start, end),
                };
            }

            // parse the initializer block
            self.next_token();
            let array_const = self.parse_array(TypeSpecifier::Array(base_array))?;

            return Ok(array_const);
        }

        Ok(base_index)
    }

    fn parse_untyped_array(&mut self) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        let elements = self.parse_expr_series(TokenKind::RightBrace)?;

        self.must_be_right_brace()?;

        let end = self.current_token().loc.end;

        Ok(ASTExpr::UntypedArray(ASTUntypedArrayExpr {
            elements,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_array(&mut self, type_spec: TypeSpecifier) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        if !matches!(type_spec, TypeSpecifier::Array(..)) {
            return Err(self.error_at_current(ParserDiagKind::NonArrayDataTypeForArrayConstruction));
        }

        self.must_be_left_brace()?;

        let mut elements: Vec<ASTExpr> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(ASTExpr::Array(ASTArrayExpr {
                elements,
                data_type: type_spec,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }));
        }

        loop {
            if self.current_token_is(TokenKind::LeftBrace) {
                let mut untyped_array: Vec<ASTExpr> = Vec::new();
                self.next_token(); // consume left brace

                loop {
                    // REVIEW: Maybe this is not necessary here anymore? Because we support untyped array constructors...
                    if self.current_token_is(TokenKind::RightBrace) {
                        return Err(self.error_at_current(ParserDiagKind::InvalidUntypedArrayConstructor));
                    }

                    untyped_array.push(self.parse_expr(Precedence::Lowest)?);

                    if self.peek_token_is(TokenKind::Comma) {
                        self.next_token(); // consume last token of the expression
                        self.next_token(); // consume comma
                    } else if self.peek_token_is(TokenKind::RightBrace) {
                        self.next_token();
                        break;
                    } else {
                        return Err(self.error_at_peek(ParserDiagKind::InvalidToken(self.peek_token().kind)));
                    }
                }

                if let TypeSpecifier::Array(inner_type_specifier, ..) = type_spec.clone() {
                    let end = self.current_token().loc.end;

                    let data_type = TypeSpecifier::Array(ArrayType {
                        size: ArrayCapacity::Fixed(Box::new(ASTExpr::Literal(ASTLiteralExpr {
                            kind: LiteralKind::Integer(
                                IntLiteralKind::Unsigned(untyped_array.len().try_into().unwrap()),
                                None,
                            ),
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        }))),
                        element_type: inner_type_specifier.element_type,
                        loc: Loc::new(self.file_id(), line, column, start, end),
                    });

                    elements.push(ASTExpr::Array(ASTArrayExpr {
                        data_type,
                        elements: untyped_array,
                        loc: Loc::new(self.file_id(), line, column, start, end),
                    }));
                } else {
                    unreachable!()
                }
            } else {
                elements.push(self.parse_expr(Precedence::Lowest)?);
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

        let end = self.current_token().loc.end;

        Ok(ASTExpr::Array(ASTArrayExpr {
            elements,
            data_type: type_spec,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_enum_struct_variant_field_inits(
        &mut self,
        column: usize,
    ) -> Result<Vec<ASTEnumStructVariantFieldInit>, Diag> {
        let mut field_inits = Vec::new();

        loop {
            match self.current_token().kind {
                TokenKind::RightBrace => {
                    break;
                }
                TokenKind::EOF => {
                    return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
                }
                TokenKind::Ident(_) => {
                    let start = self.current_token().loc.start;
                    let line = self.current_token().loc.line;
                    let ident = self.parse_ident()?;

                    self.next_token(); // consume ident

                    if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
                        // syntax shorthand for `ident:ident`
                        let ident_expr = ASTExpr::Ident(ident.clone());

                        let end = self.current_token().loc.end;

                        field_inits.push(ASTEnumStructVariantFieldInit {
                            name: ident.clone(),
                            value: ident_expr,
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        });
                    } else {
                        self.expect_current(TokenKind::Colon)?;

                        let value = self.parse_expr(Precedence::Lowest)?;
                        self.next_token();

                        let end = self.current_token().loc.end;

                        field_inits.push(ASTEnumStructVariantFieldInit {
                            name: ident.clone(),
                            value,
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        });
                    }

                    if self.current_token_is(TokenKind::RightBrace) {
                        break;
                    } else {
                        self.expect_current(TokenKind::Comma)?;
                    }
                }
                _ => {
                    return Err(self.error_invalid_token());
                }
            }
        }

        Ok(field_inits)
    }

    fn parse_enum_struct_variant_init(
        &mut self,
        operand: ASTExpr,
        ident: Ident,
        line: usize,
        column: usize,
        start: usize,
    ) -> Result<ASTExpr, Diag> {
        self.expect_current(TokenKind::LeftBrace)?;

        let field_inits = self.parse_enum_struct_variant_field_inits(column)?;

        let end = self.current_token().loc.end;

        Ok(ASTExpr::EnumStructVariantInit(ASTEnumStructVariantInit {
            operand: Box::new(operand),
            ident,
            field_inits,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_unnamed_enum_value(&mut self) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume dot

        let ident = self.parse_ident()?;

        if self.peek_token_is(TokenKind::LeftParen) {
            self.next_token(); // consume ident
            let values = self.parse_expr_series(TokenKind::RightParen)?;

            let end = self.current_token().loc.end;

            Ok(ASTExpr::UnnamedEnumValue(ASTUnnamedEnumValueExpr {
                ident,
                kind: UnnamedEnumValueKind::Tuple(values),
                loc: Loc::new(self.file_id(), line, column, start, end),
            }))
        } else if self.peek_token_is(TokenKind::LeftBrace) {
            self.next_token(); // consume ident 

            self.expect_current(TokenKind::LeftBrace)?;
            let field_inits = self.parse_enum_struct_variant_field_inits(column)?;

            let end = self.current_token().loc.end;

            Ok(ASTExpr::UnnamedEnumValue(ASTUnnamedEnumValueExpr {
                ident,
                kind: UnnamedEnumValueKind::Struct(field_inits),
                loc: Loc::new(self.file_id(), line, column, start, end),
            }))
        } else {
            let end = self.current_token().loc.end;

            Ok(ASTExpr::UnnamedEnumValue(ASTUnnamedEnumValueExpr {
                ident,
                kind: UnnamedEnumValueKind::Plain,
                loc: Loc::new(self.file_id(), line, column, start, end),
            }))
        }
    }

    fn parse_unnamed_struct_value(&mut self, repr_attr: Option<ReprAttr>) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume struct

        let align = self.parse_align_specifier()?;

        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<UnnamedStructValueField> = Vec::new();

        loop {
            match self.current_token().kind {
                TokenKind::RightBrace => {
                    break;
                }
                TokenKind::EOF => {
                    return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
                }
                TokenKind::Ident(_) => {
                    let start = self.current_token().loc.start;
                    let line = self.current_token().loc.line;
                    let ident = self.parse_ident()?;

                    self.next_token(); // consume ident

                    if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
                        // syntax shorthand for `ident:ident`
                        let ident_expr = ASTExpr::Ident(ident.clone());

                        let end = self.current_token().loc.end;

                        fields.push(UnnamedStructValueField {
                            name: ident.clone(),
                            value: Box::new(ident_expr),
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        });
                    } else {
                        self.expect_current(TokenKind::Colon)?;

                        let field_value = self.parse_expr(Precedence::Lowest)?;
                        self.next_token();

                        let end = self.current_token().loc.end;

                        fields.push(UnnamedStructValueField {
                            name: ident.clone(),
                            value: Box::new(field_value),
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        });
                    }

                    if self.current_token_is(TokenKind::RightBrace) {
                        break;
                    } else {
                        self.expect_current(TokenKind::Comma)?;
                    }
                }
                _ => {
                    return Err(self.error_invalid_token());
                }
            }
        }

        let end = self.current_token().loc.end;

        Ok(ASTExpr::UnnamedStructValue(ASTUnnamedStructValueExpr {
            fields,
            repr_attr,
            align,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }

    fn parse_unnamed_union_value(&mut self) -> Result<ASTExpr, Diag> {
        let loc = self.current_token().loc;
        let (line, column, start) = (loc.line, loc.column, loc.start);

        self.next_token(); // consume union

        self.expect_current(TokenKind::LeftBrace)?;

        let mut fields: Vec<UnnamedUnionValueField> = Vec::new();

        loop {
            match self.current_token().kind {
                TokenKind::RightBrace => {
                    break;
                }
                TokenKind::EOF => {
                    return Err(self.error_at_current(ParserDiagKind::MissingClosingBrace));
                }
                TokenKind::Ident(_) => {
                    let start = self.current_token().loc.start;
                    let line = self.current_token().loc.line;
                    let ident = self.parse_ident()?;

                    self.next_token(); // consume ident

                    if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
                        // syntax shorthand for `ident:ident`
                        let ident_expr = ASTExpr::Ident(ident.clone());

                        let end = self.current_token().loc.end;

                        fields.push(UnnamedUnionValueField {
                            name: ident.clone(),
                            value: Box::new(ident_expr),
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        });
                    } else {
                        self.expect_current(TokenKind::Colon)?;

                        let field_value = self.parse_expr(Precedence::Lowest)?;
                        self.next_token();

                        let end = self.current_token().loc.end;

                        fields.push(UnnamedUnionValueField {
                            name: ident.clone(),
                            value: Box::new(field_value),
                            loc: Loc::new(self.file_id(), line, column, start, end),
                        });
                    }

                    if self.current_token_is(TokenKind::RightBrace) {
                        break;
                    } else {
                        self.expect_current(TokenKind::Comma)?;
                    }
                }
                _ => {
                    return Err(self.error_invalid_token());
                }
            }
        }

        let end = self.current_token().loc.end;

        Ok(ASTExpr::UnnamedUnionValue(ASTUnnamedUnionValueExpr {
            fields,
            loc: Loc::new(self.file_id(), line, column, start, end),
        }))
    }
}

#[inline]
fn map_assign_kind(token_kind: TokenKind) -> Option<AssignKind> {
    match token_kind {
        TokenKind::Plus => Some(AssignKind::AddAssign),
        TokenKind::Minus => Some(AssignKind::SubAssign),
        TokenKind::Asterisk => Some(AssignKind::MulAssign),
        TokenKind::Slash => Some(AssignKind::DivAssign),
        TokenKind::Percent => Some(AssignKind::ModAssign),
        TokenKind::Ampersand => Some(AssignKind::BitwiseAndAssign),
        TokenKind::Tilde => Some(AssignKind::BitwiseXorAssign),
        TokenKind::AmpTilde => Some(AssignKind::BitwiseAndNotAssign),
        TokenKind::ShiftLeft => Some(AssignKind::LeftShiftAssign),
        TokenKind::ShiftRight => Some(AssignKind::RightShiftAssign),
        _ => None,
    }
}
