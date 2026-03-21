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
use cyrusc_tokens::literals::Literal;
use cyrusc_tokens::literals::LiteralKind;

impl<'diag, 'source_file> Parser<'diag, 'source_file> {
    pub(crate) fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr, Diag> {
        let mut lhs_start = self.current_token().loc.start;
        let mut lhs_line = self.current_token().loc.line;
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

                return Ok(Expr::Unary(UnaryExpr {
                    operand: Box::new(lhs),
                    op: UnaryOperator::PostIncrement,
                    loc: Loc::new(self.file_id(), lhs_line, lhs_start, end),
                }));
            } else if self.peek_token_is(TokenKind::Decrement) {
                self.next_token();

                let end = self.current_token().loc.end;

                return Ok(Expr::Unary(UnaryExpr {
                    operand: Box::new(lhs),
                    op: UnaryOperator::PostDecrement,
                    loc: Loc::new(self.file_id(), lhs_line, lhs_start, end),
                }));
            }

            // infix handling (respect precedence)
            let peek_prec = token_precedence_of(self.peek_token().kind);
            if self.peek_token().kind != TokenKind::EOF && precedence < peek_prec {
                match self.parse_infix_expr(lhs.clone(), lhs_line, lhs_start) {
                    Some(infix) => {
                        lhs = infix?;
                        if let Expr::Infix(infix_expr) = lhs.clone() {
                            lhs_start = infix_expr.loc.start;
                            lhs_line = infix_expr.loc.line;
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

    pub(crate) fn parse_module_import(&mut self) -> Result<ModuleImport, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        let mut segments = {
            let end = self.current_token().loc.end;

            match &self.current_token().kind {
                TokenKind::Ident(ident) => {
                    vec![ModuleSegment::SubModule(Ident {
                        value: ident.clone(),
                        loc: Loc::new(self.file_id(), line, start, end),
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

            return Ok(ModuleImport {
                segments,
                loc: Loc::new(self.file_id(), line, start, end),
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

        Ok(ModuleImport {
            segments,
            loc: Loc::new(self.file_id(), line, start, end),
        })
    }

    pub(crate) fn parse_module_path(&mut self) -> Result<ModulePath, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

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
            loc: Loc::new(self.file_id(), line, start, end),
        })
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        let expr = match &self.current_token().kind {
            TokenKind::At => {
                // return Ok(Expr::Builtin(self.parse_builtin()?));
                Expr::Builtin(self.parse_builtin()?)
            }
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
                        loc: Some(Loc::new(self.file_id(), line, start, end)),
                        hint: None,
                    });
                } else {
                    return Err(self.error_invalid_token());
                }
            }
            TokenKind::Struct => self.parse_unnamed_struct_value(None)?,
            TokenKind::Union => self.parse_unnamed_union_value(false)?,
            TokenKind::Dot => self.parse_unnamed_enum_value()?,
            TokenKind::Dynamic => self.parse_dynamic_expr()?,
            TokenKind::Inline => {
                if self.peek_token_is(TokenKind::Function) {
                    self.next_token();
                    self.parse_lambda_expr(true)?
                } else {
                    return Err(self.error_invalid_token());
                }
            }
            TokenKind::Function => self.parse_lambda_expr(false)?,
            TokenKind::Ident { .. } => {
                let module_import = self.parse_module_import()?;

                if self.current_token_is(TokenKind::LeftBrace) {
                    Expr::StructInit(self.parse_struct_init(module_import, None)?)
                } else {
                    Expr::ModuleImport(module_import)
                }
            }
            TokenKind::Ampersand => {
                self.next_token();

                let end = self.current_token().loc.end;

                Expr::AddrOf(AddrOf {
                    expr: Box::new(self.parse_expr(Precedence::Prefix)?),
                    loc: Loc::new(self.file_id(), line, start, end),
                })
            }
            TokenKind::Asterisk => {
                self.next_token();

                let end = self.current_token().loc.end;

                Expr::Deref(Deref {
                    expr: Box::new(self.parse_expr(Precedence::Prefix)?),
                    loc: Loc::new(self.file_id(), line, start, end),
                })
            }
            TokenKind::Null => {
                let end = self.current_token().loc.end;

                Expr::Literal(Literal {
                    kind: LiteralKind::Null,
                    loc: Loc::new(self.file_id(), line, start, end),
                })
            }
            bool_token @ TokenKind::True | bool_token @ TokenKind::False => {
                let value = match bool_token {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => unreachable!(),
                };

                let end = self.current_token().loc.end;

                Expr::Literal(Literal {
                    kind: LiteralKind::Bool(value),
                    loc: Loc::new(self.file_id(), line, start, end),
                })
            }
            TokenKind::Literal(value) => Expr::Literal(value.clone()),
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

                Expr::Prefix(PrefixExpr {
                    op: prefix_operator,
                    operand: Box::new(expr),
                    loc: Loc::new(self.file_id(), line, start, end),
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
            _ => {
                let type_specifier = self.parse_type_specifier()?;

                if self.peek_token_is(TokenKind::LeftBrace) {
                    self.next_token();
                    return self.parse_array(type_specifier);
                }

                Expr::TypeSpecifier(type_specifier)
            }
        };

        let mut type_args_opt: Option<Vec<TypeArg>> = None;

        let type_arg_start_detail = self.is_type_arg_start(expr.clone());
        if type_arg_start_detail.includes_type_args {
            if !type_arg_start_detail.is_array_init {
                self.next_token(); // consume current token of expr
                type_args_opt = Some(self.parse_type_arg_list()?);
            } else {
                // handle generic array init
                let type_specifier = self.parse_type_specifier()?;
                self.next_token();
                return Ok(self.parse_array(type_specifier)?);
            }
        }

        if self.peek_token_is(TokenKind::LeftBrace) {
            if let Expr::ModuleImport(module_import) = expr.clone() {
                self.next_token(); // consume struct name

                let struct_init = self.parse_struct_init(module_import, type_args_opt)?;
                Ok(Expr::StructInit(struct_init))
            } else {
                return Err(self.error_invalid_token());
            }
        } else if self.peek_token_is(TokenKind::LeftParen) {
            return self.parse_func_call(expr, type_args_opt);
        } else {
            // type args are given to the wrong expression
            if type_args_opt.is_some() {
                return Err(self.error_invalid_token());
            }

            Ok(expr)
        }
    }

    fn parse_unnamed_enum_value(&mut self) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        self.next_token(); // consume dot

        let ident = self.parse_ident()?;

        if self.peek_token_is(TokenKind::LeftParen) {
            self.next_token(); // consume ident
            let field_values = self.parse_expr_series(TokenKind::RightParen)?;

            let end = self.current_token().loc.end;

            Ok(Expr::UnnamedEnumValue(UnnamedEnumValue {
                ident,
                kind: UnnamedEnumValueKind::Fielded(field_values),
                loc: Loc::new(self.file_id(), line, start, end),
            }))
        } else {
            let end = self.current_token().loc.end;

            Ok(Expr::UnnamedEnumValue(UnnamedEnumValue {
                ident,
                kind: UnnamedEnumValueKind::Plain,
                loc: Loc::new(self.file_id(), line, start, end),
            }))
        }
    }

    fn parse_dynamic_expr(&mut self) -> Result<Expr, Diag> {
        let start = self.current_token().loc.end;
        let line = self.current_token().loc.line;

        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;

        let end = self.current_token().loc.end;

        Ok(Expr::Dynamic(Dynamic {
            operand: Box::new(expr),
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_tuple_value(&mut self, first_expr: Expr) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        let mut elements: Vec<Expr> = vec![first_expr];

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

        Ok(Expr::Tuple(TupleValue {
            elements,
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_lambda_expr(&mut self, inline: bool) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        self.next_token(); // consume function
        let params = self.parse_func_params()?;
        let return_type = self.parse_type_specifier()?;
        self.next_token(); // last token of return type

        let body = self.parse_block()?;

        let end = self.current_token().loc.end;

        Ok(Expr::Lambda(Lambda {
            params,
            body: Box::new(body),
            return_type,
            inline,
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_infix_expr(&mut self, lhs: Expr, lhs_line: usize, lhs_start: usize) -> Option<Result<Expr, Diag>> {
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
                        loc: Loc::new(self.file_id(), lhs_line, lhs_start, end),
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
                    let Some(assign_kind) = self.map_assign_kind(operator_token.clone()) else {
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

                Some(Ok(Expr::Infix(InfixExpr {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    loc: Loc::new(self.file_id(), lhs_line, lhs_start, end),
                })))
            }
            _ => None,
        }
    }

    pub(crate) fn parse_expr_series(&mut self, ending_token: TokenKind) -> Result<Vec<Expr>, Diag> {
        let mut series: Vec<Expr> = Vec::new();

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

    fn parse_method_call(
        &mut self,
        operand: Expr,
        method_name: Ident,
        is_fat_arrow: bool,
        type_args: Option<TypeArgs>,
        start: usize,
        line: usize,
    ) -> Result<Expr, Diag> {
        self.must_be_left_paren()?;

        let args = self.parse_expr_series(TokenKind::RightParen)?;
        self.must_be_right_paren()?;

        let end = self.current_token().loc.end;

        Ok(Expr::MethodCall(MethodCall {
            is_fat_arrow,
            operand: Box::new(operand),
            method_name,
            type_args,
            args,
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_field_access(&mut self, operand: Expr) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        let is_fat_arrow = {
            if self.current_token_is(TokenKind::ThinArrow) {
                self.next_token();
                true
            } else if self.current_token_is(TokenKind::Dot) {
                self.next_token();
                false
            } else {
                return Err(self.error_invalid_token())?;
            }
        };

        if matches!(self.current_token().kind, TokenKind::Ident { .. }) {
            let ident = self.parse_ident()?;

            let mut type_args: Option<Vec<TypeArg>> = None;
            if self.is_type_arg_start(operand.clone()).includes_type_args {
                self.next_token(); // consume current token of expr
                type_args = Some(self.parse_type_arg_list()?);
            }

            if self.peek_token_is(TokenKind::LeftParen) {
                self.next_token(); // consume ident

                self.parse_method_call(operand, ident, is_fat_arrow, type_args, start, line)
            } else {
                let end = self.current_token().loc.end;

                Ok(Expr::FieldAccess(FieldAccess {
                    is_fat_arrow,
                    operand: Box::new(operand),
                    field_name: ident,
                    type_args,
                    loc: Loc::new(self.file_id(), line, start, end),
                }))
            }
        } else {
            let index = self.parse_integer_without_suffix()?;
            let end = self.current_token().loc.end;

            Ok(Expr::TupleAccess(TupleAccess {
                operand: Box::new(operand),
                index: index.try_into().unwrap(),
                loc: Loc::new(self.file_id(), line, start, end),
            }))
        }
    }

    fn parse_func_call(&mut self, operand: Expr, type_args: Option<TypeArgs>) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        self.expect_peek(TokenKind::LeftParen)?;

        let args = self.parse_expr_series(TokenKind::RightParen)?;
        self.must_be_right_paren()?;

        let end = self.current_token().loc.end;

        Ok(Expr::FuncCall(FuncCall {
            operand: Box::new(operand),
            args,
            type_args,
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_struct_init(
        &mut self,
        struct_name: ModuleImport,
        type_args: Option<TypeArgs>,
    ) -> Result<StructInit, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        let mut field_inits: Vec<FieldInit> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(StructInit {
                struct_name,
                field_inits,
                type_args,
                is_const: false,
                loc: Loc::new(self.file_id(), line, start, end),
            });
        }

        loop {
            let field_name = self.parse_ident()?;
            let field_loc = self.current_token().loc;

            self.next_token(); // consume ident

            if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
                // syntax shorthand for `ident:ident`
                let ident_expr = Expr::Ident(field_name.clone());

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

        Ok(StructInit {
            struct_name,
            field_inits,
            type_args,
            is_const: false,
            loc: Loc::new(self.file_id(), line, start, end),
        })
    }

    fn map_assign_kind(&mut self, token_kind: TokenKind) -> Option<AssignKind> {
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

    fn parse_assign(&mut self, lhs: Expr, kind: AssignKind, start: usize) -> Result<Expr, Diag> {
        let line = self.current_token().loc.line;

        self.expect_current(TokenKind::Assign)?;
        let rhs = self.parse_expr(Precedence::Lowest)?;

        let end = self.current_token().loc.end;

        Ok(Expr::Assign(Box::new(Assign {
            lhs,
            rhs,
            kind,
            loc: Loc::new(self.file_id(), line, start, end),
        })))
    }

    pub(crate) fn parse_single_array_index(&mut self) -> Result<Expr, Diag> {
        self.expect_current(TokenKind::LeftBracket)?;

        if self.current_token_is(TokenKind::RightBracket) {
            return Err(self.error_invalid_token());
        }

        let index = self.parse_expr(Precedence::Lowest)?;
        self.expect_peek(TokenKind::RightBracket)?;
        Ok(index)
    }

    fn parse_array_index(&mut self, expr: Expr) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        let index_expr = self.parse_single_array_index()?;
        let mut indexes_backup: Vec<Expr> = vec![index_expr.clone()];

        let mut base_index = {
            let end = self.current_token().loc.end;

            Expr::ArrayIndex(ArrayIndex {
                operand: Box::new(expr.clone()),
                index: Box::new(index_expr.clone()),
                loc: Loc::new(self.file_id(), line, start, end),
            })
        };

        // handle chained indices ident[a][b][c]
        if self.peek_token_is(TokenKind::LeftBracket) {
            self.next_token();
        }

        while self.current_token_is(TokenKind::LeftBracket) {
            let index_expr = self.parse_single_array_index()?;

            let end = self.current_token().loc.end;

            base_index = Expr::ArrayIndex(ArrayIndex {
                operand: Box::new(base_index),
                index: Box::new(index_expr.clone()),
                loc: Loc::new(self.file_id(), line, start, end),
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
                    Expr::Ident(ident) => TypeSpecifier::Ident(ident),
                    Expr::ModuleImport(module_import) => TypeSpecifier::ModuleImport(module_import),
                    Expr::TypeSpecifier(type_specifier) => type_specifier,
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
                    loc: Loc::new(self.file_id(), line, start, end),
                }
            };

            for index in &indexes_backup {
                let end = self.current_token().loc.end;

                base_array = ArrayType {
                    size: ArrayCapacity::Fixed(Box::new(index.clone())),
                    element_type: Box::new(TypeSpecifier::Array(base_array)),
                    loc: Loc::new(self.file_id(), line, start, end),
                };
            }

            // parse the initializer block
            self.next_token();
            let array_const = self.parse_array(TypeSpecifier::Array(base_array))?;

            return Ok(array_const);
        }

        Ok(base_index)
    }

    fn parse_untyped_array(&mut self) -> Result<Expr, Diag> {
        let line = self.current_token().loc.line;
        let start = self.current_token().loc.start;
        let elements = self.parse_expr_series(TokenKind::RightBrace)?;

        self.must_be_right_brace()?;

        let end = self.current_token().loc.end;

        Ok(Expr::UntypedArray(UntypedArray {
            elements,
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_array(&mut self, type_specifier: TypeSpecifier) -> Result<Expr, Diag> {
        let line = self.current_token().loc.line;
        let start = self.current_token().loc.start;

        if !matches!(type_specifier, TypeSpecifier::Array(..)) {
            return Err(self.error_at_current(ParserDiagKind::NonArrayDataTypeForArrayConstruction));
        }

        self.must_be_left_brace()?;

        let mut elements: Vec<Expr> = Vec::new();
        self.expect_current(TokenKind::LeftBrace)?;

        if self.current_token_is(TokenKind::RightBrace) {
            let end = self.current_token().loc.end;

            return Ok(Expr::Array(Array {
                elements,
                data_type: type_specifier,
                loc: Loc::new(self.file_id(), line, start, end),
            }));
        }

        loop {
            if self.current_token_is(TokenKind::LeftBrace) {
                let mut untyped_array: Vec<Expr> = Vec::new();
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

                if let TypeSpecifier::Array(inner_type_specifier, ..) = type_specifier.clone() {
                    let end = self.current_token().loc.end;

                    let data_type = TypeSpecifier::Array(ArrayType {
                        size: ArrayCapacity::Fixed(Box::new(Expr::Literal(Literal {
                            kind: LiteralKind::Integer(untyped_array.len().try_into().unwrap(), None),
                            loc: Loc::new(self.file_id(), line, start, end),
                        }))),
                        element_type: inner_type_specifier.element_type,
                        loc: Loc::new(self.file_id(), line, start, end),
                    });

                    elements.push(Expr::Array(Array {
                        data_type,
                        elements: untyped_array,
                        loc: Loc::new(self.file_id(), line, start, end),
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

        Ok(Expr::Array(Array {
            elements,
            data_type: type_specifier,
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_unnamed_struct_value(&mut self, repr_attr: Option<ReprAttr>) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

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
                        let ident_expr = Expr::Ident(ident.clone());

                        let end = self.current_token().loc.end;

                        fields.push(UnnamedStructValueField {
                            field_name: ident.clone(),
                            field_ty: None,
                            field_value: Box::new(ident_expr),
                            loc: Loc::new(self.file_id(), line, start, end),
                        });
                    } else {
                        let mut field_ty: Option<TypeSpecifier> = None;
                        if self.current_token_is(TokenKind::Colon) {
                            self.next_token();

                            let type_specifier = self.parse_type_specifier()?;
                            self.next_token();

                            field_ty = Some(type_specifier);
                        }

                        self.expect_current(TokenKind::Assign)?;
                        let field_value = self.parse_expr(Precedence::Lowest)?;
                        self.next_token();

                        let end = self.current_token().loc.end;

                        fields.push(UnnamedStructValueField {
                            field_name: ident.clone(),
                            field_ty,
                            field_value: Box::new(field_value),
                            loc: Loc::new(self.file_id(), line, start, end),
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

        Ok(Expr::UnnamedStructValue(UnnamedStructValue {
            fields,
            repr_attr,
            align,
            loc: Loc::new(self.file_id(), line, start, end),
        }))
    }

    fn parse_unnamed_union_value(&mut self, is_const: bool) -> Result<Expr, Diag> {
        let start = self.current_token().loc.start;
        let line = self.current_token().loc.line;

        self.next_token(); // consume union

        self.expect_current(TokenKind::LeftBrace)?;

        let ident = self.parse_ident()?;

        self.next_token(); // consume ident

        if self.current_token_is(TokenKind::Comma) || self.current_token_is(TokenKind::RightBrace) {
            // syntax shorthand for `ident:ident`
            let ident_expr = Expr::Ident(ident.clone());

            self.must_be_right_brace()?;

            let end = self.current_token().loc.end;

            return Ok(Expr::UnnamedUnionValue(UnnamedUnionValue {
                field_name: ident,
                field_value: Box::new(ident_expr),
                is_const,
                loc: Loc::new(self.file_id(), line, start, end),
            }));
        } else {
            self.expect_current(TokenKind::Assign)?;

            let field_value = self.parse_expr(Precedence::Lowest)?;
            self.next_token();

            self.must_be_right_brace()?;

            let end = self.current_token().loc.end;

            return Ok(Expr::UnnamedUnionValue(UnnamedUnionValue {
                field_name: ident,
                field_value: Box::new(field_value),
                is_const,
                loc: Loc::new(self.file_id(), line, start, end),
            }));
        }
    }
}
