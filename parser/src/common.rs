use std::vec;

use crate::ParseError;
use crate::Parser;
use ast::ast::*;
use ast::token::*;
use utils::compile_time_errors::errors::CompileTimeError;
use utils::compile_time_errors::parser_errors::ParserErrorType;

impl<'a> Parser<'a> {
    pub fn parse_from_package(&mut self) -> Result<FromPackage, ParseError> {
        let start = self.current_token.span.start;

        let mut identifier: Identifier = Identifier {
            name: String::from("<UB>"),
            span: Span::default(),
            loc: Location::default(),
        };
        let mut sub_packages: Vec<PackagePath> = match self.current_token.kind.clone() {
            TokenKind::Identifier { name } => vec![PackagePath {
                package_name: Identifier {
                    name,
                    span: self.current_token.span.clone(),
                    loc: self.current_location(),
                },
                span: self.current_token.span.clone(),
                loc: self.current_location(),
            }],
            _ => {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::ExpectedIdentifier,
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(self.lexer.select(start..self.current_token.span.end)),
                    verbose: None,
                    caret: true,
                })
            }
        };
        self.next_token();

        while self.current_token_is(TokenKind::DoubleColon) {
            self.next_token();

            match self.current_token.kind.clone() {
                TokenKind::Identifier { name } => {
                    if !self.peek_token_is(TokenKind::DoubleColon) {
                        identifier = Identifier {
                            name,
                            span: self.current_token.span.clone(),
                            loc: self.current_location(),
                        };
                        break;
                    }

                    sub_packages.push(PackagePath {
                        package_name: Identifier {
                            name,
                            span: self.current_token.span.clone(),
                            loc: self.current_location(),
                        },
                        span: self.current_token.span.clone(),
                        loc: self.current_location(),
                    });
                }
                _ => {
                    break;
                }
            }

            self.next_token();
        }

        if sub_packages.len() == 1 {
            return Ok(FromPackage {
                identifier: sub_packages[0].package_name.clone(),
                sub_packages: vec![],
                span: Span {
                    start,
                    end: self.current_token.span.end,
                },
                loc: self.current_location(),
            });
        }

        Ok(FromPackage {
            identifier,
            sub_packages,
            span: Span {
                start,
                end: self.current_token.span.end,
            },
            loc: self.current_location(),
        })
    }

    // The get_type_token function is responsible for parsing a type token from the source code and returning its corresponding TokenKind.
    // This function supports both primitive types (e.g., integers, floating points, booleans) and user-defined types.
    // Additionally, it handles pointer types by recognizing dereference (*) and reference (&) symbols.
    pub fn get_type_token(&mut self) -> Result<TokenKind, ParseError> {
        let location = self.current_location();

        let data_type = match self.current_token.kind.clone() {
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
            | TokenKind::Float
            | TokenKind::Double
            | TokenKind::SizeT
            | TokenKind::Char
            | TokenKind::Bool
            | TokenKind::Void
            | TokenKind::String => self.current_token.kind.clone(),
            token_kind => match token_kind {
                TokenKind::Asterisk => {
                    self.next_token();
                    return Ok(TokenKind::Dereference(Box::new(self.get_type_token()?)));
                }
                TokenKind::Ampersand => {
                    self.next_token();
                    return Ok(TokenKind::Dereference(Box::new(self.get_type_token()?)));
                }
                TokenKind::Identifier { name: type_name } => TokenKind::UserDefinedType(Identifier {
                    name: type_name.clone(),
                    span: self.current_token.span.clone(),
                    loc: self.current_location(),
                }),
                _ => {
                    return Err(CompileTimeError {
                        location,
                        etype: ParserErrorType::InvalidTypeToken(token_kind.clone()),
                        file_name: Some(self.lexer.file_name.clone()),
                        code_raw: Some(
                            self.lexer
                                .select(self.current_token.span.start..self.current_token.span.end),
                        ),
                        verbose: None,
                        caret: true,
                    })
                }
            },
        };

        self.next_token(); // consume the data type
        Ok(data_type)
    }

    // The parse_type_token function extends get_type_token by also handling array type parsing.
    // It checks if the parsed type is followed by square brackets ([]), which indicate an array type.
    // If brackets are present, it collects the array dimensions, including possible explicit capacities.
    pub fn parse_type_token(&mut self) -> Result<TokenKind, ParseError> {
        let data_type = self.get_type_token()?;

        // Check for array data type
        if self.current_token_is(TokenKind::LeftBracket) {
            let mut dimensions: Vec<Option<TokenKind>> = Vec::new();

            while self.current_token_is(TokenKind::LeftBracket) {
                let mut capacity: Option<TokenKind> = None;

                self.next_token(); // consume left bracket

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

    /// Parses a visibility or type modifier token and returns its corresponding `VisType`.
    ///
    /// This function checks if the given token represents a valid function visibility or type modifier
    /// (e.g., `Inline`, `Extern`, or `Pub`) and converts it to the appropriate `VisType`.
    /// If the token is invalid, it returns a parsing error.
    pub fn parse_vis_type(&mut self, token: Token) -> Result<VisType, ParseError> {
        let vis_type = match token.kind {
            TokenKind::Inline => VisType::Inline,
            TokenKind::Extern => VisType::Extern,
            TokenKind::Pub => VisType::Pub,
            _ => {
                return Err(CompileTimeError {
                    location: self.current_location(),
                    etype: ParserErrorType::InvalidToken(self.current_token.kind.clone()),
                    file_name: Some(self.lexer.file_name.clone()),
                    code_raw: Some(
                        self.lexer
                            .select(self.current_token.span.start..self.current_token.span.end),
                    ),
                    verbose: Some(String::from(
                        "Expected one of: 'inline', 'extern', 'pub' as function visibility.",
                    )),
                    caret: true,
                })
            }
        };
        self.next_token(); // consume vis_type token
        Ok(vis_type)
    }
}
