use crate::ParseError;
use crate::Parser;
use ast::ast::*;
use ast::token::*;
use utils::compile_time_errors::errors::CompileTimeError;
use utils::compile_time_errors::parser_errors::ParserErrorType;

impl<'a> Parser<'a> {
    /// Parses a type token and returns its corresponding `TokenKind`.
    ///
    /// This function handles primitive types (e.g., `float`, `double`), user-defined types,
    /// and composite types such as pointers, references, and arrays. If the type token
    /// is invalid, it returns a parsing error.
    ///
    /// # Behavior
    /// - For primitive types (e.g., `float`, `bool`, `string`), it directly returns the corresponding `TokenKind`.
    /// - For pointers (`*`) and references (`&`), it recursively parses the type they point to or reference.
    /// - For user-defined types, it wraps the identifier in a `TokenKind::UserDefinedType`.
    /// - For array types (e.g., `type[]` or `type[capacity]`), it collects the dimensions and creates a `TokenKind::Array`.
    ///
    /// # Returns
    /// - `Ok(TokenKind)`: The parsed type token.
    /// - `Err(ParseError)`: If the token is not a valid type or if there is a syntax error in the array type definition.
    ///
    /// # Errors
    /// - Returns an error if an invalid type is encountered or if brackets for arrays are mismatched.
    pub fn parse_type_token(&mut self) -> Result<TokenKind, ParseError> {
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
