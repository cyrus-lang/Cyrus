use super::errors::CompileTypeErrorType;
use ast::token::TokenKind;
use core::fmt;

#[derive(Debug, Clone)]
pub enum ParserErrorType {
    UnexpectedToken(TokenKind, TokenKind),
    InvalidTypeToken(TokenKind),
    InvalidToken(TokenKind),
    MissingClosingBrace,
    MissingOpeningBrace,
    MissingClosingParen,
    MissingOpeningParen,
    ExpectedIdentifier,
    MissingSemicolon,
    MissingComma,
    IncompleteConditionalForLoop,
}

impl fmt::Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserErrorType::UnexpectedToken(_, _) => write!(f, "UnexpectedToken"),
            ParserErrorType::InvalidTypeToken(_) => write!(f, "InvalidTypeToken"),
            ParserErrorType::InvalidToken(_) => write!(f, "InvalidToken"),
            ParserErrorType::MissingClosingBrace => write!(f, "MissingClosingBrace"),
            ParserErrorType::MissingOpeningBrace => write!(f, "MissingOpeningBrace"),
            ParserErrorType::MissingClosingParen => write!(f, "MissingClosingParen "),
            ParserErrorType::MissingOpeningParen => write!(f, "MissingOpeningParen "),
            ParserErrorType::ExpectedIdentifier => write!(f, "ExpectedIdentifier"),
            ParserErrorType::MissingSemicolon => write!(f, "MissingSemicolon"),
            ParserErrorType::MissingComma => write!(f, "MissingComma"),
            ParserErrorType::IncompleteConditionalForLoop => write!(f, "IncompleteConditionalForLoop"),
        }
    }
}

impl CompileTypeErrorType for ParserErrorType {
    fn context(&self) -> String {
        match self {
            ParserErrorType::UnexpectedToken(current, expected) => {
                format!("Expected token '{}' but got '{}'", expected, current)
            }
            ParserErrorType::InvalidToken(token_kind) => {
                format!("Unexpected token: '{}'", token_kind)
            }
            ParserErrorType::InvalidTypeToken(token_kind) => {
                format!(
                    "Expected one of the following type tokens: \n
                    'i8', 'i16', 'i32', 'i64', 'i128', 'u8', 'u16', 'u32', 'u64', 'u128', \n
                    'float', 'double', 'size_t', 'char', 'bool', 'void', 'string', '*', '&', or an identifier \n
                    (e.g., user-defined type), but got '{}'",
                    token_kind
                )
            }
            ParserErrorType::MissingClosingBrace => format!("Missing closing brace '}}'"),
            ParserErrorType::MissingOpeningBrace => format!("Missing opening brace '{{'"),
            ParserErrorType::MissingClosingParen => format!("Missing closing paren ')'"),
            ParserErrorType::MissingOpeningParen => format!("Missing opening paren '('"),
            ParserErrorType::ExpectedIdentifier => format!("Expected an identifier."),
            ParserErrorType::MissingSemicolon => format!("Missing semicolon."),
            ParserErrorType::MissingComma => format!("Missing comma."),
            ParserErrorType::IncompleteConditionalForLoop => {
                format!(
                    "Defined a conditional for loop with incomplete condition. \n
                    Mind to add a condition to current for loop or change it into unconditional for loop."
                )
            }
        }
    }
}
