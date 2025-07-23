use ast::token::TokenKind;
use core::fmt;
use diag::errors::CompileTypeErrorType;

#[derive(Debug, Clone)]
pub enum ParserErrorType {
    UnexpectedToken(TokenKind, TokenKind),
    ExpectedToken(TokenKind),
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
    InvalidUntypedArrayConstructor,
    ExpectedSelfModifier(String),
    MatrixDimensionMustBeAnInteger,
    SeveralSelfModifierDefinition,
}

impl fmt::Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserErrorType::UnexpectedToken(_, _) => write!(f, "UnexpectedToken"),
            ParserErrorType::ExpectedToken(_) => write!(f, "ExpectedToken"),
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
            ParserErrorType::InvalidUntypedArrayConstructor => write!(f, "InvalidUntypedArrayConstructor"),
            ParserErrorType::ExpectedSelfModifier(_) => write!(f, "ExpectedSelfModifier"),
            ParserErrorType::SeveralSelfModifierDefinition => write!(f, "SeveralSelfModifierDefinition"),
            ParserErrorType::MatrixDimensionMustBeAnInteger => write!(f, "MatrixDimensionMustBeAnInteger"),
        }
    }
}

impl CompileTypeErrorType for ParserErrorType {
    fn context(&self) -> String {
        match self {
            ParserErrorType::UnexpectedToken(current, expected) => {
                format!("Expected token '{}' but got '{}'", expected, current)
            }
            ParserErrorType::ExpectedToken(expected) => {
                format!("Expected token '{}'", expected)
            }
            ParserErrorType::InvalidToken(token_kind) => {
                format!("Unexpected token: '{}'", token_kind)
            }
            ParserErrorType::InvalidTypeToken(token_kind) => {
                format!("Expected type token but got '{}'", token_kind)
            }
            ParserErrorType::MissingClosingBrace => format!("Missing closing brace '}}'"),
            ParserErrorType::MissingOpeningBrace => format!("Missing opening brace '{{'"),
            ParserErrorType::MissingClosingParen => format!("Missing closing paren ')'"),
            ParserErrorType::MissingOpeningParen => format!("Missing opening paren '('"),
            ParserErrorType::ExpectedIdentifier => format!("Expected an identifier"),
            ParserErrorType::MissingSemicolon => format!("Missing semicolon"),
            ParserErrorType::MissingComma => format!("Missing comma"),
            ParserErrorType::IncompleteConditionalForLoop => {
                format!(
                    "Defined a conditional for loop with incomplete condition. \n
                    Consider to add a condition to current for loop or change it into unconditional for loop"
                )
            }
            ParserErrorType::InvalidUntypedArrayConstructor => {
                "If untyped array constructor would not have an item, consider to remove it.".to_string()
            }
            ParserErrorType::SeveralSelfModifierDefinition => {
                "Cannot define self modifier several times in a function.".to_string()
            }
            ParserErrorType::ExpectedSelfModifier(name) => {
                format!("Self modifier identifier must be 'self' not '{}'.", name)
            }
            ParserErrorType::MatrixDimensionMustBeAnInteger => {
                "Matrix dimensions must a value of type integer.".to_string()
            }
        }
    }
}
