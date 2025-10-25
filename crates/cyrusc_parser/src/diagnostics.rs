use ast::token::TokenKind;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ParserDiagKind {
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
    SeveralSelfModifierDefinition,
    InvalidPrefixOperator(TokenKind),
    InvalidInfixOperator(TokenKind),
    InvalidAssignOperator(TokenKind),
    NonArrayDataTypeForArrayConstruction,
    SingleElementTupleType,
    IncompleteVariableDeclaration,
    ExpectedIntegerLiteral(TokenKind)
}

impl fmt::Display for ParserDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserDiagKind::ExpectedIntegerLiteral(token_kind) => {
                write!(f, "Expected integer literal, but found '{}'", token_kind)
            }
            ParserDiagKind::SingleElementTupleType => {
                write!(f, "Tuple type must contain at least two elements.")
            }
            ParserDiagKind::UnexpectedToken(current, expected) => {
                write!(f, "Expected token '{}' but got '{}'.", expected, current)
            }
            ParserDiagKind::ExpectedToken(expected) => {
                write!(f, "Expected token '{}'.", expected)
            }
            ParserDiagKind::InvalidToken(token_kind) => {
                write!(f, "Unexpected token: '{}'.", token_kind)
            }
            ParserDiagKind::InvalidTypeToken(token_kind) => {
                write!(f, "Expected type token but got '{}'.", token_kind)
            }
            ParserDiagKind::MissingClosingBrace => write!(f, "Missing closing brace '}}'."),
            ParserDiagKind::MissingOpeningBrace => write!(f, "Missing opening brace '{{'."),
            ParserDiagKind::MissingClosingParen => write!(f, "Missing closing paren ')'."),
            ParserDiagKind::MissingOpeningParen => write!(f, "Missing opening paren '('."),
            ParserDiagKind::ExpectedIdentifier => write!(f, "Expected an identifier."),
            ParserDiagKind::MissingSemicolon => write!(f, "Missing semicolon."),
            ParserDiagKind::MissingComma => write!(f, "Missing comma."),
            ParserDiagKind::IncompleteConditionalForLoop => {
                write!(
                    f,
                    "Defined a conditional for loop with incomplete condition. Consider to add a condition to current for loop or change it into unconditional for loop."
                )
            }
            ParserDiagKind::InvalidUntypedArrayConstructor => {
                write!(
                    f,
                    "If untyped array constructor would not have an item, consider to remove it."
                )
            }
            ParserDiagKind::SeveralSelfModifierDefinition => {
                write!(f, "Cannot define self modifier several times in a function.")
            }
            ParserDiagKind::ExpectedSelfModifier(name) => {
                write!(f, "Self modifier identifier must be 'self' not '{}'.", name)
            }
            ParserDiagKind::InvalidInfixOperator(token_kind) => {
                write!(f, "Invalid infix operator '{}'.", token_kind)
            }
            ParserDiagKind::InvalidPrefixOperator(token_kind) => {
                write!(f, "Invalid prefix operator '{}'.", token_kind)
            }
            ParserDiagKind::InvalidAssignOperator(token_kind) => {
                write!(f, "Invalid assign operator '{}'.", token_kind)
            }
            ParserDiagKind::NonArrayDataTypeForArrayConstruction => {
                write!(f, "Cannot use non-array type for array construction.")
            }
            ParserDiagKind::IncompleteVariableDeclaration => {
                write!(
                    f,
                    "Variable declaration requires an explicit type or an initializer expression."
                )
            }
        }
    }
}
