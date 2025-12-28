use cyrusc_ast::token::TokenKind;
use cyrusc_diagcentral::DiagKind;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ParserDiagKind {
    #[error("{0}")]
    InvalidModifier(String),

    #[error("Invalid ABI: '{0}'.")]
    InvalidABI(String),

    #[error("Expected string literal.")]
    ExpectedStringLiteral,

    #[error("Expected integer literal, but found '{0}'.")]
    ExpectedIntegerLiteral(TokenKind),

    #[error("Tuple type must contain at least two elements.")]
    SingleElementTupleType,

    #[error("Expected token '{1}' but got '{0}'.")]
    UnexpectedToken(TokenKind, TokenKind),

    #[error("Expected token '{0}'.")]
    ExpectedToken(TokenKind),

    #[error("Unexpected token: '{0}'.")]
    InvalidToken(TokenKind),

    #[error("Expected type token but got '{0}'.")]
    InvalidTypeToken(TokenKind),

    #[error("Missing closing brace '}}'.")]
    MissingClosingBrace,

    #[error("Missing opening brace '{{'.")]
    MissingOpeningBrace,

    #[error("Missing closing paren ')'.")]
    MissingClosingParen,

    #[error("Missing opening paren '('.")]
    MissingOpeningParen,

    #[error("Expected an identifier.")]
    ExpectedIdentifier,

    #[error("Missing semicolon.")]
    MissingSemicolon,

    #[error("Missing comma.")]
    MissingComma,

    #[error("If untyped array constructor would not have an item, consider removing it.")]
    InvalidUntypedArrayConstructor,

    #[error("Cannot define self modifier several times in a function.")]
    SeveralSelfModifierDefinition,

    #[error("Self modifier identifier must be 'self' not '{0}'.")]
    ExpectedSelfModifier(String),

    #[error("Invalid infix operator '{0}'.")]
    InvalidInfixOperator(TokenKind),

    #[error("Invalid prefix operator '{0}'.")]
    InvalidPrefixOperator(TokenKind),

    #[error("Invalid assign operator '{0}'.")]
    InvalidAssignOperator(TokenKind),

    #[error("Cannot use non-array type for array construction.")]
    NonArrayDataTypeForArrayConstruction,

    #[error("Variable declaration requires an explicit type or an initializer expression.")]
    IncompleteVariableDeclaration,
}

impl DiagKind for ParserDiagKind {}
