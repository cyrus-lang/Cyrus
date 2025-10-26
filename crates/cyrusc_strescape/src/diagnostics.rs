use thiserror::Error;

#[derive(Debug, PartialEq, Error)]
pub enum UnescapeError {
    #[error("Incomplete hex escape: \\x was not followed by two hex digits.")]
    IncompleteHexEscape,

    #[error("Invalid hex escape sequence: '\\x{0}'.")]
    InvalidHexEscape(String),

    #[error("Invalid octal escape sequence: '\\{0}'.")]
    InvalidOctalEscape(String),

    #[error("Unexpected end of input after a backslash.")]
    TrailingBackslash,

    #[error("Invalid Unicode escape sequence: '\\u{0}';")]
    InvalidUnicodeEscape(String),

    #[error("Incomplete Unicode escape sequence: '\\u{0}'.")]
    IncompleteUnicodeEscape(String),

    #[error("Invalid Unicode code point: 'U+{0:X}'.")]
    InvalidUnicodeCodePoint(u32),
}