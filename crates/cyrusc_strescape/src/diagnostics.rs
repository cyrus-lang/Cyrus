// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Error)]
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
