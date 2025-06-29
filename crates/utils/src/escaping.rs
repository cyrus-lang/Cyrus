use std::fmt;

#[derive(Debug, PartialEq)]
pub enum UnescapeError {
    IncompleteHexEscape,
    InvalidHexEscape(String),
    InvalidOctalEscape(String),
    TrailingBackslash,
}

impl fmt::Display for UnescapeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IncompleteHexEscape => write!(f, "Incomplete hex escape: \\x was not followed by two hex digits"),
            Self::InvalidHexEscape(hex) => write!(f, "Invalid hex escape sequence: \\x{}", hex),
            Self::InvalidOctalEscape(oct) => write!(f, "Invalid octal escape sequence: \\{}", oct),
            Self::TrailingBackslash => write!(f, "Unexpected end of input after a backslash"),
        }
    }
}

impl std::error::Error for UnescapeError {}

pub fn unescape_string(input: &str) -> Result<String, UnescapeError> {
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('b') => result.push('\x08'), // backspace
                Some('a') => result.push('\x07'), // bell
                Some('v') => result.push('\x0B'), // vertical tab
                Some('f') => result.push('\x0C'), // form feed
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('\'') => result.push('\''),

                // Hex escape like \x1B
                Some('x') => {
                    let h1 = chars.next();
                    let h2 = chars.next();

                    if let (Some(h1), Some(h2)) = (h1, h2) {
                        let hex = format!("{}{}", h1, h2);
                        if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                            result.push(byte as char);
                        } else {
                            return Err(UnescapeError::InvalidHexEscape(hex));
                        }
                    } else {
                        return Err(UnescapeError::IncompleteHexEscape);
                    }
                }

                // Octal escape like \033
                Some(d @ '0'..='7') => {
                    let mut oct = String::new();
                    oct.push(d);

                    // Read up to 2 more octal digits
                    for _ in 0..2 {
                        if let Some(c @ '0'..='7') = chars.peek() {
                             oct.push(*c);
                             chars.next(); // consume the peeked char
                        } else {
                            break;
                        }
                    }

                    if let Ok(byte) = u8::from_str_radix(&oct, 8) {
                        result.push(byte as char);
                    } else {
                        return Err(UnescapeError::InvalidOctalEscape(oct));
                    }
                }

                // Unrecognized escape sequence — push literally
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }

                None => return Err(UnescapeError::TrailingBackslash),
            }
        } else {
            result.push(c);
        }
    }

    Ok(result)
}

pub fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len()); // Pre-allocate for efficiency
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\t' => result.push_str("\\t"),
            '\r' => result.push_str("\\r"),
            '\x08' => result.push_str("\\b"), // backspace
            '\x07' => result.push_str("\\a"), // bell
            '\x0B' => result.push_str("\\v"), // vertical tab
            '\x0C' => result.push_str("\\f"), // form feed
            '\\' => result.push_str("\\\\"),
            '\"' => result.push_str("\\\""),
            '\'' => result.push_str("\\'"),
            _ => result.push(c),
        }
    }
    result
}

pub fn saturating_sub(value: usize, input: usize) -> usize {
    value.saturating_sub(input)
}

pub fn spaces(n: usize) -> String {
    " ".repeat(n)
}

