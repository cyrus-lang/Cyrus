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
use crate::diagnostics::UnescapeError;

pub mod diagnostics;

pub fn unescape_string(input: &str) -> Result<String, UnescapeError> {
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('b') => result.push('\x08'),
                Some('a') => result.push('\x07'),
                Some('v') => result.push('\x0B'),
                Some('f') => result.push('\x0C'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some('\'') => result.push('\''),
                // hex escape: \xNN
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
                // octal escape: \NNN
                Some(d @ '0'..='7') => {
                    let mut oct = String::new();
                    oct.push(d);
                    for _ in 0..2 {
                        if let Some(c @ '0'..='7') = chars.peek() {
                            oct.push(*c);
                            chars.next();
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

                // 16-bit Unicode: \uNNNN or variable-length \u{...}
                Some('u') => {
                    if let Some('{') = chars.peek() {
                        chars.next(); // consume '{'
                        let mut hex = String::new();
                        while let Some(&ch) = chars.peek() {
                            if ch == '}' {
                                break;
                            }
                            if ch.is_ascii_hexdigit() {
                                hex.push(ch);
                                chars.next();
                            } else {
                                return Err(UnescapeError::InvalidUnicodeEscape(hex));
                            }
                        }
                        if chars.next() != Some('}') {
                            return Err(UnescapeError::IncompleteUnicodeEscape(hex));
                        }
                        if let Ok(code) = u32::from_str_radix(&hex, 16) {
                            if let Some(ch) = std::char::from_u32(code) {
                                result.push(ch);
                            } else {
                                return Err(UnescapeError::InvalidUnicodeCodePoint(code));
                            }
                        } else {
                            return Err(UnescapeError::InvalidUnicodeEscape(hex));
                        }
                    } else {
                        // fixed-length \uNNNN
                        let mut hex = String::new();
                        for _ in 0..4 {
                            if let Some(ch) = chars.next() {
                                if ch.is_ascii_hexdigit() {
                                    hex.push(ch);
                                } else {
                                    return Err(UnescapeError::InvalidUnicodeEscape(hex));
                                }
                            } else {
                                return Err(UnescapeError::IncompleteUnicodeEscape(hex));
                            }
                        }
                        if let Ok(code) = u32::from_str_radix(&hex, 16) {
                            if let Some(ch) = std::char::from_u32(code) {
                                result.push(ch);
                            } else {
                                return Err(UnescapeError::InvalidUnicodeCodePoint(code));
                            }
                        } else {
                            return Err(UnescapeError::InvalidUnicodeEscape(hex));
                        }
                    }
                }

                // 32-bit Unicode: \UNNNNNNNN
                Some('U') => {
                    let mut hex = String::new();
                    for _ in 0..8 {
                        if let Some(ch) = chars.next() {
                            if ch.is_ascii_hexdigit() {
                                hex.push(ch);
                            } else {
                                return Err(UnescapeError::InvalidUnicodeEscape(hex));
                            }
                        } else {
                            return Err(UnescapeError::IncompleteUnicodeEscape(hex));
                        }
                    }
                    if let Ok(code) = u32::from_str_radix(&hex, 16) {
                        if let Some(ch) = std::char::from_u32(code) {
                            result.push(ch);
                        } else {
                            return Err(UnescapeError::InvalidUnicodeCodePoint(code));
                        }
                    } else {
                        return Err(UnescapeError::InvalidUnicodeEscape(hex));
                    }
                }
                Some(other) => {
                    // unrecognized escape sequence
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
    let mut result = String::with_capacity(s.len());
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
