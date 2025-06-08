pub fn unescape_string(str: String) -> String {
    str.replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\r", "\r")
        .replace("\\b", r"\b")
        .replace("\\a", r"\a")
        .replace("\\v", r"\v")
        .replace("\\f", r"\f")
        .replace("\\'", r"\'")
        .replace("\\\"", "\"")
        .replace("\\'", "'")
        .replace("\\\\", "\\")
}

pub fn escape_string(input: &str) -> String {
    let mut result = String::new();
    for c in input.chars() {
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
    if input >= value {
        return 0;
    } else {
        return value.saturating_sub(input);
    }
}

pub fn spaces(n: usize) -> String {
    " ".repeat(n)
}
