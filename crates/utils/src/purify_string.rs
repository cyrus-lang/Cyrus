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