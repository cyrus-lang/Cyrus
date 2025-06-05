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
