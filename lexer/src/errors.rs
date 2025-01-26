use core::fmt;

const ERROR_PIPE_STR: &str = "----------------------";

pub struct LexicalError {
    pub file_name: Option<String>,
    pub line: usize,
    pub column: usize,
    pub code_raw: Option<String>,
    pub etype: LexicalErrorType,
    pub verbose: Option<String>,
    pub caret: bool,
}

pub enum LexicalErrorType {
    UnterminatedStringLiteral,
    InvalidFloatLiteral,
    InvalidIntegerLiteral,
    UnterminatedMultiLineComment,
    EmptyCharLiteral,
}

impl fmt::Display for LexicalErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexicalErrorType::UnterminatedStringLiteral => write!(f, "UnterminatedStringLiteral"),
            LexicalErrorType::InvalidFloatLiteral => write!(f, "InvalidFloatLiteral"),
            LexicalErrorType::InvalidIntegerLiteral => write!(f, "InvalidIntegerLiteral"),
            LexicalErrorType::UnterminatedMultiLineComment => write!(f, "UnterminatedMultiLineComment"),
            LexicalErrorType::EmptyCharLiteral => write!(f, "EmptyCharLiteral"),
        }
    }
}

impl LexicalErrorType {
    pub fn context(&self) -> String {
        String::from(match self {
            LexicalErrorType::UnterminatedStringLiteral => "expected terminate string literal with double quote.",
            LexicalErrorType::InvalidFloatLiteral => "invalid float literal",
            LexicalErrorType::InvalidIntegerLiteral => "invalid integer literal",
            LexicalErrorType::UnterminatedMultiLineComment => "unterminated multi-line comment",
            LexicalErrorType::EmptyCharLiteral => "empty char literal is invalid",
        })
    }
}

impl LexicalError {
    pub fn print(&self) {
        println!("{}", self.to_string());
    }
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "| Error: {} {}\n", self.etype.to_string(), ERROR_PIPE_STR)?;

        if let Some(file_name) = &self.file_name {
            write!(f, "| Path: {}\n", file_name)?;
        }

        write!(f, "| At: {}:{}\n\n", self.line, self.column)?;

        if let Some(code_raw) = &self.code_raw {
            write!(f, "\t{}", code_raw)?;
        }

        if let Some(v) = &self.verbose {
            write!(f, " // {}", v)?;
        }

        write!(f, "\n")?;

        write!(f, "\t")?;
        for _ in 0..self.column {
            write!(f, " ")?;
        }

        if self.caret {
            write!(f, "^ {}\n", self.etype.context())?;
        }

        write!(f, "")
    }
}

pub fn lexer_invalid_char_error(file_name: String, line: usize, column: usize, ch: char) {
    println!("| Error: Lexination Failed {}", ERROR_PIPE_STR);
    println!("| Path: {}", file_name);
    println!("| At: {}:{}\n", line, column);
    println!(
        "\tLexical error at line {}, column {} because of invalid char '{}'.\n",
        line, column, ch
    );
}

pub fn lexer_unknown_char_error(file_name: String, line: usize, column: usize) {
    println!("| Error: Lexination Failed {}", ERROR_PIPE_STR);
    println!("| Path: {}", file_name);
    println!("| At: {}:{}\n", line, column);
    println!(
        "\tLexical error at line {}, column {} because of invalid char.\n",
        line, column
    );
}