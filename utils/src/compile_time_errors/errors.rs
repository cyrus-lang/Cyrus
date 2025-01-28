use core::fmt;
use std::fmt::Display;

pub const ERROR_PIPE_STR: &str = "-------------------------";

pub trait CompileTypeErrorType: Display {
    fn context(&self) -> String;
}

pub struct CompileTimeError<ErrorType: CompileTypeErrorType> {
    pub etype: ErrorType,
    pub file_name: Option<String>,
    pub line: usize,
    pub column: usize,
    pub code_raw: Option<String>,
    pub verbose: Option<String>,
    pub caret: bool,
}

impl<ErrorType: CompileTypeErrorType> CompileTimeError<ErrorType> {
    pub fn print(&self) {
        println!("{}", self.to_string());
    }
}

impl<ErrorType: CompileTypeErrorType> fmt::Display for CompileTimeError<ErrorType> {
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

// FIXME

#[macro_export]
macro_rules! compiler_error {
    ($s:expr) => {{
        println!("(compiler) cyrus: {}", $s);
        std::process::exit(1);
    }};
}

#[macro_export]
macro_rules! lexer_error {
    ($s:expr) => {{
        println!("(lexer) cyrus: {}", $s);
        std::process::exit(1);
    }};
}
