use ast::token::Location;
use core::fmt;
use std::fmt::{Debug, Display};

pub const ERROR_PIPE_STR: &str = "-------------------------";

pub trait CompileTypeErrorType: Display + Debug {
    fn context(&self) -> String;
}
#[derive(Debug, Clone)]
pub struct CompileTimeError<ErrorType: CompileTypeErrorType> {
    pub etype: ErrorType,
    pub file_name: Option<String>,
    pub location: Location,
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

        write!(f, "| At: {}:{}\n\n", self.location.line, self.location.column)?;

        if let Some(code_raw) = &self.code_raw {
            let code_raw = code_raw.split("\n");

            for (idx, line) in code_raw.clone().into_iter().enumerate() {
                write!(f, "{}", line)?;

                if idx == code_raw.clone().count() {
                    write!(f, "\n")?;
                }
            }
        }

        if let Some(v) = &self.verbose {
            write!(f, " // {}", v.trim())?;
        }

        write!(f, "\n")?;
        write!(f, "\t")?;

        for _ in 0..self.location.column {
            write!(f, " ")?;
        }

        if self.caret {
            write!(f, "^ {}\n", self.etype.context().trim())?;
        }

        write!(f, "")
    }
}
