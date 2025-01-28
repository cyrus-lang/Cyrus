use super::errors::CompileTypeErrorType;
use core::fmt;

pub enum ParserErrorType {}

impl fmt::Display for ParserErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl CompileTypeErrorType for ParserErrorType {
    fn context(&self) -> String {
        todo!()
    }
}
