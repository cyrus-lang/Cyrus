use ast::token::{Location, Span};
use colorized::{Color, Colors};
use console::user_attended;
use std::fmt::{Debug, Display};
use utils::{
    purify_string::{saturating_sub, spaces, unescape_string},
    tui::tui_error,
};

pub trait CompileTypeErrorType: Display + Debug {
    fn context(&self) -> String;
}
#[derive(Debug, Clone)]
pub struct CompileTimeError<ErrorType: CompileTypeErrorType> {
    pub etype: ErrorType,
    pub file_name: Option<String>,
    pub location: Location,
    pub source_content: Box<String>,
    pub verbose: Option<String>,
    pub caret: Option<Span>,
}

const PANEL_LENGTH: usize = 4;

impl<ErrorType: CompileTypeErrorType> CompileTimeError<ErrorType> {
    pub fn print(&self) {
        println!();

        let error_message = {
            if let Some(verbose) = self.verbose.clone() {
                verbose
            } else {
                self.etype.context()
            }
        };

        if let Some(file_name) = self.file_name.clone() {
            tui_error(error_message.to_lowercase());
            println!(
                "       --> {}:{}:{}",
                file_name, self.location.line, self.location.column
            );
            println!();
        }

        let mut starting_line = saturating_sub(self.location.line, PANEL_LENGTH);
        let source_content = unescape_string(*self.source_content.clone());
        let sources_lines: Vec<&str> = source_content.split("\n").collect();

        while starting_line < self.location.line + PANEL_LENGTH {
            if let Some(line_str) = sources_lines.get(starting_line) {
                if starting_line + 1 == self.location.line && user_attended() {
                    print!(
                        "{}",
                        format!("{}{}  |  {}", spaces(2), starting_line + 1, line_str).color(Colors::RedFg)
                    );
                } else {
                    print!("{}{}  |  {}", spaces(2), starting_line + 1, line_str);
                }
            } else {
                break;
            }

            starting_line += 1;
            print!("\n");
        }

        println!();
    }
}
