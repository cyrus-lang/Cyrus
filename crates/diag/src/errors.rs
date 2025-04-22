use ast::token::Location;
use colorized::{Color, Colors};
use console::user_attended;
use std::fmt::{Debug, Display};
use utils::{purify_string::unescape_string, tui::tui_error};

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
    pub caret: bool,
}

fn saturating_sub(value: usize, input: usize) -> usize {
    if input >= value {
        return 0;
    } else {
        return value.saturating_sub(input);
    }
}

impl<ErrorType: CompileTypeErrorType> CompileTimeError<ErrorType> {
    pub fn print(&self) {
        let mut starting_line = saturating_sub(self.location.line, 5);
        let source_content = unescape_string(*self.source_content.clone());
        let sources_lines: Vec<&str> = source_content.split("\n").collect();

        while starting_line < self.location.line + 5 {
            if let Some(line_str) = sources_lines.get(starting_line) {
                print!("{}| {}", starting_line + 1, line_str);

                if starting_line + 1 == self.location.line {
                    let content = {
                        if let Some(verbose) = self.verbose.clone() {
                            verbose
                        } else {
                            for _ in 0..self.location.column {
                                print!(" ");
                            }
                            self.etype.context()
                        }
                    };

                    if user_attended() {
                        println!("  {}", content.color(Colors::RedFg));
                    } else {
                        println!("{}", content);
                    }
                }
            }

            starting_line += 1;
            print!("\n");
        }

        if let Some(file_name) = self.file_name.clone() {
            println!();
            tui_error(format!("{}:{}", file_name, self.location.line));
        }
    }
}
