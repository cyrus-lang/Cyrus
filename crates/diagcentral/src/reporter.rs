use crate::{Diag, DiagLevel};
use colorized::{Color, Colors};
use console::user_attended;
use std::fmt::Display;
use std::fs;
use utils::escaping::{saturating_sub, spaces};

const PANEL_LENGTH: usize = 4;

#[derive(Debug, Clone)]
pub struct DiagReporter<K> {
    pub diags: Vec<Diag<K>>,
}

impl<K> DiagReporter<K> {
    pub fn new() -> Self {
        Self { diags: Vec::new() }
    }

    pub fn report(&mut self, diag: Diag<K>) -> &mut Self {
        self.diags.push(diag);
        self
    }

    pub fn has_errors(&self) -> bool {
        self.diags.iter().any(|d| matches!(d.level, DiagLevel::Error))
    }
}

impl<K: Display> DiagReporter<K> {
    pub fn display(&self) {
        for diag in &self.diags {
            match diag.level {
                DiagLevel::Error => eprintln!("{}", Self::format_panel(diag)),
                DiagLevel::Warning => println!("{}", Self::format_panel(diag)),
            }
        }
    }

    pub fn display_single(diag: Diag<K>) {
        let mut reporter = Self::new();
        reporter.report(diag);
        reporter.display();
    }

    pub fn format_panel(diag: &Diag<K>) -> String {
        let mut formatted = String::new();

        let level_text = match diag.level {
            DiagLevel::Error => "error".color(Colors::RedFg),
            DiagLevel::Warning => "warning".color(Colors::YellowFg),
        };

        formatted.push_str(&format!("{}: {}\n", level_text, diag.kind));

        if let Some(loc) = &diag.location {
            formatted.push_str(&format!("       --> {}:{}:{}\n\n", loc.file, loc.line, loc.column));

            let start_line = saturating_sub(loc.line, PANEL_LENGTH);
            let source = fs::read_to_string(&loc.file).unwrap_or_default();
            let lines: Vec<&str> = source.lines().collect();

            for line_no in start_line..(loc.line + PANEL_LENGTH) {
                if let Some(content) = lines.get(line_no) {
                    if line_no + 1 == loc.line && user_attended() {
                        formatted
                            .push_str(&format!("{}{}  |  {}", spaces(2), line_no + 1, content).color(Colors::RedFg));
                    } else {
                        formatted.push_str(&format!("{}{}  |  {}", spaces(2), line_no + 1, content));
                    }
                    formatted.push('\n');
                }
            }

            if let Some(hint) = &diag.hint {
                formatted.push_str(&format!("  {}: {}", "hint".color(Colors::BlueFg), hint));
            }
        }

        formatted
    }
}

#[macro_export]
macro_rules! display_single_diag {
    ($diag:expr) => {
        let output = diagcentral::reporter::DiagReporter::format_panel(&$diag);
        eprintln!("{}", output);
        std::process::exit(1);
    };
}
