// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

use crate::{Diag, DiagKind, DiagLevel};
use colorized::{Color, Colors};
use console::user_attended;
use cyrusc_strescape::{saturating_sub, spaces};
use std::fmt::{self};
use std::fs;

const PANEL_LENGTH: usize = 2;

pub struct DiagReporter {
    pub diags: Vec<Diag>,
}

impl DiagReporter {
    pub fn new() -> Self {
        Self { diags: Vec::new() }
    }

    pub fn report(&mut self, diag: Diag) -> &mut Self {
        self.diags.push(diag);
        self
    }

    pub fn has_errors(&self) -> bool {
        self.diags.iter().any(|d| matches!(d.level, DiagLevel::Error))
    }
}

impl DiagReporter {
    pub fn display(&self) {
        for diag in self.diags.iter() {
            match diag.level {
                DiagLevel::Error => eprintln!("{}", Self::format_panel(diag)),
                DiagLevel::Warning => println!("{}", Self::format_panel(diag)),
            }
        }
    }

    pub fn display_single(diag: Diag) {
        let mut reporter = Self::new();
        reporter.report(diag);
        reporter.display();
    }

    pub fn format_panel(diag: &Diag) -> String {
        let mut formatted = String::new();

        macro_rules! get_highlight_color {
            () => {
                match diag.level {
                    DiagLevel::Error => Colors::RedFg,
                    DiagLevel::Warning => Colors::YellowFg,
                }
            };
        }

        let level_text = match diag.level {
            DiagLevel::Error => "error".color(get_highlight_color!()),
            DiagLevel::Warning => "warning".color(get_highlight_color!()),
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
                        formatted.push_str(
                            &format!("{}{}  |  {}", spaces(2), line_no + 1, content).color(get_highlight_color!()),
                        );
                    } else {
                        formatted.push_str(&format!("{}{}  |  {}", spaces(2), line_no + 1, content));
                    }
                    formatted.push('\n');
                }
            }

            if let Some(hint) = &diag.hint {
                formatted.push_str("\n");
                formatted.push_str(&format!(" {}: {}", "hint".color(Colors::BlueFg), hint));
                formatted.push_str("\n");
            }
        }

        formatted
    }
}

#[macro_export]
macro_rules! display_single_diag {
    ($diag:expr) => {
        cyrusc_diagcentral::reporter::DiagReporter::display_single($diag);
        std::process::exit(1);
    };
}

#[derive(Clone, Debug)]
pub enum CustomDiagKind {
    Custom(String),
}

impl DiagKind for CustomDiagKind {}

impl fmt::Display for CustomDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CustomDiagKind::Custom(message) => write!(f, "{}", message),
        }
    }
}

#[macro_export]
macro_rules! display_single_custom_diag {
    ($msg:expr) => {
        cyrusc_diagcentral::reporter::DiagReporter::display_single(cyrusc_diagcentral::Diag {
            level: cyrusc_diagcentral::DiagLevel::Error,
            kind: Box::new(cyrusc_diagcentral::reporter::CustomDiagKind::Custom($msg)),
            location: None,
            hint: None,
        });
        std::process::exit(1);
    };
}
