/*
 * Copyright (c) 2026 The Cyrus Language
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use crate::{Diag, DiagKind, DiagLevel};
use colorized::{Color, Colors};
use console::user_attended;
use cyrusc_source_loc::{Loc, SourceMap};
use cyrusc_strescape::spaces;
use std::{cell::RefCell, fmt, process::exit};

const PANEL_LENGTH: usize = 2;
const TAB_WIDTH: usize = 4;

pub struct DiagReporter<'source_map> {
    pub source_map: Option<&'source_map SourceMap>,
    pub diags: RefCell<Vec<Diag>>,
}

impl<'source_map> DiagReporter<'source_map> {
    pub fn new(source_map: &'source_map SourceMap) -> Self {
        Self {
            source_map: Some(source_map),
            diags: RefCell::new(Vec::new()),
        }
    }

    pub fn new_with_no_source_map() -> Self {
        Self {
            source_map: None,
            diags: RefCell::new(Vec::new()),
        }
    }

    pub fn display_and_exit_if_has_errors(&self) {
        if self.has_errors() {
            self.display();
            exit(1);
        }
    }

    pub fn display(&self) {
        let mut diags = self.diags.borrow_mut();

        for diag in diags.iter() {
            match diag.level {
                DiagLevel::Error => eprintln!("{}", self.render(diag)),
                DiagLevel::Warning => println!("{}", self.render(diag)),
            }
        }

        diags.clear();
        drop(diags);
    }

    pub fn display_single(diag: Diag) {
        let reporter = DiagReporter::new_with_no_source_map();
        let output = reporter.render(&diag);
        eprintln!("{}", output);
    }

    pub fn report(&self, diag: Diag) {
        self.diags.borrow_mut().push(diag)
    }

    pub fn has_errors(&self) -> bool {
        self.diags.borrow().iter().any(|d| matches!(d.level, DiagLevel::Error))
    }
}

impl<'source_map> DiagReporter<'source_map> {
    pub(crate) fn render(&self, diag: &Diag) -> String {
        let mut out = String::new();

        let level_text = {
            let color = self.highlight_color(diag);

            match diag.level {
                DiagLevel::Error => "error".color(color),
                DiagLevel::Warning => "warning".color(color),
            }
        };

        out.push_str(&format!("[{}] {}\n", level_text, diag.kind));

        if diag.loc.is_none() || self.source_map.is_none() {
            if let Some(hint) = &diag.hint {
                out.push_str(&format!(" {}: {}\n", "hint".color(Colors::BlueFg), hint));
            }
            return out;
        }

        let loc = diag.loc.unwrap();
        let sm = self.source_map.unwrap();
        let file = match sm.get_file(loc.id) {
            Some(f) => f,
            None => {
                return out;
            }
        };

        let lines: Vec<&str> = file.content.lines().collect();

        // Render header: `--> file:line:column`
        out.push_str(&self.render_header(&file.name, loc));

        // Compute which lines to print
        let error_idx = loc.line.saturating_sub(1);
        let start_line = error_idx.saturating_sub(PANEL_LENGTH);
        let end_line = std::cmp::min(error_idx + PANEL_LENGTH + 1, lines.len());

        for line_idx in start_line..end_line {
            let line = lines[line_idx];
            let line_no = line_idx + 1;

            out.push_str(&self.render_line_number(line_no));

            if line_idx == error_idx {
                let highlighted_line = {
                    let color = self.highlight_color(diag);
                    self.render_highlighted_line(line, loc, color)
                };

                out.push_str(&highlighted_line);

                // draw pointer caret if "user attended" mode
                if user_attended() {
                    let color = self.highlight_color(diag);
                    out.push_str(&self.render_pointer_line(line, loc, color));
                }
            } else {
                out.push_str(line);
                out.push('\n');
            }
        }

        // Render hints
        if let Some(hint) = &diag.hint {
            out.push_str("\n");
            out.push_str(&format!(" {}: {}\n", "hint".color(Colors::BlueFg), hint));
        }

        out
    }

    fn render_header(&self, file: &str, loc: Loc) -> String {
        format!("       --> {}:{}:{}\n\n", file, loc.line, loc.start + 1)
    }

    fn render_line_number(&self, line: usize) -> String {
        format!("{}{:>4} | ", spaces(2), line)
    }

    fn render_highlighted_line(&self, line: &str, loc: Loc, color: Colors) -> String {
        let mut out = String::new();

        let start = loc.start.min(line.len());
        let end = loc.end.min(line.len());

        if start <= end {
            let before = &line[..start];
            let highlight = &line[start..end];
            let after = &line[end..];

            out.push_str(before);
            out.push_str(&format!("{}", highlight.color(color)));
            out.push_str(after);
        } else {
            out.push_str(line);
        }

        out.push('\n');
        out
    }

    fn render_pointer_line(&self, line: &str, loc: Loc, color: Colors) -> String {
        let mut out = String::new();

        out.push_str(&format!("{}     | ", spaces(2)));

        let start = loc.start.min(line.len());
        let end = loc.end.min(line.len());

        let before = &line[..start];
        let span = &line[start..end];

        let before_width = self.visual_width(before);
        let span_width = self.visual_width(span).max(1);

        out.push_str(&" ".repeat(before_width));

        if span_width == 1 {
            out.push_str(&format!("{}", "^".color(color)));
        } else {
            out.push_str(&format!("{}", "~".repeat(span_width).color(color)));
        }

        out.push('\n');

        out
    }

    fn visual_width(&self, text: &str) -> usize {
        let mut width = 0;

        for c in text.chars() {
            if c == '\t' {
                width += TAB_WIDTH;
            } else {
                width += 1;
            }
        }

        width
    }

    fn highlight_color(&self, diag: &Diag) -> Colors {
        match diag.level {
            DiagLevel::Error => Colors::RedFg,
            DiagLevel::Warning => Colors::YellowFg,
        }
    }
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
macro_rules! exit_with_msg {
    ($msg:expr) => {
        cyrusc_diagcentral::reporter::DiagReporter::display_single(cyrusc_diagcentral::Diag {
            level: cyrusc_diagcentral::DiagLevel::Error,
            kind: Box::new(cyrusc_diagcentral::reporter::CustomDiagKind::Custom($msg)),
            loc: None,
            hint: None,
        });
        std::process::exit(1);
    };
}

#[macro_export]
macro_rules! exit_with_single_diag {
    ($diag:expr) => {
        cyrusc_diagcentral::reporter::DiagReporter::display_single($diag);
        std::process::exit(1);
    };
}
