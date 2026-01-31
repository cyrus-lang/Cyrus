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
use cyrusc_strescape::{saturating_sub, spaces};
use std::fmt::{self};
use std::fs;

const PANEL_LENGTH: usize = 2;
const TAB_WIDTH: usize = 4;

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

        formatted.push_str(&format!("[{}] {}\n", level_text, diag.kind));

        if let Some(loc) = &diag.location {
            formatted.push_str(&format!("       --> {}:{}:{}\n\n", loc.file, loc.line, loc.column));

            // Get source lines
            let source = fs::read_to_string(&loc.file).unwrap_or_default();
            let lines: Vec<&str> = source.lines().collect();

            // Convert 1-based line number to 0-based index
            let error_line_idx = loc.line.saturating_sub(1);

            // Calculate range of lines to show
            let start_line = saturating_sub(error_line_idx, PANEL_LENGTH);
            let end_line = std::cmp::min(error_line_idx + PANEL_LENGTH + 1, lines.len());

            // Display the lines
            for line_idx in start_line..end_line {
                if let Some(line_content) = lines.get(line_idx) {
                    let line_num = line_idx + 1;

                    // Add line number
                    formatted.push_str(&format!("{}{:>4} | ", spaces(2), line_num));

                    // Handle highlighting if this is the error line
                    if line_idx == error_line_idx {
                        // Check if we have a specific range to highlight
                        if let Some(range) = loc.range {
                            let (range_start, range_end) = range;

                            // Ensure range is within bounds
                            let safe_start = range_start.min(line_content.len());
                            let safe_end = range_end.min(line_content.len());

                            // Split the line into parts
                            if safe_start <= safe_end {
                                // Part before the highlighted range
                                let before = &line_content[..safe_start];
                                formatted.push_str(before);

                                // The highlighted part itself
                                if safe_start < line_content.len() {
                                    let highlighted = &line_content[safe_start..safe_end];
                                    formatted.push_str(&format!("{}", highlighted.color(get_highlight_color!())));

                                    // Part after the highlighted range
                                    let after = &line_content[safe_end..];
                                    formatted.push_str(after);
                                }

                                formatted.push('\n');

                                // Add the pointer line (~~~~~) to show where the error is
                                if user_attended() {
                                    formatted.push_str(&format!("{}     | ", spaces(2)));

                                    // Handle tabs in the code
                                    let before_tabs = before.chars().filter(|&c| c == '\t').count();
                                    let before_visible_length = before.len() + before_tabs * (TAB_WIDTH - 1);

                                    // Add spaces to align the pointer
                                    formatted.push_str(&" ".repeat(before_visible_length));

                                    // Calculate pointer length
                                    let highlighted_len = if safe_start < safe_end {
                                        let highlighted_text = &line_content[safe_start..safe_end];
                                        // Account for tabs in highlighted text
                                        let tab_count = highlighted_text.chars().filter(|&c| c == '\t').count();
                                        highlighted_text.len() + tab_count * (TAB_WIDTH - 1)
                                    } else {
                                        1 // Single caret for empty range
                                    };

                                    // Add the pointer
                                    if highlighted_len > 0 {
                                        if highlighted_len == 1 {
                                            formatted.push_str(&format!("{}", "^".color(get_highlight_color!())));
                                        } else {
                                            formatted.push_str(&format!(
                                                "{}",
                                                "~".repeat(highlighted_len).color(get_highlight_color!())
                                            ));
                                        }
                                    } else {
                                        formatted.push_str(&format!("{}", "^".color(get_highlight_color!())));
                                    }

                                    formatted.push('\n');
                                }
                            } else {
                                // Invalid range, just show the line normally
                                formatted.push_str(&format!("{}", line_content.color(get_highlight_color!())));
                                formatted.push('\n');
                            }
                        } else {
                            // No specific range, highlight entire line
                            formatted.push_str(&format!("{}", line_content.color(get_highlight_color!())));
                            formatted.push('\n');

                            // Add a simple pointer at the column position
                            if user_attended() && loc.column > 0 {
                                formatted.push_str(&format!("{}     | ", spaces(2)));

                                // Handle tabs
                                let line_up_to_col = &line_content[..(loc.column - 1).min(line_content.len())];
                                let tab_count = line_up_to_col.chars().filter(|&c| c == '\t').count();
                                let visible_length = line_up_to_col.len() + tab_count * (TAB_WIDTH - 1);

                                formatted.push_str(&" ".repeat(visible_length));
                                formatted.push_str(&format!("{}", "^".color(get_highlight_color!())));
                                formatted.push('\n');
                            }
                        }
                    } else {
                        // Regular line (not the error line)
                        formatted.push_str(line_content);
                        formatted.push('\n');
                    }
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
