// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.
use colorized::{Color, Colors};
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, Ordering};

/// Global flag to enable/disable ANSI colors and formatting.
static ANSI_ENABLED: OnceLock<AtomicBool> = OnceLock::new();

/// Returns a reference to the global ANSI flag.
fn ansi_flag() -> &'static AtomicBool {
    ANSI_ENABLED.get_or_init(|| AtomicBool::new(console::user_attended()))
}

/// Check if ANSI output is enabled.
pub fn is_ansi_enabled() -> bool {
    ansi_flag().load(Ordering::Relaxed)
}

/// Enable ANSI output globally.
pub fn enable_ansi() {
    ansi_flag().store(true, Ordering::Relaxed);
}

/// Disable ANSI output globally.
pub fn disable_ansi() {
    ansi_flag().store(false, Ordering::Relaxed);
}

/// Internal helper to print status messages, only using ANSI codes if enabled.
fn print_status(label: &str, file_name: Option<&str>, color: Option<Colors>) {
    let formatted_label = if is_ansi_enabled() {
        match color {
            Some(c) => format!("\x1b[1m{}\x1b[0m", label.color(c)),
            None => format!("\x1b[1m{}\x1b[0m", label),
        }
    } else {
        label.to_string()
    };

    match file_name {
        Some(name) => println!("    {} {}", formatted_label, name),
        None => println!("    {}", formatted_label),
    }
}

/// Internal helper to print diagnostics (error/warning) respecting ANSI flag.
fn print_diag(label: &str, msg: &str, color: Option<Colors>) {
    if is_ansi_enabled() {
        eprintln!("{}: {}", label.color(color.unwrap_or(Colors::RedFg)), msg);
    } else {
        eprintln!("{}: {}", label, msg);
    }
}

pub fn tui_compiled(file_name: String) {
    print_status("Compiled", Some(&file_name), Some(Colors::GreenFg));
}

pub fn tui_skipped(file_name: String) {
    print_status("Skipped", Some(&file_name), Some(Colors::BlueFg));
}

pub fn tui_compile_finished() {
    print_status("Finished", None, Some(Colors::GreenFg));
}

pub fn tui_error(msg: String) {
    print_diag("error", &msg, Some(Colors::RedFg));
}

pub fn tui_warning(msg: String) {
    print_diag("warning", &msg, Some(Colors::YellowFg));
}
