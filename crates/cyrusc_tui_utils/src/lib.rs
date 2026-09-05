// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use std::sync::atomic::{AtomicBool, Ordering};

/// Global ANSI color flag
static USE_ANSI: AtomicBool = AtomicBool::new(true);

/// Set whether to use ANSI colors in output
pub fn set_ansi(enabled: bool) {
    USE_ANSI.store(enabled, Ordering::Relaxed);
}

pub fn get_ansi() -> bool {
    USE_ANSI.load(Ordering::Relaxed)
}

pub fn tui_compiled(file_name: String) {
    print_status("compiled", Some(&file_name));
}

pub fn tui_skipped(file_name: String) {
    print_status("skipped", Some(&file_name));
}

pub fn tui_error(msg: String) {
    print_diag("error", &msg);
}

pub fn tui_warning(msg: String) {
    print_diag("warning", &msg);
}

pub fn tui_note(msg: String) {
    print_diag("note", &msg);
}

/// ANSI color codes
mod colors {
    pub const RESET: &str = "\x1b[0m";
    pub const RED: &str = "\x1b[31m";
    pub const YELLOW: &str = "\x1b[33m";
    pub const GREEN: &str = "\x1b[32m";
    pub const CYAN: &str = "\x1b[36m";
    pub const BLUE: &str = "\x1b[34m";
}

fn print_diag(label: &str, msg: &str) {
    let colored_label = match label {
        "error" => colorize(label, colors::RED),
        "warning" => colorize(label, colors::YELLOW),
        "note" => colorize(label, colors::CYAN),
        _ => colorize(label, colors::BLUE),
    };

    eprintln!("{}: {}", colored_label, msg);
}

fn print_status(label: &str, file_name: Option<&str>) {
    let colored_label = match label {
        "compiled" => colorize(label, colors::GREEN),
        "skipped" => colorize(label, colors::YELLOW),
        "finished" => colorize(label, colors::CYAN),
        _ => colorize(label, colors::BLUE),
    };

    match file_name {
        Some(name) => println!("[{}] {}", colored_label, name),
        None => println!("[{}]", colored_label),
    }
}

fn colorize(text: &str, color: &str) -> String {
    if get_ansi() {
        format!("{}{}{}", color, text, colors::RESET)
    } else {
        text.to_string()
    }
}
