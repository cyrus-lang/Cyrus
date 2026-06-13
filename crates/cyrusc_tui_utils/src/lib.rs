// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

/// Internal helper to print status messages, only using ANSI codes if enabled.
fn print_status(label: &str, file_name: Option<&str>) {
    match file_name {
        Some(name) => println!("[{}] {}", label, name),
        None => println!("[{}]", label),
    }
}

/// Internal helper to print diagnostics (error/warning) respecting ANSI flag.
fn print_diag(label: &str, msg: &str) {
    eprintln!("{}: {}", label, msg);
}

pub fn tui_compiled(file_name: String) {
    print_status("compiled", Some(&file_name));
}

pub fn tui_skipped(file_name: String) {
    print_status("skipped", Some(&file_name));
}

pub fn tui_compile_finished() {
    print_status("finished", None);
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
