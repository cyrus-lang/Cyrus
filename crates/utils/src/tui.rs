use colorized::{Color, Colors};

fn user_attended() -> bool {
    console::user_attended() && crate::ANSI.load(std::sync::atomic::Ordering::Relaxed)
}

pub fn tui_compiled(file_name: String) {
    if user_attended() {
        println!("    \x1b[1m{}\x1b[0m {}", "Compiled".color(Colors::GreenFg), file_name);
    } else {
        println!("    {} {}", "Compiled", file_name);
    }
}

pub fn tui_skipped(file_name: String) {
    if user_attended() {
        println!("     \x1b[1m{}\x1b[0m {}", "Skipped".color(Colors::BlueFg), file_name);
    } else {
        println!("     {} {}", "Skipped", file_name);
    }
}

pub fn tui_compile_finished() {
    if user_attended() {
        println!("    \x1b[1m{}\x1b[0m", "Finished".color(Colors::GreenFg));
    } else {
        println!("    {}", "Finished");
    }
}

pub fn tui_error(msg: String) {
    if user_attended() {
        eprintln!("{}: {}", "error".color(Colors::RedFg), msg)
    } else {
        eprintln!("{}: {}", "error", msg)
    }
}

pub fn tui_warning(msg: String) {
    if user_attended() {
        eprintln!("{}: {}", "warning".color(Colors::YellowFg), msg);
    } else {
        eprintln!("{}: {}", "warning", msg);
    }
}
