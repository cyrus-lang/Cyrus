use colorized::{Color, Colors};
use console::user_attended;

pub fn tui_compiled(file_name: String) {
    if user_attended() {
        println!("    \x1b[1m{}\x1b[0m {}", "Compiled".color(Colors::GreenFg), file_name);
    } else {
        println!("    {} {}", "Compiled", file_name);
    }
}

pub fn tui_compile_finished() {
    if user_attended() {
        println!("    \x1b[1m{}\x1b[0m", "Finished".color(Colors::GreenFg));
    } else {
        println!("    {}", "Finished");
    }
}
