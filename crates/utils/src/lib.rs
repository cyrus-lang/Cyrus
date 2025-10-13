pub mod escaping;
pub mod fs;
pub mod generate_random_hex;
pub mod tui;

use std::sync::atomic::AtomicBool;
pub static ANSI: AtomicBool = AtomicBool::new(true) ;