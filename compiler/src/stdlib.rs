use std::{env, fs};

use crate::Compiler;

#[cfg(target_os = "windows")]
const TARGET_PATH: &str = "C:\\Program Files\\CyrusLang\\stdlib";

#[cfg(target_os = "linux")]
const TARGET_PATH: &str = "/usr/local/lib/cyruslang/stdlib";

#[cfg(target_os = "macos")]
const TARGET_PATH: &str = "/usr/local/lib/cyruslang/stdlib";

impl Compiler {
    pub fn stdlib_path(&self) -> String {
        if cfg!(debug_assertions) {
            let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
            format!("{}/stdlib", manifest_dir)
        } else {
            String::from(TARGET_PATH)
        }
    }

    pub fn file_exists(&self, path: &str) -> bool {
        fs::metadata(path).is_ok()
    }
}
