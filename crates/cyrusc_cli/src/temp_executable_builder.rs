use std::env;
use std::fs;
use std::io;
use std::path::PathBuf;

pub struct TempExecutableBuilder {
    project_name: Option<String>,
    entry_file: Option<String>,
    keep_file: bool,
}

impl TempExecutableBuilder {
    pub fn new() -> Self {
        Self {
            project_name: None,
            entry_file: None,
            keep_file: false,
        }
    }

    pub fn project_name(mut self, name: impl Into<String>) -> Self {
        self.project_name = Some(name.into());
        self
    }

    pub fn entry_file(mut self, path: impl Into<String>) -> Self {
        self.entry_file = Some(path.into());
        self
    }

    pub fn build(&self) -> io::Result<TempExecutable> {
        let mut temp_path = env::temp_dir();

        let exe_name = self.project_name.clone().unwrap_or_else(|| {
            self.entry_file
                .as_ref()
                .and_then(|p| p.split(std::path::MAIN_SEPARATOR).last())
                .unwrap_or("temp_program")
                .replace(".cyrus", "")
        });

        temp_path.push(exe_name);
        Ok(TempExecutable {
            path: temp_path,
            keep_file: self.keep_file,
        })
    }
}

pub struct TempExecutable {
    pub path: PathBuf,
    keep_file: bool,
}

impl Drop for TempExecutable {
    fn drop(&mut self) {
        if !self.keep_file {
            if let Err(err) = fs::remove_file(&self.path) {
                eprintln!("Warning: failed to remove temp file {}: {err}", self.path.display());
            }
        }
    }
}
