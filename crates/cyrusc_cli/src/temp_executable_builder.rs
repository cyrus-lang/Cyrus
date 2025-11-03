use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use uuid::Uuid;

pub struct TempExecutableBuilder {
    project_name: Option<String>,
    entry_file: Option<String>,
}

impl TempExecutableBuilder {
    pub fn new() -> Self {
        Self {
            project_name: None,
            entry_file: None,
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
        let unique_dir = env::temp_dir().join(format!("cyrus_build_{}", Uuid::new_v4()));
        fs::create_dir_all(&unique_dir)?;

        let exe_name = self
            .project_name
            .clone()
            .filter(|s| !s.trim().is_empty())
            .or_else(|| {
                self.entry_file
                    .as_ref()
                    .and_then(|p| Path::new(p).file_stem().map(|s| s.to_string_lossy().to_string()))
            })
            .unwrap_or_else(|| "main".to_string());

        // Final executable file path
        let exe_path = unique_dir.join(&exe_name);

        Ok(TempExecutable {
            dir: unique_dir,
            path: exe_path,
        })
    }
}

pub struct TempExecutable {
    pub dir: PathBuf,
    pub path: PathBuf,
}

impl Drop for TempExecutable {
    fn drop(&mut self) {
        // remove both file and its temp directory
        if let Err(err) = fs::remove_file(&self.path) {
            if err.kind() != io::ErrorKind::NotFound {
                eprintln!("Warning: failed to remove temp file {}: {err}", self.path.display());
            }
        }

        if let Err(err) = fs::remove_dir_all(&self.dir) {
            if err.kind() != io::ErrorKind::NotFound {
                eprintln!("Warning: failed to remove temp directory {}: {err}", self.dir.display());
            }
        }
    }
}
