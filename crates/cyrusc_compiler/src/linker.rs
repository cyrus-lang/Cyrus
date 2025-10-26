use crate::options::{CodeGenOptions, CodeGenSanitizer, RelocModeOptions};
use std::path::PathBuf;
use std::process::Command;

#[derive(Debug, Clone)]
pub struct Linker {
    pub path: String,
    pub options: Box<CodeGenOptions>,
    pub flags: Vec<String>,
    pub sanitizers: Vec<CodeGenSanitizer>,
    pub opt_level: Option<i32>,
    pub verbose: bool,
}

impl Linker {
    /// Link object files into a binary executable
    pub fn link_executable(&self, object_files: &[String], output_path: &str) -> Result<(), String> {
        let mut cmd = Command::new(&self.path);

        if self.options.linker_options.link_static {
            cmd.arg("-static");
        }
        if self.options.linker_options.pie {
            cmd.args(["-pie", "-fPIE"]);
        }
        if self.options.linker_options.no_pie {
            cmd.arg("-no-pie");
        }
        if !self.options.linker_options.link_static
            && (self.options.linker_options.pie || self.options.linker_options.no_pie)
        {
            cmd.args(["-ldl", "-rdynamic"]);
        }
        if self.options.linker_options.link_static {
            cmd.arg("-lc");
        }

        cmd.arg("-funroll-loops").arg("-flto");

        if let Some(level) = self.opt_level {
            cmd.arg(format!("-O{}", level));
        }

        cmd.arg("-o").arg(output_path);
        cmd.args(object_files);
        cmd.args(&self.flags);

        if !self.sanitizers.is_empty() {
            let joined_flags = self
                .sanitizers
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(",");
            cmd.arg(format!("-fsanitize={}", joined_flags));
        }

        if self.verbose {
            println!("{:?}", cmd);
        }

        let output = cmd.output().map_err(|e| format!("Failed to execute linker: {}", e))?;
        if !output.status.success() {
            return Err(format!("Linker error: {}", String::from_utf8_lossy(&output.stderr)));
        }

        Ok(())
    }

    /// Link object files into a static library
    pub fn link_static_library(
        &self,
        object_files: &[String],
        output_dir: &PathBuf,
        lib_name: &str,
    ) -> Result<PathBuf, String> {
        let filename = static_library_filename(lib_name);
        let output_path = output_dir.join(&filename);

        let mut cmd = if cfg!(target_os = "windows") {
            let mut c = Command::new("lib.exe");
            c.arg(format!("/OUT:{}", output_path.display()));
            c
        } else {
            let mut c = Command::new("ar");
            c.arg("rcs").arg(&output_path);
            c
        };

        cmd.args(object_files);

        if self.verbose {
            println!("Static library command: {:?}", cmd);
        }

        let output = cmd
            .output()
            .map_err(|e| format!("Failed to execute static library tool: {}", e))?;
        if !output.status.success() {
            return Err(format!(
                "Static library creation failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(output_path)
    }

    /// Link object files into a shared library
    pub fn link_shared_library(
        &self,
        object_files: &[String],
        output_dir: &PathBuf,
        lib_name: &str,
    ) -> Result<PathBuf, String> {
        let mut cmd = Command::new(&self.path);

        #[cfg(target_os = "linux")]
        cmd.arg("-shared");
        #[cfg(target_os = "macos")]
        cmd.arg("-dynamiclib");
        #[cfg(target_os = "windows")]
        cmd.arg("-shared");

        match self.options.linker_options.link_static {
            true => {
                cmd.arg("-static").arg("-lc");
            }
            false => {
                if matches!(
                    self.options.reloc_mode,
                    RelocModeOptions::PIC | RelocModeOptions::DynamicNoPic
                ) {
                    cmd.args(["-ldl", "-rdynamic"]);
                }
            }
        }

        let filename = shared_library_filename(lib_name);
        let output_path = output_dir.join(filename);

        cmd.arg("-o").arg(&output_path);
        cmd.args(object_files);

        let output = cmd.output().map_err(|e| format!("Failed to execute linker: {}", e))?;
        if !output.status.success() {
            return Err(format!("Linker error: {}", String::from_utf8_lossy(&output.stderr)));
        }

        Ok(output_path)
    }
}

fn static_library_filename(lib_name: &str) -> String {
    if cfg!(target_os = "windows") {
        format!("{}.lib", lib_name)
    } else {
        format!("lib{}.a", lib_name)
    }
}

fn shared_library_filename(base: &str) -> String {
    #[cfg(target_os = "linux")]
    {
        format!("lib{}.so", base)
    }

    #[cfg(target_os = "macos")]
    {
        format!("lib{}.dylib", base)
    }

    #[cfg(target_os = "windows")]
    {
        format!("{}.dll", base)
    }
}
