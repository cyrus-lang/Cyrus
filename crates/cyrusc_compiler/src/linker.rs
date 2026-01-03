/* 
 * Copyright (c) 2026 The Cyrus Language
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::options::{CodeGenOptions, RelocModeOptions};
use std::path::PathBuf;
use std::process::Command;
use which::which;

#[derive(Debug, Clone)]
pub struct Linker {
    pub linker_path: String,
    pub opts: CodeGenOptions,
}

/// Static mapping of platforms to default linkers
const DEFAULT_LINKERS: &[(&str, &str)] = &[("linux", "gcc"), ("macos", "clang"), ("windows", "link.exe")];

impl Linker {
    pub fn new(opts: CodeGenOptions) -> Result<Self, String> {
        let os = std::env::consts::OS;
        let default_linker = DEFAULT_LINKERS
            .iter()
            .find(|(platform, _)| *platform == os)
            .ok_or_else(|| format!("Unsupported platform '{}'.", os))?
            .1;

        let linker_path = opts.linker.clone().unwrap_or_else(|| default_linker.to_string());

        if which(&linker_path).is_err() {
            return Err(format!(
                "Linker '{}' not found in system PATH. Please install it or provide a valid path.",
                linker_path
            ));
        }

        Ok(Self { linker_path, opts })
    }

    /// Link object files into a binary executable
    pub fn link_executable(&self, object_files: &[String], output_path: &str) -> Result<(), String> {
        let mut cmd = Command::new(&self.linker_path);

        if self.opts.linker_options.link_static {
            cmd.arg("-static");
        }
        if self.opts.linker_options.pie {
            cmd.args(["-pie", "-fPIE"]);
        }
        if self.opts.linker_options.no_pie {
            cmd.arg("-no-pie");
        }
        if !self.opts.linker_options.link_static && (self.opts.linker_options.pie || self.opts.linker_options.no_pie) {
            cmd.args(["-ldl", "-rdynamic"]);
        }
        if self.opts.linker_options.link_static {
            cmd.arg("-lc");
        }

        cmd.arg("-funroll-loops").arg("-flto");

        if let Some(level) = self.opts.opt_level {
            cmd.arg(format!("-O{}", level));
        }

        cmd.arg("-o").arg(output_path);
        cmd.args(object_files);
        cmd.args(&self.opts.linker_flags);

        if !self.opts.sanitizer.is_empty() {
            let joined_flags = self
                .opts
                .sanitizer
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(",");
            cmd.arg(format!("-fsanitize={}", joined_flags));
        }

        if self.opts.verbose {
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

        if self.opts.verbose {
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
        let mut cmd = Command::new(&self.linker_path);

        #[cfg(target_os = "linux")]
        cmd.arg("-shared");
        #[cfg(target_os = "macos")]
        cmd.arg("-dynamiclib");
        #[cfg(target_os = "windows")]
        cmd.arg("-shared");

        match self.opts.linker_options.link_static {
            true => {
                cmd.arg("-static").arg("-lc");
            }
            false => {
                if matches!(
                    self.opts.reloc_mode,
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
