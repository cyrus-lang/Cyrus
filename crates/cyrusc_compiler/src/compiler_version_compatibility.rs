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
use cyrusc_diagcentral::exit_with_single_diag;
use cyrusc_tui_utils::tui_error;
use std::process::exit;

/// Validates if the current compiler meets the requirements of the project/source.
///
/// Principle:
/// 1. Major version must match exactly (Breaking changes).
/// 2. Current Minor/Patch must be >= Required Minor/Patch.
pub fn validate_compiler_version(compiler_version: &str, required_version_opt: Option<String>) {
    let Some(required_raw) = required_version_opt else {
        return; // No requirement specified, proceed.
    };

    let current = parse_version(compiler_version);
    let required = parse_version(&required_raw);

    match (current, required) {
        (Some(cur), Some(req)) => {
            // check Major version (breaking changes)
            if cur.0 != req.0 {
                tui_error("Incompatible Major versions!".to_string());
                tui_error(format!(
                    "Compiler: v{}, Project requires: v{}",
                    compiler_version, required_raw
                ));
                exit(1);
            }

            // check if compiler is too old
            if cur < req {
                tui_error(format!(
                    "Compiler version too old. Current: v{}, Minimum Required: v{}",
                    compiler_version, required_raw,
                ));
                exit(1);
            }

            // compiler is equal or newer within the same Major branch
        }
        _ => {
            exit_with_single_diag!("Could not parse version strings for compatibility check.".to_string());
        }
    }
}

/// Simple helper to turn "1.2.3" into (1, 2, 3) for easy comparison
fn parse_version(v: &str) -> Option<(u32, u32, u32)> {
    let parts: Vec<u32> = v.trim().split('.').filter_map(|s| s.parse().ok()).collect();

    if parts.len() >= 3 {
        Some((parts[0], parts[1], parts[2]))
    } else {
        None
    }
}
