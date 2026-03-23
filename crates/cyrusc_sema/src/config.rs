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

#[derive(Debug, Clone)]
pub struct AnalyzerConfig {
    pub warnings: WarningConfig,
    pub strict_mode: bool,
}

#[derive(Debug, Clone)]
pub struct WarningConfig {
    pub enabled: bool,
    pub warnings_as_errors: bool,

    pub unused_variables: bool,
    pub unreachable_code: bool,
    pub dead_code: bool,
}

impl Default for WarningConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            warnings_as_errors: false,
            unused_variables: true,
            unreachable_code: true,
            dead_code: true,
        }
    }
}

impl Default for AnalyzerConfig {
    fn default() -> Self {
        Self {
            warnings: WarningConfig::default(),
            strict_mode: false,
        }
    }
}