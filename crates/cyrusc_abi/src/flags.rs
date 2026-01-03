// ┌─────────────────────────────────────────────────────────────────────────┐
// │                                                                         │
// │  Cyrus Programming Language                                             │
// │  https://github.com/cyrus-lang/Cyrus                                    │
// │                                                                         │
// │  A general-purpose, statically-typed, manually memory-managed           │
// │  programming language designed for performance-critical applications.   │
// │                                                                         │
// │  Copyright (c) 2026 The Cyrus Programming Language Project              │
// │                                                                         │
// │  This program is free software: you can redistribute it and/or modify   │
// │  it under the terms of the GNU General Public License as published by   │
// │  the Free Software Foundation, either version 3 of the License, or      │
// │  (at your option) any later version.                                    │
// │                                                                         │
// │  This program is distributed in the hope that it will be useful,        │
// │  but WITHOUT ANY WARRANTY; without even the implied warranty of         │
// │  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the           │
// │  GNU General Public License for more details.                           │
// │                                                                         │
// │  You should have received a copy of the GNU General Public License      │
// │  along with this program. If not, see <https://www.gnu.org/licenses/>.  │
// │                                                                         │
// └─────────────────────────────────────────────────────────────────────────┘

/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OptionalFlag {
    NoReturn,
    NoUnwind,
    Cold,
    Hot,
    OptSize,
    OptNone,
    NoSanitize(String),
}

pub fn validate_flags(flags: &[OptionalFlag]) -> Result<Vec<OptionalFlag>, String> {
    let mut seen = HashSet::new();
    let mut has_opt_size = false;
    let mut has_opt_none = false;
    let mut has_hot = false;
    let mut has_cold = false;

    for flag in flags {
        match flag {
            OptionalFlag::NoSanitize(name) => {
                if !seen.insert(OptionalFlag::NoSanitize(name.clone())) {
                    return Err(format!("Duplicate nosanitize flag: '{}'.", name));
                }
            }
            OptionalFlag::OptSize => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate optsize flag.".into());
                }
                has_opt_size = true;
            }
            OptionalFlag::OptNone => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate optnone flag.".into());
                }
                has_opt_none = true;
            }
            OptionalFlag::Hot => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate hot flag.".into());
                }
                has_hot = true;
            }
            OptionalFlag::Cold => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate cold flag.".into());
                }
                has_cold = true;
            }
            _ => {
                if !seen.insert(flag.clone()) {
                    return Err(format!("Duplicate flag: {:?}", flag));
                }
            }
        }
    }

    if has_opt_size && has_opt_none {
        return Err("Cannot use both 'optsize' and 'optnone' flags together.".into());
    }
    if has_hot && has_cold {
        return Err("Cannot use both 'hot' and 'cold' flags together.".into());
    }

    Ok(flags.to_vec())
}
