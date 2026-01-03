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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReprKind {
    C,
    Cyrus,
    Transparent,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReprAttrKind {
    Kind(ReprKind),
    Align(u32),
}

#[derive(Debug, Clone)]
pub struct ReprAttr {
    pub items: Vec<ReprAttrKind>,
}

impl ReprAttr {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push(&mut self, item: ReprAttrKind) {
        self.items.push(item);
    }

    pub fn try_kind_from_str(s: &str) -> Result<ReprKind, String> {
        match s.to_lowercase().as_str() {
            "c" => Ok(ReprKind::C),
            "cyrus" => Ok(ReprKind::Cyrus),
            "transparent" => Ok(ReprKind::Transparent),
            _ => Err(format!("Unknown repr kind '{}'.", s)),
        }
    }

    pub fn align(&self) -> Option<u32> {
        self.items
            .iter()
            .find_map(|i| if let ReprAttrKind::Align(v) = i { Some(*v) } else { None })
    }

    pub fn kind(&self) -> Option<ReprKind> {
        self.items.iter().find_map(|i| {
            if let ReprAttrKind::Kind(k) = i {
                Some(k.clone())
            } else {
                None
            }
        })
    }
}
