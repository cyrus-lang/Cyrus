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

use std::fmt::{self, Debug, Display};
use cyrusc_source_loc::Loc;

pub mod reporter;
mod tests;

#[derive(Debug, Clone)]
pub enum DiagLevel {
    Error,
    Warning,
}

pub struct Diag {
    pub level: DiagLevel,
    pub kind: Box<dyn DiagKindClone>,
    pub loc: Option<Loc>,
    pub hint: Option<String>,
}

impl Clone for Diag {
    fn clone(&self) -> Self {
        Self {
            level: self.level.clone(),
            kind: self.kind.clone(),
            loc: self.loc,
            hint: self.hint.clone(),
        }
    }
}

pub trait DiagKind: Display + Debug + Send + Sync {}

pub trait DiagKindClone: DiagKind {
    fn clone_box(&self) -> Box<dyn DiagKindClone>;
}

impl<T> DiagKindClone for T
where
    T: DiagKind + Clone + 'static,
{
    fn clone_box(&self) -> Box<dyn DiagKindClone> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn DiagKindClone> {
    fn clone(&self) -> Box<dyn DiagKindClone> {
        self.clone_box()
    }
}

#[derive(Debug, Clone)]
pub enum CustomDiagKind {
    Custom(String),
}

impl Display for CustomDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CustomDiagKind::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl DiagKind for CustomDiagKind {}
