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
use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, exit_with_single_diag, source_loc::Loc};
use std::sync::{Arc, Mutex};

impl<'a> AnalysisContext<'a> {
    pub fn check_entry_points(entry_points_arc: Arc<Mutex<Vec<Loc>>>) {
        let entry_points = entry_points_arc.lock().unwrap();
        let mut entry_points_clone = entry_points.clone();

        if entry_points.len() == 0 {
            exit_with_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MissingEntryPoint),
                loc: None,
                hint: None,
            });
        } else if entry_points.len() > 1 {
            let loc = entry_points_clone.pop().unwrap();

            exit_with_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MultipleEntryPoints),
                loc: Some(loc),
                hint: {
                    if let Some(another_decl_loc) = entry_points_clone.pop() {
                        Some(format!("Another declaration is at {}.", another_decl_loc))
                    } else {
                        None
                    }
                },
            });
        }

        drop(entry_points);
    }
}
