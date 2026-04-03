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

use crate::context::AnalysisContext;
use cyrusc_typed_ast::stmts::TypedGotoStmt;

impl<'a> AnalysisContext<'a> {
    // FIXME
    fn analyze_goto(&mut self, goto: &mut TypedGotoStmt) {
        todo!();
        // if let Some(label_id) = scope_ref.resolve_label(&goto.name) {
        //     goto.label_id = Some(label_id);
        // } else {
        //     self.reporter.report(Diag {
        //         level: DiagLevel::Error,
        //         kind: Box::new(AnalyzerDiagKind::UndefinedGotoLabel {
        //             label_name: goto.name.clone(),
        //         }),
        //         loc: Some(goto.loc),
        //         hint: None,
        //     });
        // }
    }
}
