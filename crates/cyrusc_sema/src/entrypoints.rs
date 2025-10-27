use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::source_loc::SourceLoc;
use cyrusc_diagcentral::{Diag, DiagLevel, DiagLoc, display_single_diag};
use std::sync::{Arc, Mutex};

impl<'a> AnalysisContext<'a> {
    pub fn check_entry_points(entry_points_arc: Arc<Mutex<Vec<SourceLoc>>>) {
        let entry_points = entry_points_arc.lock().unwrap();
        let mut entry_points_clone = entry_points.clone();

        if entry_points.len() == 0 {
            display_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MissingEntryPoint),
                location: None,
                hint: None,
            });
        } else if entry_points.len() > 1 {
            let loc = entry_points_clone.pop().unwrap();

            display_single_diag!(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MultipleEntryPoints),
                location: Some(DiagLoc::new(loc)),
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
