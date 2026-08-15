// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{Diag, DiagKind, DiagLevel};
use ariadne::{Color, Label, Report, ReportKind, Source};
use cyrusc_source_loc::SourceMap;
use std::{
    cell::{Ref, RefCell, RefMut},
    fmt,
    process::exit,
    sync::Arc,
};

pub struct DiagReporter {
    source_map: Option<Arc<SourceMap>>,
    diags: RefCell<Vec<Diag>>,
}

impl DiagReporter {
    #[inline]
    pub fn new(source_map: Arc<SourceMap>) -> Self {
        Self {
            source_map: Some(source_map),
            diags: RefCell::new(Vec::new()),
        }
    }

    #[inline]
    pub fn new_with_no_source_map() -> Self {
        Self {
            source_map: None,
            diags: RefCell::new(Vec::new()),
        }
    }

    #[inline]
    pub fn diags(&self) -> Ref<'_, Vec<Diag>> {
        self.diags.borrow()
    }

    #[inline]
    pub fn diags_mut(&self) -> RefMut<'_, Vec<Diag>> {
        self.diags.borrow_mut()
    }

    pub fn display_and_exit_if_has_errors(&self) {
        if self.has_errors() {
            self.display();
            exit(1);
        }
    }

    pub fn display_first(&self) {
        let mut diags = self.diags.borrow_mut();

        if let Some(diag) = diags.first() {
            self.render_ariadne(diag);
        }

        diags.clear();
    }

    pub fn display(&self) {
        let mut diags = self.diags.borrow_mut();

        for diag in diags.iter() {
            self.render_ariadne(diag);
        }

        diags.clear();
    }

    #[inline]
    pub fn display_single(diag: Diag) {
        let reporter = DiagReporter::new_with_no_source_map();
        reporter.render_ariadne(&diag);
    }

    #[inline]
    pub fn report(&self, diag: Diag) {
        self.diags.borrow_mut().push(diag)
    }

    #[inline]
    pub fn has_errors(&self) -> bool {
        self.diags
            .borrow()
            .iter()
            .any(|d| matches!(d.level, DiagLevel::Error | DiagLevel::Unimplemented))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.diags.borrow().len()
    }

    fn render_ariadne(&self, diag: &Diag) {
        if diag.loc.is_none() || self.source_map.is_none() {
            let level_str = match diag.level {
                DiagLevel::Error => "error",
                DiagLevel::Warning => "warning",
                DiagLevel::Unimplemented => "unimplemented",
            };

            let mut msg = format!("{}: {}", level_str, diag.kind);
            if let Some(hint) = &diag.hint {
                msg.push_str(&format!("\n\nhint: {}", hint));
            }

            match diag.level {
                DiagLevel::Error | DiagLevel::Unimplemented => eprintln!("{}", msg),
                DiagLevel::Warning => println!("{}", msg),
            }
            return;
        }

        let loc = diag.loc.unwrap();
        let source_map = self.source_map.as_ref().unwrap();
        let source_file = source_map.get_file(loc.file_id).unwrap().clone();
        let file_path = source_file.file_path.to_str().unwrap_or("unknown");

        let level_str = match diag.level {
            DiagLevel::Error => "error",
            DiagLevel::Warning => "warning",
            DiagLevel::Unimplemented => "unimplemented",
        };

        let structured_msg = format!(
            "[{}][{}:{}:{}]: {}",
            level_str, file_path, loc.line, loc.column, diag.kind
        );

        match diag.level {
            DiagLevel::Error | DiagLevel::Unimplemented => eprintln!("{}", structured_msg),
            DiagLevel::Warning => println!("{}", structured_msg),
        }

        let kind = match diag.level {
            DiagLevel::Error => ReportKind::Error,
            DiagLevel::Warning => ReportKind::Warning,
            DiagLevel::Unimplemented => ReportKind::Error,
        };

        let color = match diag.level {
            DiagLevel::Error => Color::Red,
            DiagLevel::Warning => Color::Yellow,
            DiagLevel::Unimplemented => Color::Magenta,
        };

        let label_msg = match diag.level {
            DiagLevel::Error => "error occurred here",
            DiagLevel::Warning => "warning triggered here",
            DiagLevel::Unimplemented => "unimplemented feature used here",
        };

        let mut report = Report::build(kind, (file_path, loc.start..loc.end))
            .with_message(diag.kind.to_string())
            .with_label(
                Label::new((file_path, loc.start..loc.end))
                    .with_message(label_msg)
                    .with_color(color),
            );

        if let Some(hint) = &diag.hint {
            report = report.with_note(hint);
        }

        let report = report.finish();

        let mut stderr = std::io::stderr();
        let source = Source::from(source_file.content.as_str());
        let _ = report.write((file_path, source), &mut stderr);
    }
}

#[derive(Clone, Debug)]
pub enum CustomDiagKind {
    Custom(String),
}

impl DiagKind for CustomDiagKind {}

impl fmt::Display for CustomDiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CustomDiagKind::Custom(message) => write!(f, "{}", message),
        }
    }
}

#[macro_export]
macro_rules! exit_with_msg {
    ($msg:expr) => {
        $crate::reporter::DiagReporter::display_single($crate::Diag {
            level: $crate::DiagLevel::Error,
            kind: Box::new($crate::reporter::CustomDiagKind::Custom($msg.to_string())),
            loc: None,
            hint: None,
        });
        std::process::exit(1);
    };
}

#[macro_export]
macro_rules! exit_with_single_diag {
    ($diag:expr) => {
        $crate::reporter::DiagReporter::display_single($diag);
        std::process::exit(1);
    };
}
