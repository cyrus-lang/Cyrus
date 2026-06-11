// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::{Diag, DiagKind, DiagLevel};
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
            match diag.level {
                DiagLevel::Error => eprintln!("{}", self.render(diag)),
                DiagLevel::Warning => println!("{}", self.render(diag)),
                DiagLevel::Unimplemented => println!("{}", self.render(diag)),
            }
        }

        diags.clear();
    }

    pub fn display(&self) {
        let mut diags = self.diags.borrow_mut();

        for diag in diags.iter() {
            match diag.level {
                DiagLevel::Error => eprintln!("{}", self.render(diag)),
                DiagLevel::Warning => println!("{}", self.render(diag)),
                DiagLevel::Unimplemented => println!("{}", self.render(diag)),
            }
        }

        diags.clear();
        drop(diags);
    }

    #[inline]
    pub fn display_single(diag: Diag) {
        let reporter = DiagReporter::new_with_no_source_map();
        let output = reporter.render(&diag);
        eprintln!("{}", output);
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
}

impl DiagReporter {
    pub(crate) fn render(&self, diag: &Diag) -> String {
        let mut out = String::new();

        let level_text = {
            match diag.level {
                DiagLevel::Error => "error",
                DiagLevel::Warning => "warning",
                DiagLevel::Unimplemented => "unimplemented",
            }
        };

        if diag.loc.is_none() || self.source_map.is_none() {
            out.push_str(&format!("[{}]", level_text));
            out.push_str(&format!(": {}", diag.kind));
            out.push('\n');
            return out;
        }

        let loc = diag.loc.unwrap();
        let source_map = self.source_map.as_ref().unwrap();
        let source_file = { source_map.get_file(loc.file_id).unwrap().clone() };

        out.push_str(&format!("[{}]", level_text));
        out.push_str(&format!(
            "[{}:{}:{}]",
            source_file.file_path.to_str().unwrap(),
            loc.line,
            loc.column,
        ));
        out.push_str(&format!(": {}", diag.kind));
        out.push('\n');

        let lines: Vec<&str> = source_file.content.lines().collect();

        let line = lines[loc.line - 1];

        out.push_str(line);

        // render hints
        if let Some(hint) = &diag.hint {
            out.push_str("\n\n");
            out.push_str(&format!(" {}: {}\n", "hint", hint));
        }

        out
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
        cyrusc_diagcentral::reporter::DiagReporter::display_single(cyrusc_diagcentral::Diag {
            level: cyrusc_diagcentral::DiagLevel::Error,
            kind: Box::new(cyrusc_diagcentral::reporter::CustomDiagKind::Custom($msg)),
            loc: None,
            hint: None,
        });
        std::process::exit(1);
    };
}

#[macro_export]
macro_rules! exit_with_single_diag {
    ($diag:expr) => {
        cyrusc_diagcentral::reporter::DiagReporter::display_single($diag);
        std::process::exit(1);
    };
}
