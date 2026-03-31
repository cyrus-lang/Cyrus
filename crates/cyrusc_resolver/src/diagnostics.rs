use cyrusc_ast::ASTFuncDefStmt;
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
use cyrusc_diagcentral::{Diag, DiagKind, DiagLevel};
use cyrusc_internal::symbols::table::SymbolQuery;
use cyrusc_source_loc::Loc;
use cyrusc_typed_ast::SymbolID;
use thiserror::Error;

use crate::Resolver;

#[derive(Debug, Error, Clone)]
pub enum ModuleFSLoaderDiagKind {
    #[error("Module '{module_name}' couldn't be found in any of the specified source directories.")]
    ModuleNotFound { module_name: String },

    #[error(
        "Couldn't find stdlib anywhere. You can set it with 'CYRUS_STDLIB_PATH' environment variable or '--stdlib' command line argument."
    )]
    StdlibNotFound,

    #[error("Module '{module_name}' cannot exist as both a file and a directory.")]
    DuplicateModule { module_name: String },

    #[error("Module cannot import itself.")]
    ModuleCannotImportItself,
}

impl DiagKind for ModuleFSLoaderDiagKind {}

#[derive(Debug, Error, Clone)]
pub enum ResolverDiagKind {
    #[error("Label '{label_name}' already defined in this scope.")]
    LabelAlreadyDefined { label_name: String },

    #[error("Type '{type_name}' does not accept any type arguments.")]
    TypeDoesNotAcceptTypeArgs { type_name: String },

    #[error("Expected interface symbol to be implemented by the object, but got unknown.")]
    InvalidImplementInterface,

    #[error("Expected an ident in module import path, but found something else.")]
    ExpectedIdentifierInImport,

    #[error("Symbol '{symbol_name}' is private and cannot be imported.")]
    ImportSinglePrivateSymbol { symbol_name: String },

    #[error("Invalid literal suffix.")]
    InvalidLiteralSuffix,

    #[error("Symbol '{name}' not found anywhere.")]
    SymbolNotFound { name: String },

    #[error("Type '{name}' not found.")]
    TypeNotFound { name: String },

    #[error("Invalid top-level statement.")]
    InvalidTopLevelStatement,

    #[error("Invalid statement.")]
    InvalidStatement,

    #[error("Function parameter must have a type.")]
    InvalidUntypedFuncParam,

    #[error(
        "An import cycle was found, indicating a circular dependency between modules.\n\n{module_names}\n\nConsider resolving the cycle by refactoring the modules."
    )]
    ImportCycle { module_names: String },

    #[error("Cannot import module '{module_name}' twice.")]
    ImportTwice { module_name: String },

    #[error("Symbol '{symbol_name}' is not defined in module '{module_name}'.")]
    SymbolIsNotDefinedInModule { symbol_name: String, module_name: String },

    #[error("Symbol '{symbol_name}' has already been declared in this module.")]
    DuplicateSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' cannot be declared again. It already exists within the current scope.")]
    DuplicateSymbolInThisScope { symbol_name: String },

    #[error("Duplicate declaration of method '{method_name}' in '{struct_name}'.")]
    DuplicateMethodName { struct_name: String, method_name: String },

    #[error("Interface methods cannot be renamed.")]
    RenameInterfaceMethod,
}

impl DiagKind for ResolverDiagKind {}

impl Resolver {
    pub(crate) fn report_if_duplicate_symbol(&mut self, scope_id: SymbolID, symbol_name: String, loc: Loc) -> bool {
        match self.lookup_symbol_id_in_scope(scope_id, &symbol_name) {
            Some(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateSymbol { symbol_name }),
                    loc: Some(loc),
                    hint: None,
                });
                true
            }
            None => false,
        }
    }

    pub(crate) fn report_if_symbol_is_private(&mut self, symbol_id: SymbolID, loc: Loc) {
        let symbol_entry = self.lookup_symbol_entry(symbol_id).unwrap();

        // report if private symbol is being accessed if visibility specified for symbol
        if let Some(vis) = &symbol_entry.vis_opt {
            if vis.is_private() {
                let symbol_name = self.format_symbol_name(symbol_id);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::ImportSinglePrivateSymbol { symbol_name }),
                    loc: Some(loc),
                    hint: None,
                });
            }
        }
    }

    pub(crate) fn report_if_duplicate_method_names(&mut self, struct_name: &str, ast_methods: &[ASTFuncDefStmt]) {
        let mut method_names: Vec<String> = Vec::new();

        for func_def in ast_methods {
            let method_name = func_def.ident.as_string();

            if method_names.contains(&method_name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(ResolverDiagKind::DuplicateMethodName {
                        struct_name: struct_name.to_string(),
                        method_name: method_name.clone(),
                    }),
                    loc: Some(func_def.loc),
                    hint: Some("Consider to rename the method to a different name.".to_string()),
                });
                continue;
            }

            method_names.push(method_name);
        }
    }
}
