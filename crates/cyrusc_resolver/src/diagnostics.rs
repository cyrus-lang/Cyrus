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
use cyrusc_diagcentral::DiagKind;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ResolverDiagKind {
    #[error("Label '{label_name}' already defined in this scope.")]
    LabelAlreadyDefined { label_name: String },

    #[error("Type '{type_name}' does not accept any type arguments.")]
    TypeDoesNotAcceptTypeArgs { type_name: String },

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

    #[error("No local scope found here. This expression must be inside a function or block scope.")]
    RequiresLocalScope,

    #[error("Module '{module_name}' not found.")]
    ModuleImportNotFound { module_name: String },

    #[error(
        "An import cycle was found, indicating a circular dependency between modules.\nCycle Path: {module_names}\nConsider resolving the cycle by refactoring the modules."
    )]
    ImportCycle { module_names: String },

    #[error("Cannot import module '{module_name}' twice.")]
    ImportTwice { module_name: String },

    #[error("Module cannot import itself.")]
    ModuleCannotImportItself,

    #[error("Symbol '{symbol_name}' is not defined in module '{module_name}'.")]
    SymbolIsNotDefinedInModule { symbol_name: String, module_name: String },

    #[error("Symbol '{symbol_name}' has already been declared in this module.")]
    DuplicateSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' cannot be declared again. It already exists within the current scope.")]
    DuplicateSymbolInThisScope { symbol_name: String },

    #[error("Duplicate declaration of method '{method_name}' in struct '{struct_name}'.")]
    DuplicateMethodName { struct_name: String, method_name: String },

    #[error("Interface methods cannot be renamed.")]
    RenameInterfaceMethod,
}

impl DiagKind for ResolverDiagKind {}
