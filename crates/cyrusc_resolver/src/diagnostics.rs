use cyrusc_ast::token::Location;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ResolverDiagKind {
    #[error("Type '{type_name}' does not accept any type arguments.")]
    TypeDoesNotAcceptTypeArgs { type_name: String },

    #[error("Expected an identifier in module import path, but found something else.")]
    ExpectedIdentifierInImport,

    #[error("Symbol '{symbol_name}' is private and cannot be imported.")]
    ImportSinglePrivateSymbol { symbol_name: String },

    #[error("Invalid literal suffix.")]
    InvalidLiteralSuffix,

    #[error("Symbol '{name}' is already defined (first defined at {original}).")]
    SymbolAlreadyDefined { name: String, original: Location },

    #[error("Symbol '{name}' not found anywhere.")]
    SymbolNotFound { name: String },

    #[error("Type '{name}' not found.")]
    TypeNotFound { name: String },

    #[error("Function '{name}' called with {found} arguments, but {expected} expected.")]
    FuncSignatureMismatch {
        name: String,
        expected: usize,
        found: usize,
    },

    #[error("Self used outside of a instance method context.")]
    InvalidSelfUsage,

    #[error("Self modifier must be the beginning parameter of a method.")]
    InvalidSelfModifier,

    #[error("Invalid return type: expected '{expected}', but found '{found}'.")]
    InvalidReturnType { expected: String, found: String },

    #[error("Unexpected symbol type for '{name}': expected '{expected}', but found '{found}'.")]
    UnexpectedSymbolType {
        name: String,
        expected: String,
        found: String,
    },

    #[error("Invalid top-level statement.")]
    InvalidTopLevelStatement,

    #[error("Invalid statement.")]
    InvalidStatement,

    #[error("Array capacity must be a positive constant integer.")]
    InvalidArrayCapacity,

    #[error("Function parameter must have a type.")]
    InvalidUntypedFuncParam,

    #[error("No local scope found here. This expression must be inside a function or block scope.")]
    RequiresLocalScope,

    #[error("Invalid operand for function call.")]
    InvalidOperandForFuncCall,

    #[error("Invalid operand for method call.")]
    InvalidOperandForMethodCall,

    #[error("Module '{module_name}' not found.")]
    ModuleImportNotFound { module_name: String },

    #[error(
        "An import cycle was found, indicating a circular dependency between modules.\nCycle Path: {module_names:?}\nConsider resolving the cycle by refactoring the modules."
    )]
    ImportCycle { module_names: Vec<String> },

    #[error("Cannot rename imported module when you considered to import singles.")]
    InvalidRenameWhenImportingModule,

    #[error("Cannot import directory as module.")]
    CannotImportDirectoryAsModule,

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

    #[error("Method '{method_name}' is not defined for struct '{struct_name}'.")]
    MethodNotDefined { struct_name: String, method_name: String },

    #[error("Duplicate declaration of method '{method_name}' in struct '{struct_name}'.")]
    DuplicateMethodName { struct_name: String, method_name: String },

    #[error("Interface methods cannot be renamed.")]
    RenameInterfaceMethod,
}
