use crate::diagnostics::GenericsDiagKind;
use ast::source_loc::SourceLoc;
use diagcentral::{Diag, DiagLevel, DiagLoc};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use typed_ast::{
    ScopeID, SymbolID, TypedGenericParam, TypedGenericParamsList, TypedIdentifier, TypedTypeArg, TypedTypeArgs,
    types::ConcreteType,
};

mod diagnostics;

