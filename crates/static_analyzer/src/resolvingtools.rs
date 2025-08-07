use resolver::scope::{LocalOrGlobalSymbol, LocalSymbol};
use typed_ast::{types::ConcreteType, ScopeID};

use crate::context::AnalysisContext;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn get_type_from_local_or_global_symbol(
        &mut self,
        scope_id_opt: Option<ScopeID>,
        local_or_global_symbol: LocalOrGlobalSymbol,
    ) -> Option<ConcreteType> {
        match local_or_global_symbol {
            LocalOrGlobalSymbol::LocalSymbol(local_symbol) => match local_symbol {
                LocalSymbol::Variable(resolved_variable) => match resolved_variable.typed_variable.ty {
                    Some(concrete_type) => Some(concrete_type),
                    None => self.get_typed_expr_type(scope_id_opt, &resolved_variable.typed_variable.rhs.unwrap()),
                },
                _ => Some(ConcreteType::Symbol(local_symbol.get_symbol_id())),
            },
            LocalOrGlobalSymbol::GlobalSymbol(symbol_entry) => Some(ConcreteType::Symbol(symbol_entry.get_symbol_id())),
        }
    }
}
