#[macro_export]
macro_rules! update_global_symbol {
    ($self:expr, $module_id:expr, $symbol_id:expr, $pattern:pat => $var:ident, $body:block) => {{
        let mut global_symbols = $self.resolver.global_symbols.lock().unwrap();
        let symbol_table = global_symbols.get_mut(&$module_id).unwrap();
        match &mut symbol_table.entries.get_mut(&$symbol_id).unwrap().kind {
            $pattern => {
                let $var = $var;
                $body
            }
            _ => {
                unreachable!()
            }
        }
    }};
}

#[macro_export]
macro_rules! update_local_symbol {
    ($self:expr, $scope_id:expr, $symbol_id:expr, $pattern:pat => $var:ident, $body:block) => {{
        let scope_rc = $self.resolver.resolve_local_scope($self.module_id, $scope_id).unwrap();
        scope_rc
            .borrow_mut()
            .with_symbol_id_mut($symbol_id, |local_symbol| match &mut local_symbol.kind {
                $pattern => {
                    let $var = $var;
                    $body
                }
                _ => unreachable!(),
            });
    }};
}
