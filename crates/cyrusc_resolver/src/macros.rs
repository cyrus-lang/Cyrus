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

#[macro_export]
macro_rules! scope_required {
    ($self:expr, $scope_opt:expr, $loc:expr, $span_end:expr) => {{
        if $scope_opt.is_none() {
            $self.reporter.report(cyrusc_diagcentral::Diag {
                level: cyrusc_diagcentral::DiagLevel::Error,
                kind: Box::new(crate::diagnostics::ResolverDiagKind::RequiresLocalScope),
                location: Some(cyrusc_diagcentral::DiagLoc::new(
                    cyrusc_diagcentral::source_loc::SourceLoc::from_loc($loc, $self.current_file_path()),
                )),
                hint: None,
            });
        }

        $scope_opt.is_none()
    }};
}
