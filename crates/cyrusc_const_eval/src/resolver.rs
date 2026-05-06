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

use cyrusc_typed_ast::{SymbolID, decls::DeclID, exprs::TypedExpr};

pub trait ConstResolver {
    fn is_decl_const(&self, decl_id: DeclID) -> bool;
    fn resolve_symbol_expr(&mut self, decl_id: DeclID) -> Option<TypedExpr>;
    fn lookup_symbol_as_decl_id(&self, symbol_id: SymbolID) -> Option<DeclID>;
}
