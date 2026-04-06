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

use crate::context::AnalysisContext;
use cyrusc_typed_ast::{
    decls::{EnumDecl, EnumDeclID},
    exprs::{TypedEnumInit, TypedEnumStructVariantInit, TypedUnnamedEnumValue},
    types::SemanticType,
};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_enum_init(&self, enum_init: &TypedEnumInit) -> Option<SemanticType> {
        todo!();
    }

    pub(crate) fn analyze_unnamed_enum_value(
        &self,
        enum_value: &TypedUnnamedEnumValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let Some((enum_decl_id, enum_decl)) = self.infer_enum_decl_from_expected_type(expected_type) else {
            // FIXME
            todo!();
            return None;
        };

        todo!();
    }

    pub(crate) fn analyze_enum_struct_variant_init(
        &self,
        struct_variant_init: &TypedEnumStructVariantInit,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        todo!();
    }

    fn infer_enum_decl_from_expected_type(
        &self,
        expected_type: Option<SemanticType>,
    ) -> Option<(EnumDeclID, EnumDecl)> {
        expected_type.and_then(|sema_type| {
            sema_type.as_named_type().and_then(|named_type| {
                let id_opt = named_type.decl_id.as_enum();

                id_opt.map(|enum_decl_id| (enum_decl_id, self.decl_tables.enum_decl(enum_decl_id)))
            })
        })
    }
}
