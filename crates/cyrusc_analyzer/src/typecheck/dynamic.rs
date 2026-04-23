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

use crate::{context::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{exprs::TypedDynamicExpr, types::SemaType};

impl<'a> AnalysisContext<'a> {
    pub(crate) fn analyze_dynamic(
        &mut self,
        dynamic: &mut TypedDynamicExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        let operand_type = self.analyze_expr(&mut dynamic.operand, None)?;

        if dynamic.operand.kind.is_dynamic() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidMultipleDynamicType),
                loc: Some(dynamic.loc),
                hint: None,
            });
            return None;
        }

        let Some(interface_decl_id) = expected_type.clone().and_then(|sema_ty| sema_ty.as_interface()) else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotInferDynamicInterfaceType),
                loc: Some(dynamic.loc),
                hint: None,
            });
            return None;
        };

        let named_type = operand_type.as_named_type().unwrap();

        let interface_decl = self.decl_tables.interface_decl(interface_decl_id);
        let interface_type_args = expected_type
            .as_ref()
            .unwrap()
            .as_named_type()
            .cloned()
            .unwrap()
            .type_args;

        let object_methods = self.methods_decl_of_named_type(named_type).unwrap();

        self.vtable_registry.register(
            operand_type.clone(),
            interface_decl_id,
            interface_type_args,
            object_methods,
            interface_decl.loc,
        );

        dynamic.concrete_type = Some(operand_type);
        dynamic.ty = Some(expected_type.clone().unwrap());

        Some(expected_type.unwrap())
    }
}
