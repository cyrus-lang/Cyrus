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
use cyrusc_const_eval::{fold::ConstFolder, value::is_comptime_valid};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_typed_ast::{
    exprs::{TypedArrayExpr, TypedArrayIndexExpr, literal_expr_from_const_int},
    format::format_sema_type,
    types::{SemaType, TypedArrayCapacity, TypedArrayType},
};

impl<'a> AnalysisContext<'a> {
    /// Analyzes array literal expressions with type and capacity validation.
    ///
    /// Type-checks array literals by validating each element type matches the array's
    /// element type and verifying the element count matches the declared array capacity.
    pub(crate) fn analyze_array(
        &mut self,
        array: &mut TypedArrayExpr,
        expected_type: Option<SemaType>,
    ) -> Option<SemaType> {
        macro_rules! array_type {
            () => {
                array.ty.clone().unwrap().as_array_type().unwrap()
            };
        }

        let mut analyzed_first_element = false;

        let expected_element_type = expected_type.and_then(|sema_type| {
            sema_type
                .as_array_type()
                .map(|array_type| *array_type.element_type.clone())
        });

        let elements_count = array.elements.len();

        // try to infer from first element
        if array.ty.is_none() {
            if let Some(first_elem) = array.elements.first_mut() {
                if let Some(sema_type) = self.analyze_expr(first_elem, expected_element_type.clone()) {
                    let elements_count_expr =
                        literal_expr_from_const_int(elements_count.try_into().unwrap(), first_elem.loc);

                    array.ty = Some(SemaType::Array(TypedArrayType {
                        element_type: Box::new(sema_type),
                        capacity: TypedArrayCapacity::Fixed(Box::new(elements_count_expr)),
                        loc: array.loc,
                    }));
                }

                analyzed_first_element = true;
            }
        }

        // try to infer from expected type
        if array.ty.is_none() {
            if let Some(sema_type) = expected_element_type {
                let elements_count_expr = literal_expr_from_const_int(elements_count.try_into().unwrap(), array.loc);

                array.ty = Some(SemaType::Array(TypedArrayType {
                    element_type: Box::new(sema_type),
                    capacity: TypedArrayCapacity::Fixed(Box::new(elements_count_expr)),
                    loc: array.loc,
                }));
            }
        }

        if array.ty.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UntypedArrayCannotBeInferred),
                loc: Some(array.loc),
                hint: None,
            });
        }

        array.ty = match self.normalize_and_check_type_formation(array.ty.clone()?, array.loc) {
            Some(sema_type) => Some(sema_type),
            None => return None,
        };

        for (i, element) in array.elements.iter_mut().enumerate() {
            let expr_type: SemaType;

            if analyzed_first_element && element.sema_type.is_some() {
                expr_type = match self.normalize_and_check_type_formation(element.sema_type.clone().unwrap(), element.loc) {
                    Some(sema_type) => sema_type,
                    None => continue,
                };
            } else {
                expr_type = match self.analyze_expr(element, Some(*array_type!().element_type.clone())) {
                    Some(sema_type) => sema_type,
                    None => continue,
                };
            }

            if !self.is_assignable_to(expr_type.clone(), *array_type!().element_type.clone(), element.loc) {
                let element_type = format_sema_type(expr_type, self.formatter);
                let expected_type = format_sema_type(*array_type!().element_type.clone(), self.formatter);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ArrayElementTypeMismatch {
                        element_type,
                        element_index: i.try_into().unwrap(),
                        expected_type,
                    }),
                    loc: Some(array.loc),
                    hint: None,
                });
            }
        }

        let mut array_type = array_type!().clone();

        let array_capacity = match &mut array_type.capacity {
            TypedArrayCapacity::Fixed(expr) => {
                self.analyze_expr(expr, None)?;

                if !is_comptime_valid(&expr.kind) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ExprNotComptimeValid),
                        loc: Some(array_type.loc),
                        hint: None,
                    });
                }

                let mut folder = ConstFolder::new(self);
                folder.expr_as_const_int(&expr).unwrap()
            }
            TypedArrayCapacity::Dynamic => todo!(),
        };

        if array.elements.len() != array_capacity.try_into().unwrap() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ArrayElementsCountMismatch {
                    elements: array.elements.len().try_into().unwrap(),
                    expected: array_capacity.try_into().unwrap(),
                }),
                loc: Some(array.loc),
                hint: None,
            });
            return None;
        }

        Some(SemaType::Array(
            array.ty.clone().unwrap().as_array_type().unwrap().clone(),
        ))
    }

    /// Analyzes array index expressions with bounds and type validation.
    ///
    /// Type-checks array indexing operations on both arrays and pointers.
    /// Validates the index expression is integer type and the operand is
    /// indexable (array or pointer). Returns the element type with proper
    /// const qualification.
    pub(crate) fn analyze_array_index(&mut self, array_index: &mut TypedArrayIndexExpr) -> Option<SemaType> {
        let operand_type = match self.analyze_expr(&mut array_index.operand, None) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        let is_operand_const = operand_type.is_const();
        let is_operand_array = operand_type.const_inner().is_array();

        if !(operand_type.is_pointer() || is_operand_array) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ArrayIndexOnNonArrayOperand),
                loc: Some(array_index.loc),
                hint: None,
            });
            return None;
        }

        let expected_index_type = array_index.index.sema_type.clone();

        let index_concrete_type = match self.analyze_expr(&mut array_index.index, expected_index_type) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        if !index_concrete_type
            .const_inner()
            .as_plain_type()
            .and_then(|b| Some(b.is_integer()))
            .is_some()
        {
            let found_type = format_sema_type(index_concrete_type, self.formatter);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ArrayNonIntegerIndex { found_type }),
                loc: Some(array_index.loc),
                hint: None,
            });
            return None;
        }

        let sema_type = array_index.operand.sema_type.clone().unwrap();

        let element_type: SemaType;

        if is_operand_array {
            let array_type = sema_type.as_array_type().unwrap();
            element_type = *array_type.element_type.clone();
        } else {
            // array index on pointer operand
            element_type = sema_type.pointer_inner().clone();

            if element_type.is_void() {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DerefVoidPointerValue),
                    loc: Some(array_index.loc),
                    hint: None,
                });
                return None;
            }
        }

        if is_operand_const {
            Some(element_type.as_const())
        } else {
            Some(element_type)
        }
    }
}
