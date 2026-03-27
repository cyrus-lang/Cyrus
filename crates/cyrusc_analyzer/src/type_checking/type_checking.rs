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

use crate::{analyze::AnalysisContext, diagnostics::AnalyzerDiagKind};
use cyrusc_ast::{SelfModifierKind, abi::Visibility};
use cyrusc_const_eval::{fold::ConstFolder, value::is_comptime_valid};
use cyrusc_diagcentral::{Diag, DiagLevel};
use cyrusc_internal::symbols::{symbols::*, table::SymbolEntryMut};
use cyrusc_source_loc::Loc;
use cyrusc_strescape::unescape_string;
use cyrusc_tokens::{
    TokenKind,
    literals::{LiteralKind, StringPrefix},
};
use cyrusc_typed_ast::{
    exprs::*,
    format::{
        SymbolFormatterFn, format_func_type, format_missing_fields, format_sema_type, format_typed_expr,
        format_unnamed_enum_type,
    },
    generics::{generic_type::GenericType, mapping_ctx::GenericMappingCtx, substitute::*},
    sigs::*,
    stmts::*,
    types::*,
    *,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub(crate) struct FuncEnv {
    pub(crate) current_func_type: Option<TypedFuncType>,
    pub(crate) current_self_type: Option<SemanticType>,
    pub(crate) current_object_type: Option<SemanticType>,
    pub(crate) current_method_symbol_id: Option<SymbolID>,
}

#[derive(Debug, Clone)]
struct FieldEnv {
    fields: HashMap<String, SemanticType>,
}

// Analysis Entry Points
//
// These functions are the primary entry points for type checking different
// expression categories. They handle top-level analysis and dispatch to
// specialized helpers for detailed checking.
impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    pub(crate) fn analyze_expr(
        &mut self,
        expr: &mut TypedExprStmt,
        mut expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        match &expr.kind {
            TypedExprKind::Symbol(symbol_expr) => {
                let symbol_entry = self.query.get_symbol(symbol_expr.symbol_id).unwrap();
                debug_assert!(!matches!(symbol_entry.kind, SymbolEntryKind::Unresolved));

                if !symbol_entry.is_kind_of_variable() && !symbol_entry.as_func().is_some() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnknownSymbol {
                            symbol_name: fmt_symbol(symbol_expr.symbol_id),
                        }),
                        loc: Some(symbol_expr.loc),
                        hint: None,
                    });
                    return None;
                }
            }
            _ => {}
        };

        // if the expected type is a generic parameter with a default, use the default type
        if let Some(sema_type) = &expected_type {
            if let Some(generic_param) = sema_type.as_generic_param() {
                expected_type = generic_param.default.clone().map(|sema_type| *sema_type);
            }
        }

        self.analyze_expr_non_terminal(expr, expected_type)
    }

    pub(crate) fn analyze_expr_non_terminal(
        &mut self,
        expr: &mut TypedExprStmt,
        mut expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        if let Some(sema_type) = expected_type {
            expected_type = Some(sema_type.const_inner().clone());
        }

        self.lower_special_exprs(expr, expected_type.clone());

        let ty_opt = match &mut expr.kind {
            TypedExprKind::Symbol(symbol_expr) => self.resolve_symbol_type(symbol_expr.symbol_id, symbol_expr.loc),
            TypedExprKind::Assign(assign) => {
                self.analyze_assign(assign);
                assign.rhs.sema_type.clone()
            }
            TypedExprKind::Literal(literal) => self.analyze_literal(literal, expected_type),
            TypedExprKind::Prefix(prefix) => self.analyze_prefix_expr_type(prefix, expected_type),
            TypedExprKind::Infix(infix) => self.analyze_infix_expr_type(infix, expected_type),
            TypedExprKind::Unary(unary) => self.analyze_unary_expr_type(unary),
            TypedExprKind::Array(array) => self.analyze_array(array, expected_type),
            TypedExprKind::ArrayIndex(array_index) => self.analyze_array_index(array_index),
            TypedExprKind::AddrOf(addr_of) => self.analyze_addr_of_expr_type(addr_of),
            TypedExprKind::Deref(deref) => self.analyze_deref_expr_type(deref),
            TypedExprKind::StructInit(struct_init) => self.analyze_struct_init(struct_init, expected_type),
            TypedExprKind::FuncCall(func_call) => self.analyze_func_call(func_call, expected_type),
            TypedExprKind::FieldAccess(field_access) => self.analyze_field_access_type(field_access, expected_type),
            TypedExprKind::MethodCall(method_call) => self.analyze_method_call(method_call, expected_type),
            TypedExprKind::Lambda(lambda) => self.analyze_lambda(lambda),
            TypedExprKind::Tuple(tuple_value) => self.analyze_tuple_value(tuple_value, expected_type),
            TypedExprKind::TupleAccess(tuple_member_access) => {
                self.analyze_tuple_member_access(tuple_member_access, expected_type)
            }
            TypedExprKind::UnnamedStructValue(unnamed_struct_value) => {
                self.analyze_unnamed_struct_value(unnamed_struct_value, expected_type)
            }
            TypedExprKind::UnnamedEnumValue(unnamed_enum_value) => {
                self.analyze_unnamed_enum_value(unnamed_enum_value, expected_type)
            }
            TypedExprKind::UnnamedUnionValue(unnamed_union_value) => {
                self.analyze_unnamed_union_value(unnamed_union_value, expected_type)
            }
            TypedExprKind::Dynamic(dynamic) => self.analyze_dynamic_expr(dynamic, expected_type),
            TypedExprKind::Builtin(_builtin) => todo!(),

            // invalid
            TypedExprKind::SemanticType(_) => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidUsageOfTheSemanticType),
                    loc: Some(expr.loc),
                    hint: None,
                });
                return None;
            }
        };

        let normalized_type = self.normalize_sema_type(ty_opt.clone()?, expr.loc);
        expr.sema_type = Some(normalized_type.clone()?);

        if cfg!(debug_assertions) {
            if let Some(concrete_type_clone) = expr.sema_type.clone() {
                let is_unresolved_symbol = matches!(concrete_type_clone, SemanticType::UnresolvedSymbol(..));
                assert!(is_unresolved_symbol == false);
            }

            if expr.sema_type.is_none() {
                panic!("expr.sema_type is empty!");
            }
        }

        self.fold_const_expr(expr);
        normalized_type
    }

    pub(crate) fn analyze_literal(
        &mut self,
        literal: &mut TypedLiteralExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let literal_clone = literal.clone();

        let ty_opt = match &mut literal.kind {
            LiteralKind::Integer(_, suffix_opt) => {
                match infer_integer_type(&literal_clone, suffix_opt, expected_type.clone()) {
                    Ok(ty) => Some(ty),
                    Err(diag) => {
                        self.reporter.report(diag);
                        None
                    }
                }
            }
            LiteralKind::Float(_, suffix_opt) => {
                match infer_float_type(&literal_clone, suffix_opt, expected_type.clone()) {
                    Ok(ty) => Some(ty),
                    Err(diag) => {
                        self.reporter.report(diag);
                        None
                    }
                }
            }
            LiteralKind::String(value, prefix_opt) => {
                *value = match unescape_string(&value).and_then(|v| unescape_string(&v)) {
                    Ok(v) => v,
                    Err(unescape_err) => {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::UnescapeError(unescape_err)),
                            loc: Some(literal.loc),
                            hint: None,
                        });
                        return None;
                    }
                };

                let capacity = literal_expr_from_const_int(value.len().try_into().unwrap(), literal.loc);

                let ty = if let Some(prefix) = prefix_opt {
                    match prefix {
                        StringPrefix::C => SemanticType::Pointer(Box::new(SemanticType::PlainType(PlainType::Char))),
                        StringPrefix::B => SemanticType::Array(TypedArrayType {
                            element_type: Box::new(SemanticType::Const(Box::new(SemanticType::PlainType(
                                PlainType::Char,
                            )))),
                            capacity: TypedArrayCapacity::Fixed(Box::new(capacity)),
                            loc: literal.loc,
                        }),
                    }
                } else {
                    SemanticType::Pointer(Box::new(SemanticType::PlainType(PlainType::Char)))
                };

                Some(ty)
            }
            LiteralKind::Bool(_) => Some(SemanticType::PlainType(PlainType::Bool)),
            LiteralKind::Char(_) => Some(SemanticType::PlainType(PlainType::Char)),
            LiteralKind::Null => Some(SemanticType::PlainType(PlainType::Null)),
        };

        if let Some(ty) = &ty_opt {
            literal.ty = Some(ty.clone());
        }

        ty_opt
    }

    fn analyze_lambda(&mut self, lambda: &mut TypedLambdaExpr) -> Option<SemanticType> {
        let parent_func = self.fn_env.current_func_type.clone();

        self.normalize_func_params(&mut lambda.params, lambda.loc);
        lambda.ret_type = self.normalize_sema_type(lambda.ret_type.clone(), lambda.loc)?;
        let params = typed_func_params_as_func_type_params(&lambda.params);
        let func_type = TypedFuncType {
            symbol_id: None,
            def_module_id: Some(self.module_id),
            params,
            ret_type: Box::new(lambda.ret_type.clone()),
            is_public: true,
            loc: lambda.loc,
        };

        self.fn_env.current_func_type = Some(func_type.clone());
        self.analyze_block_stmt(&mut lambda.body);

        self.fn_env.current_func_type = parent_func;
        Some(SemanticType::FuncType(func_type))
    }

    /// Analyzes tuple value expressions, inferring types from elements.
    ///
    /// Type-checks tuple literals by analyzing each element expression and
    /// constructing a tuple type from the element types. Uses expected type
    /// context to guide element type inference when available.
    fn analyze_tuple_value(
        &mut self,
        tuple_value: &mut TypedTupleExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let mut elements: Vec<SemanticType> = Vec::new();

        let tuple_type_opt = match expected_type {
            Some(sema_type) => sema_type.as_tuple_type().cloned(),
            None => None,
        };

        for (i, expr) in &mut tuple_value.elements.iter_mut().enumerate() {
            let mut expected_type: Option<SemanticType> = None;

            if let Some(tuple_type) = &tuple_type_opt {
                expected_type = tuple_type.elements.get(i).cloned();
            }

            match self.analyze_expr(expr, expected_type) {
                Some(sema_type) => elements.push(sema_type),
                None => continue,
            }
        }

        Some(SemanticType::Tuple(TypedTupleType {
            elements,
            loc: tuple_value.loc,
        }))
    }

    // REVIEW: Refactor required.
    fn analyze_unnamed_union_value(
        &mut self,
        unnamed_union_value: &mut TypedUnnamedUnionValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        // REVIEW: Use helper instead of this!!
        let unnamed_union_type = match expected_type.as_ref().and_then(|sema_type| {
            if let Some(unnamed_union_type) = sema_type.as_unnamed_union() {
                return Some(unnamed_union_type);
            } else if let Some(union_id) = sema_type.as_union_symbol_id() {
                let resolved_union = self.query.get_union(union_id)?;

                return Some(union_sig_as_unnamed_union_type(
                    &resolved_union.union_sig,
                    unnamed_union_value.loc,
                ));
            } else if let Some(generic_type) = sema_type.as_generic_type() {
                let resolved_union = self.query.get_union(generic_type.base)?;

                let union_sig = substitute_union_sig(
                    self.mapping_ctx_arena.clone(),
                    &resolved_union.union_sig,
                    generic_type.mapping_ctx.clone(),
                )
                .unwrap();

                return Some(union_sig_as_unnamed_union_type(&union_sig, unnamed_union_value.loc));
            }

            None
        }) {
            Some(unnamed_union_type) => unnamed_union_type,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UnnamedUnionValueInfering),
                    loc: Some(unnamed_union_value.loc),
                    hint: None,
                });
                return None;
            }
        };

        let Some(mut field) = unnamed_union_type
            .fields
            .iter()
            .find(|field| field.name == unnamed_union_value.field_name.as_string())
            .cloned()
        else {
            let object_name = format_sema_type(expected_type.unwrap(), fmt_symbol);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                    struct_name: object_name,
                    field_name: unnamed_union_value.field_name.as_string(),
                }),
                loc: Some(unnamed_union_value.loc),
                hint: None,
            });
            return None;
        };

        field.ty = Box::new(self.normalize_sema_type(*field.ty.clone(), field.loc)?);

        self.analyze_expr(&mut unnamed_union_value.field_value, Some(*field.ty.clone()));

        unnamed_union_value.union_ty = Some(unnamed_union_type.clone());
        Some(SemanticType::UnnamedUnion(unnamed_union_type))
    }

    /// Analyzes unnamed enum value expressions.
    ///
    /// Type-checks unnamed enum value by validating variant existence,
    /// checking field requirements, and determining the resulting enum type. Handles
    /// three variant kinds:
    ///
    /// - Plain: Simple variants without fields
    /// - Fielded: Variants with field expressions
    /// - Valued: Variants with associated values
    ///
    /// Supports type inference from expected types, including generic enum substitutions,
    /// and provides detailed error reporting for mismatched variant usage.
    fn analyze_unnamed_enum_value(
        &mut self,
        unnamed_enum_value: &mut TypedUnnamedEnumValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        if let Some(sema_type) = expected_type {
            if let Some(unnamed_enum_type) = sema_type.as_unnamed_enum() {
                return self.analyze_unnamed_enum_value_from_unnamed_type(unnamed_enum_value, &unnamed_enum_type);
            }

            let (generic_type_opt, enum_sig_opt) = self.extract_enum_sig_from_expected_type(sema_type);

            if let Some(enum_sig) = enum_sig_opt {
                return self.analyze_unnamed_enum_value_from_enum_sig(unnamed_enum_value, generic_type_opt, enum_sig);
            }
        }

        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(AnalyzerDiagKind::UnnamedEnumValueInfering {
                variant_name: unnamed_enum_value.ident.as_string(),
            }),
            loc: Some(unnamed_enum_value.loc),
            hint: None,
        });
        None
    }

    /// Analyzes unnamed struct value expressions.
    ///
    /// Type-checks unnamed struct value by analyzing each field expression
    /// and constructing an unnamed struct type. Handles both explicitly typed
    /// and inferred field types.
    fn analyze_unnamed_struct_value(
        &mut self,
        unnamed_struct_value: &mut TypedUnnamedStructValue,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        self.validate_struct_repr_attr(
            &unnamed_struct_value.repr_attr,
            unnamed_struct_value.fields.len(),
            unnamed_struct_value.loc,
        );

        self.validate_align(&unnamed_struct_value.align, unnamed_struct_value.loc);

        let infer_ctx = self.field_env_from_struct_type(expected_type);

        let mut fields: Vec<TypedUnnamedStructTypeField> = Vec::new();

        for field in &mut unnamed_struct_value.fields {
            let field_expected_type = field.ty.clone().or(infer_ctx.get(&field.name).cloned());

            let field_value_type = match self.analyze_expr(&mut field.field_value, field_expected_type) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            if let Some(explicit_field_ty) = &field.ty {
                if !self.check_type_mismatch(field_value_type.clone(), explicit_field_ty.clone(), field.loc) {
                    let lhs_type = format_sema_type(explicit_field_ty.clone(), fmt_symbol);
                    let rhs_type = format_sema_type(field_value_type, fmt_symbol);
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch { lhs_type, rhs_type }),
                        loc: Some(field.loc),
                        hint: None,
                    });
                    return None;
                }
            }

            fields.push(TypedUnnamedStructTypeField {
                name: field.name.clone(),
                ty: Box::new(field.ty.clone().unwrap_or(field_value_type)),
                loc: field.loc,
            });
        }

        let unnamed_struct_type = TypedUnnamedStructType {
            fields,
            repr_attr: unnamed_struct_value.repr_attr.clone(),
            align: unnamed_struct_value.align.clone(),
            loc: unnamed_struct_value.loc,
        };

        Some(SemanticType::UnnamedStruct(unnamed_struct_type))
    }

    /// Analyzes array index expressions with bounds and type validation.
    ///
    /// Type-checks array indexing operations on both arrays and pointers.
    /// Validates the index expression is integer type and the operand is
    /// indexable (array or pointer). Returns the element type with proper
    /// const qualification.
    fn analyze_array_index(&mut self, array_index: &mut TypedArrayIndexExpr) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

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
            .as_basic_type()
            .and_then(|b| Some(b.is_integer()))
            .is_some()
        {
            let found_type = format_sema_type(index_concrete_type, fmt_symbol);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ArrayNonIntegerIndex { found_type }),
                loc: Some(array_index.loc),
                hint: None,
            });
            return None;
        }

        let sema_type = array_index.operand.sema_type.clone().unwrap();

        let element_type: SemanticType;
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

    // REVIEW: Refactor required.
    // This, and method_call does not share the same mechanism for resolving
    // the symbol method, and i wonder how we can simplify it.
    //
    /// Analyzes field access expressions with multi-dispatch for different types.
    ///
    /// Type-checks field accesses on structs, unnamed structs, unions and unnamed unions.
    /// Detects enum variant access patterns, handles generic types, and validates
    /// operator syntax. Dispatches to appropriate specialized analyzer based on
    /// the operand's type category.
    fn analyze_field_access_type(
        &mut self,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        macro_rules! not_supports_fields {
            ($this:expr, $loc:expr) => {{
                $this.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields),
                    loc: Some($loc),
                    hint: None,
                });
                return None;
            }};
        }

        // is this enum variant construction?

        self.analyze_expr_non_terminal(&mut field_access.operand, expected_type.clone())?;

        let is_operand_const = field_access.operand.sema_type.as_ref()?.is_const();

        let mut operand_type = field_access
            .operand
            .sema_type
            .as_ref()
            .map(|sema_type| sema_type.const_inner())
            .cloned()?;

        {
            if let Some(symbol_id) = operand_type.symbol_id() {
                if let Some(symbol_entry) = self.query.get_symbol(symbol_id) {
                    if self.report_if_unexpected_type_args(
                        &symbol_entry.symbol_generic_params(),
                        &field_access.type_args,
                        field_access.loc,
                    ) {
                        return None;
                    };
                }
            }
        }

        if let Some(sema_type) =
            self.merge_generic_operand_with_expected_type(operand_type.clone(), expected_type.clone())
        {
            operand_type = SemanticType::GenericType(sema_type);
        }

        {
            let (detected_as_enum_variant, sema_type) = self.maybe_enum_variant_constructor_from_field_access(
                operand_type.clone(),
                field_access,
                expected_type.clone(),
            );

            if detected_as_enum_variant {
                return sema_type;
            }
        }

        // multiplex field access

        let sema_type = self.analyze_expr(&mut field_access.operand, expected_type.clone())?;

        // for thin-arrow field access, unwrap const and pointer layers
        // to obtain the underlying pointee type used as the operand.
        let operand_type = sema_type.const_inner().pointer_inner();

        let generic_type_opt = operand_type.as_generic_type();

        let (return_sema_ty, is_generic) =
            match self.resolve_field_access_kind(&mut field_access.operand, expected_type.clone(), field_access.loc) {
                Some(field_access_kind) => match field_access_kind {
                    FieldAccessKind::UnnamedStruct(unnamed_struct_type) => (
                        self.analyze_unnamed_struct_field_access(
                            &unnamed_struct_type,
                            field_access,
                            expected_type.clone(),
                        ),
                        false,
                    ),
                    FieldAccessKind::NamedStruct(resolved_struct) => {
                        let mut struct_sig = resolved_struct.struct_sig.clone();
                        if let Some(generic_type) = generic_type_opt {
                            struct_sig = substitute_struct_sig(
                                self.mapping_ctx_arena.clone(),
                                &struct_sig,
                                generic_type.mapping_ctx.clone(),
                            )?;
                        }

                        (
                            self.analyze_struct_field_access(
                                field_access,
                                struct_sig.name.clone(),
                                struct_sig.fields.clone(),
                                struct_sig.methods.clone(),
                                resolved_struct.symbol_id,
                            ),
                            struct_sig.generic_params.is_some(),
                        )
                    }
                    FieldAccessKind::Union(resolved_union) => {
                        let mut union_sig = resolved_union.union_sig.clone();
                        if let Some(generic_type) = generic_type_opt {
                            union_sig = substitute_union_sig(
                                self.mapping_ctx_arena.clone(),
                                &union_sig,
                                generic_type.mapping_ctx.clone(),
                            )?;
                        }

                        (
                            self.analyze_union_field_access(&union_sig, field_access, expected_type),
                            union_sig.generic_params.is_some(),
                        )
                    }
                    FieldAccessKind::UnnamedUnion(unnamed_union_type) => (
                        self.analyze_unnamed_union_field_access(&unnamed_union_type, field_access, expected_type),
                        false,
                    ),
                },
                None => not_supports_fields!(self, field_access.loc),
            };

        if !is_generic && field_access.type_args.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                loc: Some(field_access.loc),
                hint: None,
            });
            return None;
        }

        if is_operand_const {
            return_sema_ty.map(|sema_type| sema_type.as_const())
        } else {
            return_sema_ty
        }
    }

    /// Analyzes struct/union initialization expressions.
    ///
    /// Type-checks struct and union initialization syntax, handling both generic and
    /// non-generic types. Delegates to specialized analyzers for structs vs unions
    /// after performing common validation and generic type initialization.
    fn analyze_struct_init(
        &mut self,
        struct_init: &mut TypedStructInitExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let symbol_entry = self.query.get_symbol(struct_init.symbol_id).unwrap();

        self.report_if_unexpected_type_args(
            &symbol_entry.symbol_generic_params(),
            &struct_init.type_args,
            struct_init.loc,
        );

        let mut sema_type = self.resolve_symbol_type(struct_init.symbol_id, struct_init.loc)?;

        if let Some(new_sema_ty) =
            self.merge_generic_operand_with_expected_type(sema_type.clone(), expected_type.clone())
        {
            sema_type = SemanticType::GenericType(new_sema_ty);
        }

        let Some(base_id) = sema_type.maybe_generic_base_symbol_id() else {
            // normalize the type to provide a meaningful error message.
            //
            // if the symbol isn't a struct/union, this might be an incorrect
            // array initialization attempt. Normalization ensures we report the
            // actual resolved type rather than a placeholder.

            sema_type = self.normalize_sema_type(sema_type, struct_init.loc)?;

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonStructSymbol {
                    symbol_name: format_sema_type(sema_type, fmt_symbol),
                }),
                loc: Some(struct_init.loc),
                hint: None,
            });
            return None;
        };

        let (generic_params, parent_mapping_ctx) = self.extract_and_merge_generic_context(
            &sema_type,
            symbol_entry.symbol_generic_params().as_ref(),
            expected_type.clone(),
        );

        let generic_type_opt = match self.init_generic_type_with_symbol_id(
            base_id,
            &mut struct_init.type_args,
            parent_mapping_ctx,
            generic_params.as_ref(),
            struct_init.loc,
        ) {
            Ok(opt) => opt?.1,
            Err(diag) => {
                self.reporter.report(diag);
                return None;
            }
        };

        if let Some(resolved_union) = symbol_entry.as_union() {
            return self.analyze_regular_union_init(struct_init, resolved_union, &generic_type_opt);
        } else if let Some(resolved_struct) = symbol_entry.as_struct() {
            let infer_ctx = self.field_env_from_struct_type(expected_type);
            return self.analyze_regular_struct_init(struct_init, resolved_struct, &generic_type_opt, &infer_ctx);
        }

        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(AnalyzerDiagKind::NonStructSymbol {
                symbol_name: fmt_symbol(base_id),
            }),
            loc: Some(struct_init.loc),
            hint: None,
        });

        None
    }

    /// Analyzes function call expressions, handling both named functions and function values.
    ///
    /// Type-checks function calls for both named functions and first-class function values
    /// (function pointers, lambdas). Handles generic function instantiation, private function
    /// access validation, and delegates to appropriate checking routines based on call type.
    fn analyze_func_call(
        &mut self,
        func_call: &mut TypedFuncCall,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let operand_type = self.analyze_expr_non_terminal(&mut func_call.operand, None)?;

        #[allow(unused_assignments)]
        let mut generic_type_opt: Option<GenericType> = None;
        let mut func_sig: FuncSig;

        if let Some(mut func_type) = operand_type.const_inner().as_func_type().cloned() {
            if !func_type.is_public && func_type.def_module_id != Some(self.module_id) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::PrivateFunctionCall {
                        name: format_typed_expr(&func_call.operand, fmt_symbol),
                    }),
                    loc: Some(func_call.loc),
                    hint: None,
                });
                return None;
            }

            if let Some(symbol_id) = func_type.symbol_id {
                func_sig = self.query.get_func(symbol_id).unwrap().func_sig;

                if self.report_if_unexpected_type_args(&func_sig.generic_params, &func_call.type_args, func_call.loc) {
                    return None;
                }

                let expected_mapping_ctx = self.export_expected_generic_mapping_ctx(expected_type);

                let (_, inner_generic_type_opt) = match self.init_generic_type_with_symbol_id(
                    symbol_id,
                    &mut func_call.type_args,
                    expected_mapping_ctx,
                    func_sig.generic_params.as_ref(),
                    func_call.loc,
                ) {
                    Ok(opt) => opt?,
                    Err(diag) => {
                        self.reporter.report(diag);
                        return None;
                    }
                };

                generic_type_opt = inner_generic_type_opt;
            } else {
                if func_call.type_args.is_some() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                        loc: Some(func_call.loc),
                        hint: Some("Lambdas never accept type args.".to_string()),
                    });
                    return None;
                }

                // normalize if is lambda call
                self.normalize_func_type_params(&mut func_type.params, func_call.loc);

                let ret_type = self.check_func_type_call(&mut func_type, &mut func_call.args, func_call.loc)?;

                func_call.ret_type = Some(ret_type.clone());
                return Some(ret_type);
            }
        } else {
            let symbol_name = format_sema_type(operand_type, fmt_symbol);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::NonFunctionSymbol { symbol_name }),
                loc: Some(func_call.loc),
                hint: None,
            });
            return None;
        }

        self.normalize_func_params(&mut func_sig.params, func_call.loc);

        self.check_func_call(
            &mut func_sig,
            &generic_type_opt,
            &mut func_call.args,
            func_call.loc,
            false,
        )?;

        self.symbol_mut
            .with_func_mut(func_sig.symbol_id.unwrap(), |resolved_func| {
                resolved_func.func_sig.params = resolved_func.func_sig.params.clone();
                resolved_func.func_sig.ret_type = func_sig.ret_type.clone();
            });

        // validate generic type instantiation
        if let Some(generic_type) = generic_type_opt {
            // substitute before analyzing body
            func_sig = substitute_func_sig(
                self.mapping_ctx_arena.clone(),
                &func_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            if let Err(diag) = generic_type.finalize(
                self.mapping_ctx_arena.clone(),
                func_sig.generic_params.clone().unwrap(),
                fmt_symbol,
            ) {
                self.reporter.report(diag);
                return None;
            }

            if !func_sig.is_func_decl {
                // only specialize function definition which necessarily includes the body block
                func_call.monomorph_id =
                    self.register_specialized_generic_func(&mut func_sig, &generic_type, None, &func_call.loc);
            }

            // substitutes the func type inside of the func_call operand
            func_call.operand.sema_type = substitute_type(
                self.mapping_ctx_arena.clone(),
                func_call.operand.sema_type.clone().unwrap(),
                generic_type.mapping_ctx,
            );
        }

        func_call.ret_type = Some(func_sig.ret_type.clone());
        Some(func_sig.ret_type)
    }

    // REVIEW: Refactor required.
    /// Analyzes method call expressions, handling both static and instance methods.
    ///
    /// Type-checks method calls by determining whether the call is on a type (static)
    /// or instance, resolving the object type, and delegating to regular method analysis.
    ///
    /// Special handling for enum variant constructors via method call syntax.
    fn analyze_method_call(
        &mut self,
        method_call: &mut TypedMethodCall,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        self.analyze_expr_non_terminal(&mut method_call.operand, expected_type.clone());

        let mut method_call_operand_ty = method_call
            .operand
            .sema_type
            .as_ref()
            .map(|sema_type| sema_type.const_inner())
            .cloned()?;

        method_call_operand_ty = self
            .normalize_sema_type(method_call_operand_ty, method_call.loc)
            .unwrap();

        // try as interface method call
        if let Some(interface_type) = method_call_operand_ty.as_interface_type() {
            return self.analyze_interface_method_call(method_call, interface_type);
        }

        // REVIEW: Refactor required.
        // Lower method_call as `TypedEnumConstructor` before it achieves to this pointer.
        // That must happen in `analyze_expr`,
        // This would also affect `CIRTraverse`.

        // try as enum variant constructor
        {
            let (detected_as_enum_variant, sema_type) = self.maybe_enum_variant_constructor_from_method_call(
                method_call_operand_ty.clone(),
                method_call,
                expected_type.clone(),
            );

            if detected_as_enum_variant {
                return sema_type;
            }
        }

        // REVIEW: END

        // method call analysis

        // this only used to determine that, it's instance/static method call.
        let unresolved_symbol_id = method_call.operand.kind.as_symbol_id();

        let is_instance_method_operand = unresolved_symbol_id
            .and_then(|symbol_id| self.query.get_symbol(symbol_id))
            .map_or(false, |symbol_entry| {
                symbol_entry.as_var().is_some() || symbol_entry.as_global_var().is_some()
            });

        let object_symbol_id = {
            let operand_type = method_call_operand_ty
                .symbol_id()
                .and_then(|symbol_id| self.resolve_var_or_global_var_type(symbol_id))
                .or(self.analyze_expr_non_terminal(&mut method_call.operand, expected_type.clone()))
                .map(|sema_type| sema_type.const_inner().clone())?;

            match operand_type {
                SemanticType::Interface(interface_type) => interface_type.symbol_id,
                SemanticType::GenericType(generic_type) => generic_type.base,
                SemanticType::ResolvedSymbol(resolved_symbol) => resolved_symbol.symbol_id(),
                SemanticType::Pointer(sema_type) => {
                    if sema_type.is_void() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                            loc: Some(method_call.loc),
                            hint: None,
                        });
                        return None;
                    }

                    self.normalize_and_extract_symbol_id(*sema_type.clone(), method_call.loc)
                        .unwrap()
                }
                _ => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                        loc: Some(method_call.loc),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        let symbol_entry = match self.query.get_symbol(object_symbol_id) {
            Some(symbol_entry) => symbol_entry,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsMethods),
                    loc: Some(method_call.loc),
                    hint: None,
                });
                return None;
            }
        };

        let object_id_opt = {
            if let Some(resolved_struct) = symbol_entry.as_struct() {
                // static method call
                Some(resolved_struct.symbol_id)
            } else if let Some(resolved_enum) = symbol_entry.as_enum() {
                Some(resolved_enum.symbol_id)
            } else if let Some(resolved_union) = symbol_entry.as_union() {
                Some(resolved_union.symbol_id)
            } else {
                // instance method call
                if let Some(resolved_var) = symbol_entry.as_var() {
                    let var_type = resolved_var
                        .variable
                        .ty
                        .clone()
                        .unwrap_or({
                            self.analyze_expr(&mut resolved_var.variable.rhs.clone().unwrap(), expected_type.clone())
                                .unwrap()
                        })
                        .const_inner()
                        .clone();

                    match self.normalize_and_extract_symbol_id(var_type, method_call.loc) {
                        Some(object_id) => Some(object_id),
                        None => None,
                    }
                } else if let Some(resolved_global_var) = symbol_entry.as_global_var() {
                    let var_type = resolved_global_var
                        .global_var_sig
                        .ty
                        .clone()
                        .unwrap()
                        .const_inner()
                        .clone();

                    match self.normalize_and_extract_symbol_id(var_type, method_call.loc) {
                        Some(object_id) => Some(object_id),
                        None => None,
                    }
                } else if let Some(resolved_interface) = symbol_entry.as_interface() {
                    Some(resolved_interface.interface_sig.symbol_id)
                } else {
                    None
                }
            }
        };

        let object_id = match object_id_opt {
            Some(object_id) => object_id,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectNotSupportsFields),
                    loc: Some(method_call.loc),
                    hint: None,
                });
                return None;
            }
        };

        // constructing generic type manually if operand is not generic but symbol is.
        // this is necessary because without it, a generic method that uses object's generic params
        // can never be inferred.
        if symbol_entry.symbol_generic_params().is_some()
            && method_call_operand_ty.pointer_inner().as_generic_type().is_none()
        {
            method_call_operand_ty = SemanticType::GenericType(GenericType {
                base: method_call_operand_ty.maybe_generic_base_symbol_id().unwrap(),
                type_args: None,
                mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
                mapping_ctx_arena: self.mapping_ctx_arena.clone(),
                generic_params: symbol_entry.symbol_generic_params().unwrap(),
                loc: method_call.loc,
            });
        }

        self.analyze_regular_method_call(
            method_call,
            object_id,
            method_call_operand_ty,
            is_instance_method_operand,
            expected_type,
        )
    }

    /// Analyzes array literal expressions with type and capacity validation.
    ///
    /// Type-checks array literals by validating each element type matches the array's
    /// element type and verifying the element count matches the declared array capacity.
    fn analyze_array(
        &mut self,
        array: &mut TypedArrayExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

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

                    array.ty = Some(SemanticType::Array(TypedArrayType {
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

                array.ty = Some(SemanticType::Array(TypedArrayType {
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

        array.ty = match self.normalize_sema_type(array.ty.clone()?, array.loc) {
            Some(sema_type) => Some(sema_type),
            None => return None,
        };

        for (i, element) in array.elements.iter_mut().enumerate() {
            let expr_type: SemanticType;

            if analyzed_first_element && element.sema_type.is_some() {
                expr_type = match self.normalize_sema_type(element.sema_type.clone().unwrap(), element.loc) {
                    Some(sema_type) => sema_type,
                    None => continue,
                };
            } else {
                expr_type = match self.analyze_expr(element, Some(*array_type!().element_type.clone())) {
                    Some(sema_type) => sema_type,
                    None => continue,
                };
            }

            if !self.check_type_mismatch(expr_type.clone(), *array_type!().element_type.clone(), element.loc) {
                let element_type = format_sema_type(expr_type, fmt_symbol);
                let expected_type = format_sema_type(*array_type!().element_type.clone(), fmt_symbol);

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

        Some(SemanticType::Array(
            array.ty.clone().unwrap().as_array_type().unwrap().clone(),
        ))
    }

    // FIXME: Replace with builtin @cast(Type, Expr);
    //
    /// Analyzes explicit type cast expressions.
    ///
    /// Validates explicit type casts by checking if the cast is allowed between
    /// the source and target types. Supports both compatible type casts and
    /// explicit conversions between distinct but convertible types.
    ///
    /// # Validation
    /// - Source type must be type-compatible with target type, or
    /// - Explicit conversion must be defined between the types.
    /// - Both operand and target types are normalized before checking.
    ///
    /// # Parameters
    /// - `scope_id_opt`: Scope for type analysis and normalization.
    /// - `cast`: The type cast expression to analyze.
    ///
    /// # Returns
    /// - `Some(SemanticType)`: The target type if cast is valid.
    /// - `None`: If cast is invalid (type mismatch and no explicit conversion).
    ///
    // fn analyze_cast(&mut self, cast: &mut TypedCastExpr) -> Option<SemanticType> {
    //     let operand = match self.analyze_expr( &mut cast.operand, Some(cast.target_type.clone())) {
    //         Some(sema_type) => sema_type.const_inner().clone(),
    //         None => return None,
    //     };

    //     cast.target_type = self
    //         .normalize_sema_type( cast.target_type.clone(), cast.loc)
    //         .unwrap()
    //         .const_inner()
    //         .clone();

    //     if !(self.check_type_mismatch( operand.clone(), cast.target_type.clone(), cast.loc)
    //         || self.check_explicit_typecast( operand.clone(), cast.target_type.clone()))
    //     {
    //         let lhs_type = format_sema_ty(cast.target_type.clone(), fmt_symbol);
    //         let rhs_type = format_sema_ty(operand, fmt_symbol);

    //         self.reporter.report(Diag {
    //             level: DiagLevel::Error,
    //             kind: Box::new(AnalyzerDiagKind::CastTypeMismatch { lhs_type, rhs_type }),
    //             loc: Some(DiagLoc::new(cast.loc)),
    //             hint: None,
    //         });
    //         return None;
    //     }

    //     Some(cast.target_type.clone())
    // }
    // FIXME

    // FIXME
    /// Analyzes dynamic expression operations (dynamic dispatch/interface implementation).
    ///
    /// Processes dynamic expressions that enable runtime polymorphism through interface types,
    /// validating type compatibility, constructing virtual method tables (vtables), and
    /// establishing dynamic dispatch capabilities. This function transforms static type
    /// information into runtime-usable dynamic type information.
    fn analyze_dynamic_expr(
        &mut self,
        dynamic: &mut TypedDynamicExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let operand_type = self.analyze_expr(&mut dynamic.operand, None)?;

        if dynamic.operand.kind.is_dynamic_expr() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidMultipleDynamicType),
                loc: Some(dynamic.loc),
                hint: None,
            });
            return None;
        }

        let Some(mut interface_type) = expected_type.and_then(|sema_type| sema_type.as_interface_type().cloned())
        else {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::CannotInferDynamicInterfaceType),
                loc: Some(dynamic.loc),
                hint: None,
            });
            return None;
        };

        let interface_name = fmt_symbol(interface_type.symbol_id);

        interface_type.methods.iter_mut().for_each(|func_sig| {
            set_self_modifier_type_in_func_sig(func_sig, &dynamic.operand.sema_type.as_ref().unwrap());

            // FIXME: We have to set this ??
            // set_self_modifier_symbol_id_in_func_sig(func_sig, SymbolID::new());
            todo!();
        });

        {
            let mut vtable_registry = self.vtable_registry.lock().unwrap();
            let vtable_id = vtable_registry.register(
                operand_type,
                interface_type.symbol_id,
                interface_name,
                interface_type.methods,
            );

            dynamic.vtable_id = Some(vtable_id);
            dynamic.object_name = Some(fmt_symbol(
                dynamic.operand.sema_type.as_ref().unwrap().symbol_id().unwrap(),
            ));

            Some(SemanticType::DynamicType(DynamicType {
                interface_id: interface_type.symbol_id,
                vtable_id,
                loc: dynamic.loc,
            }))
        }
    }
}

// Helper Functions
impl<'a, M: SymbolEntryMut> AnalysisContext<'a, M> {
    fn analyze_unnamed_enum_value_from_unnamed_type(
        &mut self,
        unnamed_enum_value: &mut TypedUnnamedEnumValue,
        unnamed_enum_type: &TypedUnnamedEnumType,
    ) -> Option<SemanticType> {
        let (sema_type, enum_ty) =
            self.validate_unnamed_enum_variant_from_unnamed_type(unnamed_enum_value, unnamed_enum_type)?;

        unnamed_enum_value.enum_ty = Some(enum_ty);
        self.normalize_sema_type(sema_type, unnamed_enum_value.loc)
    }

    fn analyze_unnamed_enum_value_from_enum_sig(
        &mut self,
        unnamed_enum_value: &mut TypedUnnamedEnumValue,
        generic_type_opt: Option<GenericType>,
        enum_sig: EnumSig,
    ) -> Option<SemanticType> {
        let (sema_type, enum_ty) =
            self.validate_unnamed_enum_variant_from_enum_sig(unnamed_enum_value, generic_type_opt.as_ref(), &enum_sig)?;

        unnamed_enum_value.enum_ty = Some(enum_ty);
        self.normalize_sema_type(sema_type, unnamed_enum_value.loc)
    }

    fn extract_enum_sig_from_expected_type(
        &mut self,
        sema_type: SemanticType,
    ) -> (Option<GenericType>, Option<EnumSig>) {
        if let Some(enum_id) = sema_type.as_enum_symbol_id() {
            if let Some(resolved_enum) = self.query.get_enum(enum_id) {
                return (None, Some(resolved_enum.enum_sig));
            }
        }

        if let Some(generic_type) = sema_type.as_generic_type() {
            if let Some(resolved_enum) = self.query.get_enum(generic_type.base) {
                let enum_sig = substitute_enum_sig(
                    self.mapping_ctx_arena.clone(),
                    &resolved_enum.enum_sig,
                    generic_type.mapping_ctx.clone(),
                )
                .unwrap();

                return (Some(generic_type.clone()), Some(enum_sig));
            }
        }

        (None, None)
    }

    fn validate_unnamed_enum_variant_from_unnamed_type(
        &mut self,
        unnamed_enum_value: &mut TypedUnnamedEnumValue,
        unnamed_enum_type: &TypedUnnamedEnumType,
    ) -> Option<(SemanticType, TypedUnnamedEnumValueTy)> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let variant = unnamed_enum_type
            .variants
            .iter()
            .find(|v| *v.ident() == unnamed_enum_value.ident)
            .or_else(|| {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::NoSuchEnumVariant {
                        enum_name: format_unnamed_enum_type(unnamed_enum_type, fmt_symbol),
                        variant_name: unnamed_enum_value.ident.as_string(),
                    }),
                    loc: Some(unnamed_enum_value.loc),
                    hint: None,
                });
                None
            })?;

        self.validate_unnamed_enum_value_variant_kind(unnamed_enum_value, variant)?;

        Some((
            SemanticType::UnnamedEnum(unnamed_enum_type.clone()),
            TypedUnnamedEnumValueTy::UnnamedEnum(unnamed_enum_type.clone()),
        ))
    }

    fn validate_unnamed_enum_variant_from_enum_sig(
        &mut self,
        unnamed_enum_value: &mut TypedUnnamedEnumValue,
        generic_type_opt: Option<&GenericType>,
        enum_sig: &EnumSig,
    ) -> Option<(SemanticType, TypedUnnamedEnumValueTy)> {
        let variant = enum_sig
            .variants
            .iter()
            .find(|v| *v.ident() == unnamed_enum_value.ident)?;

        self.validate_unnamed_enum_value_from_enum_sig_variant_kind(unnamed_enum_value, enum_sig, variant)?;

        if let Some(generic_type) = generic_type_opt {
            Some((
                SemanticType::GenericType(generic_type.clone()),
                TypedUnnamedEnumValueTy::EnumSig(enum_sig.clone()),
            ))
        } else {
            Some((
                SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(enum_sig.symbol_id)),
                TypedUnnamedEnumValueTy::EnumSig(enum_sig.clone()),
            ))
        }
    }

    // FIXME: Diags
    fn validate_unnamed_enum_value_variant_kind(
        &mut self,
        unnamed_enum_value: &mut TypedUnnamedEnumValue,
        variant: &TypedUnnamedEnumVariant,
    ) -> Option<()> {
        todo!();

        // match &mut unnamed_enum_value.kind {
        //     TypedUnnamedEnumValueKind::Plain => {
        //         if !matches!(
        //             variant,
        //             TypedUnnamedEnumVariant::Ident(..) | TypedUnnamedEnumVariant::Valued(..)
        //         ) {

        //             self.report_missing_fields_error(unnamed_enum_value);
        //             return None;
        //         }
        //     }

        //     TypedUnnamedEnumValueKind::Fielded(values) => match variant {
        //         TypedUnnamedEnumVariant::Ident(_) | TypedUnnamedEnumVariant::Valued(_, _) => {
        //             self.report_variant_does_not_accept_fields(unnamed_enum_value);
        //             return None;
        //         }

        //         TypedUnnamedEnumVariant::Variant(_, fields) => {
        //             if fields.len() != values.len() {
        //                 self.report_variant_arg_count_mismatch(unnamed_enum_value, fields.len());
        //                 return None;
        //             }

        //             for (expr, field) in values.iter_mut().zip(fields) {
        //                 self.analyze_expr(expr, Some(field.ty.clone()));
        //             }
        //         }
        //     },
        // }

        // Some(())
    }

    fn validate_unnamed_enum_value_from_enum_sig_variant_kind(
        &mut self,
        unnamed_enum_value: &mut TypedUnnamedEnumValue,
        enum_sig: &EnumSig,
        variant: &TypedEnumVariant,
    ) -> Option<()> {
        match &mut unnamed_enum_value.kind {
            TypedUnnamedEnumValueKind::Plain => {
                if !matches!(variant, TypedEnumVariant::Ident(..) | TypedEnumVariant::Valued(..)) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::VariantMissingFields {
                            enum_name: enum_sig.name.clone(),
                            variant_name: unnamed_enum_value.ident.as_string(),
                        }),
                        loc: Some(unnamed_enum_value.loc),
                        hint: None,
                    });
                    return None;
                }
            }

            TypedUnnamedEnumValueKind::Fielded(values) => match variant {
                TypedEnumVariant::Ident(_) | TypedEnumVariant::Valued(_, _) => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                            variant_name: unnamed_enum_value.ident.as_string(),
                        }),
                        loc: Some(unnamed_enum_value.loc),
                        hint: None,
                    });
                    return None;
                }

                TypedEnumVariant::Variant(_, values_fields) => {
                    if values_fields.len() != values.len() {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::EnumVariantArgCountMismatch {
                                variant_name: unnamed_enum_value.ident.as_string(),
                                expected: values_fields.len() as u32,
                                provided: values_fields.len() as u32,
                            }),
                            loc: Some(unnamed_enum_value.loc),
                            hint: None,
                        });

                        return None;
                    }

                    for (expr, field) in values.iter_mut().zip(values_fields) {
                        self.analyze_expr(expr, Some(field.ty.clone()));
                    }
                }
            },
        }

        Some(())
    }

    pub(crate) fn check_generic_typedef_missing_args(&mut self, symbol_id: SymbolID, loc: Loc) -> bool {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let Some(resolved_typedef) = self.query.get_typedef(symbol_id) else {
            return true; // it's okay
        };

        let includes_generic_params = resolved_typedef.typedef_sig.generic_params.is_some();

        if includes_generic_params {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::MissingTypeArgs {
                    type_name: fmt_symbol(resolved_typedef.symbol_id),
                }),
                loc: Some(loc),
                hint: None,
            });
            return false;
        }

        true // it's okay
    }

    /// Validates that generic types have provided type arguments when required.
    ///
    /// Checks whether a semantic type which might have generic params
    /// has the required type arguments specified. Reports an error if type args
    /// are used without type arguments in a context where they're mandatory.
    pub(crate) fn is_sema_type_missing_type_args(&mut self, sema_type: &SemanticType, loc: Loc) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        if let Some(symbol_id) = sema_type.maybe_generic_base_symbol_id() {
            let symbol_entry = self.query.get_symbol(symbol_id).unwrap();

            let is_generic_object = symbol_entry.symbol_generic_params().is_some();
            let is_generic_interface = symbol_entry.as_interface().is_some() && is_generic_object;
            let is_generic_type = sema_type.as_generic_type().is_some() || is_generic_interface;

            if is_generic_object && !is_generic_type {
                let type_name = format_sema_type(sema_type.clone(), fmt_symbol);

                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::MissingTypeArgs { type_name }),
                    loc: Some(loc),
                    hint: None,
                });
                return;
            }
        }
    }

    fn analyze_interface_method_call(
        &mut self,
        method_call: &mut TypedMethodCall,
        interface_type: &InterfaceType,
    ) -> Option<SemanticType> {
        let method_idx = interface_type
            .methods
            .iter()
            .position(|method| method.name == method_call.method_name)?;

        let mut func_sig = interface_type.methods[method_idx].clone();

        // interface uses void* for SelfType
        let self_type = SemanticType::Pointer(Box::new(SemanticType::PlainType(PlainType::Void)));
        set_self_modifier_type_in_func_sig(&mut func_sig, &self_type);
        set_self_modifier_symbol_id_in_func_sig(&mut func_sig, SymbolID::from(0));

        method_call.method_call_on_interface = Some(TypedInterfaceMethodCallMetadata {
            method_idx,
            methods_len: interface_type.methods.len(),
            method_sig: func_sig.clone(),
        });

        // always instance method call for interfaces
        let instance_method_call = true;

        let ret_type = self.check_func_call(
            &mut func_sig,
            &None,
            &mut method_call.args,
            method_call.loc,
            instance_method_call,
        )?;

        method_call.func_sig = Some(func_sig.clone());
        Some(ret_type)
    }

    /// Analyzes regular method calls on objects (structs, enums, unions, interface).
    ///
    /// Type-checks method calls on object instances, handling instance vs static
    /// method dispatch, generic method instantiation, and self parameter handling.
    /// Performs comprehensive validation including method existence, access control,
    /// and generic type inference from arguments.
    fn analyze_regular_method_call(
        &mut self,
        method_call: &mut TypedMethodCall,
        object_id: SymbolID,
        operand_type: SemanticType,
        is_instance_method_operand: bool,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        fn infer_generic_method_params<'a, M: SymbolEntryMut>(
            this: &mut AnalysisContext<'a, M>,
            generic_type_opt: Option<&GenericType>,
            func_sig_params_list: &Vec<TypedFuncParamKind>,
            method_call_args: &mut Vec<TypedExprStmt>,
        ) {
            // infer generic params from arguments
            for (i, arg) in method_call_args.iter_mut().enumerate() {
                let target_type = match func_sig_params_list.get(i).and_then(|param| param.param_type()) {
                    Some(sema_type) => sema_type,
                    None => continue,
                };

                this.analyze_expr(arg, None);

                if let Some(sema_type) =
                    this.infer_generic_param(generic_type_opt, target_type, arg.sema_type.clone(), arg.loc)
                {
                    arg.sema_type = Some(sema_type);
                }
            }
        }

        let object_name: String;
        let object_methods: Option<HashMap<String, SymbolID>>;
        let symbol_entry = self.query.get_symbol(object_id).unwrap();

        macro_rules! lookup_object_method {
            () => {{
                match object_methods.as_ref().unwrap().get(&method_call.method_name) {
                    Some(method_id) => self
                        .query
                        .get_method(*method_id)
                        .and_then(|resolved_method| Some(resolved_method.func_sig)),
                    None => None,
                }
            }};
        }

        let mut func_sig = {
            match match symbol_entry.kind.clone() {
                SymbolEntryKind::Struct(resolved_struct) => {
                    object_name = resolved_struct.struct_sig.name;
                    object_methods = Some(resolved_struct.struct_sig.methods);
                    lookup_object_method!()
                }
                SymbolEntryKind::Enum(resolved_enum) => {
                    object_name = resolved_enum.enum_sig.name;
                    object_methods = Some(resolved_enum.enum_sig.methods);
                    lookup_object_method!()
                }
                SymbolEntryKind::Union(resolved_union) => {
                    object_name = resolved_union.union_sig.name;
                    object_methods = Some(resolved_union.union_sig.methods);
                    lookup_object_method!()
                }
                SymbolEntryKind::Interface(resolved_interface) => {
                    object_name = resolved_interface.interface_sig.name.clone();
                    object_methods = None;

                    let Some(interface_method_idx) = resolved_interface
                        .interface_sig
                        .methods
                        .iter()
                        .position(|method| method.name == method_call.method_name.clone())
                    else {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::ObjectMethodNotDefined {
                                object_name: object_name.clone(),
                                method_name: method_call.method_name.clone(),
                            }),
                            loc: Some(method_call.loc),
                            hint: None,
                        });
                        return None;
                    };

                    let func_decl = &resolved_interface.interface_sig.methods[interface_method_idx];
                    let mut func_sig = typed_func_decl_as_func_sig(func_decl);

                    // NOTE: Interface is not sensitive to SelfType, that's why we can fake it with a `void*` type.
                    set_self_modifier_type_in_func_sig(
                        &mut func_sig,
                        &SemanticType::Pointer(Box::new(SemanticType::PlainType(PlainType::Void))),
                    );
                    set_self_modifier_symbol_id_in_func_sig(&mut func_sig, SymbolID::from(0));

                    method_call.method_call_on_interface = Some(TypedInterfaceMethodCallMetadata {
                        method_idx: interface_method_idx,
                        methods_len: resolved_interface.interface_sig.methods.len(),
                        method_sig: func_sig.clone(),
                    });

                    Some(func_sig)
                }
                _ => return None,
            } {
                Some(func_sig) => func_sig,
                None => {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectMethodNotDefined {
                            object_name: object_name.clone(),
                            method_name: method_call.method_name.clone(),
                        }),
                        loc: Some(method_call.loc),
                        hint: None,
                    });
                    return None;
                }
            }
        };

        // object name used later to make mangled abi name
        method_call.object_name = Some(object_name.clone());

        if self.report_if_unexpected_type_args(&func_sig.generic_params, &method_call.type_args, method_call.loc) {
            return None;
        }

        let is_instance_method_sig = func_sig.is_instance_method();
        let first_param_opt = func_sig.params.list.first();

        // invalid if static method called on instance
        let invalid_call = !is_instance_method_sig && is_instance_method_operand;

        if invalid_call {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::StaticMethodCallOnInstance {
                    method_name: method_call.method_name.clone(),
                }),
                loc: Some(method_call.loc),
                hint: Some(format!(
                    "Call it on a value of type '{}', or declare it as a static function if no instance is required.",
                    format_sema_type(operand_type, fmt_symbol)
                )),
            });
            return None;
        }

        if !self.validate_method_call(
            object_id,
            &method_call.method_name,
            operand_type.clone(),
            method_call.is_fat_arrow,
            method_call.method_call_on_interface.is_some(),
            first_param_opt,
            object_methods,
            object_name.clone(),
            &func_sig,
            method_call.loc,
        ) {
            return None;
        }

        self.fn_env.current_method_symbol_id = Some(func_sig.symbol_id.unwrap());

        let instance_method_call =
            (is_instance_method_sig && is_instance_method_operand) || method_call.method_call_on_interface.is_some();

        let mut operand_generic_type_opt = { operand_type.pointer_inner().as_generic_type().cloned() };

        let mut method_generic_type_opt = {
            match self.init_generic_type_with_symbol_id(
                func_sig.symbol_id.unwrap(),
                &mut method_call.type_args,
                None,
                func_sig.generic_params.as_ref(),
                method_call.loc,
            ) {
                Ok(opt) => opt?.1,
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            }
        };

        // 1. Generic Method, Generic Operand
        if let (Some(generic_method), Some(generic_operand)) = (&mut method_generic_type_opt, &operand_generic_type_opt)
        {
            let mut merged_generic_type = self.merge_generic_type(generic_method, generic_operand);

            if let Err(diag) = merged_generic_type.init(self.mapping_ctx_arena.clone(), fmt_symbol) {
                self.reporter.report(diag);
                return None;
            }

            infer_generic_method_params(
                self,
                Some(&merged_generic_type),
                &&func_sig.params.list,
                &mut method_call.args,
            );

            method_generic_type_opt = Some(merged_generic_type);
        }
        // 2. Generic Method, Concrete Operand
        else if let (Some(generic_method), None) = (&mut method_generic_type_opt, &operand_generic_type_opt) {
            if let Err(diag) = generic_method.init(self.mapping_ctx_arena.clone(), fmt_symbol) {
                self.reporter.report(diag);
                return None;
            }

            infer_generic_method_params(
                self,
                Some(generic_method),
                &&func_sig.params.list,
                &mut method_call.args,
            );

            method_generic_type_opt = match generic_method.finalize(
                self.mapping_ctx_arena.clone(),
                func_sig.generic_params.clone().unwrap(),
                fmt_symbol,
            ) {
                Ok(generic_type) => Some(generic_type.clone()),
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            };
        }
        // 3. Concrete Method, Generic Operand
        else if let (None, Some(generic_operand)) = (&method_generic_type_opt, &mut operand_generic_type_opt) {
            if let Err(diag) = generic_operand.init(self.mapping_ctx_arena.clone(), fmt_symbol) {
                self.reporter.report(diag);
                return None;
            }

            infer_generic_method_params(
                self,
                Some(generic_operand),
                &&func_sig.params.list,
                &mut method_call.args,
            );
        }
        // 4. Concrete Method, Concrete Operand (Non-Generic)
        else {
            // process non-generic method call
        }

        if !is_instance_method_operand && is_instance_method_sig {
            // explicit instance method
            // inferring self type from first argument type
            if let Some(mut expr) = method_call.args.first().cloned() {
                if let Some(sema_type) = self.analyze_expr(&mut expr, None) {
                    operand_generic_type_opt = sema_type.as_generic_type().cloned();

                    self.set_ty_ctx_self_type(method_call, &sema_type);
                    set_self_modifier_type_in_func_sig(&mut func_sig, &sema_type);
                }
            }
        }

        // merge method_mapping_ctx + operand_mapping_ctx
        let merged_generic_type_opt = {
            let method_mapping_ctx_sema_ty =
                method_generic_type_opt.map(|generic_type| SemanticType::GenericType(generic_type));

            self.merge_generic_operand_with_expected_type(operand_type.clone(), method_mapping_ctx_sema_ty.clone())
        };

        if let Some(generic_type) = &merged_generic_type_opt {
            // infer remaining generic params from expected type
            self.unify_generic_types_from_expected_type(&generic_type, expected_type);
        }

        // instance self type
        self.set_ty_ctx_self_type(method_call, &operand_type);

        func_sig.ret_type = self.check_func_call(
            &mut func_sig,
            &operand_generic_type_opt,
            &mut method_call.args,
            method_call.loc,
            instance_method_call,
        )?;

        if let Some(generic_type) = &merged_generic_type_opt {
            func_sig.ret_type = substitute_type(
                self.mapping_ctx_arena.clone(),
                func_sig.ret_type.clone(),
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();
        }

        // validate generic type instantiation
        if let Some(generic_type) = &merged_generic_type_opt {
            func_sig = substitute_func_sig(
                self.mapping_ctx_arena.clone(),
                &func_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            if let Some(generic_params) = func_sig.generic_params.clone() {
                if let Err(diag) = generic_type.finalize(self.mapping_ctx_arena.clone(), generic_params, fmt_symbol) {
                    self.reporter.report(diag);
                    return None;
                }
            }

            let func_call_loc = func_sig.loc;
            method_call.monomorph_id = self.register_specialized_generic_func(
                &mut func_sig,
                &generic_type,
                Some(SemanticType::GenericType(merged_generic_type_opt.clone().unwrap())),
                &func_call_loc,
            );

            // substitutes the func type inside of the func_call operand
            method_call.operand.sema_type = substitute_type(
                self.mapping_ctx_arena.clone(),
                method_call.operand.sema_type.clone().unwrap(),
                generic_type.mapping_ctx.clone(),
            );
        }

        if instance_method_call {
            let self_modifier_type = self_modifier_param_type(&func_sig.params, operand_type).unwrap();

            set_self_modifier_type_in_func_sig(&mut func_sig, &self_modifier_type);

            let self_modifier = func_sig.params.list.first().unwrap().as_self_modifier().unwrap();

            if method_call.method_call_on_interface.is_none() {
                method_call.args.insert(
                    0,
                    self.analyze_object_self_modifier_argument(
                        &method_call.operand,
                        method_call.is_fat_arrow,
                        self_modifier,
                    ),
                );
            }
        }

        method_call.func_sig = Some(func_sig.clone());
        Some(func_sig.ret_type.clone())
    }

    /// Extracts the pure symbol ID from a type, normalizing it first.
    ///
    /// Normalizes the given semantic type (resolving aliases, applying generics)
    /// then extracts the underlying pure symbol id if the type represents one.
    fn normalize_and_extract_symbol_id<'b>(&mut self, var_type: SemanticType, loc: Loc) -> Option<SymbolID> {
        self.normalize_sema_type(var_type, loc)?.maybe_generic_base_symbol_id()
    }

    /// Validates function calls against their signature, checking argument counts and types.
    ///
    /// Performs comprehensive validation of function calls including argument count checking,
    /// type compatibility validation, and variadic argument handling. Supports both regular
    /// functions and instance methods (with self parameter adjustment).
    fn check_func_call(
        &mut self,
        func_sig: &mut FuncSig,
        generic_type_opt: &Option<GenericType>,
        args: &mut Vec<TypedExprStmt>,
        loc: Loc,
        instance_method_call: bool,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let is_variadic = func_sig.params.variadic.is_some();
        let mut expected_args_len = func_sig.params.list.len();

        // if this is an instance method call, self modifier will be pushed later
        if instance_method_call && !func_sig.params.list.is_empty() {
            expected_args_len = expected_args_len.saturating_sub(1);
        }

        // check argument count
        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name: func_sig.name.clone(),
                }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        }

        // handle variadic arguments
        if is_variadic {
            self.check_func_variadic_arguments(&func_sig, args, loc);
        }

        // analyze static arguments
        let start_idx = if instance_method_call { 1 } else { 0 };

        for (param_idx, (param, arg)) in func_sig
            .params
            .list
            .iter_mut()
            .skip(start_idx)
            .zip(args.iter_mut())
            .enumerate()
        {
            let mut param_type = match self.func_param_type(param) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            let arg_type = match self.analyze_expr(arg, Some(param_type.clone())) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            param_type = match self.normalize_and_check_sema_ty(param_type, param.loc()) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            if let Some(sema_type) = self.infer_generic_param(
                generic_type_opt.as_ref(),
                param_type.clone(),
                Some(arg_type.clone()),
                arg.loc,
            ) {
                param_type = sema_type;
            }

            // skip if param type not inferred yet
            if param_type.as_generic_param().is_none() {
                continue;
            }

            if !self.check_type_mismatch(arg_type.clone(), param_type.clone(), arg.loc) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_sema_type(param_type.clone(), fmt_symbol),
                        argument_type: format_sema_type(arg_type, fmt_symbol),
                        argument_idx: param_idx as u32,
                    }),
                    loc: Some(loc),
                    hint: None,
                });
            }
        }

        self.normalize_sema_type(func_sig.ret_type.clone(), loc)
    }

    /// Validates variadic arguments in function calls, ensuring type compatibility and proper analysis.
    ///
    /// Performs type checking and semantic analysis for variadic arguments in function calls,
    /// handling both typed variadic parameters (with explicit type requirements) and untyped
    /// C-style variadic parameters. This function processes arguments beyond the static
    /// parameter list according to the function's variadic specification.
    fn check_func_variadic_arguments(&mut self, func_sig: &FuncSig, args: &mut Vec<TypedExprStmt>, loc: Loc) {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let static_params_len = func_sig.params.list.len();
        let variadic_args = &mut args[static_params_len..];

        if let Some(var_param) = &func_sig.params.variadic {
            match var_param.clone() {
                TypedFuncVariadicParams::Typed(_, variadic_param_type) => {
                    for (i, arg) in variadic_args.iter_mut().enumerate() {
                        if let Some(arg_type) = self.analyze_expr(arg, arg.sema_type.clone()) {
                            if !self.check_type_mismatch(arg_type.clone(), variadic_param_type.clone(), arg.loc) {
                                self.reporter.report(Diag {
                                    level: DiagLevel::Error,
                                    kind: Box::new(AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                        param_type: format_sema_type(variadic_param_type.clone(), fmt_symbol),
                                        argument_type: format_sema_type(arg_type, fmt_symbol),
                                        argument_idx: (i + static_params_len) as u32,
                                    }),
                                    loc: Some(loc),
                                    hint: None,
                                });
                            }
                        }
                    }
                }
                TypedFuncVariadicParams::UntypedCStyle => {
                    for arg in variadic_args.iter_mut() {
                        self.analyze_expr(arg, arg.sema_type.clone());
                    }
                }
            }
        }
    }

    /// Validates calls to function type values (function pointers, lambdas).
    ///
    /// Type-checks calls to first-class function values (function types) rather than
    /// named functions. Similar to `check_func_call` but works with function type
    /// definitions rather than function signatures.
    fn check_func_type_call(
        &mut self,
        func_type: &mut TypedFuncType,
        args: &mut Vec<TypedExprStmt>,
        loc: Loc,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let is_variadic = func_type.params.variadic.is_some();
        let expected_args_len = func_type.params.list.len();
        let func_name = format_func_type(func_type, fmt_symbol);

        // check argument count

        if (!is_variadic && args.len() != expected_args_len) || (is_variadic && args.len() < expected_args_len) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::FuncCallArgsCountMismatch {
                    args: args.len() as u32,
                    expected: expected_args_len as u32,
                    func_name,
                }),
                loc: Some(loc),
                hint: None,
            });
            return None;
        }

        // analyze static arguments

        let start_idx = 0;

        for (param_idx, (param, arg)) in func_type
            .params
            .list
            .iter_mut()
            .skip(start_idx)
            .zip(args.iter_mut())
            .enumerate()
        {
            let param_type = self.normalize_sema_type(param.clone(), loc).unwrap();

            let arg_type = match self.analyze_expr(arg, Some(param_type.clone())) {
                Some(sema_type) => sema_type,
                None => continue,
            };

            if !self.check_type_mismatch(arg_type.clone(), param_type.clone(), arg.loc) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::FuncCallParamTypeMismatch {
                        param_type: format_sema_type(param_type.clone(), fmt_symbol),
                        argument_type: format_sema_type(arg_type, fmt_symbol),
                        argument_idx: param_idx as u32,
                    }),
                    loc: Some(loc),
                    hint: None,
                });
            }
        }

        // handle variadic arguments

        if is_variadic {
            let static_params_len = func_type.params.list.len();
            let variadic_args = &mut args[static_params_len..];

            if let Some(var_param) = &func_type.params.variadic {
                match *var_param.clone() {
                    TypedFuncTypeVariadicParams::Typed(variadic_param_type) => {
                        for (i, arg) in variadic_args.iter_mut().enumerate() {
                            if let Some(arg_type) = self.analyze_expr(arg, arg.sema_type.clone()) {
                                if !self.check_type_mismatch(arg_type.clone(), variadic_param_type.clone(), arg.loc) {
                                    self.reporter.report(Diag {
                                        level: DiagLevel::Error,
                                        kind: Box::new(AnalyzerDiagKind::FuncCallVariadicParamTypeMismatch {
                                            param_type: format_sema_type(variadic_param_type.clone(), fmt_symbol),
                                            argument_type: format_sema_type(arg_type, fmt_symbol),
                                            argument_idx: (i + static_params_len) as u32,
                                        }),
                                        loc: Some(loc),
                                        hint: None,
                                    });
                                }
                            }
                        }
                    }
                    TypedFuncTypeVariadicParams::UntypedCStyle => {
                        for arg in variadic_args.iter_mut() {
                            self.analyze_expr(arg, arg.sema_type.clone());
                        }
                    }
                }
            }
        }

        Some(*func_type.ret_type.clone())
    }

    /// Extracts and normalizes the semantic type from a function parameter.
    ///
    /// Processes function parameters (regular parameters and self modifiers) by
    /// extracting their declared type and applying type normalization rules.
    /// Ensures parameter types are fully resolved before use in type checking.
    fn func_param_type(&mut self, param: &mut TypedFuncParamKind) -> Option<SemanticType> {
        match param {
            TypedFuncParamKind::FuncParam(param) => {
                let normalized = self.normalize_sema_type(param.ty.clone(), param.loc)?;
                param.ty = normalized.clone();
                Some(normalized)
            }
            TypedFuncParamKind::SelfModifier(self_modifier) => {
                let normalized = self.normalize_sema_type(self_modifier.ty.clone().unwrap(), self_modifier.loc)?;
                Some(normalized)
            }
        }
    }

    /// Analyzes union initialization expressions with exactly one field.
    ///
    /// Type-checks union initialization syntax which requires exactly one field
    /// to be initialized. Handles generic union types and validates field existence
    /// and type compatibility.
    fn analyze_regular_union_init(
        &mut self,
        union_init: &mut TypedStructInitExpr,
        resolved_union: &ResolvedUnion,
        generic_type_opt: &Option<GenericType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        if union_init.fields.len() != 1 {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnionInitWithInvalidFields),
                loc: Some(union_init.loc),
                hint: None,
            });
            return None;
        }

        let field_init = &mut union_init.fields[0];

        let field = match resolved_union
            .union_sig
            .fields
            .iter()
            .find(|f| f.name == field_init.name)
            .cloned()
        {
            Some(mut field) => {
                field.ty = self.normalize_sema_type(field.ty.clone(), field_init.value.loc)?;
                field
            }
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: resolved_union.union_sig.name.clone(),
                        field_name: field_init.name.clone(),
                    }),
                    loc: Some(field_init.loc),
                    hint: None,
                });
                return None;
            }
        };

        let field_expected_type = self
            .try_infer_generic_param_as_expected_type(field.ty.clone(), &generic_type_opt)
            .unwrap_or(field.ty.clone());

        field_init.value.sema_type = self.analyze_expr(&mut field_init.value, Some(field_expected_type));

        if generic_type_opt.is_none() {
            if !self.check_type_mismatch(
                field_init.value.sema_type.clone().unwrap(),
                field.ty.clone(),
                field_init.value.loc,
            ) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                        lhs_type: format_sema_type(field.ty.clone(), fmt_symbol),
                        rhs_type: format_sema_type(field_init.value.sema_type.clone().unwrap(), fmt_symbol),
                    }),
                    loc: Some(field_init.value.loc),
                    hint: None,
                });
                return None;
            }
        }

        if let Some(sema_type) = self.infer_generic_param(
            generic_type_opt.as_ref(),
            field.ty.clone(),
            field_init.value.sema_type.clone(),
            field_init.value.loc,
        ) {
            field_init.value.sema_type = Some(sema_type);
        }

        if let Some(generic_type) = generic_type_opt {
            // validate generic type instantiation
            let final_generic_type = match generic_type.finalize(
                self.mapping_ctx_arena.clone(),
                resolved_union.union_sig.generic_params.clone().unwrap(),
                fmt_symbol,
            ) {
                Ok(generic_type) => generic_type,
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            };

            Some(SemanticType::GenericType(final_generic_type.clone()))
        } else {
            if union_init.is_const {
                Some(SemanticType::Const(Box::new(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::Union(union_init.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Union(
                    union_init.symbol_id,
                )))
            }
        }
    }

    /// Analyzes struct initialization expressions with field assignments.
    ///
    /// Type-checks struct initialization syntax with named field assignments.
    /// Validates field existence, duplicate assignments, and missing required fields.
    /// Handles generic struct types with type inference from initialization values.
    fn analyze_regular_struct_init(
        &mut self,
        struct_init: &mut TypedStructInitExpr,
        resolved_struct: &ResolvedStruct,
        generic_type_opt: &Option<GenericType>,
        infer_ctx: &FieldEnv,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        // check duplicate field inits
        let mut field_names: Vec<String> = Vec::new();
        for field_init in &struct_init.fields {
            let struct_name = fmt_symbol(struct_init.symbol_id);

            if field_names.contains(&field_init.name) {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::DuplicateFieldName {
                        object_name: struct_name,
                        field_name: field_init.name.clone(),
                    }),
                    loc: Some(field_init.loc),
                    hint: None,
                });
                continue;
            }

            field_names.push(field_init.name.clone());
        }

        let mut missing_fields: Vec<String> = resolved_struct
            .struct_sig
            .fields
            .iter()
            .map(|field| field.name.clone())
            .collect();

        for field_init in struct_init.fields.iter_mut() {
            let field = match resolved_struct
                .struct_sig
                .fields
                .iter()
                .cloned()
                .find(|field| field.name == field_init.name)
            {
                Some(mut field) => {
                    field.ty = self.normalize_sema_type(field.ty.clone(), field_init.value.loc)?;
                    field
                }
                None => {
                    let struct_name = fmt_symbol(struct_init.symbol_id);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                            struct_name,
                            field_name: field_init.name.clone(),
                        }),
                        loc: Some(field_init.loc),
                        hint: None,
                    });
                    continue;
                }
            };

            let infer_ctx_expected_type = infer_ctx.get(&field.name).cloned();
            let field_expected_type = self
                .try_infer_generic_param_as_expected_type(field.ty.clone(), &generic_type_opt)
                .or(infer_ctx_expected_type)
                .unwrap_or(field.ty.clone());

            let field_value_ty = self.analyze_expr(&mut field_init.value, Some(field_expected_type.clone()));

            if let Some(sema_type) = self.infer_generic_param(
                generic_type_opt.as_ref(),
                field.ty.clone(),
                field_value_ty.clone(),
                field_init.loc,
            ) {
                field_init.value.sema_type = Some(sema_type);
            }

            if generic_type_opt.is_none() {
                if !self.check_type_mismatch(
                    field_init.value.sema_type.clone().unwrap(),
                    field.ty.clone(),
                    field_init.value.loc,
                ) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_type(field.ty.clone(), fmt_symbol),
                            rhs_type: format_sema_type(field_init.value.sema_type.clone().unwrap(), fmt_symbol),
                        }),
                        loc: Some(field_init.value.loc),
                        hint: None,
                    });
                    return None;
                }
            }

            let missing_fields_idx = match missing_fields
                .iter()
                .position(|field_name| *field_name == field_init.name.clone())
            {
                Some(i) => i,
                None => continue,
            };

            missing_fields.remove(missing_fields_idx);
        }

        if !missing_fields.is_empty() {
            let struct_name = fmt_symbol(struct_init.symbol_id);
            let missing_field_names = format_missing_fields(&missing_fields);

            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::StructMissingFields {
                    struct_name,
                    missing_field_names,
                }),
                loc: Some(struct_init.loc),
                hint: None,
            });
        }

        if let Some(generic_type) = generic_type_opt {
            let final_generic_type = {
                // validate generic type instantiation
                match generic_type.finalize(
                    self.mapping_ctx_arena.clone(),
                    resolved_struct.struct_sig.generic_params.clone().unwrap(),
                    fmt_symbol,
                ) {
                    Ok(generic_type) => generic_type,
                    Err(diag) => {
                        self.reporter.report(diag);
                        return None;
                    }
                }
            };

            Some(SemanticType::GenericType(final_generic_type.clone()))
        } else {
            if struct_init.is_const {
                Some(SemanticType::Const(Box::new(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::Struct(struct_init.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Struct(
                    struct_init.symbol_id,
                )))
            }
        }
    }

    /// Attempts to interpret a field access as an enum unit variant constructor.
    ///
    /// Detects when a field access on an enum type is actually accessing a unit variant
    /// (e.g., `Color.Red`). If successful, performs the appropriate enum variant analysis.
    fn maybe_enum_variant_constructor_from_field_access(
        &mut self,

        operand_type: SemanticType,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> (bool, Option<SemanticType>) {
        let Some(symbol_id) = operand_type.maybe_generic_base_symbol_id() else {
            return (false, None);
        };

        let Some(resolved_enum) = self.query.get_enum(symbol_id) else {
            return (false, None);
        };

        let attempted = true;

        field_access.object_symbol_id = Some(resolved_enum.symbol_id);

        {
            let (generic_params, mapping_ctx) = self.extract_and_merge_generic_context(
                &operand_type,
                resolved_enum.enum_sig.generic_params.as_ref(),
                expected_type,
            );

            let sema_type =
                self.analyze_enum_variant_no_field(&resolved_enum, field_access, generic_params.as_ref(), mapping_ctx);

            (attempted, sema_type)
        }
    }

    /// Attempts to interpret a method call as an enum variant constructor with fields.
    ///
    /// Detects when a method call on an enum type is actually constructing a fielded
    /// enum variant (e.g., `Color::Rgb(255, 0, 0)`). If successful, performs the
    /// appropriate enum variant analysis with field validation.
    fn maybe_enum_variant_constructor_from_method_call(
        &mut self,

        operand_type: SemanticType,
        method_call: &mut TypedMethodCall,
        expected_type: Option<SemanticType>,
    ) -> (bool, Option<SemanticType>) {
        let Some(symbol_id) = operand_type.maybe_generic_base_symbol_id() else {
            return (false, None);
        };

        let Some(resolved_enum) = self.query.get_enum(symbol_id) else {
            return (false, None);
        };

        method_call.enum_constructor = Some(resolved_enum.symbol_id);

        let enum_variant_opt = resolved_enum
            .enum_sig
            .variants
            .iter()
            .find(|v| v.ident().as_string() == method_call.method_name);

        let Some(enum_variant) = enum_variant_opt else {
            return (false, None);
        };

        // method_call.object_symbol_id = Some(resolved_enum.symbol_id);

        let (generic_params, mapping_ctx) = self.extract_and_merge_generic_context(
            &operand_type,
            resolved_enum.enum_sig.generic_params.as_ref(),
            expected_type,
        );

        let sema_ty_opt = self.analyze_enum_variant(
            enum_variant.clone(),
            method_call,
            &resolved_enum,
            generic_params.as_ref(),
            mapping_ctx,
        );

        (true, sema_ty_opt)
    }

    /// Analyzes field access expressions on union types.
    ///
    /// Type-checks field accesses on union instances by verifying the field exists
    /// in the union definition and the access uses correct pointer syntax.
    /// Annotates the field access AST node with resolved type and index information.
    fn analyze_union_field_access(
        &mut self,

        union_sig: &UnionSig,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_type = match self.analyze_expr(&mut field_access.operand, expected_type) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        field_access.operand.sema_type = Some(operand_type.clone());

        let union_field_idx = match union_sig
            .fields
            .iter()
            .position(|field| *field.name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: union_sig.name.clone(),
                        field_name: field_access.field_name.clone(),
                    }),
                    loc: Some(field_access.loc),
                    hint: None,
                });
                return None;
            }
        };

        let field = &union_sig.fields[union_field_idx];
        let field_ty = match self.normalize_sema_type(field.ty.clone(), field_access.loc) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        if !self.validate_union_field_access(operand_type.const_inner().clone(), &field_access) {
            return None;
        }

        field_access.field_index = Some(union_field_idx);
        field_access.field_ty = Some(field_ty.clone());
        field_access.object_symbol_id = Some(union_sig.symbol_id);

        Some(field_ty)
    }

    fn analyze_unnamed_union_field_access(
        &mut self,
        unnamed_union_type: &TypedUnnamedUnionType,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let operand_type = match self.analyze_expr(&mut field_access.operand, expected_type) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        field_access.operand.sema_type = Some(operand_type.clone());

        let field_idx = match unnamed_union_type
            .fields
            .iter()
            .position(|field| *field.name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: format_sema_type(
                            SemanticType::UnnamedUnion(unnamed_union_type.clone()),
                            fmt_symbol,
                        ),
                        field_name: field_access.field_name.clone(),
                    }),
                    loc: Some(field_access.loc),
                    hint: None,
                });
                return None;
            }
        };

        let field = &unnamed_union_type.fields[field_idx];
        let field_ty = match self.normalize_sema_type(*field.ty.clone(), field_access.loc) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        field_access.field_index = Some(field_idx);
        field_access.field_ty = Some(field_ty.clone());

        Some(field_ty.clone())
    }

    /// Analyzes field access expressions on unnamed (anonymous) struct types.
    ///
    /// Type-checks field accesses on instances of anonymous structs by locating the
    /// field in the struct's type definition and resolving its type. Unlike named
    /// structs, anonymous structs require special handling for error messages and
    /// type resolution.
    fn analyze_unnamed_struct_field_access(
        &mut self,
        unnamed_struct_type: &TypedUnnamedStructType,
        field_access: &mut TypedFieldAccess,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let operand_type = match self.analyze_expr(&mut field_access.operand, expected_type) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        field_access.operand.sema_type = Some(operand_type.clone());

        let field_idx = match unnamed_struct_type
            .fields
            .iter()
            .position(|field| *field.name == field_access.field_name.clone())
        {
            Some(union_field) => union_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name: format_sema_type(
                            SemanticType::UnnamedStruct(unnamed_struct_type.clone()),
                            fmt_symbol,
                        ),
                        field_name: field_access.field_name.clone(),
                    }),
                    loc: Some(field_access.loc),
                    hint: None,
                });
                return None;
            }
        };

        let field = &unnamed_struct_type.fields[field_idx];
        let field_ty = match self.normalize_sema_type(*field.ty.clone(), field_access.loc) {
            Some(sema_type) => sema_type,
            None => return None,
        };

        field_access.field_index = Some(field_idx);
        field_access.field_ty = Some(field_ty.clone());

        Some(field_ty.clone())
    }

    /// Analyzes field access expressions on named struct types.
    ///
    /// Type-checks field accesses on struct instances by resolving the field name,
    /// normalizing its type, and validating access permissions and syntax.
    /// Handles both visibility checks and pointer access operator validation.
    fn analyze_struct_field_access(
        &mut self,

        field_access: &mut TypedFieldAccess,
        struct_name: String,
        struct_fields: Vec<TypedStructField>,
        struct_methods: HashMap<String, SymbolID>,
        struct_id: SymbolID,
    ) -> Option<SemanticType> {
        let field_index = match struct_fields
            .iter()
            .position(|typed_struct_field| typed_struct_field.name == field_access.field_name)
        {
            Some(typed_struct_field) => typed_struct_field,
            None => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::ObjectHasNoFieldNamed {
                        struct_name,
                        field_name: field_access.field_name.clone(),
                    }),
                    loc: Some(field_access.loc),
                    hint: None,
                });
                return None;
            }
        };

        let mut typed_struct_field = struct_fields.get(field_index).unwrap().clone();

        typed_struct_field.ty = self
            .normalize_sema_type(typed_struct_field.ty.clone(), field_access.loc)
            .unwrap();

        if !self.validate_struct_field_access(
            &field_access.operand,
            &field_access,
            typed_struct_field.vis.clone(),
            &struct_methods,
            &struct_name,
        ) {
            return None;
        }

        field_access.field_index = Some(field_index);
        field_access.field_ty = Some(typed_struct_field.ty.clone());
        field_access.object_symbol_id = Some(struct_id);

        Some(typed_struct_field.ty.clone())
    }

    /// Analyzes enum variant construction via method-call syntax.
    ///
    /// Type-checks enum variant instantiation that uses method-call syntax (e.g., `Color.Red(255)`).
    /// Handles generic enum instantiation, field type checking, and generic parameter inference.
    fn analyze_enum_variant(
        &mut self,
        mut enum_variant: TypedEnumVariant,
        method_call: &mut TypedMethodCall,
        resolved_enum: &ResolvedEnum,
        generic_params: Option<&TypedGenericParamsList>,
        mapping_ctx: Option<Rc<GenericMappingCtx>>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let mut valued_fields = self.analyze_enum_fielded_variant(&mut enum_variant, method_call)?;

        let is_const = false;

        let (_, generic_type_opt) = match self.init_generic_type_with_symbol_id(
            resolved_enum.symbol_id,
            &mut method_call.type_args,
            mapping_ctx,
            generic_params,
            method_call.loc,
        ) {
            Ok(opt) => opt?,
            Err(diag) => {
                self.reporter.report(diag);
                return None;
            }
        };

        for (typed_expr, enum_valued_field) in method_call.args.iter_mut().zip(valued_fields.iter_mut()) {
            let field_expected_type = self
                .try_infer_generic_param_as_expected_type(enum_valued_field.ty.clone(), &generic_type_opt)
                .unwrap_or(enum_valued_field.ty.clone());

            self.analyze_expr(typed_expr, Some(field_expected_type));

            if let Some(sema_type) = self.infer_generic_param(
                generic_type_opt.as_ref(),
                enum_valued_field.ty.clone(),
                typed_expr.sema_type.clone(),
                typed_expr.loc,
            ) {
                enum_valued_field.ty = sema_type;
            }

            if generic_type_opt.is_none() {
                enum_valued_field.ty = self.normalize_sema_type(enum_valued_field.ty.clone(), typed_expr.loc)?;

                if !self.check_type_mismatch(
                    typed_expr.sema_type.clone().unwrap(),
                    enum_valued_field.ty.clone(),
                    typed_expr.loc,
                ) {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::AssignmentTypeMismatch {
                            lhs_type: format_sema_type(enum_valued_field.ty.clone(), fmt_symbol),
                            rhs_type: format_sema_type(typed_expr.sema_type.clone().unwrap(), fmt_symbol),
                        }),
                        loc: Some(typed_expr.loc),
                        hint: None,
                    });
                    return None;
                }
            }
        }

        if let Some(generic_type) = generic_type_opt {
            // validate generic type instantiation

            let generic_params = generic_params
                .or(resolved_enum.enum_sig.generic_params.as_ref())
                .cloned()
                .unwrap();

            let final_generic_type =
                match generic_type.finalize(self.mapping_ctx_arena.clone(), generic_params, fmt_symbol) {
                    Ok(generic_type) => generic_type,
                    Err(diag) => {
                        self.reporter.report(diag);
                        return None;
                    }
                };

            method_call.operand.sema_type = Some(SemanticType::GenericType(final_generic_type.clone()));
            Some(SemanticType::GenericType(final_generic_type.clone()))
        } else {
            method_call.operand.sema_type = Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                resolved_enum.symbol_id,
            )));

            if is_const {
                Some(SemanticType::Const(Box::new(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::Enum(resolved_enum.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                    resolved_enum.symbol_id,
                )))
            }
        }
    }

    /// Analyzes method calls as enum variant constructions with field values.
    ///
    /// Validates and extracts field information when an enum variant is being
    /// constructed with field values via method-call syntax. Ensures the variant
    /// accepts fields and the argument count matches the variant's field count.
    fn analyze_enum_fielded_variant<'b>(
        &mut self,

        mut enum_variant: &'b mut TypedEnumVariant,
        method_call: &TypedMethodCall,
    ) -> Option<Vec<TypedEnumValuedField>> {
        match &mut enum_variant {
            TypedEnumVariant::Variant(_, valued_fields) => {
                if valued_fields.len() != method_call.args.len() {
                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::EnumVariantArgCountMismatch {
                            variant_name: method_call.method_name.clone(),
                            expected: valued_fields.len() as u32,
                            provided: method_call.args.len() as u32,
                        }),
                        loc: Some(method_call.loc),
                        hint: None,
                    });
                    return None;
                }

                for valued_field in &mut *valued_fields {
                    valued_field.ty = self.normalize_sema_type(valued_field.ty.clone(), valued_field.loc)?;
                }

                Some(valued_fields.clone())
            }
            _ => {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::EnumVariantDoesNotAcceptFields {
                        variant_name: method_call.method_name.clone(),
                    }),
                    loc: Some(method_call.loc),
                    hint: None,
                });
                return None;
            }
        }
    }

    /// Analyzes enum variant access for unit (fieldless) variants.
    ///
    /// Type-checks accesses to enum variants without fields (unit variants) through
    /// field access syntax (e.g., `Color.Red`). Handles both non-generic and generic
    /// enum types, and validates that the accessed variant is indeed a unit variant.
    fn analyze_enum_variant_no_field(
        &mut self,
        resolved_enum: &ResolvedEnum,
        field_access: &mut TypedFieldAccess,
        generic_params: Option<&TypedGenericParamsList>,
        mapping_ctx: Option<Rc<GenericMappingCtx>>,
    ) -> Option<SemanticType> {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        field_access.object_symbol_id = Some(resolved_enum.symbol_id);

        let enum_variant_idx_opt = resolved_enum
            .enum_sig
            .variants
            .iter()
            .position(|variant| variant.ident().as_string() == field_access.field_name);

        let enum_variant_opt = enum_variant_idx_opt.and_then(|i| Some(resolved_enum.enum_sig.variants.get(i).unwrap()));

        if enum_variant_opt.is_none() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VariantNotDefinedForEnum {
                    enum_name: resolved_enum.enum_sig.name.clone(),
                    variant_name: field_access.field_name.clone(),
                }),
                loc: Some(field_access.loc),
                hint: None,
            });
            return None;
        } else if matches!(enum_variant_opt, Some(TypedEnumVariant::Variant(..))) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::VariantMissingFields {
                    enum_name: resolved_enum.enum_sig.name.clone(),
                    variant_name: field_access.field_name.clone(),
                }),
                loc: Some(field_access.loc),
                hint: None,
            });
            return None;
        }

        field_access.field_index = Some(enum_variant_idx_opt.unwrap());

        let is_const = false;

        let (_, generic_type_opt) = match self.init_generic_type_with_symbol_id(
            resolved_enum.symbol_id,
            &mut field_access.type_args,
            mapping_ctx,
            generic_params,
            field_access.loc,
        ) {
            Ok(opt) => opt?,
            Err(diag) => {
                self.reporter.report(diag);
                return None;
            }
        };

        if let Some(generic_type) = generic_type_opt {
            // validate generic type instantiation
            let final_generic_type = match generic_type.finalize(
                self.mapping_ctx_arena.clone(),
                resolved_enum.enum_sig.generic_params.clone().unwrap(),
                fmt_symbol,
            ) {
                Ok(generic_type) => generic_type,
                Err(diag) => {
                    self.reporter.report(diag);
                    return None;
                }
            };

            field_access.operand.sema_type = Some(SemanticType::GenericType(final_generic_type.clone()));
            Some(SemanticType::GenericType(final_generic_type.clone()))
        } else {
            field_access.operand.sema_type = Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                resolved_enum.symbol_id,
            )));

            if is_const {
                Some(SemanticType::Const(Box::new(SemanticType::ResolvedSymbol(
                    ResolvedSymbol::Enum(resolved_enum.symbol_id),
                ))))
            } else {
                Some(SemanticType::ResolvedSymbol(ResolvedSymbol::Enum(
                    resolved_enum.symbol_id,
                )))
            }
        }
    }

    /// Resolves the kind of member access operation based on operand type analysis.
    ///
    /// Determines whether a member access expression refers to a named struct, unnamed struct,
    /// or union member by analyzing the operand's type and resolving the underlying symbol.
    /// This function handles multiple type categories and extracts the appropriate access
    /// context for subsequent field/member resolution.
    fn resolve_field_access_kind(
        &mut self,
        operand: &mut TypedExprStmt,
        expected_type: Option<SemanticType>,
        loc: Loc,
    ) -> Option<FieldAccessKind> {
        let operand_type = match &operand.kind {
            TypedExprKind::Symbol(symbol_expr) => self.resolve_var_or_global_var_type(symbol_expr.symbol_id)?,
            _ => self.analyze_expr(operand, expected_type.clone())?,
        };

        let object_symbol_id = match match operand_type.const_inner() {
            SemanticType::ResolvedSymbol(resolved_symbol) => Some(resolved_symbol.symbol_id()),
            SemanticType::Pointer(sema_type) => {
                if sema_type.is_void() {
                    return None;
                } else if let Some(unnamed_struct_type) = sema_type.as_unnamed_struct() {
                    return Some(FieldAccessKind::UnnamedStruct(Box::new(unnamed_struct_type)));
                }

                self.normalize_and_extract_symbol_id(*sema_type.clone(), loc)
            }
            SemanticType::UnnamedStruct(unnamed_struct_type) => {
                return Some(FieldAccessKind::UnnamedStruct(Box::new(unnamed_struct_type.clone())));
            }
            SemanticType::UnnamedUnion(unnamed_union_type) => {
                return Some(FieldAccessKind::UnnamedUnion(Box::new(unnamed_union_type.clone())));
            }
            SemanticType::GenericType(generic_type) => Some(generic_type.base),
            _ => None,
        } {
            Some(symbol_id) => symbol_id,
            None => return None,
        };

        let symbol_entry = match self.query.get_symbol(object_symbol_id) {
            Some(symbol_entry) => symbol_entry,
            None => return None,
        };

        symbol_entry
            .as_struct()
            .map(|resolved_struct| FieldAccessKind::NamedStruct(Box::new(resolved_struct.clone())))
            .or_else(|| {
                symbol_entry
                    .as_union()
                    .map(|resolved_union| FieldAccessKind::Union(Box::new(resolved_union.clone())))
            })
    }

    /// Analyzes tuple member access expressions (e.g., `tuple.0`, `tuple.1`).
    ///
    /// Type-checks tuple indexing operations by verifying the operand is a tuple type
    /// and the index is within bounds for that tuple. Returns the type of the accessed
    /// element if the access is valid.
    fn analyze_tuple_member_access(
        &mut self,
        tuple_member_access: &mut TypedTupleAccessExpr,
        expected_type: Option<SemanticType>,
    ) -> Option<SemanticType> {
        let operand_type = self.analyze_expr(&mut tuple_member_access.operand, expected_type)?;

        if !operand_type.const_inner().as_tuple_type().is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleMemberAccessOnNonTupleOperand),
                loc: Some(tuple_member_access.loc),
                hint: None,
            });
            return None;
        }

        let tuple_type = operand_type.as_tuple_type().unwrap();

        // inbounds check for tuple type

        if tuple_member_access.index > (tuple_type.elements.len() - 1) {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::TupleIndexOutOfRange {
                    index: tuple_member_access.index.try_into().unwrap(),
                    length: tuple_type.elements.len(),
                }),
                loc: Some(tuple_member_access.loc),
                hint: None,
            });
            return None;
        }

        let element_type = tuple_type.elements.get(tuple_member_access.index).unwrap();

        Some(element_type.clone())
    }

    /// Sets contextual type information for the current method call's 'self' parameter.
    ///
    /// Updates context state with the type of the object instance being operated on
    /// in a method call. This contextual information is used for subsequent semantic
    /// analysis within the method body (e.g., field access validation, implicit 'self').
    pub(crate) fn set_ty_ctx_self_type(&mut self, method_call: &mut TypedMethodCall, sema_type: &SemanticType) {
        self.fn_env.current_object_type = Some(sema_type.const_inner().pointer_inner().const_inner().clone());
        self.fn_env.current_self_type = Some(sema_type.clone());
        method_call.self_ty = Some(sema_type.clone());
    }

    // FIXME: Must be removed.
    //
    /// Analyzes and updates the type of 'self' modifier parameters in generic contexts.
    ///
    /// Processes the first parameter of a function when it is a 'self' modifier (copied
    /// or referenced), updating its semantic type based on the resolved generic type.
    /// This ensures that 'self' parameters in generic methods receive the correct
    /// concrete type when the method is instantiated.
    // pub(crate) fn analyze_generic_self_modifier(
    //     &self,
    //     scope_id: ScopeID,
    //     params: &TypedFuncParams,
    //     sema_type: SemanticType,
    // ) {
    //     let scope_rc = self.resolver.resolve_local_scope(self.module_id, scope_id).unwrap();

    //     if let Some(first_param) = params.list.first() {
    //         if let Some(self_modifier) = first_param.as_self_modifier() {
    //             let mut scope_ref = scope_rc.borrow_mut();

    //             let new_self_modifier_ty = match self_modifier.kind {
    //                 SelfModifierKind::Copied => sema_type,
    //                 SelfModifierKind::Referenced => SemanticType::Pointer(Box::new(sema_type)),
    //             };

    //             scope_ref.with_symbol_id_mut(self_modifier.self_id.unwrap(), |local_symbol| {
    //                 let resolved_var = local_symbol.as_variable_mut().unwrap();
    //                 resolved_var.variable.ty = Some(new_self_modifier_ty);
    //             });
    //             drop(scope_ref);
    //         }
    //     }
    // }

    /// Analyzes and potentially transforms the 'self' argument for object method calls.
    ///
    /// Processes the operand passed to a method call based on the 'self' modifier kind
    /// and access operator. For referenced 'self' parameters with dot notation ('.'),
    /// automatically inserts an address-of operation to create a pointer to the object.
    fn analyze_object_self_modifier_argument(
        &mut self,
        operand: &TypedExprStmt,
        is_fat_arrow: bool,
        self_modifier: &TypedSelfModifier,
    ) -> TypedExprStmt {
        match self_modifier.kind {
            SelfModifierKind::Copied => operand.clone(),
            SelfModifierKind::Referenced => {
                // only take address if not a fat arrow
                if is_fat_arrow {
                    operand.clone()
                } else {
                    let expr_ty = operand.sema_type.clone().unwrap();
                    TypedExprStmt {
                        kind: TypedExprKind::AddrOf(TypedAddrOfExpr {
                            operand: Box::new(operand.clone()),
                            loc: operand.loc,
                        }),
                        sema_type: Some(SemanticType::Pointer(Box::new(expr_ty))),
                        mloc: MemoryLocation::LValue,
                        loc: operand.loc,
                    }
                }
            }
        }
    }

    /// Validates that type arguments are not provided for non-generic types.
    ///
    /// Checks whether type arguments (e.g., `<T, U>`) are unexpectedly provided
    /// for a type that does not have generic parameters. This prevents syntax
    /// like `NonGenericType<int>` which would be invalid.
    fn report_if_unexpected_type_args(
        &mut self,
        generic_params: &Option<TypedGenericParamsList>,
        type_args: &Option<TypedTypeArgs>,
        loc: Loc,
    ) -> bool {
        if generic_params.is_none() && type_args.is_some() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UnexpectedTypeArgs),
                loc: Some(loc),
                hint: None,
            });
            return true;
        }
        false
    }

    /// Resolves semantic type of a variable or global variable.
    fn resolve_var_or_global_var_type(&mut self, symbol_id: SymbolID) -> Option<SemanticType> {
        let symbol_entry = self.query.get_symbol(symbol_id).unwrap();

        if let Some(resolved_var) = symbol_entry.as_var() {
            Some(resolved_var.variable.ty.clone().unwrap())
        } else if let Some(resolved_global_var) = symbol_entry.as_global_var() {
            Some(resolved_global_var.global_var_sig.ty.clone().unwrap())
        } else {
            None
        }
    }

    /// Validates struct field access syntax, visibility, and pointer semantics.
    fn validate_struct_field_access(
        &mut self,
        operand: &TypedExprStmt,
        field_access: &TypedFieldAccess,
        field_vis: Visibility,
        struct_methods: &HashMap<String, SymbolID>,
        struct_name: &str,
    ) -> bool {
        let mut result = true;

        let access_violation = if let Some(current_method_symbol_id) = self.fn_env.current_method_symbol_id {
            let method_symbol_ids = struct_methods.values().cloned().collect::<Vec<SymbolID>>();

            if method_symbol_ids.contains(&current_method_symbol_id) {
                false
            } else {
                !field_vis.is_public()
            }
        } else {
            !field_vis.is_public()
        };

        if access_violation {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InternalFieldAccess {
                    field_name: field_access.field_name.clone(),
                    struct_name: struct_name.to_string(),
                }),
                loc: Some(field_access.loc),
                hint: None,
            });
            result = false;
        }

        let base_type = operand
            .sema_type
            .as_ref()
            .expect("SemanticType should be set before field access")
            .const_inner();

        let is_pointer = base_type.is_pointer() || base_type.as_generic_type().is_some();
        let is_struct = base_type.is_resolved_symbol() || base_type.as_generic_type().is_some();

        if field_access.is_fat_arrow {
            if !is_pointer {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                    loc: Some(field_access.loc),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
                result = false;
            }
        } else {
            if !is_struct {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                    loc: Some(field_access.loc),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
                result = false;
            }
        }

        result
    }

    /// Validates union field access operator usage based on pointer semantics.
    fn validate_union_field_access(&mut self, operand_type: SemanticType, field_access: &TypedFieldAccess) -> bool {
        let mut result = true;

        if operand_type.is_pointer() && !field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                loc: Some(field_access.loc),
                hint: Some("Use '->' when accessing through a pointer.".to_string()),
            });
            result = false;
        } else if !operand_type.is_pointer() && field_access.is_fat_arrow {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                loc: Some(field_access.loc),
                hint: Some("Use '.' instead of '->'.".to_string()),
            });
            result = false;
        }

        result
    }

    /// Validates a method call's accessibility, syntax, and mutability constraints.
    fn validate_method_call(
        &mut self,

        instance_symbol_id: SymbolID,
        method_name: &String,
        method_call_operand_ty: SemanticType,
        is_fat_arrow: bool,
        method_call_on_interface: bool,
        first_param_opt: Option<&TypedFuncParamKind>,
        object_methods_opt: Option<HashMap<String, SymbolID>>,
        object_name: String,
        func_sig: &FuncSig,
        loc: Loc,
    ) -> bool {
        let fmt_symbol: SymbolFormatterFn = &|symbol_id| self.query.format_symbol_name(symbol_id);

        let mut result = true;
        let vis = &func_sig.modifiers.vis;

        let access_violation = if let Some(current_method_symbol_id) = self.fn_env.current_method_symbol_id {
            let object_contains_method = {
                if let Some(object_methods) = object_methods_opt {
                    object_methods
                        .values()
                        .cloned()
                        .collect::<Vec<SymbolID>>()
                        .contains(&current_method_symbol_id)
                } else {
                    true
                }
            };

            if object_contains_method {
                false
            } else {
                !vis.is_public() && !method_call_on_interface
            }
        } else {
            !vis.is_public() && !method_call_on_interface
        };

        if access_violation {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InternalMethodCall {
                    method_name: func_sig.name.clone(),
                    object_name,
                }),
                loc: Some(loc),
                hint: None,
            });
            result = false;
        }

        let is_pointer = method_call_operand_ty.const_inner().is_pointer();
        let is_operand_const = method_call_operand_ty.is_const();
        let is_object = method_call_operand_ty.const_inner().is_resolved_symbol()
            || method_call_operand_ty.as_generic_type().is_some()
            || method_call_on_interface;

        if is_fat_arrow {
            if !is_pointer {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::InvalidThinArrow),
                    loc: Some(loc),
                    hint: Some("Use '.' instead of '->'.".to_string()),
                });
                result = false;
            }
        } else {
            if !is_object {
                self.reporter.report(Diag {
                    level: DiagLevel::Error,
                    kind: Box::new(AnalyzerDiagKind::UseThinArrow),
                    loc: Some(loc),
                    hint: Some("Use '->' when accessing through a pointer.".to_string()),
                });
                result = false;
            }
        }

        if let Some(first_param) = first_param_opt {
            if let TypedFuncParamKind::SelfModifier(typed_self_modifier) = first_param {
                if typed_self_modifier.kind == SelfModifierKind::Referenced && is_operand_const {
                    let instance_name = fmt_symbol(instance_symbol_id);

                    self.reporter.report(Diag {
                        level: DiagLevel::Error,
                        kind: Box::new(AnalyzerDiagKind::MutationPossibleMethodCallOnConstInstance {
                            method_name: method_name.clone(),
                            instance_name: instance_name.clone(),
                        }),
                        loc: Some(loc),
                        hint: Some(format!(
                            "Instance '{}' is declared as 'const' and cannot be modified.",
                            instance_name
                        )),
                    });
                    result = false;
                }
            }
        }

        result
    }

    /// Validates that an expression type is suitable for use as a boolean condition.
    pub(crate) fn is_cond_expr(&mut self, sema_type: SemanticType, loc: Loc) {
        if !sema_type.is_bool() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ConditionExprMustBeOfTypeBool),
                loc: Some(loc),
                hint: None,
            });
        }
    }

    fn field_env_from_struct_type(&self, expected_type: Option<SemanticType>) -> FieldEnv {
        let Some(sema_type) = expected_type else {
            return FieldEnv::default();
        };

        if let Some(unnamed_struct_type) = sema_type.as_unnamed_struct() {
            field_env_from_unnamed_struct_type(&unnamed_struct_type)
        } else if let Some(struct_id) = sema_type.as_struct_symbol_id() {
            let resolved_struct = self.query.get_struct(struct_id).unwrap();

            field_env_from_struct_sig(&resolved_struct.struct_sig)
        } else if let Some(generic_type) = sema_type.as_generic_type() {
            let resolved_struct = self.query.get_struct(generic_type.base).unwrap();

            let struct_sig = substitute_struct_sig(
                self.mapping_ctx_arena.clone(),
                &resolved_struct.struct_sig,
                generic_type.mapping_ctx.clone(),
            )
            .unwrap();

            field_env_from_struct_sig(&struct_sig)
        } else {
            FieldEnv::default()
        }
    }
}

impl FuncEnv {
    pub fn new() -> Self {
        Self {
            current_func_type: None,
            current_self_type: None,
            current_object_type: None,
            current_method_symbol_id: None,
        }
    }
}

impl Default for FieldEnv {
    fn default() -> Self {
        Self {
            fields: Default::default(),
        }
    }
}

impl FieldEnv {
    pub fn new() -> Self {
        Self { fields: HashMap::new() }
    }

    pub fn insert(&mut self, key: String, value: SemanticType) {
        self.fields.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&SemanticType> {
        self.fields.get(key)
    }
}

fn field_env_from_unnamed_struct_type(unnamed_struct_type: &TypedUnnamedStructType) -> FieldEnv {
    let mut infer_ctx = FieldEnv::new();
    for field in &unnamed_struct_type.fields {
        infer_ctx.insert(field.name.clone(), *field.ty.clone());
    }
    infer_ctx
}

fn field_env_from_struct_sig(struct_sig: &StructSig) -> FieldEnv {
    let mut infer_ctx = FieldEnv::new();
    for field in &struct_sig.fields {
        infer_ctx.insert(field.name.clone(), field.ty.clone());
    }
    infer_ctx
}

#[derive(Debug, Clone)]
enum FieldAccessKind {
    UnnamedStruct(Box<TypedUnnamedStructType>),
    NamedStruct(Box<ResolvedStruct>),
    Union(Box<ResolvedUnion>),
    UnnamedUnion(Box<TypedUnnamedUnionType>),
}

fn self_modifier_param_type(params: &TypedFuncParams, sema_type: SemanticType) -> Option<SemanticType> {
    if let Some(first_param) = params.list.first() {
        if let Some(self_modifier) = first_param.as_self_modifier() {
            return Some(match self_modifier.kind {
                SelfModifierKind::Copied => sema_type,
                SelfModifierKind::Referenced => SemanticType::Pointer(Box::new(sema_type)),
            });
        }
    }
    None
}

fn infer_integer_type(
    literal: &TypedLiteralExpr,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<SemanticType>,
) -> Result<SemanticType, Diag> {
    if let Some(suffix) = suffix_opt {
        return match map_integer_suffix_to_sema_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidIntegerLiteralSuffix),
                loc: Some(literal.loc),
                hint: Some(format!("Invalid suffix {:?} for integer literal.", suffix)),
            }),
        };
    } else if let Some(ty) = expected {
        if ty.is_integer() {
            return Ok(ty);
        }
    }

    // default integer type
    Ok(SemanticType::PlainType(PlainType::Int))
}

fn infer_float_type(
    literal: &TypedLiteralExpr,
    suffix_opt: &Option<Box<TokenKind>>,
    expected: Option<SemanticType>,
) -> Result<SemanticType, Diag> {
    if let Some(suffix) = suffix_opt {
        return match map_float_suffix_to_sema_type(&suffix) {
            Some(ty) => Ok(ty),
            None => Err(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::InvalidFloatLiteralSuffix),
                loc: Some(literal.loc),
                hint: Some(format!("Invalid suffix {} for float literal.", suffix)),
            }),
        };
    } else if let Some(ty) = expected {
        if ty.is_float() {
            return Ok(ty);
        }
    }

    // default float type
    Ok(SemanticType::PlainType(PlainType::Float64))
}
