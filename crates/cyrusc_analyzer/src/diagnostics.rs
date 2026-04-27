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
use cyrusc_diagcentral::{Diag, DiagKind, DiagLevel};
use cyrusc_source_loc::Loc;
use cyrusc_strescape::diagnostics::UnescapeError;
use cyrusc_typed_ast::{
    stmts::{TypedFuncParamKind, TypedFuncVariadicParam},
    types::SemaType,
};
use std::ops::RangeInclusive;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum AnalyzerDiagKind {
    #[error("Cannot discard 'const' qualifier when converting from '{from}' to '{to}'.")]
    CannotDiscardConst { from: String, to: String },

    #[error(
        "Repr 'c' enum cannot contain non-integer variants, because their layout cannot be represented in the C ABI."
    )]
    ReprCEnumWithNonIntegerVariant,

    #[error("Enum tag type must be a scalar integer, char or bool but got '{got}'.")]
    InvalidEnumTagType { got: String },

    #[error("{err}")]
    InvalidReprAttr { err: String },

    #[error("Alignment must be a power of two, got {value}.")]
    InvalidAlign { value: usize },

    #[error("Global variable requires explicit type annotation.")]
    GlobalVarRequiresTypeAnnotation,

    #[error("Cannot infer dynamic interface type.")]
    CannotInferDynamicInterfaceType,

    #[error("Cannot apply 'dynamic' to an already dynamic value.")]
    InvalidMultipleDynamicType,

    #[error("Cannot call static method '{method_name}' on an instance.")]
    StaticMethodCallOnInstance { method_name: String },

    #[error("Cannot call instance method '{method_name}' without an object.")]
    InstanceMethodCallOnType { method_name: String },

    #[error("Self type can only be used inside an object context.")]
    SelfTypeOutsideOfAnObject,

    #[error("Redundant 'const' qualifier on an already-const type.")]
    RedundantConstQualifier,

    #[error("Element type of an array or slice cannot be 'void'.")]
    VoidElementTypeNotAllowed,

    #[error("Cannot pass a 'void' value into a variadic parameter.")]
    VoidArgumentInVariadicParam,

    #[error("Tuple element cannot have type 'void'.")]
    VoidTupleElementNotAllowed,

    #[error("Field cannot have type 'void'.")]
    VoidFieldType,

    #[error("Function parameter cannot have type 'void'.")]
    VoidParameterType,

    #[error("Unary operator minus is not permitted on unsigned types.")]
    UnaryOperatorMinusOnUnsignedInteger,

    #[error("Value of type '{got_type}' is not compatible with the enum's tag type '{expected_type}'.")]
    InvalidEnumVariantValueType { got_type: String, expected_type: String },

    #[error("Value of type '{got_type}' is not assignable to field type '{expected_type}'.")]
    InvalidEnumVariantFieldValueType { got_type: String, expected_type: String },

    #[error("Invalid enum constructor target; expression '{expr}' is not an enum.")]
    InvalidEnumConstructorTarget { expr: String },

    #[error("Variant '.{variant_name}' has only one field, but multiple were provided.")]
    ValuedEnumVariantCanOnlyExportOneField { variant_name: String },

    #[error("Variant '.{variant_name}' is listed more than once in the patterns.")]
    DuplicateEnumVariantInSwitchPatterns { variant_name: String },

    #[error("Range lower bound must not exceed upper bound.")]
    InvalidRange,

    #[error("Overlapping range in switch case.")]
    OverlappingSwitchCaseRange,

    #[error("Cannot destructure tuple in export without a value.")]
    TupleDestructionWithNoRhs,

    #[error("Missing type argument for generic parameter '{param_name}' in type '{type_name}'.")]
    MissingGenericArgument { type_name: String, param_name: String },

    #[error("Type arguments supplied to non-generic type '{type_name}'.")]
    UnexpectedTypeArgs { type_name: String },

    #[error("Generic static method type arguments must be applied to the operand, not the method.")]
    GenericStaticMethodWrongTypeArgs,

    #[error("Could not resolve type for generic parameter '{param_name}'.")]
    UnresolvedGenericParameter { param_name: String },

    #[error("Type '{type_name}' expects {expected} type arguments but {provided} were provided.")]
    WrongNumberOfTypeArgs {
        type_name: String,
        expected: usize,
        provided: usize,
    },

    #[error("Type arguments must be applied to the enum type, not the variant.")]
    TypeArgsMustBeSuppliedToEnumTypeNotVariant,

    #[error("Unknown symbol '{symbol_name}'.")]
    UnknownSymbol { symbol_name: String },

    #[error("Recursive type '{type_name}' has infinite size.")]
    InfiniteSizeRecursiveType { type_name: String },

    #[error(
        "Method '{method_name}' implemented in object '{object_name}' does not match the method from interface '{interface_name}'."
    )]
    InterfaceMethodTypeMismatch {
        method_name: String,
        object_name: String,
        interface_name: String,
    },

    #[error(
        "Object '{object_name}' does not implement required method '{method_name}' from interface '{interface_name}'."
    )]
    MissingInterfaceMethodImpl {
        object_name: String,
        method_name: String,
        interface_name: String,
    },

    #[error(
        "Mismatch between number of exported values and tuple elements: expected {expected}, but {provided} were provided."
    )]
    TupleExportedValuesAndTupleElementsCountMismatch { expected: usize, provided: usize },

    #[error("Tuple member access with index {index} exceeds tuple length {length}.")]
    TupleIndexOutOfRange { index: usize, length: usize },

    #[error("The type 'void' cannot be used in this context.")]
    VoidVariableType,

    #[error("Enum variant '{variant_name}' expects {expected} fields, but {provided} arguments were provided.")]
    EnumVariantArgCountMismatch {
        variant_name: String,
        expected: u32,
        provided: u32,
    },

    #[error("Enum variant '{variant_name}' does not accept fields.")]
    EnumVariantDoesNotAcceptFields { variant_name: String },

    #[error("Enum '{enum_name}' does not have a variant named '{variant_name}'.")]
    NoSuchEnumVariant { enum_name: String, variant_name: String },

    #[error("Could not infer type of unnamed enum value '.{variant_name}'")]
    CannotInferEnumForUnnamedVariant { variant_name: String },

    #[error("Union initialization must contain exactly one field.")]
    UnionInitMustContainExactlyOneField,

    #[error("Only enum variants are allowed here.")]
    OnlyVariantPatternIsAllowedInSwitch,

    #[error("Unknown field '{field_name}' in enum struct pattern.")]
    UnknownFieldInEnumStructPattern { field_name: String },

    #[error("Switch expression must be an enum type, but found '{expr_type}'.")]
    SwitchOperandIsNotEnum { expr_type: String },

    #[error("Variant '{variant_name}' does not export any fields, but you attempted to destructure it.")]
    VariantDoesNotExportAnyField { variant_name: String },

    #[error("Invalid construction of enum variant.")]
    EnumVariantKindMismatch {
        variant_name: String,
        expected_kind: String,
        provided_kind: String,
    },

    #[error("Variant '{variant_name}' has no field named '{field_name}'.")]
    InvalidEnumVariantField { variant_name: String, field_name: String },

    #[error("Missing field '{field_name}' in initializer for variant '{variant_name}'.")]
    MissingEnumVariantField { field_name: String, variant_name: String },

    #[error(
        "Switch statement must contain at least one case or consider removing it or replacing it with an if statement."
    )]
    EmptyCaseSwitchStatement,

    #[error("Enum variant kind does not match the pattern form.")]
    EnumVariantKindMismatchInSwitchPattern,

    #[error("Only one exporting pattern is allowed per switch case.")]
    MultipleExportingPatternsInSwitchCase,

    #[error("Case pattern with type '{pattern_type}' is not compatible with switch operand of type '{operand_type}'.")]
    TypeMismatchInCasePattern { operand_type: String, pattern_type: String },

    #[error("Cannot call method '{method_name}' on constant instance '{instance_name}'.")]
    MutationPossibleMethodCallOnConstInstance { method_name: String, instance_name: String },

    #[error("Cannot access internal field '{field_name}' of struct '{object_name}' from outside its definition.")]
    InternalFieldAccess { field_name: String, object_name: String },

    #[error("Cannot call internal method '{method_name}' of object '{object_name}' from outside its definition.")]
    InternalMethodCall { method_name: String, object_name: String },

    #[error("Use '->' instead of '.' when accessing a member via a pointer.")]
    UseThinArrow,

    #[error(
        "Interface method '{method_name}' called on a non-dynamic object; expected a dynamic interface object of type '{expected_interface}'."
    )]
    InterfaceMethodCallOnNonDynamicObject {
        method_name: String,
        expected_interface: String,
    },

    #[error(
        "Cannot convert to dynamic interface '{interface_type}'. Type '{concrete_type}' does not implement the interface."
    )]
    DynamicConversionMissingInterface {
        interface_type: String,
        concrete_type: String,
    },

    #[error("Invalid field access (not supported for this symbol).")]
    ObjectNotSupportsFields,

    #[error("Invalid method call (not supported for this symbol).")]
    ObjectNotSupportsMethods,

    #[error("Invalid usage of the thin arrow.")]
    InvalidThinArrow,

    #[error("Method '{method_name}' not defined for '{object_name}'.")]
    ObjectMethodNotDefined { object_name: String, method_name: String },

    #[error("Rhs of the shift must be unsigned integer.")]
    RhsOfShiftMustBeUnsignedInteger,

    #[error("Constant variable must be initialized with a value.")]
    ConstVariableMustBeInitialized,

    #[error("Interface type value must be initialized.")]
    InterfaceTypeMustBeInitialized,

    #[error("Lambda type value must be uninitialized.")]
    LambdaTypeMustBeInitialized,

    #[error("Cyclic type definition found for type '{symbol_name}'.")]
    CyclicTypeDefinition { symbol_name: String },

    #[error("The program entry point is a linkage-visible symbol and cannot be private.")]
    PrivateEntryPoint,

    #[error("No entry point found (missing 'main' function).")]
    MissingEntryPoint,

    #[error("Multiple entry points are not allowed.")]
    MultipleEntryPoints,

    #[error("Missing return statement.")]
    MissingReturn,

    #[error("Return statement requires an argument of type '{argument_type}'.")]
    ReturnStatementNeedsAnArgument { argument_type: String },

    #[error("Return statement argument must be a value of type '{expected}' but got '{got}'.")]
    ReturnStatementTypeMismatch { expected: String, got: String },

    #[error("Function with void return type cannot return a value.")]
    VoidFunctionReturnsValue,

    #[error("Unreachable code.")]
    UnreachableCode,

    #[error("Invalid usage of the continue statement. It must be inside a loop statement.")]
    InvalidContinueStatement,

    #[error("Invalid usage of the break statement. It must be inside a loop/switch statement.")]
    InvalidBreakStatement,

    #[error("Top-level statements cannot be used within a block scope; only at compilation unit level.")]
    InvalidStatement,

    #[error("Condition expression must be of type 'bool'.")]
    ConditionExprMustBeOfTypeBool,

    #[error("{kind} '{name}' does not follow {expected} naming convention.")]
    NamingConv {
        kind: String,
        name: String,
        expected: String,
    },

    #[error("Global variable expression is not valid at compile time.")]
    GlobalVariableExprNotComptimeValid,

    #[error("Expression is not valid at compile time.")]
    ExprNotComptimeValid,

    #[error("Cannot assign to immutable lvalue.")]
    CannotAssignToConstLValue,

    #[error("Missing required field {missing_field_names} in struct '{struct_name}'.")]
    StructMissingFields {
        struct_name: String,
        missing_field_names: String,
    },

    #[error("'{object_name}' has no field named '{field_name}'.")]
    ObjectHasNoFieldNamed { object_name: String, field_name: String },

    #[error("Invalid integer literal suffix.")]
    InvalidIntegerLiteralSuffix,

    #[error("Invalid float literal suffix.")]
    InvalidFloatLiteralSuffix,

    #[error("Array index must be an integer (found '{found_type}').")]
    ArrayNonIntegerIndex { found_type: String },

    #[error("Cannot index non-array value.")]
    ArrayIndexOnNonArrayOperand,

    #[error("Cannot access member of non-tuple value.")]
    TupleMemberAccessOnNonTupleOperand,

    #[error("Cannot take the address of a temporary value.")]
    AddressOfRvalue,

    #[error("Cannot dereference non-pointer value.")]
    DerefNonPointerValue,

    #[error("Cannot dereference a pointer of type 'void*'.")]
    DerefVoidPointerValue,

    #[error("Method generic parameter '{param_name}' shadows generic parameter from '{object_name}'.")]
    ShadowsObjectGenericParam { param_name: String, object_name: String },

    #[error("Duplicate parameter name '{param_name}' at index {param_idx}.")]
    DuplicateFuncParameter { param_name: String, param_idx: u32 },

    #[error("Duplicate declaration of variadic parameter '{param_name}'.")]
    DuplicateFuncVariadicParameter { param_name: String },

    #[error("Duplicate declaration of enum variant '{variant_name}' in enum '{enum_name}'.")]
    DuplicateEnumVariantName { enum_name: String, variant_name: String },

    #[error("Duplicate declaration of field '{field_name}' in '{object_name}'.")]
    DuplicateFieldName { object_name: String, field_name: String },

    #[error("Duplicate initializer for field '{field_name}'.")]
    DuplicateFieldInitializer { field_name: String },

    #[error("Function '{func_name}' expects {expected} arguments, but {args} was provided.")]
    FuncCallArgsCountMismatch {
        args: u32,
        expected: u32,
        func_name: String,
    },

    #[error(
        "Argument at index {argument_idx} has type '{argument_type}', but the variadic parameter expects type '{param_type}'."
    )]
    FuncCallVariadicParamTypeMismatch {
        param_type: String,
        argument_idx: u32,
        argument_type: String,
    },

    #[error(
        "Argument at index {argument_idx} has type '{argument_type}', but the expected parameter type is '{param_type}'."
    )]
    FuncCallParamTypeMismatch {
        param_type: String,
        argument_idx: u32,
        argument_type: String,
    },

    #[error("Symbol '{symbol_name}' is not an interface.")]
    NonInterfaceSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not an union.")]
    NonUnionSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not a typedef.")]
    NonTypedefSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not a variable.")]
    NonVariableSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not a global variable.")]
    NonGlobalVarSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not a function.")]
    NonFunctionSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not a struct.")]
    NonStructSymbol { symbol_name: String },

    #[error("Symbol '{symbol_name}' is not an enum.")]
    NonEnumSymbol { symbol_name: String },

    #[error("Cannot use {elements} elements in an array of size {expected}.")]
    ArrayElementsCountMismatch { elements: u32, expected: u32 },

    #[error("An untyped array was constructed, but the compiler was unable to infer a suitable element type.")]
    UntypedArrayCannotBeInferred,

    #[error("Cannot infer type for generic parameter '{param_name}' in '{type_name}'.")]
    CannotInferGenericArgument { type_name: String, param_name: String },

    #[error(
        "Type mismatch for value of type '{element_type}' at index {element_index} for array of type '{expected_type}'."
    )]
    ArrayElementTypeMismatch {
        element_type: String,
        element_index: u32,
        expected_type: String,
    },

    #[error("Variable has a const-qualified type but is not declared const.")]
    ConstQualifiedTypeAssignedToNonConstVariable,

    #[error("Cannot assign value of type '{rhs_type}' to variable of type '{lhs_type}'.")]
    AssignmentTypeMismatch { lhs_type: String, rhs_type: String },

    #[error("Cannot apply minus operator to value of type '{operand_type}'.")]
    PrefixMinusOnNonInteger { operand_type: String },

    #[error("Cannot apply logical-not operator to value of type '{operand_type}'.")]
    PrefixBangOnNonBool { operand_type: String },

    #[error("Cannot apply infix operator between values of type '{lhs_type}' and '{rhs_type}'.")]
    InvalidInfix { lhs_type: String, rhs_type: String },

    #[error("Cannot apply unary operator to value of type '{operand_type}'.")]
    InvalidUnary { operand_type: String },

    #[error("Duplicate method '{method_name}' in interface '{interface_name}'.")]
    InterfaceDuplicateMethod {
        interface_name: String,
        method_name: String,
    },

    #[error("Instance methods must have self modifier.")]
    InterfaceMethodsMustHaveSelfModifier,

    #[error("Interface methods must use referenced self. By-value 'self' is not allowed.")]
    InterfaceMethodsMustUseReferencedSelf,

    #[error("Function declarations cannot have generic parameters.")]
    GenericFunctionDeclaration,

    #[error("Interface method parameter type cannot contain 'Self'.")]
    InterfaceMethodParamContainsSelf {
        interface_name: String,
        method_name: String,
    },

    #[error("Interface method return type cannot contain 'Self'.")]
    InterfaceMethodReturnTypeContainsSelf {
        interface_name: String,
        method_name: String,
    },

    #[error("{0}")]
    UnescapeError(UnescapeError),
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn report_const_qualified_type_assigned_to_non_const_variable(&mut self, loc: Loc) {
        self.reporter.report(Diag {
            level: DiagLevel::Warning,
            kind: Box::new(AnalyzerDiagKind::ConstQualifiedTypeAssignedToNonConstVariable),
            loc: Some(loc),
            hint: Some(
                "Prefer declaring the variable itself as const instead of using a const-qualified type.".to_string(),
            ),
        });
    }

    /// Validates that an expression type is suitable for use as a boolean condition.
    pub(crate) fn report_if_not_cond_expr(&mut self, sema_type: SemaType, loc: Loc) {
        if !sema_type.is_bool() {
            self.reporter.report(Diag {
                level: DiagLevel::Error,
                kind: Box::new(AnalyzerDiagKind::ConditionExprMustBeOfTypeBool),
                loc: Some(loc),
                hint: None,
            });
        }
    }

    pub(crate) fn report_if_duplicate_param_names(
        &mut self,
        params: &[TypedFuncParamKind],
        variadic: Option<&TypedFuncVariadicParam>,
        loc: Loc,
    ) {
        let mut param_names: Vec<String> = Vec::new();

        for (param_idx, param) in params.iter().enumerate() {
            match param {
                TypedFuncParamKind::FuncParam(typed_func_param) => {
                    if param_names.contains(&typed_func_param.ident.value) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::DuplicateFuncParameter {
                                param_name: typed_func_param.ident.as_string(),
                                param_idx: param_idx.try_into().unwrap(),
                            }),
                            loc: Some(loc),
                            hint: Some("Consider to rename the parameter to a different name.".to_string()),
                        });
                        continue;
                    }

                    param_names.push(typed_func_param.ident.as_string());
                }
                TypedFuncParamKind::SelfModifier(_) => {
                    param_names.push("self".to_string());
                }
            }
        }

        if let Some(variadic_param) = variadic {
            match variadic_param {
                TypedFuncVariadicParam::Typed { var_decl_id, .. } => {
                    let var_decl = self.decl_tables.var_decl(*var_decl_id);
                    let var_name = &var_decl.name;

                    if param_names.contains(var_name) {
                        self.reporter.report(Diag {
                            level: DiagLevel::Error,
                            kind: Box::new(AnalyzerDiagKind::DuplicateFuncVariadicParameter {
                                param_name: var_name.clone(),
                            }),
                            loc: Some(loc),
                            hint: Some("Consider to rename the parameter to a different name.".to_string()),
                        });
                    }
                }
                TypedFuncVariadicParam::UntypedCStyle => {}
            }
        }
    }

    /// Applies a function to a mutable reference of diagnostics within a specified range.
    ///
    /// This method iterates over a slice of diagnostics, determined by `start` and `end_inclusive`,
    /// and applies the provided closure `f` to each diagnostic in mutable form.
    ///
    /// Handles invalid ranges (start > end, start out of bounds) by returning early.
    /// Ensures the range does not exceed the bounds of the diagnostic reporter.
    /// Lazily drops the mutable borrow of diagnostics after the loop.
    pub(crate) fn apply_error_originated_from_on_diag_range<F>(&mut self, range: RangeInclusive<usize>, mut f: F)
    where
        F: FnMut(&mut Diag),
    {
        let len = self.reporter.len();
        let start = *range.start();
        let end_inclusive = *range.end();

        if start > end_inclusive {
            return;
        }
        if start >= len {
            return;
        }

        let end = (end_inclusive + 1).min(len);

        let mut diags = self.reporter.diags_mut();
        for diag in &mut diags[start..end] {
            f(diag);
        }
        drop(diags);
    }

    pub(crate) fn report_non_struct_symbol(&self, symbol_name: String, loc: Loc) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(AnalyzerDiagKind::NonStructSymbol { symbol_name }),
            loc: Some(loc),
            hint: None,
        });
    }

    pub(crate) fn report_non_enum_symbol(&self, symbol_name: String, loc: Loc) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(AnalyzerDiagKind::NonEnumSymbol { symbol_name }),
            loc: Some(loc),
            hint: None,
        });
    }

    pub(crate) fn report_non_union_symbol(&self, symbol_name: String, loc: Loc) {
        self.reporter.report(Diag {
            level: DiagLevel::Error,
            kind: Box::new(AnalyzerDiagKind::NonUnionSymbol { symbol_name }),
            loc: Some(loc),
            hint: None,
        });
    }
}

impl DiagKind for AnalyzerDiagKind {}
