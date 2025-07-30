#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CodeGenDiagKind {
    NoEntryPointDetected,
    InvalidTypeToken,
    DerefNonPointerType,
    InfixNonBasic,
    NonInternalEntryPoint,
    UnimplementedFeature,
    InvalidTokenAsArrayCapacity,
    IdentifierNotDefined(String),
    TypeAnnotationRequired,
    UndefinedDataType(String),
    FuncNotFound(String),
    ModuleNotFound(String),
    ModuleImportNotFound(String),
    FuncCallArgumentCountMismatch(String, i32, i32),
    MethodCallArgumentCountMismatch(String, i32, i32),
    TypeAnnotationRequiredForParam(String, String),
    MethodNotDefinedForStruct(String, String),
    SizeOfOperatorOnUnsizedObject,
    CannotUseModuleImportIfImportsSingles,
    FuncCallInvalidOperand,
    DuplicateNaming(String),
    SymbolNotFoundInModule(String, String),
    ImportingPrivateFunc(String),
    ImportingPrivateStruct(String),
    ImportingPrivateTypedef(String),
    InvalidStructAccessSpecifier,
    InvalidEnumAccessSpecifier,
    MethodCallOnNonStructValue,
    MethodIsStatic(String),
    MethodIsAnInstance(String),
    SymbolIsNotAnStruct(String, String),
    MustBeComptimeExpr,
    FieldNotFoundForEnumVariant(String),
    EnumVariantNotDefined(String, String),
    Custom(String),
}

impl fmt::Display for CodeGenDiagKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = match self {
            DiagKind::Custom(str) => str,
            DiagKind::InvalidTypeToken => "Invalid type token.",
            DiagKind::UnimplementedFeature => "Unimplemented.",
            DiagKind::DerefNonPointerType => "Cannot dereference a non-pointer type.",
            DiagKind::NoEntryPointDetected => "No entry point detected.",
            DiagKind::NonInternalEntryPoint => "Entry pont must be defined internally.",
            DiagKind::TypeAnnotationRequiredForParam(param, func) => &format!(
                "Type annotation required for parameter '{}' in function '{}'.",
                param, func
            ),
            DiagKind::TypeAnnotationRequired => &format!("Type annotation required.",),
            DiagKind::InfixNonBasic => "Cannot build infix expression for non-basic value.",
            DiagKind::InvalidTokenAsArrayCapacity => "Invalid token given as array capacity.",
            DiagKind::IdentifierNotDefined(value) => &format!("The '{}' not found anywhere.", value),
            DiagKind::FuncNotFound(func_name) => &format!("Unable to resolve function '{}'.", func_name),
            DiagKind::ModuleNotFound(name) => &format!(
                "The module '{}' could not be found in any of the specified source directories.",
                name
            ),
            DiagKind::FuncCallArgumentCountMismatch(func_name, current, expected) => &format!(
                "Expected {} arguments for function '{}', but got {}.",
                expected, func_name, current
            ),
            DiagKind::MethodCallArgumentCountMismatch(func_name, current, expected) => &format!(
                "Expected {} arguments for method '{}', but got {}.",
                expected, func_name, current
            ),
            DiagKind::UndefinedDataType(type_name) => {
                &format!("The data type '{}' is not defined in this module.", type_name)
            }
            DiagKind::SizeOfOperatorOnUnsizedObject => {
                "Cannot determine complete sizeof with flexible member at compile time."
            }
            DiagKind::CannotUseModuleImportIfImportsSingles => "Cannot use module import if it imports singles.",
            DiagKind::FuncCallInvalidOperand => "Invalid operand for function call.",
            DiagKind::DuplicateNaming(name) => {
                &format!("Another object already declared with name '{}' in this module.", name)
            }
            DiagKind::SymbolNotFoundInModule(symbol, module_name) => {
                &format!("Symbol '{}' not found in module '{}'.", symbol, module_name)
            }
            DiagKind::ImportingPrivateFunc(func_name) => &format!("Cannot import private function '{}'.", func_name),
            DiagKind::ImportingPrivateStruct(struct_name) => {
                &format!("Cannot import private struct '{}'.", struct_name)
            }
            DiagKind::ImportingPrivateTypedef(typedef_name) => {
                &format!("Cannot import private typedef '{}'.", typedef_name)
            }
            DiagKind::InvalidStructAccessSpecifier => {
                "Structs must be declared with public or internal access specifier."
            }
            DiagKind::InvalidEnumAccessSpecifier => "Enums must be declared with public or internal access specifier.",
            DiagKind::ModuleImportNotFound(module_name) => &format!("Module '{}' not found.", module_name),
            DiagKind::MethodCallOnNonStructValue => "Cannot build method call for non-struct values.",
            DiagKind::MethodNotDefinedForStruct(method_name, struct_name) => {
                &format!("Method '{}' not defined for struct '{}'.", method_name, struct_name)
            }
            DiagKind::MethodIsStatic(method_name) => {
                &format!("Method '{}' is static, cannot be called on an instance.", method_name)
            }
            DiagKind::MethodIsAnInstance(method_name) => {
                &format!("Method '{}' belongs to an instance, not the type itself.", method_name)
            }
            DiagKind::SymbolIsNotAnStruct(symbol_name, module_name) => &format!(
                "Symbol '{}' from module '{}' is not a struct.",
                symbol_name, module_name
            ),
            DiagKind::MustBeComptimeExpr => {
                "The value assigned to an enum variant must be determinable at compile time. This means you cannot use complex expressions or logic that would require runtime evaluation or execution within a function's basic block. Please ensure the value is a constant expression."
            }
            DiagKind::FieldNotFoundForEnumVariant(field_name) => {
                &format!("'{}' field is not defined for enum variants.", field_name)
            }
            DiagKind::EnumVariantNotDefined(variant_name, enum_name) => &format!(
                "Enum variant '{}' is not defined for enum '{}'.",
                variant_name, enum_name,
            ),
        };
        write!(f, "{}", msg)
    }
}
