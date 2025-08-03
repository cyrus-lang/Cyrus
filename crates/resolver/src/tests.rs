#[cfg(test)]
mod tests {
    #[cfg(test)]
    mod tests {
        use crate::{Resolver, scope::SymbolEntry};
        use ast::AccessSpecifier;
        use ast::token::Location;
        use typed_ast::types::{BasicConcreteType, ConcreteType};
        use typed_ast::{
            TypedASTModule, TypedBlockStatement, TypedFuncDef, TypedFuncParam, TypedFuncParamKind, TypedFuncParams,
            TypedStatement,
        };

        #[test]
        fn test_resolve_single_module_with_function() {
            let func = TypedFuncDef {
                name: "foo".to_string(),
                params: TypedFuncParams {
                    list: vec![TypedFuncParamKind::FuncParam(TypedFuncParam {
                        name: "x".to_string(),
                        ty: Some(ConcreteType::BasicType(BasicConcreteType::Float64)),
                        loc: Location::default(),
                    })],
                    variadic: None,
                },
                return_type: ConcreteType::BasicType(BasicConcreteType::Float64),
                vis: AccessSpecifier::Public,
                loc: Location::default(),
                body: Box::new(TypedBlockStatement::new_empty()),
            };

            let ast_module = TypedASTModule {
                body: vec![TypedStatement::FuncDef(func)],
            };

            let module_id = 1;
            let mut resolver = Resolver::new();
            let result = resolver.resolve_module(module_id, &ast_module);
            assert!(result.is_ok());

            let symbol_table = resolver.global_symbols.get(&module_id).expect("Module not found.");
            let symbol_entry = symbol_table.get("foo").expect("Function not found.");

            match symbol_entry {
                SymbolEntry::Func(resolved_fn) => {
                    assert_eq!(resolved_fn.func_sig.name, "foo");
                    assert_eq!(resolved_fn.func_sig.params.list.len(), 1);
                    assert_eq!(
                        resolved_fn.func_sig.return_type,
                        ConcreteType::BasicType(BasicConcreteType::Float64)
                    );
                }
                _ => panic!("Expected SymbolEntry::Func"),
            }
        }
    }
}
