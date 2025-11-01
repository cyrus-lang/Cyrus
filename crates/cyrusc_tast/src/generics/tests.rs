#[cfg(test)]
mod tests {
    use crate::{
        exprs::TypedIdentifier,
        generics::{
            generic_type::{GenericType, GenericTypeState},
            mapping_ctx::GenericMappingCtx,
        },
        stmts::{TypedGenericParam, TypedGenericParamsList, TypedTypeArg},
        types::{PlainType, SemanticType},
    };
    use cyrusc_ast::source_loc::SourceLoc;
    use std::{cell::RefCell, rc::Rc};

    fn make_generic_params() -> TypedGenericParamsList {
        TypedGenericParamsList {
            list: vec![
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "A".to_string(),
                        symbol_id: 1000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: None,
                },
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "B".to_string(),
                        symbol_id: 2000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: None,
                },
            ],
        }
    }

    #[test]
    pub fn test_generic_type_with_positional_args() {
        let generic_params = make_generic_params();

        // type args: <Bool, Int>
        let input_type_args = vec![
            TypedTypeArg::Positional {
                idx: 0,
                ty: SemanticType::PlainType(PlainType::Bool),
                loc: SourceLoc::default(),
            },
            TypedTypeArg::Positional {
                idx: 1,
                ty: SemanticType::PlainType(PlainType::Int),
                loc: SourceLoc::default(),
            },
        ];

        let generic_type = GenericType {
            base: 1,
            state: GenericTypeState::Unresolved {
                type_args: input_type_args,
            },
            mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
            is_const: false,
        };

        let result = generic_type.init(generic_params);
        assert!(result.is_ok(), "Expected generic type to resolve successfully.");

        let generic_type = result.unwrap();
        let ctx = generic_type.mapping_ctx.borrow();

        assert!(
            matches!(
                ctx.named.get(&TypedIdentifier {
                    name: "A".into(),
                    symbol_id: 1000,
                    loc: SourceLoc::default()
                }),
                Some(SemanticType::PlainType(PlainType::Bool))
            ),
            "Param 'A' should map to Bool, got {:?}.",
            ctx.named.get(&TypedIdentifier {
                name: "A".into(),
                symbol_id: 1000,
                loc: SourceLoc::default()
            })
        );

        assert!(
            matches!(
                ctx.named.get(&TypedIdentifier {
                    name: "B".into(),
                    symbol_id: 2000,
                    loc: SourceLoc::default()
                }),
                Some(SemanticType::PlainType(PlainType::Int))
            ),
            "Param 'B' should map to Int, got {:?}.",
            ctx.named.get(&TypedIdentifier {
                name: "B".into(),
                symbol_id: 2000,
                loc: SourceLoc::default()
            })
        );

        assert_eq!(ctx.named.len(), 2);
    }

    #[test]
    pub fn test_generic_type_with_mixed_args() {
        let generic_params = make_generic_params();

        // <Bool, B=Int>
        let input_type_args = vec![
            TypedTypeArg::Positional {
                idx: 0,
                ty: SemanticType::PlainType(PlainType::Bool),
                loc: SourceLoc::default(),
            },
            TypedTypeArg::Named {
                key: "B".to_string(),
                ty: SemanticType::PlainType(PlainType::Int),
                loc: SourceLoc::default(),
            },
        ];

        let generic_type = GenericType {
            base: 2,
            state: GenericTypeState::Unresolved {
                type_args: input_type_args,
            },
            mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
            is_const: false,
        };

        let result = generic_type.init(generic_params);
        assert!(
            result.is_ok(),
            "Mixed positional + named arguments should resolve cleanly."
        );

        let generic_type = result.unwrap();
        let ctx = generic_type.mapping_ctx.borrow();

        let a = ctx.named.get(&TypedIdentifier {
            name: "A".into(),
            symbol_id: 1000,
            loc: SourceLoc::default(),
        });
        let b = ctx.named.get(&TypedIdentifier {
            name: "B".into(),
            symbol_id: 2000,
            loc: SourceLoc::default(),
        });

        assert!(
            matches!(a, Some(SemanticType::PlainType(PlainType::Bool))),
            "Expected A=Bool, got {:?}.",
            a
        );
        assert!(
            matches!(b, Some(SemanticType::PlainType(PlainType::Int))),
            "Expected B=Int, got {:?}.",
            b
        );
        assert_eq!(ctx.named.len(), 2);
    }

    #[test]
    pub fn test_generic_type_with_out_of_order_named_args() {
        // Template: <A, B, C>
        let generic_params = TypedGenericParamsList {
            list: vec![
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "A".to_string(),
                        symbol_id: 1000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: None,
                },
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "B".to_string(),
                        symbol_id: 2000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: None,
                },
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "C".to_string(),
                        symbol_id: 3000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: None,
                },
            ],
        };

        // Input: <int, C = bool, B = char>
        let input_type_args = vec![
            TypedTypeArg::Positional {
                idx: 0,
                ty: SemanticType::PlainType(PlainType::Int),
                loc: SourceLoc::default(),
            },
            TypedTypeArg::Named {
                key: "C".to_string(),
                ty: SemanticType::PlainType(PlainType::Bool),
                loc: SourceLoc::default(),
            },
            TypedTypeArg::Named {
                key: "B".to_string(),
                ty: SemanticType::PlainType(PlainType::Char),
                loc: SourceLoc::default(),
            },
        ];

        let generic_type = GenericType {
            base: 3,
            state: GenericTypeState::Unresolved {
                type_args: input_type_args,
            },
            mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
            is_const: false,
        };

        let result = generic_type.init(generic_params);
        assert!(
            result.is_ok(),
            "Expected out-of-order named arguments to resolve successfully."
        );

        let generic_type = result.unwrap();
        let ctx = generic_type.mapping_ctx.borrow();

        let a = ctx.named.get(&TypedIdentifier {
            name: "A".into(),
            symbol_id: 1000,
            loc: SourceLoc::default(),
        });
        let b = ctx.named.get(&TypedIdentifier {
            name: "B".into(),
            symbol_id: 2000,
            loc: SourceLoc::default(),
        });
        let c = ctx.named.get(&TypedIdentifier {
            name: "C".into(),
            symbol_id: 3000,
            loc: SourceLoc::default(),
        });

        assert!(
            matches!(a, Some(SemanticType::PlainType(PlainType::Int))),
            "Expected A=int, got {:?}",
            a
        );
        assert!(
            matches!(b, Some(SemanticType::PlainType(PlainType::Char))),
            "Expected B=char, got {:?}",
            b
        );
        assert!(
            matches!(c, Some(SemanticType::PlainType(PlainType::Bool))),
            "Expected C=bool, got {:?}",
            c
        );
        assert_eq!(ctx.named.len(), 3);
    }
}
