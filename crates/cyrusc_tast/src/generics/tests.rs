#[cfg(test)]
mod tests {
    use crate::{
        exprs::TypedIdentifier,
        generics::{
            generic_type::GenericType,
            mapping_ctx::{GenericMappingCtx, mapping_ctx_eq},
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

    fn make_simple_ctx_a() -> GenericMappingCtx {
        let mut ctx = GenericMappingCtx::new_root();
        ctx.named.insert(
            TypedIdentifier {
                name: "A".into(),
                symbol_id: 1,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Int),
        );
        ctx
    }

    fn make_simple_ctx_b() -> GenericMappingCtx {
        let mut ctx = GenericMappingCtx::new_root();
        ctx.named.insert(
            TypedIdentifier {
                name: "A".into(),
                symbol_id: 1,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Int),
        );
        ctx
    }

    #[test]
    pub fn test_generic_type_with_mixed_args() {
        let generic_params = make_generic_params();

        // type args: <Bool, B=Int>
        let type_args = vec![
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

        let mut generic_type = GenericType {
            base: 2,
            type_args,
            mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
            altered_generic_params: None,
            is_const: false,
            loc: SourceLoc::default(),
        };

        let result = generic_type.init(generic_params);
        assert!(
            result.is_ok(),
            "Mixed positional + named arguments should resolve cleanly."
        );

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
        // template: <A, B, C>
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

        // type args: <int, C = bool, B = char>
        let type_args = vec![
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

        let mut generic_type = GenericType {
            base: 3,
            type_args,
            mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
            altered_generic_params: None,
            is_const: false,
            loc: SourceLoc::default(),
        };

        let result = generic_type.init(generic_params);
        assert!(
            result.is_ok(),
            "Expected out-of-order named arguments to resolve successfully."
        );

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

    #[test]
    pub fn test_generic_type_finalize_with_defaults() {
        // template: <A, B = Int, C = Bool>
        let generic_params = TypedGenericParamsList {
            list: vec![
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "A".into(),
                        symbol_id: 1000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: None,
                },
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "B".into(),
                        symbol_id: 2000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: Some(SemanticType::PlainType(PlainType::Int)),
                },
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "C".into(),
                        symbol_id: 3000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: Some(SemanticType::PlainType(PlainType::Bool)),
                },
            ],
        };

        // type args: <A = Char>
        let type_args = vec![TypedTypeArg::Named {
            key: "A".into(),
            ty: SemanticType::PlainType(PlainType::Char),
            loc: SourceLoc::default(),
        }];

        let mut generic_type = GenericType {
            base: 10,
            type_args,
            mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
            altered_generic_params: None,
            is_const: false,
            loc: SourceLoc::default(),
        };

        let result = generic_type.init(generic_params.clone());
        assert!(result.is_ok(), "Expected init() to succeed.");

        let finalize_result = generic_type.finalize(generic_params.clone(), |sid| format!("T{}", sid));
        assert!(finalize_result.is_ok(), "Expected finalize() to fill defaults.");

        // validate the inferred context
        let ctx = generic_type.mapping_ctx.borrow();

        let a = ctx.get_with_symbol_id(1000);
        let b = ctx.get_with_symbol_id(2000);
        let c = ctx.get_with_symbol_id(3000);

        assert!(
            matches!(a, Some(SemanticType::PlainType(PlainType::Char))),
            "Expected A=Char, got {:?}",
            a
        );
        assert!(
            matches!(b, Some(SemanticType::PlainType(PlainType::Int))),
            "Expected B=Int (default), got {:?}",
            b
        );
        assert!(
            matches!(c, Some(SemanticType::PlainType(PlainType::Bool))),
            "Expected C=Bool (default), got {:?}",
            c
        );
        assert_eq!(ctx.named.len(), 3);
    }

    #[test]
    pub fn test_generic_type_finalize_requires_explicit_type_args() {
        // template: <A, B = Int, C>
        let generic_params = TypedGenericParamsList {
            list: vec![
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "A".into(),
                        symbol_id: 1000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: None,
                },
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "B".into(),
                        symbol_id: 2000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: Some(SemanticType::PlainType(PlainType::Int)),
                },
                TypedGenericParam {
                    param_name: TypedIdentifier {
                        name: "C".into(),
                        symbol_id: 3000,
                        loc: SourceLoc::default(),
                    },
                    bounds: None,
                    default: None,
                },
            ],
        };

        // type args: <A = Bool>
        let type_args = vec![TypedTypeArg::Named {
            key: "A".into(),
            ty: SemanticType::PlainType(PlainType::Bool),
            loc: SourceLoc::default(),
        }];

        let mut generic_type = GenericType {
            base: 11,
            type_args,
            mapping_ctx: Rc::new(RefCell::new(GenericMappingCtx::new_root())),
            altered_generic_params: None,
            is_const: false,
            loc: SourceLoc::default(),
        };

        let result = generic_type.init(generic_params.clone());
        assert!(result.is_ok(), "Expected init() to succeed.");

        let finalize_result = generic_type.finalize(generic_params.clone(), |sid| format!("T{}", sid));
        assert!(
            finalize_result.is_err(),
            "Expected finalize() to fail due to missing C."
        );

        let err = finalize_result.unwrap_err();
        let err_msg = err.kind.to_string();

        // check that it prone 'requires explicit type arguments' correctly
        assert!(
            err_msg.contains("requires explicit type arguments"),
            "Expected error message to indicate missing explicit type args, got: {}",
            err_msg
        );

        let ctx = generic_type.mapping_ctx.borrow();
        assert!(ctx.get_with_symbol_id(1000).is_some(), "Param A should be set");
        assert!(
            ctx.get_with_symbol_id(2000).is_some(),
            "Param B should be filled from default"
        );
        assert!(ctx.get_with_symbol_id(3000).is_none(), "Param C should be missing");
    }

    #[test]
    fn test_generic_type_eq_same_base_and_ctx() {
        let ctx1 = Rc::new(RefCell::new(make_simple_ctx_a()));
        let ctx2 = Rc::new(RefCell::new(make_simple_ctx_b()));

        let gt1 = GenericType {
            base: 100,
            type_args: vec![],
            mapping_ctx: ctx1,
            is_const: true,
            loc: Default::default(),
            altered_generic_params: None,
        };

        let gt2 = GenericType {
            base: 100,
            type_args: vec![TypedTypeArg::Positional {
                idx: 0,
                ty: SemanticType::PlainType(PlainType::Bool),
                loc: Default::default(),
            }],
            mapping_ctx: ctx2,
            is_const: false,
            loc: Default::default(),
            altered_generic_params: None,
        };

        // type_args and is_const ignored, so equality depends only on base + mapping_ctx
        assert!(mapping_ctx_eq(&gt1.mapping_ctx.borrow(), &gt2.mapping_ctx.borrow()));
        assert_eq!(gt1.base, gt2.base);
    }

    #[test]
    fn test_generic_type_eq_different_base() {
        let ctx1 = Rc::new(RefCell::new(make_simple_ctx_a()));
        let ctx2 = Rc::new(RefCell::new(make_simple_ctx_a()));

        let gt1 = GenericType {
            base: 1,
            type_args: vec![],
            mapping_ctx: ctx1,
            is_const: false,
            altered_generic_params: None,
            loc: Default::default(),
        };
        let gt2 = GenericType {
            base: 2,
            type_args: vec![],
            mapping_ctx: ctx2,
            is_const: false,
            loc: Default::default(),
            altered_generic_params: None,
        };

        assert!(mapping_ctx_eq(&gt1.mapping_ctx.borrow(), &gt2.mapping_ctx.borrow()));
        assert_ne!(gt1.base, gt2.base);
    }

    #[test]
    fn test_generic_type_eq_different_ctx() {
        let mut ctx1 = GenericMappingCtx::new_root();
        ctx1.named.insert(
            TypedIdentifier {
                name: "A".into(),
                symbol_id: 1,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Int),
        );

        let mut ctx2 = GenericMappingCtx::new_root();
        ctx2.named.insert(
            TypedIdentifier {
                name: "A".into(),
                symbol_id: 1,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Bool), // different type
        );

        let gt1 = GenericType {
            base: 50,
            type_args: vec![],
            mapping_ctx: Rc::new(RefCell::new(ctx1)),
            is_const: false,
            loc: Default::default(),
            altered_generic_params: None,
        };
        let gt2 = GenericType {
            base: 50,
            type_args: vec![],
            mapping_ctx: Rc::new(RefCell::new(ctx2)),
            is_const: false,
            loc: Default::default(),
            altered_generic_params: None,
        };

        assert!(!mapping_ctx_eq(&gt1.mapping_ctx.borrow(), &gt2.mapping_ctx.borrow()));
    }

    #[test]
    fn test_generic_type_eq_with_parent_ctx() {
        let mut parent = GenericMappingCtx::new_root();
        parent.named.insert(
            TypedIdentifier {
                name: "P".into(),
                symbol_id: 10,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Char),
        );

        let mut child1 = GenericMappingCtx::new_root();
        child1.parent = Some(Rc::new(parent.clone()));
        child1.named.insert(
            TypedIdentifier {
                name: "C".into(),
                symbol_id: 20,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Int),
        );

        let mut child2 = GenericMappingCtx::new_root();
        child2.parent = Some(Rc::new(parent.clone()));
        child2.named.insert(
            TypedIdentifier {
                name: "C".into(),
                symbol_id: 20,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Int),
        );

        let gt1 = GenericType {
            base: 5,
            type_args: vec![],
            mapping_ctx: Rc::new(RefCell::new(child1)),
            is_const: false,
            loc: Default::default(),
            altered_generic_params: None,
        };
        let gt2 = GenericType {
            base: 5,
            type_args: vec![],
            mapping_ctx: Rc::new(RefCell::new(child2)),
            is_const: false,
            loc: Default::default(),
            altered_generic_params: None,
        };

        assert!(mapping_ctx_eq(&gt1.mapping_ctx.borrow(), &gt2.mapping_ctx.borrow()));
    }

    #[test]
    fn test_generic_type_eq_with_different_parent_ctx() {
        let mut parent1 = GenericMappingCtx::new_root();
        parent1.named.insert(
            TypedIdentifier {
                name: "P".into(),
                symbol_id: 10,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Char),
        );

        let mut parent2 = GenericMappingCtx::new_root(); // different type in parent
        parent2.named.insert(
            TypedIdentifier {
                name: "P".into(),
                symbol_id: 10,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Bool),
        );

        let mut child1 = GenericMappingCtx::new_root();
        child1.parent = Some(Rc::new(parent1));
        child1.named.insert(
            TypedIdentifier {
                name: "C".into(),
                symbol_id: 20,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Int),
        );

        let mut child2 = GenericMappingCtx::new_root();
        child2.parent = Some(Rc::new(parent2));
        child2.named.insert(
            TypedIdentifier {
                name: "C".into(),
                symbol_id: 20,
                loc: Default::default(),
            },
            SemanticType::PlainType(PlainType::Int),
        );

        let gt1 = GenericType {
            base: 5,
            type_args: vec![],
            mapping_ctx: Rc::new(RefCell::new(child1)),
            is_const: false,
            loc: Default::default(),
            altered_generic_params: None,
        };
        let gt2 = GenericType {
            base: 5,
            type_args: vec![],
            mapping_ctx: Rc::new(RefCell::new(child2)),
            is_const: false,
            loc: Default::default(),
            altered_generic_params: None,
        };

        assert!(!mapping_ctx_eq(&gt1.mapping_ctx.borrow(), &gt2.mapping_ctx.borrow()));
    }
}
