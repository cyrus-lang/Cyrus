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

    #[test]
    pub fn test_init_generic_type() {
        // template: <A, B>
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
            ],
        };

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

        assert!(result.is_ok());

        let generic_type = result.unwrap();

        let mapping_ctx_ref = generic_type.mapping_ctx.borrow();

        let a_ty = mapping_ctx_ref.named.get(&TypedIdentifier {
            name: "A".to_string(),
            symbol_id: 1000,
            loc: SourceLoc::default(),
        });
        let b_ty = mapping_ctx_ref.named.get(&TypedIdentifier {
            name: "B".to_string(),
            symbol_id: 2000,
            loc: SourceLoc::default(),
        });

        assert!(
            matches!(a_ty, Some(SemanticType::PlainType(PlainType::Bool))),
            "Expected generic param 'A' to map to Bool, got {:?}.",
            a_ty
        );
        assert!(
            matches!(b_ty, Some(SemanticType::PlainType(PlainType::Int))),
            "Expected generic param 'B' to map to Int, got {:?}.",
            b_ty
        );

        // Optional: ensure only those two entries exist
        assert_eq!(mapping_ctx_ref.named.len(), 2);
    }
}
