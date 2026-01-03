// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

#[cfg(test)]
mod tests {
    use cyrusc_ast::source_loc::SourceLoc;

    use crate::{
        exprs::TypedIdentifier,
        stmts::{TypedGenericParam, TypedGenericParamsList},
    };

    fn make_param(name: &str) -> TypedGenericParam {
        TypedGenericParam {
            param_name: TypedIdentifier {
                name: name.to_string(),
                symbol_id: 1000,
                loc: SourceLoc::default(),
            },
            bounds: None,
            default: None,
        }
    }

    #[test]
    fn test_new_list_is_empty() {
        let list = TypedGenericParamsList::new();
        assert_eq!(list.list.len(), 0);
    }

    #[test]
    fn test_push_adds_param() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("T"));
        assert_eq!(list.list.len(), 1);
        assert_eq!(list.list[0].param_name.name, "T");
    }

    #[test]
    fn test_get_named_found() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("A"));
        list.push(make_param("B"));

        let found = list.get_named(&"B".to_string());
        assert!(found.is_some());
        assert_eq!(found.unwrap().param_name.name, "B");
    }

    #[test]
    fn test_get_named_not_found() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("X"));

        let found = list.get_named(&"Y".to_string());
        assert!(found.is_none());
    }

    #[test]
    fn test_get_positional_found() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("T"));
        list.push(make_param("U"));

        let found = list.get_positional(1);
        assert!(found.is_some());
        assert_eq!(found.unwrap().param_name.name, "U");
    }

    #[test]
    fn test_get_positional_out_of_bounds() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("T"));

        let found = list.get_positional(5);
        assert!(found.is_none());
    }
}
