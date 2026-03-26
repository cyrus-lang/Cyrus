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
#[cfg(test)]
mod tests {
    use crate::stmts::{TypedGenericParam, TypedGenericParamsList};
    use cyrusc_ast::Ident;
    use cyrusc_source_loc::{FileID, Loc};

    fn make_param(name: &str) -> TypedGenericParam {
        TypedGenericParam {
            name: Ident {
                value: name.to_string(),
                loc: Loc::default(FileID(0)),
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
        assert_eq!(list.list[0].name.value, "T");
    }

    #[test]
    fn test_get_named_found() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("A"));
        list.push(make_param("B"));

        let found = list.lookup_named(&"B".to_string());
        assert!(found.is_some());
        assert_eq!(found.unwrap().name.value, "B");
    }

    #[test]
    fn test_get_named_not_found() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("X"));

        let found = list.lookup_named(&"Y".to_string());
        assert!(found.is_none());
    }

    #[test]
    fn test_get_positional_found() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("T"));
        list.push(make_param("U"));

        let found = list.lookup_positional(1);
        assert!(found.is_some());
        assert_eq!(found.unwrap().name.value, "U");
    }

    #[test]
    fn test_get_positional_out_of_bounds() {
        let mut list = TypedGenericParamsList::new();
        list.push(make_param("T"));

        let found = list.lookup_positional(5);
        assert!(found.is_none());
    }
}
