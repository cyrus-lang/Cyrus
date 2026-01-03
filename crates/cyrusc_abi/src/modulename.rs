// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use std::path::{Path, PathBuf};

pub fn make_module_name_from_filepath<P: AsRef<Path>>(
    path: P,
    base_path: Option<&Path>,
    stdlib_path: Option<&Path>,
) -> String {
    let path_ref = path.as_ref();
    let path = path_ref.canonicalize().unwrap_or_else(|_| PathBuf::from(path_ref));

    // try to strip the base path if provided
    let relative_path = if let Some(base) = base_path {
        path.strip_prefix(base).unwrap_or(&path).to_path_buf()
    } else {
        path.clone()
    };

    let mut parts: Vec<String> = relative_path
        .iter()
        .filter_map(|c| {
            let s = c.to_string_lossy();
            if s.is_empty() { None } else { Some(s.to_string()) }
        })
        .collect();

    // remove extension from last component
    if let Some(last) = parts.last_mut() {
        if let Some(stripped) = last.strip_suffix(".cyrus") {
            *last = stripped.to_string();
        }
    }

    // detect if path belongs to stdlib
    let is_stdlib = stdlib_path.map(|s| path.starts_with(s)).unwrap_or(false);

    let mut module_name = parts.join("_");

    // avoid double `stdlib_stdlib` prefix
    if module_name.starts_with("stdlib_") && is_stdlib {
        // already prefixed
    } else if is_stdlib {
        module_name = format!("stdlib_{}", module_name);
    }

    // remove leading underscores
    module_name = module_name.trim_start_matches('_').to_string();

    // sanitize
    module_name
        .chars()
        .map(|ch| if ch.is_alphanumeric() || ch == '_' { ch } else { '_' })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn tmp_module_with_base_path() {
        let name = make_module_name_from_filepath(
            "/home/taha/Code/Cyrus/tmp/main.cyrus",
            Some(Path::new("/home/taha/Code/Cyrus")),
            None,
        );
        assert_eq!(name, "tmp_main");
    }

    #[test]
    fn stdlib_module_with_prefix() {
        let name = make_module_name_from_filepath(
            "/home/taha/Code/Cyrus/stdlib/math.cyrus",
            Some(Path::new("/home/taha/Code/Cyrus")),
            Some(Path::new("/home/taha/Code/Cyrus/stdlib")),
        );
        assert_eq!(name, "stdlib_math");
    }

    #[test]
    fn stdlib_nested_module() {
        let name = make_module_name_from_filepath(
            "/home/taha/Code/Cyrus/stdlib/libc/abi.cyrus",
            Some(Path::new("/home/taha/Code/Cyrus")),
            Some(Path::new("/home/taha/Code/Cyrus/stdlib")),
        );
        assert_eq!(name, "stdlib_libc_abi");
    }

    #[test]
    fn non_stdlib_nested_module() {
        let name = make_module_name_from_filepath(
            "/home/taha/Code/Cyrus/app/core/math_utils.cyrus",
            Some(Path::new("/home/taha/Code/Cyrus")),
            Some(Path::new("/home/taha/Code/Cyrus/stdlib")),
        );
        assert_eq!(name, "app_core_math_utils");
    }

    #[test]
    fn handles_weird_characters() {
        let name = make_module_name_from_filepath(
            "/weird-paths/foo-bar/baz@v1.cyrus",
            Some(Path::new("/weird-paths")),
            None,
        );
        assert_eq!(name, "foo_bar_baz_v1");
    }
}
