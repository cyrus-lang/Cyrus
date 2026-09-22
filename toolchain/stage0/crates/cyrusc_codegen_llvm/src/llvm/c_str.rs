// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#[macro_export]
macro_rules! c {
    ($s:expr) => {{
        use std::ffi::CString;

        let s: &str = $s;

        let c_string = if s.bytes().any(|b| b == 0) {
            panic!("c! macro doesn't accept strings with embedded nulls");
        } else {
            CString::new(s).unwrap()
        };

        c_string
    }};
}
