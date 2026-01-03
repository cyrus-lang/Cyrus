// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

/* 
 * Copyright (c) 2026 The Cyrus Programming Language Project
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
use std::{
    borrow::Cow,
    ffi::{CStr, CString},
};

pub(crate) fn to_c_str(mut s: &str) -> Cow<'_, CStr> {
    if s.is_empty() {
        s = "\0";
    }

    if !s.chars().rev().any(|ch| ch == '\0') {
        return Cow::from(CString::new(s).expect("Unreachable since null bytes are checked."));
    }

    unsafe { Cow::from(CStr::from_ptr(s.as_ptr() as *const _)) }
}
