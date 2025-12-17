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
