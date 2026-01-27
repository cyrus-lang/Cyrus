use std::fmt;

/// Opaque identifier for a registered vtable.
///
/// VTableIDs are stable for the duration of compilation and are
/// assigned sequentially.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VTableID(pub u32);

impl fmt::Display for VTableID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}
