use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OptionalFlag {
    NoReturn,
    NoUnwind,
    Cold,
    Hot,
    OptSize,
    OptNone,
    NoSanitize(String),
}

pub fn validate_flags(flags: &[OptionalFlag]) -> Result<Vec<OptionalFlag>, String> {
    let mut seen = HashSet::new();
    let mut has_opt_size = false;
    let mut has_opt_none = false;
    let mut has_hot = false;
    let mut has_cold = false;

    for flag in flags {
        match flag {
            OptionalFlag::NoSanitize(name) => {
                if !seen.insert(OptionalFlag::NoSanitize(name.clone())) {
                    return Err(format!("Duplicate nosanitize flag: '{}'.", name));
                }
            }
            OptionalFlag::OptSize => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate optsize flag.".into());
                }
                has_opt_size = true;
            }
            OptionalFlag::OptNone => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate optnone flag.".into());
                }
                has_opt_none = true;
            }
            OptionalFlag::Hot => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate hot flag.".into());
                }
                has_hot = true;
            }
            OptionalFlag::Cold => {
                if !seen.insert(flag.clone()) {
                    return Err("Duplicate cold flag.".into());
                }
                has_cold = true;
            }
            _ => {
                if !seen.insert(flag.clone()) {
                    return Err(format!("Duplicate flag: {:?}", flag));
                }
            }
        }
    }

    if has_opt_size && has_opt_none {
        return Err("Cannot use both 'optsize' and 'optnone' flags together.".into());
    }
    if has_hot && has_cold {
        return Err("Cannot use both 'hot' and 'cold' flags together.".into());
    }

    Ok(flags.to_vec())
}
