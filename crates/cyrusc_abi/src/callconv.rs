use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CallConv {
    C,
    Naked,
    Interrupt,
    Fast,
    Cold,
    Aapcs,
    Stdcall,
    Fastcall,
    Thiscall,
    Vectorcall,
    SysV64,
    Win64,
    System,
}

#[derive(Debug, Clone)]
pub struct ParseCallConvError(pub String);

impl std::fmt::Display for ParseCallConvError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid call convention: '{}'.", self.0)
    }
}

impl std::error::Error for ParseCallConvError {}

impl TryFrom<String> for CallConv {
    type Error = ParseCallConvError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        CallConv::try_from(value.as_str())
    }
}

impl TryFrom<&str> for CallConv {
    type Error = ParseCallConvError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.to_lowercase().as_str() {
            "c" => Ok(CallConv::C),
            "naked" => Ok(CallConv::Naked),
            "interrupt" => Ok(CallConv::Interrupt),
            "fast" => Ok(CallConv::Fast),
            "cold" => Ok(CallConv::Cold),
            "aapcs" => Ok(CallConv::Aapcs),
            "stdcall" => Ok(CallConv::Stdcall),
            "fastcall" => Ok(CallConv::Fastcall),
            "thiscall" => Ok(CallConv::Thiscall),
            "vectorcall" => Ok(CallConv::Vectorcall),
            "sysv64" => Ok(CallConv::SysV64),
            "win64" => Ok(CallConv::Win64),
            "system" => Ok(CallConv::System),
            other => Err(ParseCallConvError(other.to_string())),
        }
    }
}
