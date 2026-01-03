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
macro_rules! define_call_convs {
    ($( $variant:ident => $str:expr ),* $(,)?) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum CallConv {
            $( $variant ),*
        }

        #[derive(Debug, Clone)]
        pub struct ParseCallConvError(pub String);

        impl std::fmt::Display for ParseCallConvError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "Invalid call convention: '{}'.", self.0)
            }
        }

        impl std::error::Error for ParseCallConvError {}

        impl std::convert::TryFrom<String> for CallConv {
            type Error = ParseCallConvError;

            fn try_from(value: String) -> Result<Self, Self::Error> {
                CallConv::try_from(value.as_str())
            }
        }

        impl std::convert::TryFrom<&str> for CallConv {
            type Error = ParseCallConvError;

            fn try_from(value: &str) -> Result<Self, Self::Error> {
                match value.to_lowercase().as_str() {
                    $( $str => Ok(CallConv::$variant), )*
                    other => Err(ParseCallConvError(other.to_string())),
                }
            }
        }

        impl std::fmt::Display for CallConv {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( CallConv::$variant => write!(f, "{}", $str), )*
                }
            }
        }
    };
}

define_call_convs! {
    C => "c",
    Naked => "naked",
    Interrupt => "interrupt",
    Fast => "fast",
    Cold => "cold",
    Aapcs => "aapcs",
    Stdcall => "stdcall",
    Fastcall => "fastcall",
    Thiscall => "thiscall",
    Vectorcall => "vectorcall",
    SysV64 => "sysv64",
    Win64 => "win64",
    System => "system",
}
