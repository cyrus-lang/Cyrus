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
use crate::{
    callconv::CallConv,
    mangler::{ABINameMangler, C_ABI, CYRUS_ABI},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Linkage {
    Extern(Option<CallConv>),
    Weak,
    LinkOnce,
}

impl Linkage {
    pub fn abi_mangler(&self) -> &'static dyn ABINameMangler {
        match self {
            Linkage::Extern(call_conv_opt) => match call_conv_opt {
                Some(call_conv) => match call_conv {
                    CallConv::C
                    | CallConv::Stdcall
                    | CallConv::Fastcall
                    | CallConv::Thiscall
                    | CallConv::Vectorcall
                    | CallConv::SysV64
                    | CallConv::Win64
                    | CallConv::System => &*C_ABI,

                    CallConv::Naked | CallConv::Interrupt | CallConv::Fast | CallConv::Cold | CallConv::Aapcs => {
                        &*CYRUS_ABI
                    }
                },
                None => &*C_ABI,
            },
            Linkage::Weak => &*CYRUS_ABI,
            Linkage::LinkOnce => &*CYRUS_ABI,
        }
    }
}
