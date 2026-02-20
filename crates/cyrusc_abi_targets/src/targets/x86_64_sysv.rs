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

use crate::{ABIArgInfo, ABIFunctionInfo, ABITarget, ABITypeLayout, TargetABI, layouts::type_layout, types::ABIType};
use cyrusc_cir::types::{CIRFuncTy, CIRTy};

pub struct X86_64SysV;

impl X86_64SysV {
    pub fn new() -> Self {
        Self
    }
}

impl TargetABI for X86_64SysV {
    fn classify_arg(&self, layout: &ABITypeLayout) -> ABIArgInfo {
        if layout.is_aggregate {
            // if the size is greater than 16 bytes, it passes in memory.
            if layout.size > 16 {
                return ABIArgInfo::Indirect { by_val: true };
            }

            // if it fits in 16 bytes, System V tries to pack it into registers
            match layout.size {
                1..=8 => ABIArgInfo::Direct {
                    coerce_to: Some(ABIType::Integer(64)),
                },
                9..=16 => {
                    todo!();
                    // ABIArgInfo::Direct {
                    // coerce_to: Some("{ i64, i64 }".to_string()),
                    // }
                }
                _ => ABIArgInfo::Ignore, // ZSTs
            }
        } else {
            ABIArgInfo::Direct { coerce_to: None }
        }
    }

    fn classify_return(&self, layout: &ABITypeLayout) -> ABIArgInfo {
        // System V return rules for structs are nearly identical to arguments.
        self.classify_arg(layout)
    }

    fn stack_alignment(&self) -> u32 {
        16
    }
}

pub(crate) fn classify_func_x86_64_sysv(target: &ABITarget, fn_ty: &CIRFuncTy) -> ABIFunctionInfo {
    let mut params_types = Vec::new();
    let mut params_infos = Vec::new();
    let mut has_sret = false;

    // classify return type

    let ret_layout = type_layout(&target.info, &fn_ty.ret);

    let ret_info = {
        if fn_ty.ret.is_scalar() {
            ABIArgInfo::Direct { coerce_to: None }
        } else if ret_layout.size <= 16 {
            ABIArgInfo::Direct { coerce_to: None }
        } else {
            has_sret = true;
            params_types.push(CIRTy::Pointer(fn_ty.ret.clone()));
            ABIArgInfo::Indirect { by_val: false }
        }
    };

    // classify static params

    for param_ty in &fn_ty.params {
        let param_layout = type_layout(&target.info, param_ty);

        let arg_info = {
            if param_ty.is_scalar() {
                params_types.push(param_ty.clone());
                ABIArgInfo::Direct { coerce_to: None }
            } else if param_layout.size <= 16 {
                params_types.push(param_ty.clone());
                ABIArgInfo::Direct { coerce_to: None }
            } else {
                params_types.push(CIRTy::Pointer(Box::new(param_ty.clone())));
                ABIArgInfo::Indirect { by_val: false }
            }
        };

        params_infos.push(arg_info);
    }

    ABIFunctionInfo {
        ret_info,
        params_infos,
        params_types,
        has_sret,
    }
}
