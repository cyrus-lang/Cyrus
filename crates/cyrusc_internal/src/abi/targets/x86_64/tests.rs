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

#[cfg(test)]
mod tests {
    use cyrusc_ast::abi::CallConv;
    use cyrusc_diagcentral::source_loc::SourceLoc;
    use cyrusc_tast::{types::PlainType, vtable::VTableID};

    use crate::{
        abi::{
            args::{ABIArgKind, ABIRetInfoKind},
            target::{ABITargetArch, ABITargetInfo, ABITargetOS, ABITargetObjectFormat, TargetABI},
            targets::x86_64::classify::X86_64,
        },
        cir::types::{CIRArrayTy, CIRDynamicTy, CIRFuncTy, CIRStructTy, CIRTy, CIRUnionTy},
    };

    fn abi() -> X86_64 {
        let info = ABITargetInfo {
            os: ABITargetOS::Linux,
            arch: ABITargetArch::X86_64,
            format: ABITargetObjectFormat::Elf,
        };
        X86_64::new(info)
    }

    fn i8() -> CIRTy {
        CIRTy::PlainType(PlainType::Int8)
    }

    fn i32() -> CIRTy {
        CIRTy::PlainType(PlainType::Int32)
    }

    fn i64() -> CIRTy {
        CIRTy::PlainType(PlainType::Int64)
    }

    fn u8() -> CIRTy {
        CIRTy::PlainType(PlainType::UInt8)
    }

    fn f32() -> CIRTy {
        CIRTy::PlainType(PlainType::Float32)
    }

    fn f64() -> CIRTy {
        CIRTy::PlainType(PlainType::Float64)
    }

    fn struct_ty(fields: Vec<CIRTy>) -> CIRTy {
        let fields = fields.iter().map(|ty| ("".to_string(), ty.clone())).collect();

        CIRTy::Struct(CIRStructTy {
            name: None,
            fields,
            repr_attr: None,
            align: None,
            loc: SourceLoc::default(),
        })
    }

    fn union_ty(fields: Vec<CIRTy>) -> CIRTy {
        let fields = fields.iter().map(|ty| ("".to_string(), ty.clone())).collect();

        CIRTy::Union(CIRUnionTy {
            name: None,
            fields,
            align: None,
            repr_attr: None,
            loc: SourceLoc::default(),
        })
    }

    fn array_ty(ty: CIRTy, len: usize) -> CIRTy {
        CIRTy::Array(CIRArrayTy { ty: Box::new(ty), len })
    }

    #[test]
    fn classify_i32_argument_integer() {
        let abi = abi();
        let (info, regs) = abi.classify_argument(&i32(), 6, true);

        assert!(matches!(
            info.kind,
            ABIArgKind::DirectPair { .. } | ABIArgKind::DirectCoerce { .. }
        ));
        assert_eq!(regs.int_regs, 1);
        assert_eq!(regs.sse_regs, 0);
    }

    #[test]
    fn classify_i64_argument_integer() {
        let abi = abi();
        let (info, regs) = abi.classify_argument(&i64(), 6, true);

        assert!(matches!(
            info.kind,
            ABIArgKind::DirectPair { .. } | ABIArgKind::DirectCoerce { .. }
        ));
        assert_eq!(regs.int_regs, 1);
        assert_eq!(regs.sse_regs, 0);
    }

    #[test]
    fn classify_f32_argument_sse() {
        let abi = abi();
        let (info, regs) = abi.classify_argument(&f32(), 6, true);

        assert!(matches!(
            info.kind,
            ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. }
        ));
        assert_eq!(regs.int_regs, 0);
        assert_eq!(regs.sse_regs, 1);
    }

    #[test]
    fn classify_f64_argument_sse() {
        let abi = abi();
        let (info, regs) = abi.classify_argument(&f64(), 6, true);

        assert!(matches!(
            info.kind,
            ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. }
        ));
        assert_eq!(regs.int_regs, 0);
        assert_eq!(regs.sse_regs, 1);
    }

    #[test]
    fn classify_struct_two_i64_direct_pair() {
        let abi = abi();
        let ty = struct_ty(vec![i64(), i64()]);

        let (info, regs) = abi.classify_argument(&ty, 6, true);

        match info.kind {
            ABIArgKind::DirectPair { .. } => {}
            _ => panic!("expected DirectPair"),
        }

        assert_eq!(regs.int_regs, 2);
    }

    #[test]
    fn classify_struct_two_f64_sse_pair() {
        let abi = abi();
        let ty = struct_ty(vec![f64(), f64()]);

        let (info, regs) = abi.classify_argument(&ty, 6, true);

        match info.kind {
            ABIArgKind::DirectPair { .. } => {}
            _ => panic!("expected DirectPair"),
        }

        assert_eq!(regs.sse_regs, 2);
    }

    #[test]
    fn classify_struct_int_float_pair() {
        let abi = abi();
        let ty = struct_ty(vec![i32(), f64()]);

        let (info, regs) = abi.classify_argument(&ty, 6, true);

        match info.kind {
            ABIArgKind::DirectPair { .. } => {}
            _ => panic!("expected DirectPair"),
        }

        assert_eq!(regs.int_regs, 1);
        assert_eq!(regs.sse_regs, 1);
    }

    #[test]
    fn classify_struct_small_integer_pack() {
        let abi = abi();
        let ty = struct_ty(vec![i32(), i32()]);

        let (info, regs) = abi.classify_argument(&ty, 6, true);

        assert!(matches!(
            info.kind,
            ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. }
        ));
        assert_eq!(regs.int_regs, 1);
    }

    #[test]
    fn classify_struct_large_memory() {
        let abi = abi();
        let ty = struct_ty(vec![i64(), i64(), i64()]);

        let (info, _) = abi.classify_argument(&ty, 6, true);

        assert!(matches!(info.kind, ABIArgKind::Indirect { .. }));
    }

    #[test]
    fn classify_union_integer_float() {
        let abi = abi();
        let ty = union_ty(vec![i64(), f64()]);

        let (info, regs) = abi.classify_argument(&ty, 6, true);

        assert_eq!(regs.int_regs, 1);
        assert!(matches!(
            info.kind,
            ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. }
        ));
    }

    #[test]
    fn classify_array_two_f32_vector() {
        let abi = abi();
        let ty = array_ty(f32(), 2);

        let (info, regs) = abi.classify_argument(&ty, 6, true);

        assert_eq!(regs.sse_regs, 1);
        assert!(matches!(
            info.kind,
            ABIArgKind::Direct { .. } | ABIArgKind::DirectCoerce { .. }
        ));
    }

    #[test]
    fn classify_array_four_f32_two_sse() {
        let abi = abi();
        let ty = array_ty(f32(), 4);

        let (info, regs) = abi.classify_argument(&ty, 6, true);

        match info.kind {
            ABIArgKind::DirectPair { .. } | ABIArgKind::Direct { .. } => {}
            _ => panic!("unexpected classification"),
        }

        assert!(regs.sse_regs >= 1);
    }

    #[test]
    fn classify_dynamic_two_pointers() {
        let abi = abi();
        let ty = CIRTy::Dynamic(CIRDynamicTy { vtable_id: VTableID(0) });

        let (info, regs) = abi.classify_argument(&ty, 6, true);

        match info.kind {
            ABIArgKind::DirectPair { .. } => {}
            _ => panic!("dynamic must be two pointer registers"),
        }

        assert_eq!(regs.int_regs, 2);
    }

    #[test]
    fn classify_return_i64() {
        let abi = abi();
        let ret = abi.classify_return(&i64());

        match ret.kind {
            ABIRetInfoKind::Direct { .. } => {}
            _ => panic!("expected direct return"),
        }
    }

    #[test]
    fn classify_return_two_i64_struct() {
        let abi = abi();
        let ty = struct_ty(vec![i64(), i64()]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("expected DirectPair return"),
        }
    }

    #[test]
    fn classify_return_double_pair() {
        let abi = abi();
        let ty = struct_ty(vec![f64(), f64()]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("expected DirectPair return"),
        }
    }

    #[test]
    fn classify_return_large_struct_sret() {
        let abi = abi();
        let ty = struct_ty(vec![i64(), i64(), i64()]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::Indirect { sret: true } => {}
            _ => panic!("expected sret"),
        }
    }

    #[test]
    fn sysv_integer_register_exhaustion() {
        let abi = abi();

        let fn_ty = CIRFuncTy {
            params: vec![i64(), i64(), i64(), i64(), i64(), i64(), i64()],
            ret: Box::new(CIRTy::PlainType(PlainType::Void)),
            callconv: CallConv::SysV64,
            abi_func_info: None,
            is_var: false,
        };

        let info = abi.classify_func(&fn_ty).unwrap();

        assert_eq!(info.params_infos.len(), 7);

        assert!(matches!(
            info.params_infos[6].kind,
            ABIArgKind::Indirect { .. } | ABIArgKind::DirectCoerce { .. } | ABIArgKind::Direct { .. }
        ));
    }

    #[test]
    fn sysv_sse_register_exhaustion() {
        let abi = abi();

        let params = vec![f64(), f64(), f64(), f64(), f64(), f64(), f64(), f64(), f64()];

        let fn_ty = CIRFuncTy {
            params,
            ret: Box::new(CIRTy::PlainType(PlainType::Void)),
            callconv: CallConv::SysV64,
            abi_func_info: None,
            is_var: false,
        };

        let info = abi.classify_func(&fn_ty).unwrap();

        assert_eq!(info.params_infos.len(), 9);
    }

    #[test]
    fn variadic_promotion_int8() {
        let abi = abi();
        let promoted = abi.apply_variadic_argument_promote(&i8());

        assert!(matches!(promoted, CIRTy::PlainType(PlainType::Int)));
    }

    #[test]
    fn variadic_promotion_float32() {
        let abi = abi();
        let promoted = abi.apply_variadic_argument_promote(&f32());

        assert!(matches!(promoted, CIRTy::PlainType(PlainType::Float64)));
    }

    #[test]
    fn variadic_array_decay() {
        let abi = abi();
        let promoted = abi.apply_variadic_argument_promote(&array_ty(u8(), 8));

        assert!(matches!(promoted, CIRTy::Pointer(_)));
    }

    #[test]
    fn classify_return_i32() {
        let abi = abi();
        let ret = abi.classify_return(&i32());

        assert!(matches!(ret.kind, ABIRetInfoKind::Direct { .. }));
    }

    #[test]
    fn classify_return_f32() {
        let abi = abi();
        let ret = abi.classify_return(&f32());

        assert!(matches!(ret.kind, ABIRetInfoKind::Direct { .. }));
    }

    #[test]
    fn classify_return_f64() {
        let abi = abi();
        let ret = abi.classify_return(&f64());

        assert!(matches!(ret.kind, ABIRetInfoKind::Direct { .. }));
    }

    #[test]
    fn classify_return_i128() {
        let abi = abi();
        let ty = CIRTy::PlainType(PlainType::Int128);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("i128 must return in rax/rdx"),
        }
    }

    #[test]
    fn classify_return_u128() {
        let abi = abi();
        let ty = CIRTy::PlainType(PlainType::UInt128);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("u128 must return in rax/rdx"),
        }
    }

    #[test]
    fn classify_return_pointer() {
        let abi = abi();

        let ty = CIRTy::Pointer(Box::new(i32()));

        let ret = abi.classify_return(&ty);

        assert!(matches!(ret.kind, ABIRetInfoKind::Direct { .. }));
    }

    #[test]
    fn classify_return_function_pointer() {
        let abi = abi();

        let fn_ty = CIRTy::FuncType(CIRFuncTy {
            params: vec![],
            ret: Box::new(i32()),
            callconv: CallConv::SysV64,
            abi_func_info: None,
            is_var: false,
        });

        let ret = abi.classify_return(&fn_ty);

        assert!(matches!(ret.kind, ABIRetInfoKind::Direct { .. }));
    }

    #[test]
    fn classify_return_struct_single_i64() {
        let abi = abi();
        let ty = struct_ty(vec![i64()]);

        let ret = abi.classify_return(&ty);

        assert!(matches!(ret.kind, ABIRetInfoKind::Direct { .. }));
    }

    #[test]
    fn classify_return_struct_i32_i32() {
        let abi = abi();
        let ty = struct_ty(vec![i32(), i32()]);

        let ret = abi.classify_return(&ty);

        assert!(matches!(ret.kind, ABIRetInfoKind::Direct { .. }));
    }

    #[test]
    fn classify_return_struct_i64_i32() {
        let abi = abi();
        let ty = struct_ty(vec![i64(), i32()]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("expected pair return"),
        }
    }

    #[test]
    fn classify_return_struct_float_int() {
        let abi = abi();
        let ty = struct_ty(vec![f64(), i64()]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("mixed sse/integer should be pair"),
        }
    }

    #[test]
    fn classify_return_struct_three_i32() {
        let abi = abi();
        let ty = struct_ty(vec![i32(), i32(), i32()]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("12 byte struct must use two registers"),
        }
    }

    #[test]
    fn classify_return_struct_exact_16_bytes() {
        let abi = abi();
        let ty = struct_ty(vec![i64(), i64()]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("16 byte struct must use two registers"),
        }
    }

    #[test]
    fn classify_return_array_two_i64() {
        let abi = abi();
        let ty = array_ty(i64(), 2);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("array[2] i64 must return in two registers"),
        }
    }

    #[test]
    fn classify_return_array_three_i64_memory() {
        let abi = abi();
        let ty = array_ty(i64(), 3);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::Indirect { sret: true } => {}
            _ => panic!(">16 byte array must use sret"),
        }
    }

    #[test]
    fn classify_return_array_two_f64() {
        let abi = abi();
        let ty = array_ty(f64(), 2);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("two doubles must return via xmm0/xmm1"),
        }
    }

    #[test]
    fn classify_return_union_i64_f64() {
        let abi = abi();
        let ty = union_ty(vec![i64(), f64()]);

        let ret = abi.classify_return(&ty);

        assert!(matches!(ret.kind, ABIRetInfoKind::Direct { .. }));
    }

    #[test]
    fn classify_return_union_large_memory() {
        let abi = abi();
        let ty = union_ty(vec![array_ty(u8(), 24)]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::Indirect { sret: true } => {}
            _ => panic!("large union must use sret"),
        }
    }

    #[test]
    fn classify_return_dynamic_interface_object() {
        let abi = abi();
        let ty = CIRTy::Dynamic(CIRDynamicTy { vtable_id: VTableID(1) });

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("dynamic must return two pointers"),
        }
    }

    #[test]
    fn classify_return_nested_struct_pair() {
        let abi = abi();

        let inner = struct_ty(vec![i64(), i64()]);
        let outer = struct_ty(vec![inner]);

        let ret = abi.classify_return(&outer);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("nested struct <=16 bytes must be pair"),
        }
    }

    #[test]
    fn classify_return_struct_with_array_field() {
        let abi = abi();

        let ty = struct_ty(vec![array_ty(i32(), 4)]);

        let ret = abi.classify_return(&ty);

        match ret.kind {
            ABIRetInfoKind::DirectPair { .. } => {}
            _ => panic!("16 byte struct with array must be pair"),
        }
    }

    #[test]
    fn classify_return_void_ignore() {
        let abi = abi();

        let ret = abi.classify_return(&CIRTy::PlainType(PlainType::Void));

        assert!(matches!(ret.kind, ABIRetInfoKind::Ignore));
    }
}
