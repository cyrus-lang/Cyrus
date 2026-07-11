// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

use crate::llvm::dwarf::{DW_TAG_CONST_TYPE, DWARF_PRODUCER_NAME};
use fx_hash::{FxHashMap, FxHashMapExt};
use inkwell::{
    builder::Builder,
    context::{AsContextRef, Context},
    debug_info::LLVMDWARFTypeEncoding,
    llvm_sys::{
        LLVMModuleFlagBehavior,
        core::{
            LLVMAddModuleFlag, LLVMConstInt, LLVMGetGlobalParent, LLVMGetInsertBlock, LLVMGetMDKindIDInContext,
            LLVMGetModuleContext, LLVMGetModuleFlag, LLVMGlobalSetMetadata, LLVMInt32Type,
            LLVMSetCurrentDebugLocation2, LLVMValueAsMetadata,
        },
        debuginfo::*,
        prelude::{LLVMDIBuilderRef, LLVMMetadataRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef},
    },
};
use std::ffi::CString;

#[derive(Debug, Clone)]
pub struct DebugFile {
    pub metadata: LLVMMetadataRef,
}

#[derive(Debug, Clone)]
pub struct BlockScope {
    pub lexical_block: LLVMMetadataRef,
    pub inline_loc: LLVMMetadataRef,
}

#[derive(Debug, Clone)]
pub struct DebugContext {
    pub builder: LLVMDIBuilderRef,
    pub compile_unit: LLVMMetadataRef,
    pub func: LLVMMetadataRef,
    pub file: DebugFile,
    pub block_stack: Vec<BlockScope>,
    pub emit_expr_loc: bool,
    pub type_cache: FxHashMap<LLVMTypeRef, LLVMMetadataRef>,

    #[allow(unused)]
    pub runtime_version: u32,
}

impl DebugContext {
    pub unsafe fn new(module: LLVMModuleRef, file_name: &str, dir: &str) -> Self {
        let builder = unsafe { LLVMCreateDIBuilder(module) };

        let mut dctx = DebugContext {
            builder,
            file: DebugFile {
                metadata: std::ptr::null_mut(),
            },
            compile_unit: std::ptr::null_mut(),
            func: std::ptr::null_mut(),
            type_cache: FxHashMap::new(),
            block_stack: Vec::new(),
            runtime_version: 0,
            emit_expr_loc: true,
        };

        unsafe { create_compile_unit(&mut dctx, file_name, dir) };
        dctx
    }
}

pub unsafe fn create_compile_unit(dctx: &mut DebugContext, file: &str, dir: &str) {
    let file_c = CString::new(file).unwrap();
    let dir_c = CString::new(dir).unwrap();
    let producer = CString::new(DWARF_PRODUCER_NAME).unwrap();

    let file_meta =
        unsafe { LLVMDIBuilderCreateFile(dctx.builder, file_c.as_ptr(), file.len(), dir_c.as_ptr(), dir.len()) };

    dctx.file.metadata = file_meta;

    dctx.compile_unit = unsafe {
        LLVMDIBuilderCreateCompileUnit(
            dctx.builder,
            LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageC,
            file_meta,
            producer.as_ptr(),
            producer.as_bytes().len(),
            0,
            std::ptr::null(),
            0,
            0,
            std::ptr::null(),
            0,
            LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull,
            0,
            0,
            0,
            std::ptr::null(),
            0,
            std::ptr::null(),
            0,
        )
    };
}

pub fn debug_current_scope(dctx: &DebugContext) -> LLVMMetadataRef {
    if let Some(block) = dctx.block_stack.last() {
        block.lexical_block
    } else {
        dctx.func
    }
}

pub unsafe fn set_debug_location(dctx: &DebugContext, ctx: &Context, builder: &Builder, row: u32, col: u32) {
    let scope = debug_current_scope(dctx);
    let loc = unsafe { create_debug_location_with_inline(dctx, ctx, row, col, scope) };
    unsafe { LLVMSetCurrentDebugLocation2(builder.as_mut_ptr(), loc) };
}

pub unsafe fn create_debug_lexical_block(
    dctx: &DebugContext,
    parent: LLVMMetadataRef,
    line: u32,
    col: u32,
) -> LLVMMetadataRef {
    unsafe { LLVMDIBuilderCreateLexicalBlock(dctx.builder, parent, dctx.file.metadata, line, col) }
}

pub unsafe fn create_debug_parameter(
    dctx: &DebugContext,
    name: &str,
    line: u32,
    ty: LLVMMetadataRef,
    arg_num: u32,
) -> LLVMMetadataRef {
    let scope = debug_current_scope(dctx);

    let cname = CString::new(name).unwrap();

    unsafe {
        LLVMDIBuilderCreateParameterVariable(
            dctx.builder,
            scope,
            cname.as_ptr(),
            name.len(),
            arg_num, // must be 1-based
            dctx.file.metadata,
            line,
            ty,
            1, // alwaysPreserve
            0,
        )
    }
}

pub unsafe fn create_debug_variable(
    dctx: &DebugContext,
    name: &str,
    line: u32,
    ty: LLVMMetadataRef,
    align: u32,
) -> LLVMMetadataRef {
    let scope = debug_current_scope(dctx);

    let cname = CString::new(name).unwrap();

    unsafe {
        LLVMDIBuilderCreateAutoVariable(
            dctx.builder,
            scope,
            cname.as_ptr(),
            name.len(),
            dctx.file.metadata,
            line,
            ty,
            1, // alwaysPreserve
            0, // flags
            align,
        )
    }
}

pub unsafe fn create_debug_location_with_inline(
    dctx: &DebugContext,
    ctx: &Context,
    row: u32,
    col: u32,
    scope: LLVMMetadataRef,
) -> LLVMMetadataRef {
    let col = if dctx.emit_expr_loc { col } else { 0 };

    let inline_at = dctx
        .block_stack
        .last()
        .and_then(|b| {
            if b.inline_loc.is_null() {
                None
            } else {
                Some(b.inline_loc)
            }
        })
        .unwrap_or(std::ptr::null_mut());

    unsafe { LLVMDIBuilderCreateDebugLocation(ctx.as_ctx_ref(), row, col, scope, inline_at) }
}

pub unsafe fn debug_simple_type(
    dctx: &DebugContext,
    name: &str,
    bits: u64,
    encoding: LLVMDWARFTypeEncoding,
) -> LLVMMetadataRef {
    let cname = CString::new(name).unwrap();

    unsafe { LLVMDIBuilderCreateBasicType(dctx.builder, cname.as_ptr(), name.len(), bits, encoding, LLVMDIFlagZero) }
}

pub unsafe fn debug_const_type(dctx: &DebugContext, base_type: LLVMMetadataRef) -> LLVMMetadataRef {
    unsafe { LLVMDIBuilderCreateQualifiedType(dctx.builder, DW_TAG_CONST_TYPE as LLVMDWARFTypeEncoding, base_type) }
}

pub unsafe fn debug_pointer_type(
    dctx: &DebugContext,
    inner: LLVMMetadataRef,
    size_bits: u64,
    align_bits: u32,
    name: &str,
) -> LLVMMetadataRef {
    let cname = CString::new(name).unwrap();

    unsafe {
        LLVMDIBuilderCreatePointerType(
            dctx.builder,
            inner,
            size_bits,
            align_bits,
            0, // address space
            cname.as_ptr(),
            name.len(),
        )
    }
}

pub unsafe fn debug_enum_type(
    dctx: &DebugContext,
    name: &str,
    line: u32,
    tag_type: LLVMMetadataRef,
    variants: &[(String, i64, LLVMMetadataRef)],
    size_bits: u64,
    align_bits: u32,
) -> LLVMMetadataRef {
    let scope = debug_current_scope(dctx);

    // payload union

    let union_name = format!("{}$payload", name);
    let union_name_cstr = CString::new(union_name.clone()).unwrap();

    let mut union_fields: Vec<LLVMMetadataRef> = Vec::new();

    for (variant_name, _, payload_ty) in variants {
        let cname = CString::new(variant_name.as_str()).unwrap();

        let member = unsafe {
            LLVMDIBuilderCreateMemberType(
                dctx.builder,
                scope,
                cname.as_ptr(),
                variant_name.len(),
                dctx.file.metadata,
                line,
                0,
                0,
                0,
                LLVMDIFlagZero,
                *payload_ty,
            )
        };

        union_fields.push(member);
    }

    let payload_union = unsafe {
        LLVMDIBuilderCreateUnionType(
            dctx.builder,
            scope,
            union_name_cstr.as_ptr(),
            union_name.len(),
            dctx.file.metadata,
            line,
            size_bits,
            align_bits,
            LLVMDIFlagZero,
            union_fields.as_mut_ptr(),
            union_fields.len() as u32,
            0,
            union_name_cstr.as_ptr(),
            union_name.len(),
        )
    };

    // tag field

    let tag_name = CString::new("tag").unwrap();

    let tag_member = unsafe {
        LLVMDIBuilderCreateMemberType(
            dctx.builder,
            scope,
            tag_name.as_ptr(),
            3,
            dctx.file.metadata,
            line,
            8,
            8,
            0,
            LLVMDIFlagZero,
            tag_type,
        )
    };

    // payload field

    let payload_name = CString::new("payload").unwrap();

    let payload_member = unsafe {
        LLVMDIBuilderCreateMemberType(
            dctx.builder,
            scope,
            payload_name.as_ptr(),
            7,
            dctx.file.metadata,
            line,
            size_bits - 8,
            align_bits,
            8,
            LLVMDIFlagZero,
            payload_union,
        )
    };

    let mut members = vec![tag_member, payload_member];

    // final struct

    let cname = CString::new(name).unwrap();

    unsafe {
        LLVMDIBuilderCreateStructType(
            dctx.builder,
            scope,
            cname.as_ptr(),
            name.len(),
            dctx.file.metadata,
            line,
            size_bits,
            align_bits,
            LLVMDIFlagZero,
            std::ptr::null_mut(),
            members.as_mut_ptr(),
            members.len() as u32,
            0,
            std::ptr::null_mut(),
            cname.as_ptr(),
            name.len(),
        )
    }
}

pub unsafe fn debug_scalar_enum_type(
    dctx: &DebugContext,
    name: &str,
    variants: &[(String, i64)],
    size_bits: u64,
    align_bits: u32,
    line: u32,
    underlying: LLVMMetadataRef,
) -> LLVMMetadataRef {
    let scope = debug_current_scope(dctx);

    let mut elements: Vec<LLVMMetadataRef> = Vec::with_capacity(variants.len());

    for (variant, value) in variants {
        let cname = CString::new(variant.as_str()).unwrap();

        let enumerator = unsafe {
            LLVMDIBuilderCreateEnumerator(
                dctx.builder,
                cname.as_ptr(),
                variant.len(),
                *value,
                1, // signed
            )
        };

        elements.push(enumerator);
    }

    let cname = CString::new(name).unwrap();

    unsafe {
        LLVMDIBuilderCreateEnumerationType(
            dctx.builder,
            scope,
            cname.as_ptr(),
            name.len(),
            dctx.file.metadata,
            line,
            size_bits,
            align_bits,
            elements.as_mut_ptr(),
            elements.len() as u32,
            underlying,
        )
    }
}

pub unsafe fn debug_array_type(
    dctx: &DebugContext,
    element_type: LLVMMetadataRef,
    length: u64,
    size_bits: u64,
    align_bits: u32,
) -> LLVMMetadataRef {
    let sub_range = unsafe { LLVMDIBuilderGetOrCreateSubrange(dctx.builder, 0, length as i64) };

    let mut ranges = vec![sub_range];

    unsafe {
        LLVMDIBuilderCreateArrayType(
            dctx.builder,
            size_bits,
            align_bits,
            element_type,
            ranges.as_mut_ptr(),
            ranges.len() as u32,
        )
    }
}

pub unsafe fn debug_union_type(
    dctx: &DebugContext,
    name: &str,
    members: &mut [LLVMMetadataRef],
    size_bits: u64,
    align_bits: u32,
    line: u32,
) -> LLVMMetadataRef {
    let scope = debug_current_scope(dctx);

    let cname = CString::new(name).unwrap();

    unsafe {
        LLVMDIBuilderCreateUnionType(
            dctx.builder,
            scope,
            cname.as_ptr(),
            name.len(),
            dctx.file.metadata,
            line,
            size_bits,
            align_bits,
            LLVMDIFlagZero,
            members.as_mut_ptr(),
            members.len() as u32,
            dctx.runtime_version,
            cname.as_ptr(),
            name.len(),
        )
    }
}

pub unsafe fn debug_member_type(
    dctx: &DebugContext,
    name: &str,
    ty_meta: LLVMMetadataRef,
    offset_bits: u64,
    line: u32,
) -> LLVMMetadataRef {
    let scope = debug_current_scope(dctx);

    let cname = CString::new(name).unwrap();

    unsafe {
        LLVMDIBuilderCreateMemberType(
            dctx.builder,
            scope,
            cname.as_ptr(),
            name.len(),
            dctx.file.metadata,
            line,
            0,
            0,
            offset_bits,
            LLVMDIFlagZero,
            ty_meta,
        )
    }
}

pub unsafe fn debug_dynamic_type(
    dctx: &DebugContext,
    data_ptr_ty: LLVMMetadataRef,
    vtable_ptr_ty: LLVMMetadataRef,
    ptr_size_bits: u64,
    align_bits: u32,
) -> LLVMMetadataRef {
    let scope = debug_current_scope(dctx);

    let data_member = unsafe {
        LLVMDIBuilderCreateMemberType(
            dctx.builder,
            scope,
            c"data_ptr".as_ptr(),
            8,
            dctx.file.metadata,
            0,
            ptr_size_bits,
            align_bits,
            0,
            LLVMDIFlagZero,
            data_ptr_ty,
        )
    };

    let vtable_member = unsafe {
        LLVMDIBuilderCreateMemberType(
            dctx.builder,
            scope,
            c"vtable_ptr".as_ptr(),
            10,
            dctx.file.metadata,
            0,
            ptr_size_bits,
            align_bits,
            ptr_size_bits,
            LLVMDIFlagZero,
            vtable_ptr_ty,
        )
    };

    let mut members = [data_member, vtable_member];

    unsafe {
        LLVMDIBuilderCreateStructType(
            dctx.builder,
            scope,
            c"DynamicValue".as_ptr(),
            8,
            dctx.file.metadata,
            0,
            ptr_size_bits * 2,
            align_bits,
            LLVMDIFlagZero,
            std::ptr::null_mut(),
            members.as_mut_ptr(),
            members.len() as u32,
            0,
            std::ptr::null_mut(),
            std::ptr::null(),
            0,
        )
    }
}

pub unsafe fn debug_struct_type(
    dctx: &DebugContext,
    name: &str,
    elements: &mut [LLVMMetadataRef],
    size_bits: u64,
    align_bits: u32,
    line: u32,
) -> LLVMMetadataRef {
    let scope = debug_current_scope(dctx);

    let cname = CString::new(name).unwrap();

    unsafe {
        LLVMDIBuilderCreateStructType(
            dctx.builder,
            scope,
            cname.as_ptr(),
            name.len(),
            dctx.file.metadata,
            line,
            size_bits,
            align_bits,
            LLVMDIFlagZero,
            std::ptr::null_mut(),
            elements.as_mut_ptr(),
            elements.len() as u32,
            dctx.runtime_version,
            std::ptr::null_mut(),
            name.as_ptr() as *const i8,
            name.len(),
        )
    }
}

pub unsafe fn debug_func_type(
    dctx: &DebugContext,
    ret_type: Option<LLVMMetadataRef>,
    params: &[LLVMMetadataRef],
) -> LLVMMetadataRef {
    let mut types: Vec<LLVMMetadataRef> = Vec::new();

    if let Some(ret) = ret_type {
        types.push(ret);
    } else {
        types.push(std::ptr::null_mut());
    }

    for p in params {
        types.push(*p);
    }

    unsafe {
        LLVMDIBuilderCreateSubroutineType(
            dctx.builder,
            dctx.file.metadata,
            types.as_ptr() as *mut _,
            types.len() as u32,
            LLVMDIFlagZero,
        )
    }
}

pub unsafe fn emit_debug_function(
    dctx: &mut DebugContext,
    func: LLVMValueRef,
    name: &str,
    line: u32,
    debug_type: LLVMMetadataRef,
) {
    let cname = CString::new(name).unwrap();

    let sub_program = unsafe {
        LLVMDIBuilderCreateFunction(
            dctx.builder,
            dctx.file.metadata,
            cname.as_ptr(),
            name.len(),
            cname.as_ptr(),
            name.len(),
            dctx.file.metadata,
            line,
            debug_type,
            0,
            1,
            line,
            LLVMDIFlagPrototyped,
            0,
        )
    };

    dctx.func = sub_program;

    unsafe { LLVMSetSubprogram(func, sub_program) };
}

pub unsafe fn emit_global_debug(
    dctx: &DebugContext,
    global: LLVMValueRef,
    scope: LLVMMetadataRef,
    file: LLVMMetadataRef,
    name: &str,
    linkage_name: &str,
    line: u32,
    ty: LLVMMetadataRef,
    is_local: bool,
) -> LLVMMetadataRef {
    let cname = CString::new(name).unwrap();
    let clinkage = CString::new(linkage_name).unwrap();

    let expr = unsafe { LLVMDIBuilderCreateExpression(dctx.builder, std::ptr::null_mut(), 0) };

    let gv_expr = unsafe {
        LLVMDIBuilderCreateGlobalVariableExpression(
            dctx.builder,
            scope,
            cname.as_ptr(),
            name.len(),
            clinkage.as_ptr(),
            linkage_name.len(),
            file,
            line,
            ty,
            is_local as i32,
            expr,
            std::ptr::null_mut(),
            0,
        )
    };

    let dbg = CString::new("dbg").unwrap();

    let dbg_kind =
        unsafe { LLVMGetMDKindIDInContext(LLVMGetModuleContext(LLVMGetGlobalParent(global)), dbg.as_ptr(), 3) };

    unsafe { LLVMGlobalSetMetadata(global, dbg_kind, gv_expr) };

    gv_expr
}

pub unsafe fn emit_dbg_declare(
    dctx: &DebugContext,
    ctx: &Context,
    builder: &Builder,
    storage: LLVMValueRef,
    var_meta: LLVMMetadataRef,
    line: u32,
    col: u32,
) {
    let expr = unsafe { LLVMDIBuilderCreateExpression(dctx.builder, std::ptr::null_mut(), 0) };

    let scope = debug_current_scope(dctx);

    let loc = unsafe { create_debug_location_with_inline(dctx, ctx, line, col, scope) };

    unsafe {
        LLVMDIBuilderInsertDeclareAtEnd(
            dctx.builder,
            storage,
            var_meta,
            expr,
            loc,
            LLVMGetInsertBlock(builder.as_mut_ptr()),
        )
    };
}

pub unsafe fn finalize_debug(dctx: &DebugContext) {
    unsafe { LLVMDIBuilderFinalize(dctx.builder) };
}

unsafe fn module_flag(module: LLVMModuleRef, behavior: LLVMModuleFlagBehavior, name: &str, value: u32) {
    let cname = CString::new(name).unwrap();

    // check if flag already exists
    let existing = unsafe { LLVMGetModuleFlag(module, cname.as_ptr(), name.len()) };

    if !existing.is_null() {
        return;
    }

    let int_value = unsafe { LLVMConstInt(LLVMInt32Type(), value as u64, 0) };
    let metadata_value = unsafe { LLVMValueAsMetadata(int_value) };

    unsafe { LLVMAddModuleFlag(module, behavior, cname.as_ptr(), name.len(), metadata_value) };
}

pub unsafe fn emit_debug_module_flags(module: LLVMModuleRef) {
    use LLVMModuleFlagBehavior::*;

    unsafe { module_flag(module, LLVMModuleFlagBehaviorWarning, "Dwarf Version", 4) };
    unsafe { module_flag(module, LLVMModuleFlagBehaviorWarning, "Debug Info Version", 3) };
    unsafe { module_flag(module, LLVMModuleFlagBehaviorWarning, "wchar_size", 4) };

    unsafe { module_flag(module, LLVMModuleFlagBehaviorOverride, "PIE Level", 2) };
    unsafe { module_flag(module, LLVMModuleFlagBehaviorOverride, "PIC Level", 2) };

    unsafe { module_flag(module, LLVMModuleFlagBehaviorError, "uwtable", 2) };

    unsafe { module_flag(module, LLVMModuleFlagBehaviorWarning, "frame-pointer", 2) };
}
