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

#![allow(unused)]

pub const DWARF_PRODUCER_NAME: &str = "cyrus";

pub const DW_TAG_ARRAY_TYPE: u16 = 0x01;
pub const DW_TAG_CLASS_TYPE: u16 = 0x02;
pub const DW_TAG_ENTRY_POINT: u16 = 0x03;
pub const DW_TAG_ENUMERATION_TYPE: u16 = 0x04;
pub const DW_TAG_FORMAL_PARAMETER: u16 = 0x05;
pub const DW_TAG_IMPORTED_DECLARATION: u16 = 0x08;
pub const DW_TAG_LABEL: u16 = 0x0a;
pub const DW_TAG_LEXICAL_BLOCK: u16 = 0x0b;
pub const DW_TAG_MEMBER: u16 = 0x0d;
pub const DW_TAG_POINTER_TYPE: u16 = 0x0f;
pub const DW_TAG_REFERENCE_TYPE: u16 = 0x10;
pub const DW_TAG_COMPILE_UNIT: u16 = 0x11;
pub const DW_TAG_STRING_TYPE: u16 = 0x12;
pub const DW_TAG_STRUCTURE_TYPE: u16 = 0x13;
pub const DW_TAG_SUBROUTINE_TYPE: u16 = 0x15;
pub const DW_TAG_TYPEDEF: u16 = 0x16;
pub const DW_TAG_UNION_TYPE: u16 = 0x17;
pub const DW_TAG_UNSPECIFIED_PARAMETERS: u16 = 0x18;
pub const DW_TAG_VARIANT: u16 = 0x19;
pub const DW_TAG_COMMON_BLOCK: u16 = 0x1a;
pub const DW_TAG_COMMON_INCLUSION: u16 = 0x1b;
pub const DW_TAG_INHERITANCE: u16 = 0x1c;
pub const DW_TAG_INLINED_SUBROUTINE: u16 = 0x1d;
pub const DW_TAG_MODULE: u16 = 0x1e;
pub const DW_TAG_PTR_TO_MEMBER_TYPE: u16 = 0x1f;
pub const DW_TAG_SET_TYPE: u16 = 0x20;
pub const DW_TAG_SUBRANGE_TYPE: u16 = 0x21;
pub const DW_TAG_WITH_STMT: u16 = 0x22;
pub const DW_TAG_ACCESS_DECLARATION: u16 = 0x23;
pub const DW_TAG_BASE_TYPE: u16 = 0x24;
pub const DW_TAG_CATCH_BLOCK: u16 = 0x25;
pub const DW_TAG_CONST_TYPE: u16 = 0x26;
pub const DW_TAG_CONSTANT: u16 = 0x27;
pub const DW_TAG_ENUMERATOR: u16 = 0x28;
pub const DW_TAG_FILE_TYPE: u16 = 0x29;
pub const DW_TAG_FRIEND: u16 = 0x2a;
pub const DW_TAG_NAMELIST: u16 = 0x2b;
pub const DW_TAG_NAMELIST_ITEM: u16 = 0x2c;
pub const DW_TAG_PACKED_TYPE: u16 = 0x2d;
pub const DW_TAG_SUBPROGRAM: u16 = 0x2e;
pub const DW_TAG_TEMPLATE_TYPE_PARAM: u16 = 0x2f;
pub const DW_TAG_TEMPLATE_VALUE_PARAM: u16 = 0x30;
pub const DW_TAG_THROWN_TYPE: u16 = 0x31;
pub const DW_TAG_TRY_BLOCK: u16 = 0x32;
pub const DW_TAG_VARIANT_PART: u16 = 0x33;
pub const DW_TAG_VARIABLE: u16 = 0x34;
pub const DW_TAG_VOLATILE_TYPE: u16 = 0x35;

// CHILDREN

pub const DW_CHILDREN_NO: u8 = 0;
pub const DW_CHILDREN_YES: u8 = 1;

// LANGUAGE

pub const DW_LANG_C89: u16 = 0x0001;

// ATTRIBUTES

pub const DW_AT_SIBLING: u16 = 0x01;
pub const DW_AT_LOCATION: u16 = 0x02;
pub const DW_AT_NAME: u16 = 0x03;
pub const DW_AT_ORDERING: u16 = 0x09;
pub const DW_AT_BYTE_SIZE: u16 = 0x0b;
pub const DW_AT_BIT_OFFSET: u16 = 0x0c;
pub const DW_AT_BIT_SIZE: u16 = 0x0d;
pub const DW_AT_STMT_LIST: u16 = 0x10;
pub const DW_AT_LOW_PC: u16 = 0x11;
pub const DW_AT_HIGH_PC: u16 = 0x12;
pub const DW_AT_LANGUAGE: u16 = 0x13;
pub const DW_AT_DISCR: u16 = 0x15;
pub const DW_AT_DISCR_VALUE: u16 = 0x16;
pub const DW_AT_VISIBILITY: u16 = 0x17;
pub const DW_AT_IMPORT: u16 = 0x18;
pub const DW_AT_STRING_LENGTH: u16 = 0x19;
pub const DW_AT_COMMON_REFERENCE: u16 = 0x1a;
pub const DW_AT_COMP_DIR: u16 = 0x1b;
pub const DW_AT_CONST_VALUE: u16 = 0x1c;
pub const DW_AT_CONTAINING_TYPE: u16 = 0x1d;
pub const DW_AT_DEFAULT_VALUE: u16 = 0x1e;
pub const DW_AT_INLINE: u16 = 0x20;
pub const DW_AT_IS_OPTIONAL: u16 = 0x21;
pub const DW_AT_LOWER_BOUND: u16 = 0x22;
pub const DW_AT_PRODUCER: u16 = 0x25;
pub const DW_AT_PROTOTYPED: u16 = 0x27;
pub const DW_AT_RETURN_ADDR: u16 = 0x2a;
pub const DW_AT_START_SCOPE: u16 = 0x2c;
pub const DW_AT_STRIDE_SIZE: u16 = 0x2e;
pub const DW_AT_UPPER_BOUND: u16 = 0x2f;
pub const DW_AT_ABSTRACT_ORIGIN: u16 = 0x31;
pub const DW_AT_ACCESSIBILITY: u16 = 0x32;
pub const DW_AT_ADDRESS_CLASS: u16 = 0x33;
pub const DW_AT_ARTIFICIAL: u16 = 0x34;
pub const DW_AT_BASE_TYPES: u16 = 0x35;
pub const DW_AT_CALLING_CONVENTION: u16 = 0x36;
pub const DW_AT_COUNT: u16 = 0x37;
pub const DW_AT_DATA_MEMBER_LOCATION: u16 = 0x38;
pub const DW_AT_DECL_COLUMN: u16 = 0x39;
pub const DW_AT_DECL_FILE: u16 = 0x3a;
pub const DW_AT_DECL_LINE: u16 = 0x3b;
pub const DW_AT_DECLARATION: u16 = 0x3c;
pub const DW_AT_DISCR_LIST: u16 = 0x3d;
pub const DW_AT_ENCODING: u16 = 0x3e;
pub const DW_AT_EXTERNAL: u16 = 0x3f;
pub const DW_AT_FRAME_BASE: u16 = 0x40;
pub const DW_AT_FRIEND: u16 = 0x41;
pub const DW_AT_IDENTIFIER_CASE: u16 = 0x42;
pub const DW_AT_MACRO_INFO: u16 = 0x43;
pub const DW_AT_NAMELIST_ITEM: u16 = 0x44;
pub const DW_AT_PRIORITY: u16 = 0x45;
pub const DW_AT_SEGMENT: u16 = 0x46;
pub const DW_AT_SPECIFICATION: u16 = 0x47;
pub const DW_AT_STATIC_LINK: u16 = 0x48;
pub const DW_AT_TYPE: u16 = 0x49;
pub const DW_AT_USE_LOCATION: u16 = 0x4a;
pub const DW_AT_VARIABLE_PARAMETER: u16 = 0x4b;
pub const DW_AT_VIRTUALITY: u16 = 0x4c;
pub const DW_AT_VTABLE_ELEM_LOCATION: u16 = 0x4d;

pub const DW_AT_LO_USER: u16 = 0x2000;
pub const DW_AT_HI_USER: u16 = 0x3fff;

pub const DW_AT_USER_BLOCK: u16 = 0x2650;
pub const DW_AT_USER_LEVEL: u16 = 0x2651;

//
// FORMS
//

pub const DW_FORM_ADDR: u16 = 0x01;
pub const DW_FORM_BLOCK2: u16 = 0x03;
pub const DW_FORM_BLOCK4: u16 = 0x04;
pub const DW_FORM_DATA2: u16 = 0x05;
pub const DW_FORM_DATA4: u16 = 0x06;
pub const DW_FORM_DATA8: u16 = 0x07;
pub const DW_FORM_STRING: u16 = 0x08;
pub const DW_FORM_BLOCK: u16 = 0x09;
pub const DW_FORM_BLOCK1: u16 = 0x0a;
pub const DW_FORM_DATA1: u16 = 0x0b;
pub const DW_FORM_FLAG: u16 = 0x0c;
pub const DW_FORM_SDATA: u16 = 0x0d;
pub const DW_FORM_STRP: u16 = 0x0e;
pub const DW_FORM_UDATA: u16 = 0x0f;
pub const DW_FORM_REF_ADDR: u16 = 0x10;
pub const DW_FORM_REF1: u16 = 0x11;
pub const DW_FORM_REF2: u16 = 0x12;
pub const DW_FORM_REF4: u16 = 0x13;
pub const DW_FORM_REF_UDATA: u16 = 0x15;
pub const DW_FORM_INDIRECT: u16 = 0x16;

// ENCODINGS

pub const DW_ATE_ADDRESS: u8 = 0x1;
pub const DW_ATE_BOOLEAN: u8 = 0x2;
pub const DW_ATE_COMPLEX_FLOAT: u8 = 0x3;
pub const DW_ATE_FLOAT: u8 = 0x4;
pub const DW_ATE_SIGNED: u8 = 0x5;
pub const DW_ATE_SIGNED_CHAR: u8 = 0x6;
pub const DW_ATE_UNSIGNED: u8 = 0x7;
pub const DW_ATE_UNSIGNED_CHAR: u8 = 0x8;

// CALLING CONVENTION

pub const DW_CC_NORMAL: u8 = 0x1;
pub const DW_CC_PROGRAM: u8 = 0x2;
pub const DW_CC_NOCALL: u8 = 0x3;
