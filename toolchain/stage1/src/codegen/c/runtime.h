// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#ifndef CYRUS_CODEGEN_C_RUNTIME_H
#define CYRUS_CODEGEN_C_RUNTIME_H

#include <stdint.h>

typedef struct {
    uint64_t lo;
    int64_t hi;
} __cyrus_int128;

typedef struct {
    uint64_t lo;
    uint64_t hi;
} __cyrus_uint128;

__cyrus_uint128 __cyrus_u128_from_u64(uint64_t lo);
__cyrus_uint128 __cyrus_u128_of_i128(__cyrus_int128 a);
__cyrus_int128 __cyrus_i128_from_i64(int64_t v);
__cyrus_int128 __cyrus_i128_of_u128(__cyrus_uint128 a);

uint64_t __cyrus_u128_to_u64(__cyrus_uint128 a);
uint64_t __cyrus_i128_to_u64(__cyrus_int128 a);

int __cyrus_u128_iszero(__cyrus_uint128 a);
int __cyrus_i128_iszero(__cyrus_int128 a);
int __cyrus_u128_cmp(__cyrus_uint128 a, __cyrus_uint128 b);
int __cyrus_i128_cmp(__cyrus_int128 a, __cyrus_int128 b);

__cyrus_uint128 __cyrus_u128_band(__cyrus_uint128 a, __cyrus_uint128 b);
__cyrus_uint128 __cyrus_u128_bxor(__cyrus_uint128 a, __cyrus_uint128 b);
__cyrus_uint128 __cyrus_u128_bor(__cyrus_uint128 a, __cyrus_uint128 b);
__cyrus_uint128 __cyrus_u128_add(__cyrus_uint128 a, __cyrus_uint128 b);
__cyrus_uint128 __cyrus_u128_sub(__cyrus_uint128 a, __cyrus_uint128 b);
__cyrus_uint128 __cyrus_u128_mul(__cyrus_uint128 a, __cyrus_uint128 b);
__cyrus_uint128 __cyrus_u128_div(__cyrus_uint128 a, __cyrus_uint128 b);
__cyrus_uint128 __cyrus_u128_mod(__cyrus_uint128 a, __cyrus_uint128 b);
__cyrus_int128 __cyrus_i128_band(__cyrus_int128 a, __cyrus_int128 b);
__cyrus_int128 __cyrus_i128_bxor(__cyrus_int128 a, __cyrus_int128 b);
__cyrus_int128 __cyrus_i128_add(__cyrus_int128 a, __cyrus_int128 b);
__cyrus_int128 __cyrus_i128_sub(__cyrus_int128 a, __cyrus_int128 b);
__cyrus_int128 __cyrus_i128_mul(__cyrus_int128 a, __cyrus_int128 b);
__cyrus_int128 __cyrus_i128_bor(__cyrus_int128 a, __cyrus_int128 b);
__cyrus_int128 __cyrus_i128_div(__cyrus_int128 a, __cyrus_int128 b);
__cyrus_int128 __cyrus_i128_mod(__cyrus_int128 a, __cyrus_int128 b);
__cyrus_uint128 __cyrus_u128_shl(__cyrus_uint128 a, uint64_t n_);
__cyrus_uint128 __cyrus_u128_shr(__cyrus_uint128 a, uint64_t n_);
__cyrus_int128 __cyrus_i128_shr(__cyrus_int128 a, uint64_t n_);
__cyrus_int128 __cyrus_i128_shl(__cyrus_int128 a, uint64_t n);
__cyrus_uint128 __cyrus_u128_bnot(__cyrus_uint128 a);
__cyrus_uint128 __cyrus_u128_neg(__cyrus_uint128 a);
__cyrus_int128 __cyrus_i128_bnot(__cyrus_int128 a);
__cyrus_int128 __cyrus_i128_neg(__cyrus_int128 a);

void __cyrus_fail_(
    const char *kind,
    const char *prefix,
    const char *file,
    int line,
    int col,
    const char *mesg
);

float __cyrus_f16_round(double d);

struct __cyrus_test_case {
    const char *name;
    const char *file;
    unsigned long line;
    void (*run)(void);
};

int __cyrus_run_tests(const struct __cyrus_test_case *test_cases,
                      unsigned long count);

#endif
