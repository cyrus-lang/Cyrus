// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#include "runtime.h"

__cyrus_uint128 __cyrus_u128_from_u64(uint64_t lo) {
  __cyrus_uint128 r;
  r.lo = lo;
  r.hi = 0;
  return r;
}

__cyrus_uint128 __cyrus_u128_of_i128(__cyrus_int128 a) {
  __cyrus_uint128 r;
  r.lo = a.lo;
  r.hi = (uint64_t)a.hi;
  return r;
}

__cyrus_int128 __cyrus_i128_from_i64(int64_t v) {
  __cyrus_int128 r;
  r.lo = (uint64_t)v;
  r.hi = v < 0 ? -1 : 0;
  return r;
}

__cyrus_int128 __cyrus_i128_of_u128(__cyrus_uint128 a) {
  __cyrus_int128 r;
  r.lo = a.lo;
  r.hi = (int64_t)a.hi;
  return r;
}

uint64_t __cyrus_u128_to_u64(__cyrus_uint128 a) { return a.lo; }

uint64_t __cyrus_i128_to_u64(__cyrus_int128 a) { return a.lo; }

int __cyrus_u128_iszero(__cyrus_uint128 a) { return a.lo == 0 && a.hi == 0; }

int __cyrus_i128_iszero(__cyrus_int128 a) { return a.lo == 0 && a.hi == 0; }

int __cyrus_u128_cmp(__cyrus_uint128 a, __cyrus_uint128 b) {
  if (a.hi != b.hi)
    return (a.hi < b.hi) ? -1 : 1;

  if (a.lo != b.lo)
    return (a.lo < b.lo) ? -1 : 1;

  return 0;
}

int __cyrus_i128_cmp(__cyrus_int128 a, __cyrus_int128 b) {
  if (a.hi != b.hi)
    return (a.hi < b.hi) ? -1 : 1;

  if (a.lo != b.lo)
    return (a.lo < b.lo) ? -1 : 1;

  return 0;
}

__cyrus_uint128 __cyrus_u128_add(__cyrus_uint128 a, __cyrus_uint128 b) {
  __cyrus_uint128 r;
  r.lo = a.lo + b.lo;
  r.hi = a.hi + b.hi + (uint64_t)(r.lo < a.lo);
  return r;
}

__cyrus_uint128 __cyrus_u128_sub(__cyrus_uint128 a, __cyrus_uint128 b) {
  __cyrus_uint128 r;
  r.lo = a.lo - b.lo;
  r.hi = a.hi - b.hi - (uint64_t)(a.lo < b.lo);
  return r;
}

__cyrus_uint128 __cyrus_u128_band(__cyrus_uint128 a, __cyrus_uint128 b) {
  __cyrus_uint128 r;
  r.lo = a.lo & b.lo;
  r.hi = a.hi & b.hi;
  return r;
}

__cyrus_uint128 __cyrus_u128_bor(__cyrus_uint128 a, __cyrus_uint128 b) {
  __cyrus_uint128 r;
  r.lo = a.lo | b.lo;
  r.hi = a.hi | b.hi;
  return r;
}

__cyrus_uint128 __cyrus_u128_bxor(__cyrus_uint128 a, __cyrus_uint128 b) {
  __cyrus_uint128 r;
  r.lo = a.lo ^ b.lo;
  r.hi = a.hi ^ b.hi;
  return r;
}

__cyrus_uint128 __cyrus_u128_bnot(__cyrus_uint128 a) {
  __cyrus_uint128 r;
  r.lo = ~a.lo;
  r.hi = ~a.hi;
  return r;
}

__cyrus_uint128 __cyrus_u128_neg(__cyrus_uint128 a) {
  return __cyrus_u128_sub(__cyrus_u128_from_u64(0), a);
}

__cyrus_uint128 __cyrus_u128_shl(__cyrus_uint128 a, uint64_t n_) {
  __cyrus_uint128 r;

  uint64_t n = n_ & 127;

  if (n == 0)
    return a;

  if (n >= 64) {
    r.lo = 0;
    r.hi = a.lo << (n - 64);
    return r;
  }

  r.lo = a.lo << n;
  r.hi = (a.hi << n) | (a.lo >> (64 - n));

  return r;
}

__cyrus_uint128 __cyrus_u128_shr(__cyrus_uint128 a, uint64_t n_) {
  __cyrus_uint128 r;

  uint64_t n = n_ & 127;

  if (n == 0)
    return a;

  if (n >= 64) {
    r.lo = a.hi >> (n - 64);
    r.hi = 0;
    return r;
  }

  r.lo = (a.lo >> n) | (a.hi << (64 - n));
  r.hi = a.hi >> n;

  return r;
}

__cyrus_int128 __cyrus_i128_shr(__cyrus_int128 a, uint64_t n_) {
  __cyrus_int128 r;

  uint64_t n = n_ & 127;

  if (n == 0)
    return a;

  if (n >= 64) {
    r.lo = (uint64_t)(a.hi >> (n - 64));
    r.hi = a.hi < 0 ? -1 : 0;
    return r;
  }

  r.lo = (a.lo >> n) | ((uint64_t)a.hi << (64 - n));
  r.hi = a.hi >> n;

  return r;
}

static int __cyrus_u128_bit(__cyrus_uint128 a, uint64_t i) {
  if (i >= 64)
    return (int)((a.hi >> (i - 64)) & 1);
  else
    return (int)((a.lo >> i) & 1);
}

static void __cyrus_u128_setbit(__cyrus_uint128 *a, uint64_t i) {
  if (i >= 64)
    a->hi |= (uint64_t)1 << (i - 64);
  else
    a->lo |= (uint64_t)1 << i;
}

__cyrus_uint128 __cyrus_u128_mul(__cyrus_uint128 a, __cyrus_uint128 b) {
  __cyrus_uint128 r;

  r.lo = 0;
  r.hi = 0;

  while (a.lo != 0 || a.hi != 0) {
    if (a.lo & 1) {
      r = __cyrus_u128_add(r, b);
    }

    b = __cyrus_u128_shl(b, 1);
    a = __cyrus_u128_shr(a, 1);
  }

  return r;
}

__cyrus_uint128 __cyrus_u128_div(__cyrus_uint128 a, __cyrus_uint128 b) {
  __cyrus_uint128 q;
  __cyrus_uint128 rem;

  q.lo = 0;
  q.hi = 0;
  rem.lo = 0;
  rem.hi = 0;

  if (b.lo == 0 && b.hi == 0)
    return q;

  for (int i = 127; i >= 0; i -= 1) {
    rem = __cyrus_u128_shl(rem, 1);
    if (__cyrus_u128_bit(a, (uint64_t)i)) {
      rem.lo |= 1;
    }
    if (__cyrus_u128_cmp(rem, b) >= 0) {
      rem = __cyrus_u128_sub(rem, b);
      __cyrus_u128_setbit(&q, (uint64_t)i);
    }
  }

  return q;
}

__cyrus_uint128 __cyrus_u128_mod(__cyrus_uint128 a, __cyrus_uint128 b) {
  if (b.lo == 0 && b.hi == 0)
    return a;

  return __cyrus_u128_sub(a, __cyrus_u128_mul(__cyrus_u128_div(a, b), b));
}

__cyrus_int128 __cyrus_i128_add(__cyrus_int128 a, __cyrus_int128 b) {
  return __cyrus_i128_of_u128(
      __cyrus_u128_add(__cyrus_u128_of_i128(a), __cyrus_u128_of_i128(b)));
}

__cyrus_int128 __cyrus_i128_sub(__cyrus_int128 a, __cyrus_int128 b) {
  return __cyrus_i128_of_u128(
      __cyrus_u128_sub(__cyrus_u128_of_i128(a), __cyrus_u128_of_i128(b)));
}

__cyrus_int128 __cyrus_i128_mul(__cyrus_int128 a, __cyrus_int128 b) {
  return __cyrus_i128_of_u128(
      __cyrus_u128_mul(__cyrus_u128_of_i128(a), __cyrus_u128_of_i128(b)));
}

__cyrus_int128 __cyrus_i128_shl(__cyrus_int128 a, uint64_t n) {
  return __cyrus_i128_of_u128(__cyrus_u128_shl(__cyrus_u128_of_i128(a), n));
}

__cyrus_int128 __cyrus_i128_band(__cyrus_int128 a, __cyrus_int128 b) {
  return __cyrus_i128_of_u128(
      __cyrus_u128_band(__cyrus_u128_of_i128(a), __cyrus_u128_of_i128(b)));
}

__cyrus_int128 __cyrus_i128_bor(__cyrus_int128 a, __cyrus_int128 b) {
  return __cyrus_i128_of_u128(
      __cyrus_u128_bor(__cyrus_u128_of_i128(a), __cyrus_u128_of_i128(b)));
}

__cyrus_int128 __cyrus_i128_bxor(__cyrus_int128 a, __cyrus_int128 b) {
  return __cyrus_i128_of_u128(
      __cyrus_u128_bxor(__cyrus_u128_of_i128(a), __cyrus_u128_of_i128(b)));
}

__cyrus_int128 __cyrus_i128_bnot(__cyrus_int128 a) {
  return __cyrus_i128_of_u128(__cyrus_u128_bnot(__cyrus_u128_of_i128(a)));
}

__cyrus_int128 __cyrus_i128_neg(__cyrus_int128 a) {
  return __cyrus_i128_of_u128(__cyrus_u128_neg(__cyrus_u128_of_i128(a)));
}

__cyrus_int128 __cyrus_i128_div(__cyrus_int128 a, __cyrus_int128 b) {
  int neg = (a.hi < 0) != (b.hi < 0);

  __cyrus_uint128 ua = __cyrus_u128_of_i128(a);
  __cyrus_uint128 ub = __cyrus_u128_of_i128(b);

  if (a.hi < 0)
    ua = __cyrus_u128_of_i128(__cyrus_i128_neg(a));

  if (b.hi < 0)
    ub = __cyrus_u128_of_i128(__cyrus_i128_neg(b));

  __cyrus_int128 q = __cyrus_i128_of_u128(__cyrus_u128_div(ua, ub));
  return neg ? __cyrus_i128_neg(q) : q;
}

__cyrus_int128 __cyrus_i128_mod(__cyrus_int128 a, __cyrus_int128 b) {
  if (b.hi == 0 && b.lo == 0)
    return a;

  return __cyrus_i128_sub(a, __cyrus_i128_mul(__cyrus_i128_div(a, b), b));
}
