// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#include "runtime.h"

// Because TCC has no half type, we implement `float16` as a C `float`
// and round to IEEE-754 binary16 at every conversion boundary.
//
// This ensures that the semantics of `float16` are
// preserved even though the underlying
// representation is a `float`.

float __cyrus_f16_round(double d) {
  union {
    float f;
    unsigned int u;
  } x;

  unsigned int sign, sig, q, rem, hlf, mant;
  int e, exp, sh;

  x.f = (float)d;
  sign = x.u & 0x80000000u;
  exp = (int)((x.u >> 23) & 0xffu);
  mant = x.u & 0x7fffffu;

  if (exp == 0xff)
    return x.f;

  if (exp == 0)
    return sign ? -0.0f : 0.0f;

  e = exp - 127;

  if (e > 15) {
    x.u = sign | 0x7f800000u;
    return x.f;
  }

  sig = (1u << 23) | mant;

  if (e < -14) {
    sh = -e - 1;

    if (sh >= 32)
      return sign ? -0.0f : 0.0f;

    q = sig >> sh;
    rem = sig & ((1u << sh) - 1u);
    hlf = 1u << (sh - 1);

    if (rem > hlf || (rem == hlf && (q & 1u)))
      q += 1u;

    if (q == 0)
      return sign ? -0.0f : 0.0f;

    {
      int qe = 31;

      while ((q & (1u << qe)) == 0)
        qe -= 1;

      x.u = sign | ((unsigned int)(qe - 24 + 127) << 23) |
            ((q & ((1u << qe) - 1u)) << (23 - qe));

      return x.f;
    }
  }

  q = sig >> 13;
  rem = sig & 0x1fffu;
  hlf = 0x1000u;

  if (rem > hlf || (rem == hlf && (q & 1u))) {
    q += 1u;
    
    if (q == (1u << 11)) {
      q >>= 1;
      e += 1;
      
      if (e > 15) {
        x.u = sign | 0x7f800000u;
        return x.f;
      }
    }
  }

  x.u = sign | ((unsigned int)(e + 127) << 23) | ((q & 0x3ffu) << 13);

  return x.f;
}
