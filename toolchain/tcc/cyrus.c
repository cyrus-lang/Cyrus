// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#include "libtcc.h"
#include <stdint.h>
#include <stdio.h>

static void __cyrus_tcc_error(void *opaque, const char *message) {
  fprintf((FILE *)opaque, "%s\n", message);
}

/*
 * Compile a NUL-terminated C source string into an object file.
 *
 * output_path is the full destination filename (e.g. "./build/module.o").
 * Its parent directory must already exist.
 *
 * Returns:
 *   0  success
 *  -1  invalid argument
 *  -2  failed to create TCC state
 *  -3  failed to configure object output
 *  -4  compilation failed
 *  -5  failed to write object file
 * 
 */
extern int32_t __cyrus_tcc_compile_string(const char *source, const char *output_path,
                                   int output_kind) {
  TCCState *state;
  int32_t result = 0;

  if (!source || !output_path || !output_path[0])
    return -1;

  state = tcc_new();
  if (!state) {
    fprintf(stderr, "Could not create TCC state\n");
    return -2;
  }

  tcc_set_error_func(state, stderr, __cyrus_tcc_error);

  if (tcc_set_output_type(state, output_kind) < 0) {
    result = -3;
    goto cleanup;
  }

  if (tcc_compile_string(state, source) < 0) {
    result = -4;
    goto cleanup;
  }

  if (tcc_output_file(state, output_path) < 0) {
    result = -5;
    goto cleanup;
  }

cleanup:
  tcc_delete(state);
  return result;
}
