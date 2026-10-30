// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#include "libtcc.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void __cyrus_tcc_error(void *opaque, const char *message) {
  fprintf((FILE *)opaque, "%s\n", message);
}

#define CYRUS_TCC_ADD_FILE 0
#define CYRUS_TCC_ADD_LIB_PATH 1
#define CYRUS_TCC_ADD_LIBRARY 2

static int __cyrus_tcc_add_list(TCCState *state, const char *list, int kind) {
  const char *item = list;

  if (!list || !*list)
    return 0;

  while (*item) {
    const char *end = strchr(item, '\n');
    size_t len = end ? (size_t)(end - item) : strlen(item);

    if (len != 0) {
      char *copy = malloc(len + 1);
      int result;

      if (!copy)
        return -1;

      memcpy(copy, item, len);
      copy[len] = '\0';

      switch (kind) {
      case CYRUS_TCC_ADD_FILE:
        result = tcc_add_file(state, copy);
        break;
      case CYRUS_TCC_ADD_LIB_PATH:
        result = tcc_add_library_path(state, copy);
        break;
      default:
        result = tcc_add_library(state, copy);
        break;
      }

      free(copy);

      if (result < 0)
        return result;
    }

    if (!end)
      break;

    item = end + 1;
  }

  return 0;
}

/*
 * Compile a null-terminated C source string into an object file.
 *
 * output_path is the full destination filename (e.g. "./build/module.o").
 * Its parent directory must already exist.
 *
 * extra_files, lib_paths and libraries are null-terminated, newline
 * separated lists of additional link inputs (tcc_add_file), library
 * search paths (tcc_add_library_path) and libraries (tcc_add_library).
 * Empty strings mean "none".
 *
 * Returns:
 *   0  success
 *  -1  invalid argument
 *  -2  failed to create TCC state
 *  -3  failed to configure object output
 *  -4  compilation failed
 *  -5  failed to write object file
 *  -6  failed to add an extra input file
 *  -7  failed to add a library search path
 *  -8  failed to add a library
 */
extern int32_t
__cyrus_tcc_compile_string(const char *source, const char *output_path,
                           int output_kind, const char *extra_files,
                           const char *lib_paths, const char *libraries) {
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

  /* Search paths have to be registered before any lookup through them. */
  if (__cyrus_tcc_add_list(state, lib_paths, CYRUS_TCC_ADD_LIB_PATH) < 0) {
    result = -7;
    goto cleanup;
  }

  if (__cyrus_tcc_add_list(state, extra_files, CYRUS_TCC_ADD_FILE) < 0) {
    result = -6;
    goto cleanup;
  }

  if (tcc_compile_string(state, source) < 0) {
    result = -4;
    goto cleanup;
  }

  if (__cyrus_tcc_add_list(state, libraries, CYRUS_TCC_ADD_LIBRARY) < 0) {
    result = -8;
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
