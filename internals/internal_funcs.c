#include "internal_string.h"
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void internal_println(String v, ...) {
  va_list args;
  va_start(args, v);
  vprintf(v.data, args);
  va_end(args);
  putchar('\n');
}

void internal_printf(String v, ...) {
  va_list args;
  va_start(args, v);
  vprintf(v.data, args);
  va_end(args);
}

void internal_eprintln(String v, ...) {
  va_list args;
  va_start(args, v);
  vfprintf(stderr, v.data, args);
  va_end(args);
  putchar('\n');
}

void internal_eprintf(String v, ...) {
  va_list args;
  va_start(args, v);
  vfprintf(stderr, v.data, args);
  va_end(args);
}

void internal_exit(int32_t status) { exit(status); }

void internal_panic(String file_name, int32_t line, int argc, ...) {
  fprintf(stderr, "explicit panic at %s:%d", (char *)file_name.data, line);

  if (argc > 0) {
    fprintf(stderr, "\n");
    va_list args;
    va_start(args, argc);
    for (int i = 0; i < argc; ++i) {
      String arg = va_arg(args, String);
      fprintf(stderr, "%s\n", (char *)arg.data);
    }
    va_end(args);
    exit(1);
  }
}

int32_t internal_len_string(String input) { return input.len; }

// TODO
// Not completed yet
void internal_copy(void *source, size_t source_len, size_t elem_len, void *dest,
                   size_t dest_len, size_t source_start_idx,
                   size_t dest_start_idx, size_t count) {}