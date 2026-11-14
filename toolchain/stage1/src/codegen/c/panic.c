// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#include "runtime.h"

#include <stdlib.h>
#include <unistd.h>

extern int pthread_getname_np(unsigned long, char *, unsigned long);
extern unsigned long pthread_self(void);

static void __cyrus_write_cstr(int fd, const char *s) {
  const char *p = s;

  while (*p) {
    p += 1;
  }

  (void)write(fd, s, (size_t)(p - s));
}

static void __cyrus_write_int(int fd, int value) {
  char buf[16];
  int i = 15;

  unsigned int u = (unsigned int)value;

  if (value < 0)
    u = (unsigned int)(-(value + 1)) + 1u;

  buf[i] = 0;

  do {
    i -= 1;
    buf[i] = (char)(48 + (u % 10));
    u /= 10;
  } while (u > 0);

  if (value < 0) {
    i -= 1;
    buf[i] = '-';
  }

  (void)write(fd, buf + i, (size_t)(15 - i));
}

void __cyrus_fail_(const char *kind, const char *prefix, const char *file,
                   int line, int col, const char *mesg) {
  char name[64];
  name[0] = 0;

  pthread_getname_np(pthread_self(), name, 64);
  
  __cyrus_write_cstr(2, "thread '");
  __cyrus_write_cstr(2, name);
  __cyrus_write_cstr(2, "' ");
  __cyrus_write_cstr(2, kind);
  __cyrus_write_cstr(2, " at ");
  __cyrus_write_cstr(2, file);
  __cyrus_write_cstr(2, ":");
  __cyrus_write_int(2, line);
  __cyrus_write_cstr(2, ":");
  __cyrus_write_int(2, col);
  __cyrus_write_cstr(2, "\n");
  __cyrus_write_cstr(2, prefix);
  __cyrus_write_cstr(2, mesg);
  __cyrus_write_cstr(2, "\n");

  abort();
}
