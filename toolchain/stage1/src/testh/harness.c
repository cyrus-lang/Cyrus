// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#include "../codegen/c/runtime.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

static int test_color_enabled(void) {
  if (getenv("NO_COLOR") != NULL) {
    return 0;
  }

  const char *force = getenv("FORCE_COLOR");
  if (force != NULL) {
    return strcmp(force, "0") != 0;
  }

  const char *term = getenv("TERM");
  return isatty(STDOUT_FILENO) && (term == NULL || strcmp(term, "dumb") != 0);
}

static double test_elapsed_ms(const struct timespec *start) {
  struct timespec end = {0, 0};
  
  if (clock_gettime(CLOCK_MONOTONIC, &end) != 0) {
    return 0;
  }

  double elapsed = (double)(end.tv_sec - start->tv_sec) * 1000.0 +
                   (double)(end.tv_nsec - start->tv_nsec) / 1000000.0;

  return elapsed < 0 ? 0 : elapsed;
}

static void test_print_text(const char *text) {
  const unsigned char *cursor = (const unsigned char *)text;
  
  while (*cursor != 0) {
    unsigned char character = *cursor++;

    if (character == '\n') {
      fputs("\\n", stdout);
    } else if (character == '\r') {
      fputs("\\r", stdout);
    } else if (character == '\t') {
      fputs("\\t", stdout);
    } else if (character < 32 || character == 127) {
      printf("\\x%02x", (unsigned int)character);
    } else {
      putchar(character);
    }
  }
}

static void test_print_output(FILE *output) {
  rewind(output);
  
  int character;
  int line_start = 1;

  while ((character = fgetc(output)) != EOF) {
    if (line_start) {
      fputs("          | ", stdout);
    }
    putchar(character);
    line_start = character == '\n';
  }

  if (!line_start) {
    putchar('\n');
  }
}

static void test_print_result(const struct __cyrus_test_case *test_case,
                              int passed, double elapsed, int color) {
  fputs("    ", stdout);
  
  if (color) {
    fputs(passed ? "\033[1;32m" : "\033[1;31m", stdout);
  }

  fputs(passed ? "PASS" : "FAIL", stdout);
  
  if (color) {
    fputs("\033[0m", stdout);
  }
  
  fputs("  ", stdout);
  test_print_text(test_case->name);
  
  if (color) {
    fputs("\033[2m", stdout);
  }
  
  printf(" (%.1f ms)", elapsed);
  
  if (color) {
    fputs("\033[0m", stdout);
  }
  
  putchar('\n');
  
  if (!passed) {
    fputs("          at ", stdout);
    test_print_text(test_case->file);
    printf(":%lu\n", test_case->line);
  }
}

static int test_run_case(const struct __cyrus_test_case *test_case, int color) {
  struct timespec start = {0, 0};

  (void)clock_gettime(CLOCK_MONOTONIC, &start);
  fflush(NULL);

  FILE *output = tmpfile();

  if (output == NULL) {
    int saved_error = errno;
    test_print_result(test_case, 0, test_elapsed_ms(&start), color);
    printf("          cannot capture test output: %s\n", strerror(saved_error));
    return 0;
  }

  pid_t child = fork();

  if (child == 0) {
    const struct rlimit core_limit = {0, 0};
    
    (void)setrlimit(RLIMIT_CORE, &core_limit);
    
    if (dup2(fileno(output), STDOUT_FILENO) < 0 ||
        dup2(fileno(output), STDERR_FILENO) < 0) {
      _exit(125);
    }
    
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
    test_case->run();
    fflush(NULL);
    _exit(0);
  }

  if (child < 0) {
    int saved_error = errno;
    test_print_result(test_case, 0, test_elapsed_ms(&start), color);
    printf("          cannot start test process: %s\n", strerror(saved_error));
    fclose(output);
    return 0;
  }

  int status = 0;
  pid_t waited;

  do {
    waited = waitpid(child, &status, 0);
  } while (waited < 0 && errno == EINTR);

  int saved_error = errno;
  int passed = waited == child && WIFEXITED(status) && WEXITSTATUS(status) == 0;
  test_print_result(test_case, passed, test_elapsed_ms(&start), color);

  if (waited < 0) {
    printf("          cannot wait for test process: %s\n", strerror(saved_error));
  } else if (WIFSIGNALED(status)) {
    int signal_number = WTERMSIG(status);
    const char *description = strsignal(signal_number);
    printf("          terminated by signal %d (%s)\n", signal_number,
           description != NULL ? description : "unknown signal");
  } else if (!passed) {
    printf("          exited with code %d\n", WEXITSTATUS(status));
  }

  test_print_output(output);
  fclose(output);
  fflush(stdout);
  return passed;
}

int __cyrus_run_tests(const struct __cyrus_test_case *test_cases,
                      unsigned long count) {
  int color = test_color_enabled();
  struct timespec start = {0, 0};
  (void)clock_gettime(CLOCK_MONOTONIC, &start);

  if (color) {
    fputs("\033[1m", stdout);
  }

  printf("Running %lu test%s", count, count == 1 ? "" : "s");
  
  if (color) {
    fputs("\033[0m", stdout);
  }
  
  fputs("\n", stdout);

  unsigned long passed = 0;
  const char *previous_file = NULL;

  for (unsigned long test_index = 0; test_index < count; test_index += 1) {
    const struct __cyrus_test_case *test_case = &test_cases[test_index];
    
    if (previous_file == NULL || strcmp(previous_file, test_case->file) != 0) {
      fputs("\n  ", stdout);
      
      if (color) {
        fputs("\033[1m", stdout);
      }
      
      test_print_text(test_case->file);
      
      if (color) {
        fputs("\033[0m", stdout);
      }
      
      putchar('\n');
      previous_file = test_case->file;
    }

    passed += (unsigned long)test_run_case(test_case, color);
  }

  unsigned long failed = count - passed;
  fputs("\nTests  ", stdout);

  if (color) {
    fputs("\033[1;32m", stdout);
  }
  
  printf("%lu passed", passed);
  
  if (color) {
    fputs("\033[0m", stdout);
  }
  
  fputs(", ", stdout);
  
  if (color && failed != 0) {
    fputs("\033[1;31m", stdout);
  }
  
  printf("%lu failed", failed);
  
  if (color && failed != 0) {
    fputs("\033[0m", stdout);
  }
  
  printf(", %lu total\nTime   %.1f ms\n", count, test_elapsed_ms(&start));
  return failed != 0;
}
