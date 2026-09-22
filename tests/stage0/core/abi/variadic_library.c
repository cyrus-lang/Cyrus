#include <stdarg.h>

long sum_variadic(int count, ...) {
  va_list args;
  va_start(args, count);

  long total = 0;

  for (int i = 0; i < count; i++)
    total += va_arg(args, long);

  va_end(args);

  return total;
}