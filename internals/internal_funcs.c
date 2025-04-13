#include <stdio.h>
#include <stdarg.h>
#include "internal_string.h"

void internal_println(String v, ...) {
    va_list args;
    va_start(args, v);
    vprintf(v.data, args);
    va_end(args);
}