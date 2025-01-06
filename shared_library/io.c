#include "headers/io.h"
#include <stdarg.h>
#include <stdio.h>

void cyrus_builtin__cprintf(int argc, ...)
{
    va_list args;
    va_start(args, argc);
    char *fmt = va_arg(args, char *);
    vprintf(fmt, args);
    va_end(args);
}