#include "headers/io.h"
#include "headers/fmt.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

void cyrus_builtin__printf(int argc, ...)
{
    va_list args;
    va_start(args, argc);

    char *formatted_str = dynamic_format_string(args);

    printf("%s", formatted_str);

    free(formatted_str);

    va_end(args);
}