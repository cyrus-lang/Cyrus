#include "headers/io.h"
#include <stdio.h>
#include <stdarg.h>

void cyrus_builtin__printf(int argc, ...)
{

    va_list args;
    va_start(args, argc);

    for (int i = 0; i < argc; i++)
    {
        const char *str = va_arg(args, const char *);

        printf("Arg(str): %s\n", str);
    }

    printf("Arg counts: %d\n", argc);

    // vprintf(fmt, args);

    // const char *fmt = (const char *)values[0];
    // int fmt = *(int *) values[0];

    // for (int i = 0; i < count; i++)
    // {
    //     switch (types[i])
    //     {
    //     case TYPE_INT:
    //     {
    //         int arg_int = va_arg(args, int); // Retrieve as int
    //         printf("number: %d\n", arg_int);
    //         break;
    //     }
    //     case TYPE_DOUBLE:
    //     {
    //         double arg_double = va_arg(args, double); // Retrieve as double
    //         printf("double: %f\n", arg_double);
    //         break;
    //     }
    //     case TYPE_STRING:
    //     {
    //         char *arg_string = va_arg(args, char *); // Retrieve as char*
    //         printf("string: %s\n", arg_string);
    //         break;
    //     }
    //     default:
    //         printf("unknown\n");
    //         break;
    //     }
    // }
}