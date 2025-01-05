#include "headers/io.h"
#include <stdio.h>
#include <stdarg.h>

void cyrus_builtin__printf(const char* fmt)
{

    printf("%s\n", fmt);

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