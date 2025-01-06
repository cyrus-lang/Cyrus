#include "headers/fmt.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

char *dynamic_format_string(va_list args)
{
    const char *format = va_arg(args, const char *);

    char buff[1024] = "";
    int buff_len = 0;

    int i = 0;
    while (format[i] != '\0')
    {
        if (format[i] == '{')
        {
            char placeholder[10] = {0};
            int j = 0;
            while (format[i] != '}' && format[i] != '\0')
            {
                placeholder[j++] = format[i++];
            }
            if (format[i] == '}')
            {
                placeholder[j++] = format[i++];
            }
            placeholder[j] = '\0';

            if (strcmp(placeholder, "{}") == 0)
            {
                const char *str_arg = va_arg(args, const char *);
                buff_len += snprintf(buff + buff_len, sizeof(buff) - buff_len, "%s", str_arg);
            }
            else if (strcmp(placeholder, "{d}") == 0)
            {
                int int_arg = va_arg(args, int);
                buff_len += snprintf(buff + buff_len, sizeof(buff) - buff_len, "%d", int_arg);
            }
            else if (strncmp(placeholder, "{f:", 3) == 0)
            {
                int prec = 0;
                sscanf(placeholder + 3, "%d}", &prec);
                double double_arg = va_arg(args, double);
                buff_len += snprintf(buff + buff_len, sizeof(buff) - buff_len, "%.*f", prec, double_arg);
            }
            else if (strcmp(placeholder, "{c}") == 0)
            {
                char char_arg = (char)va_arg(args, int);
                buff_len += snprintf(buff + buff_len, sizeof(buff) - buff_len, "%c", char_arg);
            }
            else
            {
                buff_len += snprintf(buff + buff_len, sizeof(buff) - buff_len, "%s", placeholder);
            }
        }
        else
        {
            buff_len += snprintf(buff + buff_len, sizeof(buff) - buff_len, "%c", format[i++]);
        }
        if (buff_len >= sizeof(buff))
        {
            fprintf(stderr, "Buffer Overflow!\n");
            return NULL;
        }
    }

    char *buff_alloc = (char *)malloc(buff_len + 1);
    strcpy(buff_alloc, buff);

    return buff_alloc;
}

char *cyrus_builtin__format(int argc, ...)
{
    va_list args;
    va_start(args, argc);

    char *fmt = dynamic_format_string(args);
    if (fmt == NULL)
    {
        printf("allocation in cyrus_builtin__format failed.");
        exit(1);
    }

    return fmt;
}