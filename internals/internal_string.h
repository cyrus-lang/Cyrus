#ifndef INTERNAL_STRING_H
#define INTERNAL_STRING_H
#include <stdint.h>

typedef struct String
{
    void *data;
    int64_t len;
} String;

#endif //INTERNAL_STRING_H
