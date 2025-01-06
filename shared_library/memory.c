#include "headers/memory.h"
#include <stdio.h>
#include <stdlib.h>

void cyrus_builtin__free_object(void *obj) {
    printf("object freed\n");
    free(obj);
}