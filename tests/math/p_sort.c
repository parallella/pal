#include <stdlib.h>
#include <string.h>
#include "simple.h"

/* Override simple.c's compare function. Here we want exact matches */
bool compare(PTYPE x, PTYPE y)
{
    return x == y;
}

/* qsort compare function */
static int comp(const void * a, const void * b)
{
    if (*(PTYPE*)a > *(PTYPE*)b) {
        return 1;
    }
    if (*(PTYPE*)a < *(PTYPE*)b) {
        return -1;
    }
    return 0;
}

void generate_ref(PTYPE *out, size_t n)
{
    memcpy(out, ai, n*sizeof(PTYPE));
    qsort(out, n, sizeof(PTYPE), comp);
}
