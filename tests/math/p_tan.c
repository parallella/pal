#include <math.h>
#include "simple.h"

void generate_ref(PTYPE *out, size_t n)
{
    size_t i;

    for (i = 0; i < n; i++)
        out[i] = tanf(ai[i]);
}
