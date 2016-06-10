#include <math.h>
#include "check_scalar_and_index.h"

void generate_ref(PTYPE *outValue, int* outIndex, size_t n)
{
    int i;

    *outIndex = 0;
    *outValue = ai[0];

    for (i = 1; i < n; i++) {
        if (ai[i] > *outValue)
        {
            *outIndex = i;
            *outValue = ai[i];
        }
    }
}
