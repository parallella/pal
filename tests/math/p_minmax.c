#include <math.h>
#include "check_scalar_and_index.h"

void generate_ref(PTYPE *outValue1, PTYPE *outValue2, int *outIndex1, int *outIndex2, size_t n)
{
    int i;

    *outIndex1 = 0;
    *outIndex2 = 0;
    *outValue1 = ai[0];
    *outValue2 = ai[0];

    for (i = 1; i < n; i++) {
        if (ai[i] < *outValue1)
        {
            *outIndex1 = i;
            *outValue1 = ai[i];
        }
        if (ai[i] > *outValue2)
        {
            *outIndex2 = i;
            *outValue2 = ai[i];
        }
    }
}
