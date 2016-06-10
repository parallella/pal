#include <pal.h>

#include "p_asin.h"

/**
 *
 * Computes the inverse cosine (arc cosine) of the input vector 'a'. Input
 * values to acos must be in the range -1 to 1. The result values are in the
 * range 0 to pi. The function does not check for illegal input values.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */
void PSYM(p_acos)(const PTYPE *a, PTYPE *c, int n)
{

    int i;
    /* acos x = pi/2 - asin x */
    for (i = 0; i < n; i++) {
        c[i] = pi_2 - _p_asin(a[i]);
    }
}
