#include <pal.h>

#include "p_exp.h"

/**
 *
 * Calculate the exponent e^a where e is Euler's number (2.71...).
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
void PSYM(p_exp)(const PTYPE *a, PTYPE *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_exp(a[i]);
    }
}
