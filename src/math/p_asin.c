#include <pal.h>

#include "p_asin.h"

/**
 *
 * Caclulates the inverse sine (arc sine) of the argument 'a'. Arguments must be
 * in the range -1 to 1. The function does not check for illegal input values.
 * Results are in the range -pi/2 to pi/2.
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
void PSYM(p_asin)(const PTYPE *a, PTYPE *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_asin(a[i]);
    }
}
