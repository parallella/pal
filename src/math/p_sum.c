#include <pal.h>

/**
 *
 * Calculates the sum of all elements vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output scalar
 *
 * @param n     Size of 'a' vector.
 *
 * @return      None
 *
 */

void PSYM(p_sum)(const PTYPE *a, PTYPE *c, int n)
{
    PTYPE tmp = PCONST(0.0);

    for (; n > 0; n--) {
        tmp += *(a++);
    }
    *c = tmp;
}
