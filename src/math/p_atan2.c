#include <pal.h>
#include <math.h>

/**
 *
 * Calculates the inverse tangent (arc tangent) of b/a. Stability not guaranteed
 * for 'a' values near 0. Results are in the range of -pi to pi.
 *
 * @param a     Pointer to denominator input vector
 *
 * @param b     Pointer to numerator input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 * @todo        Implement without using libm
 *
 */
void PSYM(p_atan2)(const PTYPE *a, const PTYPE *b, PTYPE *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = atan2f(*(b + i), *(a + i));
    }
}
