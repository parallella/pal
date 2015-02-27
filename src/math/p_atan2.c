#include <pal.h>

/**
 *
 * Calculates the inverse tangent (arc tangent) of b/a. Stability not guaranteed
 * for 'a' values near 0. Results are in the range of −pi to pi.
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
#include <math.h>
void p_atan2_32f(float *a, float *b, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = atan2f(*(b + i), *(a + i));
    }
}
