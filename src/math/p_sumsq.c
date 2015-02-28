#include <pal.h>

/**
 *
 * Calculates the sum of the square of all of the elements vector 'a'.
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

void p_sumsq_f32(float *a, float *c, int n)
{

    int i = 0;
    *c = 0.0f;
    for (i = 0; i < n; i++) {
        *c += *(a + i) * *(a + i);
    }
}
