#include <pal.h>

/**
 *
 * Calculates the natural logarithm of 'a', (where the base is 'e'=2.71828)
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
void p_ln_f32(float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = logf(*(a + i));
    }
}
