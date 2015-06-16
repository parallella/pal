#include <pal.h>

/**
 *
 * Calculates the hyperbolic cosine of the vector 'a'. Angles are specified
 * in radians.
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
void p_cosh_f32(const float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = coshf(*(a + i));
    }
}
