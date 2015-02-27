#include <pal.h>

/**
 *
 * Calculates the hyperbolic tangent of the input vector 'a'.
 * Angles are specified in radians.
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
void p_tanh_32f(float *a, float *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = tanhf(*(a + i));
    }
}
