#include <pal.h>

/**
 *
 * calculates the inverse hyperbolic sine of 'a'. The function does not check
 * for illegal input values.
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
void p_asinh_f32(float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = asinhf(*(a + i));
    }
}
