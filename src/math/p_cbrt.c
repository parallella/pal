#include <pal.h>
#include <math.h>

/**
 *
 * Calculates the cube root of the input vector 'a'.
 *
 * @param a     Pointer to input vector
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
void p_cbrt_f32(const float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = cbrtf(*(a + i));
    }
}
