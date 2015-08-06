#include <pal.h>
#include <math.h>

/**
 *
 * Calculates inverse tangent (arc tangent) of the input value. The function
 * does not check for illegal input values.
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
void p_atanh_f32(const float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = atanhf(*(a + i));
    }
}
