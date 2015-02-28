#include <pal.h>

/**
 *
 * Calculates the mean of input vector 'a'.
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
void p_mean_f32(float *a, float *c, int n)
{

    int i = 0;
    *(c + i) = 0.0f;
    for (i = 0; i < n; i++) {
        *(c + i) += *(a + i);
    }
    *(c + i) = *(c + i) / n;
}
