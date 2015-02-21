/**
 *
 * Calculates the inverse cube root of the input vector 'a'.
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
void p_invcbrt_32f(float *a, float *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = 1.0f / cbrtf(*(a + i));
    }
}
