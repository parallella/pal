#include <pal.h>

/**
 *
 * Element wise vector multiplication between input vectors 'a' and 'b'
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

void p_mul_32f(float *a, float *b, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = *(a + i) * *(b + i);
    }
}
