#include <pal.h>

/**
 *
 * Element wise vector addition between input vectors 'a' and 'b'
 *
 * @param a     Pointer to first input vector
 *
 * @param b     Pointer to second input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */

void p_add_f32(const float *a, const float *b, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = *(a + i) + *(b + i);
    }
}
