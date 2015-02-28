#include <pal.h>

/**
 *
 * Compute an element wise absolute difference of two vectors 'a' and 'b'.
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */

void p_absdiff_f32(float *a, float *b, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = *(a + i) - *(b + i);
        if (*(c + i) < 0) {
            *(c + i) = -*(c + i);
        }
    }
}
