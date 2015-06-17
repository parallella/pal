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

void p_absdiff_f32(const float *a, const float *b, float *c, int n)
{
    union {
        float f;
        uint32_t u;
    } diff;
    for (int i = 0; i < n; i++) {
        diff.f = a[i] - b[i];
        diff.u &= 0x7FFFFFFF;
        c[i] = diff.f;
    }
}
