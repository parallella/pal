#include <pal.h>

/**
 *
 * Calculates the means vector 'c' of matrix 'a' columns.
 *
 * @param a     Pointer to input matrix
 *
 * @param c     Pointer to output vector
 *
 * @param m     Number of matrix rows
 *
 * @param n     Number of matrix columns and size of output vector
 *
 * @return      None
 *
 */

void p_colmeans_f32(const float *a, float *c, size_t m, size_t n)
{
    const float *pa;
    float *pc;
    size_t i;

    pa = a;
    pc = c;

    p_colsums_f32(pa, pc, m, n);

    for (i = 0; i < n; ++i) {
        *(pc + i) /= n;
    }
}
