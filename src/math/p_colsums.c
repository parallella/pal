#include <pal.h>

/**
 *
 * Calculates the sums vector 'c' of matrix 'a' columns.
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

void p_colsums_f32(const float *a, float *c, size_t m, size_t n)
{
    const float *pa;
    float *pc;
    size_t i;

    pa = a;
    pc = c;

    for (i = 0; i < m * n; ++i) {
        *(pc + i % n) += *(pa + i);
    }
}
