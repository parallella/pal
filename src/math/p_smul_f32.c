#include<pal.h>

/**
 *
 * Scalar multiplication
 *
 * @param a     Pointer to input matrix
 *
 * @param b     input scalar
 *
 * @param c     Pointer to output matrix
 *
 * @param n1    Number of lines of the matrix
 *
 * @param n2    Number of columns of the matrix
 *
 * @return      none
 *
 */

void p_smul_f32(const float *a, float b, float *c, int n1, int n2)
{
    int i = 0;
    for (; i < n1*n2; i++)
        *(c + i) = *(a + i)*b;
}
