#include<pal.h>

/**
 *
 * Multiplies two matrixes
 *
 * @param a     Pointer to input matrix
 *
 * @param b     Pointer to input matrix
 *
 * @param c     Pointer to output matrix
 *
 * @param n1    Number of lines of the first matrix
 *
 * @param n2    Number of columns of the first matrix and lines of the second matrix
 *
 * @param n3    Number of columns of the second matrix
 *
 * @return      none
 *
 */

void p_matrix_mul_f32(const float *a, const float *b, float *c, int n1, int n2, int n3)
{
    int i = 0;
    int j = 0;
    int k = 0;
    int ct = 0;
    int temp = 0;
    for (; i < n1; i++) {
        for (; j < n3; j++,ct++) {
            temp = 0;
            for (; k < n2; k++) {
                temp += *(a + k + (i*n2))*(*(b + j + (k*n3)));
                *(c + ct) = temp;
            }
        }
    }
    return 0;
}
 
