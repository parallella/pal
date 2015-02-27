/**
 *
 * Calculates the sum of the square of all of the elements vector 'a'.
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

void p_sumsq_32f(float *a, float *c, int n)
{

    int i = 0;
    *(c + i) = 0.0f;
    for (i = 0; i < n; i++) {
        *(c + i) += *(a + i) * *(a + i);
    }
}
