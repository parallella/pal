#include <pal.h>

/**
 *
 * Calculates the dot product between vectors 'a' and 'b', producing
 * a scalar result 'c'.
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input vector
 *
 * @param c     Pointer to output scalar
 *
 * @param n     Size of 'a', 'b'
 *
 * @return      None
 *
 */
void p_dot_f32(float *a, float *b, float *c, int n)
{
    float tmp = 0.0f;
    int i;

    for (i = 0; i < n; i++) {
        tmp += *(a + i) * *(b + i);
    }
    *c = tmp;
}
