#include <pal.h>

/**
 *
 * Calculates the sum of the square of all of the elements vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output scalar
 *
 * @param n     Size of 'a' vector.
 *
 * @return      None
 *
 */

void p_sumsq_f32(const float *a, float *c, int n)
{
    float tmp = 0.0f;

    for (; n > 0; n--) {
        tmp += *(a) * *(a++);
    }
    *c = tmp;
}
