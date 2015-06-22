#include <pal.h>

/**
 *
 * Compute the absolute value of the vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to result vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */
void p_abs_f32(const float *a, float *c, int n)
{
    union {
        float f;
        uint32_t u;
    } tmp;
    int i;

    for (i = 0; i < n; i++) {
        tmp.f = *(a + i);
        tmp.u &= 0x7FFFFFFF;
        *(c + i) = tmp.f;
    }
}
