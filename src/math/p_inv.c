#include <pal.h>

/**
 *
 * Element wise inversion (reciprocal) of elements in 'a'.
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
#include <math.h>

void p_inv_f32(const float *a, float *c, int n)
{
    int i;
    float cur;
    for (i = 0; i < n; i++) {
        cur = *(a + i);
        union {
            float f;
            uint32_t x;
        } u = {cur};
        /* First approximation */
        u.x = 0x7EEEEBB3 - u.x;
        /* Refine */
        u.f = u.f * (2 - u.f * cur);
        u.f = u.f * (2 - u.f * cur);
        u.f = u.f * (2 - u.f * cur);
        *(c + i) = u.f;
    }
}
