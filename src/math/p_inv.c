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
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */
#include <math.h>

void p_inv_f32(const float *a, float *c, int n, int p, p_team_t team)
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
        u.x = 0x7EF312AC - u.x;
        /* Refine */
        u.f = u.f * (2 - u.f * cur);
        u.f = u.f * (2 - u.f * cur);
        *(c + i) = u.f;
    }
}
