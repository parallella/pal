#include <pal.h>

/**
 *
 * Calculates the inverse cube root of the input vector 'a'.
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
void p_invcbrt_f32(const float *a, float *c, int n, int p, p_team_t team)
{
    int i;
    union {
        int32_t x;
        float f;
    } u;
    for (i = 0; i < n; i++) {
        //*(c + i) = 1.0f / cbrtf(*(a + i));
        u.f = cbrtf(*(a + i));
        
        /* First approximation */
        u.x = 0x7EEEEBB3 - u.x;
        /* Refine */
        u.f = u.f * (2 - u.f * cur);
        u.f = u.f * (2 - u.f * cur);
        u.f = u.f * (2 - u.f * cur);
        *(c + i) = u.f;
        
    }
}
