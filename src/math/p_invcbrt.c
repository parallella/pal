#include <pal.h>
#include <math.h>

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
 * @return      None
 *
 * @todo        Implement without using libm
 *
 */
void p_invcbrt_f32(const float *a, float *c, int n)
{
    union {
        float f;
        uint32_t x;
    } u;
    int i;
    float cur;
    for (i = 0; i < n; i++) {
        //*(c + i) = 1.0f / cbrtf(*(a + i));
        u.f = cbrtf(*(a + i));
        cur = u.f;
        
        /* First approximation */
        u.x = 0x7EEEEBB3 - u.x;
        /* Refine */
        u.f = u.f * (2 - u.f * cur);
        u.f = u.f * (2 - u.f * cur);
        u.f = u.f * (2 - u.f * cur);
        *(c + i) = u.f;
        
    }
}
