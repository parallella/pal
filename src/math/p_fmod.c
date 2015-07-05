#include <pal.h>

#include "p_fmod.h"

/**
 * Compute the floating-point remainder value of a[i] / x.
 *
 * The floating-point remainder of a[i] / x is r = a[i] - j * x
 * for some integer j, for a[i] >= 0, and r = a[i] - j * x + x
 * for a[i] < 0, such that 0 <= r <= x.
 *
 * @param a     Pointer to the input vector
 * @param c     Pointer to the output vector
 * @param n     The size of the a and c vectors
 * @param x     The divisor
 * @return      None
 */
void p_fmod_f32(const float *a, float *c, int n, const float x)
{
    _p_fmod(a, c, n, x);
}

/**
 * Compute the floating-point remainder with divisor 2pi.
 *
 * See p_fmod_f32 for details.
 */
void p_fmod_2pi_f32(const float *a, float *c, int n)
{
    _p_fmod(a, c, n, 2.f * M_PI);
}
