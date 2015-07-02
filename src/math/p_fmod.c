#include <pal.h>

#include "p_fmod.h"

/**
 * Compute the floating-point remainder value of x / y.
 *
 * The floating-point remainder of x / y is r = x - i * y for some i,
 * such that 0 <= r <= y.
 *
 * @param a     Pointer to the input vector
 * @param c     Pointer to the output vector
 * @param n     The size of the a and c vectors
 * @param x     The divisor
 * @return      None
 */
void p_fmod_f32(const float *a, float *c, int n, const float x)
{
    _fmod(a, c, n, x);
}
