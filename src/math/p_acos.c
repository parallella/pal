#include <pal.h>

#include "p_asin.h"

/* acos z = pi/2 - asin z */
static inline float _p_acos(const float z)
{
    return pi_2 - _p_asin(z);
}

/**
 *
 * Computes the inverse cosine (arc cosine) of the input vector 'a'. Input
 * values to acos must be in the range -1 to 1. The result values are in the
 * range 0 to pi. The function does not check for illegal input values.
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
void p_acos_f32(const float *a, float *c, int n)
{
    p_map_unary(&_p_acos, a, c, n);
}
