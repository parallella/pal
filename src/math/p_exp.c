#include <pal.h>

#include "p_exp.h"

/**
 *
 * Calculate the exponent e^a where e is Euler's number (2.71...).
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
void p_exp_f32(const float *a, float *c, int n)
{
    p_map_unary(&_p_exp, a, c, n);
}
