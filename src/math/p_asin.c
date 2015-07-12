#include <pal.h>

#include "p_asin.h"

/**
 *
 * Caclulates the inverse sine (arc sine) of the argument 'a'. Arguments must be
 * in the range -1 to 1. The function does not check for illegal input values.
 * Results are in the range -pi/2 to pi/2.
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
void p_asin_f32(const float *a, float *c, int n)
{
    p_map_unary(&_p_asin, a, c, n);
}
