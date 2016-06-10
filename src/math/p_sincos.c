#include <pal.h>

/**
 *
 * Compute the sine and cosine of the vector 'a'.
 *
 * Angles are specified in * radians.
 * The radian number must be in the range 0 to 2pi,
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to sine output vector
 *
 * @param z     Pointer to cosine output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 * 
 * @return      None
 *
 */
void PSYM(p_sincos)(const PTYPE *a, PTYPE *c, PTYPE *z,
                  int n)
{
    PSYM(p_sin)(a, c, n);
    PSYM(p_cos)(a, z, n);
}
