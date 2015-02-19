/**
 *
 * Compute the sine of the vector 'a'. Angles are specified in radians.
 * The radian number must be in the range 0 to 2pi,
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
#include "pal_math.h"
void p_sin_f32(float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = sinf(*(a + i));
    }
}
