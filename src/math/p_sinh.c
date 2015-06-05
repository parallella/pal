#include <pal.h>

/**
 *
 * Calculates the hyperbolic sine of the vector 'a'. Angles are specified
 * in radians.
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
void p_sinh_f32(const float *a, float *c, int n, int p, p_team_t team)
{
    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = sinhf(*(a + i));
    }
}
