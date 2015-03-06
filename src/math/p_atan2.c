#include <pal.h>

/**
 *
 * Calculates the inverse tangent (arc tangent) of b/a. Stability not guaranteed
 * for 'a' values near 0. Results are in the range of -pi to pi.
 *
 * @param a     Pointer to denominator input vector
 *
 * @param b     Pointer to numerator input vector
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
void p_atan2_f32(float *a, float *b, float *c, int n, int p, p_team_t team)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) = atan2f(*(b + i), *(a + i));
    }
}
