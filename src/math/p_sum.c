#include <pal.h>

/**
 *
 * Calculates the sum of all elements vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output scalar
 *
 * @param n     Size of 'a' vector.
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */

void p_sum_f32(float *a, float *c, int n, int p, p_team_t team)
{

    int i = 0;
    *c = 0.0f;
    for (i = 0; i < n; i++) {
        *c += *(a + i);
    }
}
