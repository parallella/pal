#include <pal.h>

/**
 *
 * Calculates the median value of input vector 'a'.
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

void p_median_f32(const float *a, float *c, int n, int p, p_team_t team)
{
    float copy[n];
    int i;
    for (i = 0; i < n; ++i)
        copy[i] = a[i];

    if (n & 1) {
        p_find_kth_f32(copy, c, n, n >> 1, p, team);
    } else {
        float m1, m2;
        p_find_kth_f32(copy, &m1, n, n >> 1, p, team);
        p_find_kth_f32(copy, &m2, n, (n>>1)+1, p, team);
        *c = (m1 + m2) * .5;
    }
}

