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

void p_median_f32(float *a, float *c, int n, int p, p_team_t team)
{
    float sort[n];

    p_sort_f32(a, sort, n, p, team);

    if(n%2)
        *c=a[n>>1];
    else
        *c=(a[n>>1] + a[(n-1)>>1])*.5;
}
