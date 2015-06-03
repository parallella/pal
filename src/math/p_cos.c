#include <pal.h>

/**
 *
 * Compute the cosine of the vector 'a'. Angles are specified in radians.
 * The radian number must be in the range 0 to 2pi,
 *
 * Author: Matt Thompson <mthompson@hexwave.com>
 * Date: Jun 2, 2015
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

#define COS_ITERATIONS 5

void p_cos_f32(float *a, float *c, int n, int p, p_team_t team)
{

    int i;
    for (i = 0; i < n; i++) {
        float *pa = (a+i);
        float *pc = (c+i);
        float val = 1;
        int k;
        float theta = M_NORMALIZE_RADIANS(*pa);

        for(k=COS_ITERATIONS-1; k>=0; --k)
          val = 1 - theta*theta/(2*k+2)/(2*k+1)*val;

        *pc = val;

    }
}
