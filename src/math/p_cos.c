#include <pal.h>

/**
 *
 * Compute the cosine of the vector 'a'. Angles are specified in radians.
 * The radian number must be in the range 0 to 2pi,
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

void p_cos_f32(const float *a, float *c, int n, int p, p_team_t team)
{

    int i;
    for (i = 0; i < n; i++) {
        const float *pa = (a+i);
        float *pc = (c+i);
        float val = 1;
        int k;
        float theta = M_NORMALIZE_RADIANS(*pa);

        //for(k=COS_ITERATIONS-1; k>=0; --k)
          //val = 1 - theta*theta/(2*k+2)/(2*k+1)*val;

        val = 1 - theta*theta*0.10000000*0.11111111*val;
        val = 1 - theta*theta*0.12500000*0.14285714*val;
        val = 1 - theta*theta*0.16666667*0.20000000*val;
        val = 1 - theta*theta*0.25000000*0.33333333*val;
        val = 1 - theta*theta*0.50000000*1.00000000*val;

        *pc = val;

    }
}
