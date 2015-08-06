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
 * @return      None
 *
 */

#define COS_ITERATIONS 5

void p_cos_f32(const float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        const float *pa = (a+i);
        float *pc = (c+i);
        float val = 1;
        int k;
        float theta = *pa;
        //float theta = M_NORMALIZE_RADIANS(*pa);

        //for(k=COS_ITERATIONS-1; k>=0; --k)
          //val = 1 - theta*theta/(2*k+2)/(2*k+1)*val;

        val = 1.0f - theta*theta*0.083333333f*0.090909090f*val;
        val = 1.0f - theta*theta*0.10000000f*0.11111111f*val;
        val = 1.0f - theta*theta*0.12500000f*0.14285714f*val;
        val = 1.0f - theta*theta*0.16666667f*0.20000000f*val;
        val = 1.0f - theta*theta*0.25000000f*0.33333333f*val;
        val = 1.0f - theta*theta*0.50000000f*1.00000000f*val;

        *pc = val;

    }
}
