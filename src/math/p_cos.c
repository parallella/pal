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

void PSYM(p_cos)(const PTYPE *a, PTYPE *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        const PTYPE *pa = (a+i);
        PTYPE *pc = (c+i);
        PTYPE val = 1;
        int k;
        PTYPE theta = *pa;
	PTYPE theta2 = *pa * *pa;
        //PTYPE theta = M_NORMALIZE_RADIANS(*pa);

        //for(k=COS_ITERATIONS-1; k>=0; --k)
          //val = 1 - theta*theta/(2*k+2)/(2*k+1)*val;

        val = PCONST(1.0) - theta2 * PCONST(0.083333333) * PCONST(0.090909090) * val;
        val = PCONST(1.0) - theta2 * PCONST(0.10000000) * PCONST(0.11111111) * val;
        val = PCONST(1.0) - theta2 * PCONST(0.12500000) * PCONST(0.14285714) * val;
        val = PCONST(1.0) - theta2 * PCONST(0.16666667) * PCONST(0.20000000) * val;
        val = PCONST(1.0) - theta2 * PCONST(0.25000000) * PCONST(0.33333333) * val;
        val = PCONST(1.0) - theta2 * PCONST(0.50000000) * PCONST(1.00000000) * val;

        *pc = val;

    }
}
