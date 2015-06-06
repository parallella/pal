#include <pal.h>

/**
 *
 * Element wise approximate inversion (reciprocal) of elements in 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @param it    The number of iterations to run; also, a function of the maximum magnitude that can be run.
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return      None
 *
 */
#include <math.h>
void p_a_inv_f32(const float *a, float *c, int n, int p,
                 int iterations, p_team_t team)
{
    int i, j;
    float max = iterations * iterations * iterations;
    float inv_max = 1.0/max; // only computed once, hence speedups
    for (i = 0; i < n; i++) {
        float ai = a[i];
        int negate = ai < 0;

        if (negate) ai *= -1;
        c[i] = ai > 1 ? inv_max : max;
        for (j = 0; j < iterations; j++)
            c[i] *= (2 - ai * c[i]);
        if (negate)
            c[i] *= -1;
    }
}
