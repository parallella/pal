#include <pal.h>

/**
 *
 * Compute the absolute value of the vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to result vector
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
void p_abs_f32(float *a, float *c, int n, int p, p_team_t team)
{

    int i;
    for (i = 0; i < n; i++) {
        float v = *(a + i);
        *(c + i) = (-(*((int *) &v) & (1 << 31)) << 1 + 1) * v;
    }
}
