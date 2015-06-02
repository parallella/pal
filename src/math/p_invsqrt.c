#include <pal.h>

/**
 *
 * Calculates the inverse square root of the input vector 'a' using Fast Inverse Square Root.
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
 **
 * @return      None
 *
 */
#include <math.h>
void p_invsqrt_f32(float *a, float *c, int n, int p, p_team_t team)
{
    long j;
    float half, y;

    int i;
    for (i = 0; i < n; i++) {
        half = 0.5F * *(a + i);
        y = *(a + i);
        i = *(long *) &y;
        i = 0x5f3759df - (i >> 1);
        y = *(float *) &i;
        y = y * (1.5F - half * y * y); // Repeat this line for more accuracy
        *(c + i) = y;
    }
}
