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
void p_abs_f32(const float *a, float *c, int n, int p, p_team_t team)
{
    uint32_t tmp;
    int i;

    for (i = 0; i < n; i++) {
        tmp = *(uint32_t*)(a + i);
        tmp &= 0x7FFFFFFF;
        *(c + i) = *(float*)&tmp;
    }
}
