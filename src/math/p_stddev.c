#include <pal.h>

/**
 *
 * Calculates the standard deviation of all of the elements vector 'a'.
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

void p_stddev_f32(const float *a, float *c, int n, int p, p_team_t team)
{
    float tmp = 0.0f, mean = 0.0f, meansq = 0.0f;
    int i;

    for (i = 0; i < n; i++) {
        tmp += *(a + i);
    }
    mean = tmp / n;

    tmp = 0.0f;
    for (i = 0; i < n; i++) {
        tmp += (*(a + i) - mean) * (*(a + i) - mean);
    }
    meansq = tmp / (n - 1);

    float x;
    int32_t j;
    float xhalf = 0.5f*meansq;

    j = *(int32_t*)(&meansq);
    j = 0x5f375a86 - (j>>1);
    x = *(float*)&j;

    // Newton steps, repeating this increases accuracy
    x = x*(1.5f - xhalf*x*x);
    x = x*(1.5f - xhalf*x*x);
    x = x*(1.5f - xhalf*x*x);

    // x contains the inverse sqrt

    // Multiply the inverse sqrt by the input to get the sqrt
    *c = meansq * x;
}