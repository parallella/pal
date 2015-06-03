#include <pal.h>

/**
 *
 * Finds the maximum value in vector 'a'. Returns the index of the max
 *
 * @param a           Pointer to input vector
 *
 * @param c           Pointer to output scalar
 *
 * @param[out] index  Pointer to return index of max
 *
 * @param n      Size of 'a' vector.
 *
 * @param p     Number of processor to use (task parallelism)
 *
 * @param team  Team to work with 
 *
 * @return       None
 *
 */

void p_max_f32(float *a, float *c, int *index, int n, int p, p_team_t team)
{
    float max;
    int i, pos;

    max = *a;
    pos = 0;

    for (i = 1; i < n; i++) {
        int greater = (*(a + i) > max);
        pos = greater * i + (1 - greater) * pos;
        max = greater * *(a + i) + (1 - greater) * max;
    }
    *c = max;
    *index = pos;
}
