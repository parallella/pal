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
 * @return       None
 *
 */

void p_max_f32(float *a, float *c, int *index, int n)
{
    float max;
    int i, pos;

    max = *a;
    pos = 0;

    for (i = 1; i < n; i++) {
        if (*(a + i) > max) {
            pos = i;
            max = *(a + i);
        }
    }
    *c = max;
    *index = pos;
}
