#include <pal.h>

/**
 *
 * Finds the maximum value in vector 'a'. Returns the index of the max
 *
 * @param a           Pointer to input vector
 *
 * @param c           Pointer to output scalar
 *
 * @param[out] index  Pointer to return index of max (note that if there are multiple cells with max, the first index will be returned)
 *
 * @param n      Size of 'a' vector.
 *
 * @return       None
 *
 */

void p_max_f32(const float *a, float *c, int *index, int n)
{
    if (n==0) return; // only do work if there are elements
    int pos = 0,i=1;
    
    for (; i < n; i++) {
        int greater = (*(a + i) > *(a + pos));
        pos = greater * i + (1 - greater) * pos;
    }
    
    *c = *(a + pos);
    *index = pos;
}
