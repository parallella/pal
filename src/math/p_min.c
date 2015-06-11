#include <pal.h>

/**
 *
 * Finds the minimum value in vector 'a'. Returns the min value and the index
 * of the minimum value.
 *
 * @param      a      Pointer to input vector
 *
 * @param      c      Pointer to output scalar
 *
 * @param[out] index  Pointer to return index of min
 *
 * @param n           Size of 'a' vector.
 * 
 * @param p           Number of processor to use (task parallelism)
 *
 * @param team        Team to work with 
 *
 * @return            None
 *
 */
void p_min_f32(const float *a, float *c, int *index,
               int n, int p, p_team_t team)
{
    if (n==0) return; // only do work if there are elements
    int i, pos = 0;

    for (i = 1; i < n; i++) {
        if (*(a + i) < *(a + pos)) {
            pos = i;
        }
    }
    
    *c = *(a + pos);
    *index = pos;
}
