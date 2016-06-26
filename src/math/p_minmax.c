#include <pal.h>

/**
 *
 * Finds the minimum and maximum values in vector 'a'. Returns the min and max
 * values and the indices of the minimum and maximum values.
 *
 * @param      a      Pointer to input vector
 *
 * @param      c1     Pointer to output scalar (minimum)
 *
 * @param      c2     Pointer to output scalar (maximum)
 *
 * @param[out] index1 Pointer to return index of min
 *
 * @param[out] index2 Pointer to return index of max
 *
 * @param n           Size of 'a' vector.
 *
 * @return            None
 *
 */
void PSYM(p_minmax)(const PTYPE *a, PTYPE *c1, PTYPE *c2, int *index1, int *index2, int n)
{
    int pos_min = 0;
    int pos_max = 0;
    PTYPE min = *a;
    PTYPE max = *a;
    int i;

    for (i = 1; i < n; i++) {
        if (*(a + i) < min) {
            pos_min = i;
            min = *(a + i);
        }
        else if (*(a + i) > max) {
            pos_max = i;
            max = *(a + i);
        }
    }
    *c1 = min;
    *c2 = max;
    *index1 = pos_min;
    *index2 = pos_max;
}
