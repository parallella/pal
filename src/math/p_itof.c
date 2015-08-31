#include <pal.h>

/**
 *
 * Converts integer values in 'a' to floating point values.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */
void p_itof(const int *a, float *c, int n)
{
int i=0;    // outside of for loop for C compatibility
    for(; i < n; i++)
        *(c + i) = (float)(*(a + i));
}
