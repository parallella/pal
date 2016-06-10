#include <pal.h>

/**
 *
 * Calculates the mean of input vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to output scalar
 *
 * @param n     Size of 'a' vector.
 *
 * @return      None
 *
 */
void PSYM(p_mean)(const PTYPE *a, PTYPE *c, int n)
{
    PTYPE tmp = 0.0f;
    int i;

    for (i = 0; i < n; i++) {
        tmp += *(a + i);
    }
    *c = tmp / n;
}
