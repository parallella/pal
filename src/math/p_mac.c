#include <pal.h>

/**
 *
 * Element wise multiply accumulate: c += a * b'
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */

void PSYM(p_mac)(const PTYPE *a, const PTYPE *b, PTYPE *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        *(c + i) += *(a + i) * *(b + i);
    }
}
