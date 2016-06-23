#include <pal.h>

/**
 *
 * Calculates the dot product between vectors 'a' and 'b', producing
 * a scalar result 'c'.
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input vector
 *
 * @param c     Pointer to output scalar
 *
 * @param n     Size of 'a', 'b'
 *
 * @return      None
 *
 */
void PSYM(p_dot)(const PTYPE *a, const PTYPE *b, PTYPE *c, int n )
{
    PTYPE tmp = PCONST(0.0);
    int i;

    for (i = 0; i < n; i++) {
        tmp += *(a + i) * *(b + i);
    }
    *c = tmp;
}
