#include <pal.h>

#if (P_FLOAT_TYPE == P_FLOAT_SINGLE)
# define ABS_MASK 0x7FFFFFFF
#else
# define ABS_MASK 0x7FFFFFFFFFFFFFFFULL
#endif

/**
 *
 * Compute the absolute value of the vector 'a'.
 *
 * @param a     Pointer to input vector
 *
 * @param c     Pointer to result vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */

void PSYM(p_abs)(const PTYPE *a, PTYPE *c, int n)
{
    union {
        PTYPE f;
        PUTYPE u;
    } tmp;
    int i;

    for (i = 0; i < n; i++) {
        tmp.f = *(a + i);
        tmp.u &= ABS_MASK;
        *(c + i) = tmp.f;
    }
}
