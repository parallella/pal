#include <pal.h>

#if (P_FLOAT_TYPE == P_FLOAT_SINGLE)
# define ABS_MASK 0x7FFFFFFF
#else
# define ABS_MASK 0x7FFFFFFFFFFFFFFFULL
#endif

/**
 *
 * Compute an element wise absolute difference of two vectors 'a' and 'b'.
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

void PSYM(p_absdiff)(const PTYPE *a, const PTYPE *b, PTYPE *c, int n)
{
    union {
        PTYPE f;
        PUTYPE u;
    } diff;
    for (int i = 0; i < n; i++) {
        diff.f = a[i] - b[i];
        diff.u &= ABS_MASK;
        c[i] = diff.f;
    }
}
