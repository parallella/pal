#include <pal.h>

/**
 *
 * Element wise inversion (reciprocal) of elements in 'a'.
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

#if (P_FLOAT_TYPE == P_FLOAT_SINGLE)
# define INV_APPROX 0x7EEEEBB3
#else
 /* TODO: What is the optimal first approx. for double precision? */
# define INV_APPROX 0x7EEEEEEEEEEEEEEEULL
#endif

void PSYM(p_inv)(const PTYPE *a, PTYPE *c, int n)
{
    int i;
    PTYPE cur;
    for (i = 0; i < n; i++) {
        cur = *(a + i);
        union {
            PTYPE f;
            PUTYPE x;
        } u = {cur};

        /* First approximation */
        u.x = INV_APPROX - u.x;

        /* Refine */
        u.f = u.f * (2.0 - u.f * cur);
        u.f = u.f * (2.0 - u.f * cur);
        u.f = u.f * (2.0 - u.f * cur);
        *(c + i) = u.f;
    }
}
