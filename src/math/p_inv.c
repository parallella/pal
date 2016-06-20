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
 /* https://www.pvk.ca/Blog/LowLevel/software-reciprocal.html */
# define INV_APPROX 0x7FDE623822FC16E6ULL
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
        u.f = u.f * (PCONST(2.0) - u.f * cur);
        u.f = u.f * (PCONST(2.0) - u.f * cur);
        u.f = u.f * (PCONST(2.0) - u.f * cur);
        *(c + i) = u.f;
    }
}
