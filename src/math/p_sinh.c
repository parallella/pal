#include <pal.h>

#include "p_exp.h"

/*
 * sinh z = (exp z - exp(-z)) / 2
 */
static inline PTYPE _p_sinh(const PTYPE z)
{
    PTYPE exp_z = _p_exp(z);
    return PCONST(0.5) * (exp_z - PCONST(1.0) / exp_z);
}

/**
 *
 * Calculates the hyperbolic sine of the vector 'a'. Angles are specified
 * in radians.
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
void PSYM(p_sinh)(const PTYPE *a, PTYPE *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_sinh(a[i]);
    }
}
