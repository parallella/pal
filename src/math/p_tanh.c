#include <pal.h>

#include "p_exp.h"

/*
 * tanh z = sinh z / cosh z
 *        = (exp z - exp -z) / (exp z + ezp -z)
 *        = (exp 2z - 1) / (exp 2z + 1)
 */
static inline PTYPE _p_tanh(const PTYPE z)
{
    PTYPE exp_2z = _p_exp(PCONST(2.0) * z);
    return (exp_2z - PCONST(1.0)) / (exp_2z + PCONST(1.0));
}

/**
 *
 * Calculates the hyperbolic tangent of the input vector 'a'.
 * Angles are specified in radians.
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
void PSYM(p_tanh)(const PTYPE *a, PTYPE *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_tanh(a[i]);
    }
}
