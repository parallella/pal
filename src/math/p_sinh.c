#include <pal.h>

#include "p_exp.h"

/*
 * sinh z = (exp z - exp(-z)) / 2
 */
static inline float _p_sinh(const float z)
{
    float exp_z = _p_exp(z);
    return 0.5f * (exp_z - 1.f / exp_z);
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
void p_sinh_f32(const float *a, float *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_sinh(a[i]);
    }
}
