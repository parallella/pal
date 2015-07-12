#include <pal.h>

#include "p_exp.h"

/*
 * tanh z = sinh z / cosh z
 *        = (exp z - exp -z) / (exp z + ezp -z)
 *        = (exp 2z - 1) / (exp 2z + 1)
 */
static inline float _p_tanh(const float z) __attribute__((always_inline));
static inline float _p_tanh(const float z)
{
    float exp_2z = _p_exp(2.f * z);
    return (exp_2z - 1.f) / (exp_2z + 1.f);
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
void p_tanh_f32(const float *a, float *c, int n)
{
    p_map_unary(&_p_tanh, a, c, n);
}
