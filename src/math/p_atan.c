#include <pal.h>

/*
 * -1 <= x <= 1
 * atan x = a1 * x + a3 * x^3 + ... + a9 * x^9 + e(x)
 * |e(x)| <= 10^-5
 */
static inline float _p_atan(const float x)
{
    const float a1 =  0.9998660f;
    const float a3 = -0.3302995f;
    const float a5 =  0.1801410f;
    const float a7 = -0.0851330f;
    const float a9 =  0.0208351f;
    return a1 * x +
        a3 * x * x * x +
        a5 * x * x * x * x * x +
        a7 * x * x * x * x * x * x * x +
        a9 * x * x * x * x * x * x * x * x * x;
}

/**
 *
 * Calculates inverse tangent (arc tangent) of the input value. The function
 * returns a value between -pi/2 to pi/2 but does not check for illegal input
 * values.
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
void p_atan_f32(const float *a, float *c, int n)
{
    p_map_unary(&_p_atan, a, c, n);
}
