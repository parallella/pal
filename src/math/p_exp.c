#include <math.h>
#include <pal.h>

/*
 * 0 <= x <= ln 2
 * e^-x = 1 + a1 * x + a2 * x^2 + ... + a7 * x^7 + e(x)
 * |e(x)| <= 2 * 10^-10
 */
static inline float __p_exp_ln2(const float x)
{
    const float a1 = -0.9999999995f;
    const float a2 =  0.4999999206f;
    const float a3 = -0.1666653019f;
    const float a4 =  0.0416573475f;
    const float a5 = -0.0083013598f;
    const float a6 =  0.0013298820f;
    const float a7 = -0.0001413161f;
    float exp_x;
    exp_x = 1.f +
        a1 * x +
        a2 * x * x +
        a3 * x * x * x +
        a4 * x * x * x * x +
        a5 * x * x * x * x * x +
        a6 * x * x * x * x * x * x +
        a7 * x * x * x * x * x * x * x;
    return 1.f / exp_x;
}

/*
 * x > ln 2
 * exp x = exp(x' + ln 2) = (exp x') * 2
 */
static inline float __p_exp_pos(const float x)
{
    if (x <= M_LN2)
        return __p_exp_ln2(x);
    else
        return 2.f * __p_exp_pos(x - M_LN2);
}

static inline float _p_exp(const float x)
{
    if (x >= 0.f)
        return __p_exp_pos(x);
    else
        return 1.f / __p_exp_pos(-x);
}

/**
 *
 * Calculate the exponent e^a where e is Euler's number (2.71...).
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
void p_exp_f32(const float *a, float *c, int n)
{

    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_exp(a[i]);
    }
}
