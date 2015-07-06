#include <math.h>
#include <pal.h>

/*
 * 0 <= x <= ln 2
 * e^-x = 1 + a1 * x + a2 * x^2 + ... + a4 * x^4 + e(x)
 * |e(x)| <= 3 * 10^-5
 */
static inline float __p_exp_ln2(const float x)
{
    const float a1 = -0.9998684f;
    const float a2 =  0.4982926f;
    const float a3 = -0.1595332f;
    const float a4 =  0.0293641f;
    float exp_x;
    exp_x = 1.f +
        a1 * x +
        a2 * x * x +
        a3 * x * x * x +
        a4 * x * x * x * x;
    return 1.f / exp_x;
}

static const float ln2 = (float) M_LN2;

/*
 * x >= 0
 * exp x = exp(x' + k ln 2) = (exp x') * 2^k
 */
static inline float __p_exp_pos(const float x)
{
    long int k, twok;
    float x_;
    k = x / ln2;
    twok = 1U << k;
    x_ = x - (float) k * ln2;
    return (float) twok * __p_exp_ln2(x_);
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
