#include <math.h>
#include <pal.h>

static const float pi_4 = (float) M_PI / 4.f;
static const float pi_2 = (float) M_PI / 2.f;
static const float pi =   (float) M_PI;

/*
 * 0 <= x <= pi/4
 * tan x / x = 1 + a2 * x^2 + a4 * x^4 + ... + a12 * x^12 + e(x)
 * |e(x)| <= 2 * 10^-8
 */
static inline float __p_tan_pi_4(const float x)
{
    const float  a2 = 0.3333314036f;
    const float  a4 = 0.1333923995f;
    const float  a6 = 0.0533740603f;
    const float  a8 = 0.0245650893f;
    const float a10 = 0.0029005250f;
    const float a12 = 0.0095168091f;
    float x2, tanx_x;
    x2 = x * x;
    tanx_x = 1.f +
         a2 * x2 +
         a4 * x2 * x2 +
         a6 * x2 * x2 * x2 +
         a8 * x2 * x2 * x2 * x2 +
        a10 * x2 * x2 * x2 * x2 * x2 +
        a12 * x2 * x2 * x2 * x2 * x2 * x2;
    return tanx_x * x;
}

/*
 * 0 <= x <= pi/2
 * x = x' + pi/4
 * tan x = tan(x' + pi/4) = (tan x' + 1) / (1 - tan x')
 */
static inline float __p_tan_pi_2(const float x)
{
    float x_, tanx_;
    if (x <= pi_4)
        return __p_tan_pi_4(x);
    x_ = x - pi_4;
    tanx_ = __p_tan_pi_4(x_);
    return (tanx_ + 1.f) / (1.f - tanx_);
}

/*
 * 0 <= x <= pi
 * x = x' + pi/2
 * tan x = tan (x' + pi/2) = -1 / tan x'
 */
static inline float __p_tan_pi(const float x)
{
    float x_;
    if (x <= pi_2)
        return __p_tan_pi_2(x);
    x_ = x - pi_2;
    return -1.f / __p_tan_pi_2(x_);
}

/* 0 <= x <= 2pi */
static inline float _p_tan(const float x)
{
    if (x <= pi)
        return __p_tan_pi(x);
    else
        return __p_tan_pi(x - pi);
}

/**
 *
 * Calculates the tangent of the input vector 'a'.
 * Angles are specified in radians.
 * Input is assumed to be bound to [0, 2pi].
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
void p_tan_f32(const float *a, float *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_tan(a[i]);
    }
}
