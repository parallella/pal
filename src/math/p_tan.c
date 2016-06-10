#include <pal.h>

static const PTYPE pi_4 = (PTYPE) M_PI / 4.0;
static const PTYPE pi_2 = (PTYPE) M_PI / 2.0;
static const PTYPE pi =   (PTYPE) M_PI;

/*
 * 0 <= x <= pi/4
 * tan x / x = 1 + a2 * x^2 + a4 * x^4 + ... + a12 * x^12 + e(x)
 * |e(x)| <= 2 * 10^-8
 */
static inline PTYPE __p_tan_pi_4(const PTYPE x)
{
    const PTYPE  a2 = 0.3333314036;
    const PTYPE  a4 = 0.1333923995;
    const PTYPE  a6 = 0.0533740603;
    const PTYPE  a8 = 0.0245650893;
    const PTYPE a10 = 0.0029005250;
    const PTYPE a12 = 0.0095168091;
    PTYPE x2, tanx_x;
    x2 = x * x;
    tanx_x = 1.f + x2 * (a2 + x2 * (a4 + x2 * (a6 + x2 * (a8 + x2 * (a10 + x2 * a12)))));
    return tanx_x * x;
}

/*
 * 0 <= x <= pi/2
 * x = x' + pi/4
 * tan x = tan(x' + pi/4) = (tan x' + 1) / (1 - tan x')
 */
static inline PTYPE __p_tan_pi_2(const PTYPE x)
{
    PTYPE x_, tanx_;
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
static inline PTYPE __p_tan_pi(const PTYPE x)
{
    PTYPE x_;
    if (x <= pi_2)
        return __p_tan_pi_2(x);
    x_ = x - pi_2;
    return -1.f / __p_tan_pi_2(x_);
}

/* 0 <= x <= 2pi */
static inline PTYPE _p_tan(const PTYPE x)
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
void PSYM(p_tan)(const PTYPE *a, PTYPE *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        c[i] = _p_tan(a[i]);
    }
}
