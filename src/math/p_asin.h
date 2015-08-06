#pragma once

#include "p_sqrt.h"

static const float pi_2 = (float) M_PI / 2.f;

/*
 * 0 <= x <= 1
 * asin x = pi/2 - (1 - x)^(1/2) * (a0 + a1 * x + ... + a3 * x^3) + e(x)
 * |e(x)| <= 5 * 10^-5
 */
static inline float __p_asin_pos(const float x)
{
    const float a0 =  1.5707288f;
    const float a1 = -0.2121144f;
    const float a2 =  0.0742610f;
    const float a3 = -0.0187293f;
    float a = _p_sqrt(1.f - x);
    return pi_2 - a * (a0 + a1 * x + a2 * x * x + a3 * x * x * x);
}

/*
 * -1 <= x <= 1
 * asin(-x) = - asin x
 */
static inline float _p_asin(const float x)
{
    if (x >= 0.f)
        return __p_asin_pos(x);
    else
        return -1.f * __p_asin_pos(-x);
}
