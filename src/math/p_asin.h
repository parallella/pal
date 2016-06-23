#pragma once
#include <pal.h>
#include "p_sqrt.h"

static const PTYPE pi_2 = (PTYPE) PCONST(M_PI) / PCONST(2.0);

/*
 * 0 <= x <= 1
 * asin x = pi/2 - (1 - x)^(1/2) * (a0 + a1 * x + ... + a3 * x^3) + e(x)
 * |e(x)| <= 5 * 10^-5
 */
static inline PTYPE __p_asin_pos(const PTYPE x)
{
    const PTYPE a0 = PCONST(1.5707288);
    const PTYPE a1 = PCONST(-0.2121144);
    const PTYPE a2 = PCONST(0.0742610);
    const PTYPE a3 = PCONST(-0.0187293);
    PTYPE a = _p_sqrt(PCONST(1.0) - x);
    return pi_2 - a * (a0 + a1 * x + a2 * x * x + a3 * x * x * x);
}

/*
 * -1 <= x <= 1
 * asin(-x) = - asin x
 */
static inline PTYPE _p_asin(const PTYPE x)
{
    if (x >= PCONST(0.0))
        return __p_asin_pos(x);
    else
        return PCONST(-1.0) * __p_asin_pos(-x);
}
