#pragma once
#include <pal.h>

static const PTYPE ln2 = PCONST(M_LN2);

/*
 * 0 <= x <= ln 2
 * e^-x = 1 + a1 * x + a2 * x^2 + ... + a4 * x^4 + e(x)
 * |e(x)| <= 3 * 10^-5
 */
static inline PTYPE __p_exp_ln2(const PTYPE x)
{
    const PTYPE a1 = PCONST(-0.9998684);
    const PTYPE a2 = PCONST(0.4982926);
    const PTYPE a3 = PCONST(-0.1595332);
    const PTYPE a4 = PCONST(0.0293641);
    PTYPE exp_x;
    exp_x = PCONST(1.0) +
        a1 * x +
        a2 * x * x +
        a3 * x * x * x +
        a4 * x * x * x * x;
    return PCONST(1.0) / exp_x;
}

/*
 * x >= 0
 * exp x = exp(x' + k ln 2) = (exp x') * 2^k
 */
static inline PTYPE __p_exp_pos(const PTYPE x)
{
    long int k, twok;
    PTYPE x_;
    k = x / ln2;
    twok = 1ULL << k;
    x_ = x - (PTYPE) k * ln2;
    return (PTYPE) twok * __p_exp_ln2(x_);
}

static inline PTYPE _p_exp(const PTYPE x)
{
    if (x >= PCONST(0.0))
        return __p_exp_pos(x);
    else
        return PCONST(1.0) / __p_exp_pos(-x);
}
