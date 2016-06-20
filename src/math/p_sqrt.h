#pragma once

/*
 * This uses a method to approximate sqrt which only applies to IEEE 754 floating point numbers,
 * described in [1]. The optimized magic constant is from Chris Lomont[2]
 *
 * References:
 * 1: http://en.wikipedia.org/wiki/Fast_inverse_square_root
 * 2: http://www.lomont.org/Math/Papers/2003/InvSqrt.pdf
 */

#if (P_FLOAT_TYPE == P_FLOAT_SINGLE)
# define ISQRT_APPROX 0x5f375a86
#else
# define ISQRT_APPROX 0x5fe6eb50c7b537a9ULL
#endif

static inline PTYPE _p_sqrt(const PTYPE z)
{
    PTYPE x;
    union {
        PTYPE f;
        PUTYPE i;
    } j;
    PTYPE xhalf = PCONST(0.5) * z;

    j.f = z;
    j.i = ISQRT_APPROX - (j.i >> 1);
    x = j.f;

    // Newton steps, repeating this increases accuracy
    x = x*(PCONST(1.5) - xhalf*x*x);
    x = x*(PCONST(1.5) - xhalf*x*x);
    x = x*(PCONST(1.5) - xhalf*x*x);

    // x contains the inverse sqrt

    // Multiply the inverse sqrt by the input to get the sqrt
    return z * x;
}
