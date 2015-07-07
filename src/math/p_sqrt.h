#pragma once

/*
 * This uses a method to approximate sqrt which only applies to IEEE 754 floating point numbers,
 * described in [1]. The optimized magic constant is from Chris Lomont[2]
 *
 * References:
 * 1: http://en.wikipedia.org/wiki/Fast_inverse_square_root
 * 2: http://www.lomont.org/Math/Papers/2003/InvSqrt.pdf
 */
static inline float _p_sqrt(const float z)
{
    float x;
    union {
        float f;
        int32_t i;
    } j;
    float xhalf = 0.5f*z;

    j.f = z;
    j.i = 0x5f375a86 - (j.i >> 1);
    x = j.f;

    // Newton steps, repeating this increases accuracy
    x = x*(1.5f - xhalf*x*x);
    x = x*(1.5f - xhalf*x*x);
    x = x*(1.5f - xhalf*x*x);

    // x contains the inverse sqrt

    // Multiply the inverse sqrt by the input to get the sqrt
    return z * x;
}
