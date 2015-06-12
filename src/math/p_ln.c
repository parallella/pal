#include <pal.h>

/**
 *
 * Calculates the natural logarithm of 'a', (where the base is 'e'=2.71828)
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

void p_ln_f32(const float *a, float *c, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        union
        {
            float f;
            uint32_t i;
        } u = { *(a + i) };

        // Calculate the exponent (which is the floor of the logarithm) minus one
        int e = ((u.i >> 23) & 0xff) - 0x80;

        // Mask off the exponent, leaving just the mantissa
        u.i = (u.i & 0x7fffff) + 0x3f800000;

        // Interpolate using a cubic minimax polynomial derived with
        // the Remez exchange algorithm. Coefficients courtesy of Alex Kan.
        // This approximates 1 + log2 of the mantissa.
        float r = ((0.15824870f * u.f - 1.05187502f) * u.f + 3.04788415f) * u.f - 1.15360271f;

        // The log2 of the complete value is then the sum
        // of the previous quantities (the 1's cancel), and
        // we find the natural log by scaling by log2(e).
        *(c + i) = (e + r) * 0.69314718f;
    }
}
