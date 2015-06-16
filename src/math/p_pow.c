#include <pal.h>

/**
 *
 * Calculate 'a' raised to the exponent 'b'
 *
 * @param a     Pointer to input vector
 *
 * @param b     Pointer to input vector
 *
 * @param c     Pointer to output vector
 *
 * @param n     Size of 'a' and 'c' vector.
 *
 * @return      None
 *
 */

void p_pow_f32(const float *a, const float *b, float *c, int n)
{
    int i = 0;
    float x, *c_ = c;
    const float *a_ = a, *b_ = b;
    while (i < n) {
		/* a^b = e^(b*ln(a)) */
		p_ln_f32(a_++, &x, 1);
		x *= *b_++;
		p_exp_f32(&x, c_++, 1);
		++i;
	}
}
