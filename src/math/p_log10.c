#include <pal.h>

/**
 *
 * Calculates the base 10 logarithm of 'a'.
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
#include <math.h>
void p_log10_f32(const float *a, float *c, int n)
{
    p_ln_f32(a, c, n, p, team);
    int i;
    for (i = 0; i < n; i++) {
	    // log10(x) = ln(x) / ln(10)
	    c[i] /= 2.302585;
    }
}
